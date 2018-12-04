-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for reyong and trompong.
module Synth.Sampler.Patch.Reyong (patches, checkFilenames) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Map
import qualified Util.Seq as Seq
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.C.Prelude.Note as Prelude.Note
import qualified Derive.Call as Call
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Legong as Legong

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Code as Code
import qualified Synth.Sampler.Patch.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


patches :: [Patch.Patch]
patches =
    [ makePatch "reyong-trompong"
        (Scale.Range
            (Scale.range_bottom Legong.trompong_range)
            (Scale.range_top Legong.reyong_range))
    , makePatch "reyong" Legong.reyong_range
    , makePatch "trompong" Legong.trompong_range
    ]

makePatch :: Note.PatchName -> Scale.Range -> Patch.Patch
makePatch name range = (Patch.patch name)
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._preprocess = inferDuration
    , Patch._karyaPatch = ImInst.code #= code $ ImInst.range range $
        ImInst.make_patch $ Im.Patch.patch
            { Im.Patch.patch_controls = mconcat
                [ Control.supportPitch
                , Control.supportDyn
                , Control.supportVariation
                ]
            , Im.Patch.patch_attribute_map = const () <$> attributeMap
            }
    }
    where
    code = note <> Util.thru dir convert
    note = Bali.zero_dur_mute_with ""
        (\args -> transform args . Call.multiply_dynamic 0.65)
        (\args -> transform args $
            Prelude.Note.default_note Prelude.Note.use_attributes args)
        where transform args = Code.withSymbolicPitch args . Code.withVariation
    dir = "reyong"

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (cek <> loose, CekOpen)
    , (cek, CekClosed)
    , (mute <> loose, MuteOpen)
    , (mute, MuteClosed)
    , (mempty, Open)
    ]
    where
    mute = Attrs.mute
    loose = Attrs.loose
    -- TODO from Derive.C.Bali.Reyong, or from a common attrs module
    cek = Attrs.attr "cek"

-- * preprocess

inferDuration :: [Note.Note] -> [Note.Note]
inferDuration = map infer . Util.nexts
    where
    infer (note, nexts) = case inferEnd note nexts of
        Nothing -> note
        Just end -> note { Note.duration = end - Note.start note }

-- | Open notes ring until a mute at the same pitch.
inferEnd :: Note.Note -> [Note.Note] -> Maybe RealTime
inferEnd note nexts = case articulationOf note of
    Open -> case List.find isMute nexts of
        Nothing -> Just Sample.forever
        Just mute -> Just $ Note.start mute
    CekClosed -> Just Sample.forever
    CekOpen -> Just Sample.forever
    MuteClosed -> Nothing
    MuteOpen -> Nothing
    where
    pitch :: Either Text (Either Pitch.Note Pitch.NoteNumber)
    pitch = Util.symbolicPitch note
    isMute next =
        Util.symbolicPitch next == pitch && case articulationOf next of
            MuteClosed -> True
            MuteOpen -> True
            _ -> False
    articulationOf =
        Util.articulationDefault Open attributeMap . Note.attributes

-- * checks

checkFilenames :: IO [FilePath]
checkFilenames = filterM (fmap not . exists) allFilenames
    where exists = Directory.doesFileExist . ("../data/sampler/reyong" </>)

allFilenames :: [FilePath]
allFilenames = map fst3 $ Either.rights
    [ toFilename articulation (Left pitch) dyn variation
    | articulation <- Util.enumAll
    , pitch <- map snd (Map.elems nnKeys)
    , dyn <- Util.enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]
    where fst3 (a, _, _) = a

-- * convert

convert :: Note.Note -> Patch.ConvertM (RealTime, Sample.Sample)
convert note = do
    let articulation = Util.articulationDefault Open attributeMap $
            Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamicRange minDyn note
    symPitch <- Util.symbolicPitch note
    let var = Util.variation (variationsOf articulation) note
    (filename, noteNn, sampleNn) <-
        tryRight $ toFilename articulation symPitch dyn var
    -- Log.debug $ "note at " <> pretty (Note.start note) <> ": "
    --     <> pretty ((dyn, scale), (symPitch, sampleNn), var)
    --     <> ": " <> txt filename
    let dur
            | isMute articulation = Sample.forever
            | otherwise = Note.duration note + muteTime
    return $ (dur,) $ Sample.Sample
        { filename = filename
        , offset = 0
        , envelope = if isMute articulation
            then Signal.constant dynVal
            else Signal.from_pairs
                [ (Note.start note, dynVal), (Note.end note, dynVal)
                , (Note.end note + muteTime, 0)
                ]
        , ratio = Signal.constant $
            Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) noteNn
        }

isMute :: Articulation -> Bool
isMute = \case
    MuteClosed -> True
    MuteOpen -> True
    _ -> False

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.085

minDyn :: Signal.Y
minDyn = 0.5

{- |
    > 45-1-31-cek+{closed,open}+v{1..6}.wav
    > 45-1-31-mute+{closed,open}+v{1..4}.wav
    > 45-109-127-open+v{1..4}.wav

    keys: 45 48 50 52 55 57 60 62 64 67 69 72 74 76 79
-}
toFilename :: Articulation -> Either Pitch.Note Pitch.NoteNumber -> Dynamic
    -> Util.Variation
    -> Either Text (FilePath, Pitch.NoteNumber, Pitch.NoteNumber)
toFilename articulation symPitch dyn variation = do
    (sampleNn, noteNn, Midi.Key sampleKey) <- findPitch symPitch
    return (sampleName sampleKey ++ ".flac", noteNn, sampleNn)
    where
    sampleName sampleKey = Seq.join "-"
        [show sampleKey, show lowVel, show highVel, group]
    (lowVel, highVel) = dynamicRange dyn
    group = articulationFile articulation <> "+v" <> show (variation + 1)

articulationFile :: Articulation -> String
articulationFile = \case
    CekClosed -> "cek+closed"
    CekOpen -> "cek+open"
    MuteClosed -> "mute+closed"
    MuteOpen -> "mute+open"
    Open -> "open"

findPitch :: Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch.NoteNumber, Pitch.NoteNumber, Midi.Key)
findPitch = \case
    Left sym -> do
        (sampleNn, (key, _)) <- tryJust ("invalid pitch: " <> pretty sym) $
            List.find ((==sym) . snd . snd) (Map.toList nnKeys)
        return (sampleNn, sampleNn, key)
    Right noteNn -> return (sampleNn, noteNn, key)
        where
        Just (sampleNn, (key, _)) = Util.Map.lookup_closest noteNn nnKeys

--             trompong---------------------
--                      reyong-----------------------------
-- 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
nnKeys :: Map Pitch.NoteNumber (Midi.Key, Pitch.Note)
nnKeys = Map.fromList $ zip reyongTuning $ take 15 $ drop 4
    [ (key+oct*12, Pitch.Note (showt (Midi.from_key oct) <> sym))
    | oct <- [3..], (key, sym) <- baseKeys
    ]
    where
    baseKeys =
        [ (Key.c_1, "i")
        , (Key.d_1, "o")
        , (Key.e_1, "e")
        , (Key.g_1, "u")
        , (Key.a_1, "a")
        ]

reyongTuning :: [Pitch.NoteNumber]
reyongTuning =
    [ 56.77
    , 60.83 -- 4i
    , 62.82
    , 63.36
    , 67.72
    , 68.35
    , 72.60 -- 5i
    , 74.09
    , 75.54
    , 79.45
    , 80.50
    , 84.53 -- 6i
    , 86.08
    , 87.82
    , 91.82
    ]


data Articulation = CekClosed | CekOpen | MuteClosed | MuteOpen | Open
    deriving (Eq, Ord, Show, Enum, Bounded)

variationsOf :: Articulation -> Util.Variation
variationsOf = \case
    CekClosed -> 6
    CekOpen -> 6
    MuteClosed -> 4
    MuteOpen -> 4
    Open -> 4

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Enum, Bounded, Show)
instance Pretty Dynamic where pretty = showt

dynamicRange :: Dynamic -> (Int, Int)
dynamicRange = \case
    PP -> (1, 31)
    MP -> (32, 64)
    MF -> (65, 108)
    FF -> (109, 127)

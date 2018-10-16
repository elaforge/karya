-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Reyong (patches, checkFilenames) where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Legong as Legong

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
-- import qualified Synth.Sampler.Patch.ReyongCode as ReyongCode
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
        (Scale.Range (Scale.range_bottom Legong.trompong_range)
            (Scale.range_top Legong.reyong_range))
    , makePatch "reyong" Legong.reyong_range
    , makePatch "trompong" Legong.trompong_range
    ]

makePatch :: Note.PatchName -> Scale.Range -> Patch.Patch
makePatch name range = Patch.Patch
    { _name = name
    , _dir = "reyong"
    , _convert = convert
    -- , _karyaPatch = ImInst.code #= ReyongCode.code $ ImInst.range range $
    , _karyaPatch = ImInst.range range $
        ImInst.default_scale Legong.scale_id $
        ImInst.make_patch $ Im.Patch.patch
            { Im.Patch.patch_controls = mconcat
                [ Control.supportPitch
                , Control.supportDyn
                , Control.supportVariation
                ]
            , Im.Patch.patch_attribute_map = const () <$> attributeMap
            }
    }

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (cek <> open, CekOpen)
    , (cek, CekClosed)
    , (mute <> open, MuteOpen)
    , (mute, MuteClosed)
    , (mempty, Open)
    ]
    where
    mute = Attrs.mute
    open = Attrs.open
    -- TODO from Derive.C.Bali.Reyong, or from a common attrs module
    cek = Attrs.attr "cek"

-- * checks

checkFilenames :: IO [FilePath]
checkFilenames = filterM (fmap not . exists) allFilenames
    where exists = Directory.doesFileExist . ("../data/sampler/reyong" </>)

allFilenames :: [FilePath]
allFilenames = map fst3 $ Either.rights
    [ toFilename articulation (Left pitch) dyn variation
    | articulation <- Util.enumAll
    , pitch <- map (snd . snd) nnKeys
    , dyn <- Util.enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]
    where fst3 (a, _, _) = a

-- * convert

convert :: Note.Note -> Patch.ConvertM (RealTime, Sample.Sample)
convert note = do
    let articulation = Util.articulation Open attributeMap $
            Note.attributes note
    let (dyn, scale) = Util.dynamic dynamicRange note
    symPitch <- if Text.null (Note.element note)
        then Right <$> tryJust "no pitch" (Note.initialPitch note)
        else return $ Left $ Pitch.Note (Note.element note)
    let var = Util.variation (variationsOf articulation) note
    (filename, noteNn, sampleNn) <-
        tryRight $ toFilename articulation symPitch dyn var
    Log.debug $ "note at " <> pretty (Note.start note) <> ": "
        <> pretty ((dyn, scale), (symPitch, sampleNn), var)
        <> ": " <> txt filename
    let dynVal = Num.scale dynFactor 1 scale
    return $ (Note.duration note + muteTime,) $ Sample.Sample
        { filename = filename
        , offset = 0
        , envelope = Signal.from_pairs
            [ (Note.start note, dynVal), (Note.end note, dynVal)
            , (Note.end note + muteTime, 0)
            ]
        , ratio = Signal.constant $
            Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) noteNn
        }

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.085

-- | Reyong samples are not normalized, so each sample scales from this value
-- to 1 within its 'dynamicRange'
-- Since dyn signal is in dB, this is -x*96 dB.
dynFactor :: Signal.Y
dynFactor = 1 -- TODO adjust

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
            List.find ((==sym) . snd . snd) nnKeys
        return (sampleNn, sampleNn, key)
    Right noteNn -> return (sampleNn, noteNn, key)
        where (sampleNn, (key, _)) = Util.findBelow fst noteNn nnKeys

--             trompong---------------------
--                      reyong-----------------------------
-- 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
nnKeys :: [(Pitch.NoteNumber, (Midi.Key, Pitch.Note))]
nnKeys = zip reyongTuning $ take 15 $ drop 4
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

-- TODO measure
reyongTuning :: [Pitch.NoteNumber]
reyongTuning =
    [ 56.82
    , 60.73 -- 4i
    , 62.80
    , 63.35
    , 67.70
    , 68.20
    , 72.46 -- 5i
    , 73.90
    , 75.50
    , 79.40
    , 80.50
    , 84.46 -- 6i
    , 86.00
    , 87.67
    , 91.74
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

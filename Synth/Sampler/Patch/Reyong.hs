-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for reyong and trompong.
module Synth.Sampler.Patch.Reyong (patches) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Bali as Lib.Bali
import           Synth.Sampler.Patch.Lib.Bali (Pitch(..), PitchClass(..))
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


patches :: [Patch.Patch]
patches =
    [ makePatch "reyong-trompong" Nothing $ Scale.Range
        (Scale.range_bottom Legong.trompong_range)
        (Scale.range_top Legong.reyong_range)
    , makePatch "reyong" (Just BaliScales.Isep) Legong.reyong_range
    , makePatch "trompong" (Just BaliScales.Umbang) Legong.trompong_range
    ]

makePatch :: Note.PatchName -> Maybe BaliScales.Tuning -> Scale.Range
    -> Patch.Patch
makePatch name tuning range = (Patch.patch name)
    { Patch._dir = dir
    , Patch._convert = convert
    , Patch._allFilenames = allFilenames
    , Patch._preprocess = inferDuration
    , Patch._karyaPatch = ImInst.code #= code $ ImInst.range range $
        maybe id (ImInst.environ EnvKey.tuning . ShowVal.show_val) tuning $
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
    code = Lib.Bali.zeroDurMute 0.8
        <> Util.thru dir convert
        <> ImInst.postproc DUtil.with_symbolic_pitch
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

allFilenames :: Set FilePath
allFilenames = Util.assertLength 1440 $ Set.fromList
    [ toFilename articulation pitch dyn variation
    | articulation <- Util.enumAll
    , pitch <- Map.elems nnToPitch
    , dyn <- Util.enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]

-- * convert

convert :: Note.Note -> Patch.ConvertM Sample.Sample
convert note = do
    let articulation = Util.articulationDefault Open attributeMap $
            Note.attributes note
    let (dyn, dynVal) = Util.dynamicMidi dynamicRange minDyn note
    symPitch <- Util.symbolicPitch note
    let var = Util.variation (variationsOf articulation) note
    (pitch, (sampleNn, noteNn)) <- tryRight $ findPitch symPitch
    let filename = toFilename articulation pitch dyn var
    dynVal <- return $ dynVal * tweakDynamic articulation pitch dyn
    -- Log.debug $ "note at " <> pretty (Note.start note) <> ": "
    --     <> pretty ((dyn, scale), (symPitch, sampleNn), var)
    --     <> ": " <> txt filename
    return $ (Sample.make filename)
        { Sample.envelope = if
            | isMute articulation -> Signal.constant dynVal
            | otherwise -> Util.sustainRelease dynVal muteTime note
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio sampleNn noteNn
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

tweakDynamic :: Articulation -> Pitch -> Util.Dynamic -> Signal.Y
tweakDynamic Open (Pitch 5 I) Util.PP = 0.9
tweakDynamic Open (Pitch 6 U) _ = 0.9
tweakDynamic _ _ _ = 1

{- |
    > 45-1-31-cek+{closed,open}+v{1..6}.wav
    > 45-1-31-mute+{closed,open}+v{1..4}.wav
    > 45-109-127-open+v{1..4}.wav

    keys: 45 48 50 52 55 57 60 62 64 67 69 72 74 76 79
-}
toFilename :: Articulation -> Pitch -> Util.Dynamic -> Util.Variation
    -> FilePath
toFilename articulation pitch dyn variation = Lists.join "-"
    [ show (Midi.from_key (pitchToKey pitch) :: Int)
    , show lowVel
    , show highVel
    , group
    ] ++ ".flac"
    where
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
    -> Either Text (Pitch, (Pitch.NoteNumber, Pitch.NoteNumber))
findPitch = \case
    Left (Pitch.Note sym) -> do
        pitch <- tryJust ("can't parse symbolic pitch: " <> sym) $
            Lib.Bali.parsePitch (untxt sym)
        sampleNn <- tryJust ("pitch out of range: " <> pretty pitch) $
            Map.lookup pitch pitchToNn
        return (pitch, (sampleNn, sampleNn))
    Right noteNn -> return (pitch, (sampleNn, noteNn))
        where Just (sampleNn, pitch) = Maps.lookupClosest noteNn nnToPitch
    where
    pitchToNn = Maps.invert nnToPitch

--             trompong---------------------
--                      reyong-----------------------------
-- 3i 3o 3e 3u 3a 4i 4o 4e 4u 4a 5i 5o 5e 5u 5a 6i 6o 6e 6u 6a 7i
nnToPitch :: Map Pitch.NoteNumber Pitch
nnToPitch = Map.fromList $ zip reyongTuning $
    take 15 $ drop 4 [Pitch oct pc | oct <- [3..], pc <- Util.enumAll]

pitchToKey :: Pitch -> Midi.Key
pitchToKey (Pitch oct pc) = Midi.to_key (oct*12) + baseKey pc
    where
    baseKey = \case
        I -> Key.c_1
        O -> Key.d_1
        E -> Key.e_1
        U -> Key.g_1
        A -> Key.a_1

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

dynamicRange :: Util.Dynamic -> (Int, Int)
dynamicRange = \case
    Util.PP -> (1, 31)
    Util.MP -> (32, 64)
    Util.MF -> (65, 108)
    Util.FF -> (109, 127)

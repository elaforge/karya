-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for the gender wayang instrument family.
module Synth.Sampler.Patch.Wayang (
    patches
    -- * interactive
    , checkFilenames, checkStarts
    , showPitchTable
) where
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read

import qualified System.Directory as Directory
import           System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveT as DeriveT
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale.BaliScales as BaliScales
import           Derive.Scale.BaliScales (Tuning(..))
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime

import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Patch.WayangCode as WayangCode
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Global
import           Synth.Types


patches :: [Patch.DbPatch]
patches =
    map (Patch.DbPatch . make)
        [ (Pemade, Umbang), (Pemade, Isep)
        , (Kantilan, Umbang), (Kantilan, Isep)
        ]
    where
    make (inst, tuning) =
        (Patch.patch $ Text.intercalate "-"
            ["wayang", Util.showtLower inst, Util.showtLower tuning])
        { Patch._dir = dir
        , Patch._convert = convert inst tuning
        , Patch._karyaPatch = ImInst.code #= code inst tuning $
            setRange inst $ setTuning tuning $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportPitch
                    , Control.supportDyn
                    , Control.supportVariation
                    , Map.singleton Control.mute
                        "Amount of mute. This becomes a shortened envelope."
                    ]
                , Im.Patch.patch_attribute_map = const () <$> attributeMap
                }
        }
    setRange inst = ImInst.range $ BaliScales.instrument_range $ case inst of
        Pemade -> Wayang.pemade
        Kantilan -> Wayang.kantilan
    setTuning tuning = -- ImInst.default_scale Wayang.scale_id
        ImInst.environ EnvKey.tuning (ShowVal.show_val tuning)
    code inst tuning = WayangCode.code
        <> Util.thru dir (convert inst tuning)
        <> ImInst.postproc with_symbolic_pitch
    with_symbolic_pitch = DUtil.when_env "symbolic-pitch" (Just True) $
        DUtil.add_symbolic_pitch_convert pitchConvert
    dir = "wayang"

pitchConvert :: PSignal.Transposed -> Either Text Pitch.Note
pitchConvert pitch = do
    note <- first pretty $ PSignal.pitch_note pitch
    if scale_id == "wayang-srg"
        then Pitch.Note <$> pitchSrgpd (Pitch.note_text note)
        else return note
    where
    scale_id = DeriveT.pscale_scale_id $ DeriveT.pitch_scale pitch

-- | This is a hack to convert wayang-srg to ioeua notation so symbolic pitch
-- recognizes it.  It would be better to go through Pitch.Pitch parsing to do
-- the conversion, but better than that is if I make up my mind about what kind
-- of notation I want.
pitchSrgpd :: Text -> Either Text Text
pitchSrgpd pitch = case Map.lookup pc to_ioe of
    Nothing -> Left $ "not in srgpd: " <> pitch
    Just (offset, pc) -> do
        let Right (octi, _) = Text.Read.signed Text.Read.decimal oct
        return $ showt (octi + offset) <> pc
    where
    (oct, pc) = Text.span (\c -> c == '-' || Char.isDigit c) pitch
    to_ioe = Map.fromList
        [ ("s", (-1, "e"))
        , ("r", (-1, "u"))
        , ("g", (-1, "a"))
        , ("p", (0, "i"))
        , ("d", (0, "o"))
        ]

attributeMap :: Common.AttributeMap Articulation
attributeMap = Common.attribute_map
    [ (calung <> mute, CalungMute)
    , (calung, Calung)
    , (mute <> Attrs.loose, LooseMute)
    , (mute, Mute)
    , (mempty, Open)
    ]
    where
    mute = Attrs.mute
    calung = Attrs.attr "calung"

-- * checks

checkFilenames :: IO [FilePath]
checkFilenames = filterM (fmap not . exists) allFilenames
    where exists = Directory.doesFileExist . ("data/sampler/wayang" </>)

allFilenames :: [FilePath]
allFilenames = map fst3 $ Either.rights
    [ toFilename instrument tuning articulation (Right nn) dyn variation
    | instrument <- [Pemade, Kantilan]
    , tuning <- [Umbang, Isep]
    , articulation <- Util.enumAll
    , nn <- map fst $ instrumentKeys instrument tuning articulation
    , dyn <- Util.enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]
    where fst3 (a, _, _) = a

checkStarts :: (Sample.Sample, [[Sample.Sample]])
checkStarts = (makeSample reference,)
    [ map makeSample $ makeFilenames instrument tuning articulation pitch dyn
    | instrument <- [Pemade, Kantilan]
    , tuning <- [Umbang, Isep]
    , articulation <- Util.enumAll
    , pitch <- map (snd . snd) $ instrumentKeys instrument tuning articulation
    , dyn <- Util.enumAll
    ]
    where
    Right (reference, _, _) =
        toFilename Pemade Umbang Mute (Left "4i") Util.FF 0
    makeFilenames instrument tuning articulation pitch dyn =
        map fst3 $ Either.rights $
        map (toFilename instrument tuning articulation (Left pitch) dyn)
            [0 .. variationsOf articulation - 1]
    fst3 (a, _, _) = a
    makeSample fname = (Sample.make fname)
        { Sample.envelope = Signal.from_pairs
            [(0, 1), (0 + dur, 1), (0 + dur + muteTime, 0)]
        , Sample.ratios = Signal.constant 1
        }
    dur = 1

-- * convert

{- | Convert Note.Note into Sample.Sample.

    * map Note.instrument to (Instrument, Tuning)
    * map attrs to Articulation
    * map dynamic
    This is select the next higher Dynamic, then scale down by the difference
    * pitch

    I can use duration, or can extend duration until the next mute, and add
    a silent mute.  If the latter, I have to do it as a preprocess step, since
    it affects overlap calculation.
-}
convert :: Instrument -> Tuning -> Note.Note -> Patch.ConvertM Sample.Sample
convert instrument tuning note = do
    let articulation = Util.articulationDefault Open attributeMap $
            Note.attributes note
    let (dyn, dynVal) = Util.dynamic dynamicRange minDyn note
    symPitch <- Util.symbolicPitch note
    dyn <- pure $ workaround instrument tuning articulation dyn
    let var = Util.variation (variationsOf articulation) note
    (filename, noteNn, sampleNn) <- tryRight $
        toFilename instrument tuning articulation
            symPitch dyn var
    Log.debug $ "note at " <> pretty (Note.start note) <> ": "
        <> pretty ((dyn, dynVal), (symPitch, sampleNn), var)
        <> ": " <> txt filename
    let variableMute = RealTime.seconds $ Note.initial0 Control.mute note
    return $ (Sample.make filename)
        { Sample.envelope = if
            | isMute articulation -> Signal.constant dynVal
            | variableMute > 0 -> Signal.from_pairs
                [ (Note.start note, dynVal)
                , (Note.start note
                    + uncurry Num.scale variableMuteRange (1-variableMute), 0)
                ]
            | otherwise -> Signal.from_pairs
                [ (Note.start note, dynVal), (Note.end note, dynVal)
                , (Note.end note + muteTime, 0)
                ]
        , Sample.ratios = Signal.constant $ Sample.pitchToRatio sampleNn noteNn
        }

isMute :: Articulation -> Bool
isMute = \case
    Mute -> True
    LooseMute -> True
    CalungMute -> True
    _ -> False

-- | I'm missing these samples, so substitute some others.
workaround :: Instrument -> Tuning -> Articulation -> Util.Dynamic
    -> Util.Dynamic
workaround Kantilan Umbang CalungMute Util.FF = Util.MF
workaround _ _ _ dyn = dyn

variableMuteRange :: (RealTime, RealTime)
variableMuteRange = (0.85, 4)

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.35

-- | Wayang samples are normalized, so it just scales by Control.dynamic, where
-- 0 gets this value.
minDyn :: Signal.Y
minDyn = 0.5

-- * toFilename

{- | Find the sample.

    File structure:
    > {pemade,kantilan}/{isep,umbang}/{normal,calung}/
    >     $key-$lowVel-$highVel-$group.flac
    > normal groups = mute{1..8} loose{1..8} open{1..4}
    > calung groups = calung{1..3} calung+mute{1..6}
    > mute and loose start at Key.f0 (17)
    > open starts at Key.f4 65

    TODO The complicated encoding is leftover kontakt nonsense.  I could rename
    to $symPitch-$dyn-$articulation-$var.flac.  On the other hand, it means I
    can easily map back to the kontakt instrument, though it would be easier
    without the ncw nonsense.
-}
toFilename :: Instrument -> Tuning -> Articulation
    -> Either Pitch.Note Pitch.NoteNumber -> Util.Dynamic -> Util.Variation
    -> Either Text (FilePath, Pitch.NoteNumber, Pitch.NoteNumber)
toFilename instrument tuning articulation symPitch dyn variation = do
    (sampleNn, noteNn, Midi.Key sampleKey) <-
        findPitch instrument tuning articulation symPitch
    return
        ( toDir instrument </> toDir tuning </> panggul
            </> sampleName sampleKey ++ ".flac"
        , noteNn
        , sampleNn
        )
    where
    toDir :: Show a => a -> FilePath
    toDir = Util.showLower
    panggul = case articulation of
        CalungMute -> "calung"
        Calung -> "calung"
        _ -> "normal"
    sampleName sampleKey = Seq.join "-"
        [show sampleKey, show lowVel, show highVel, group]
    (lowVel, highVel) = dynamicRange dyn
    group = articulationFile articulation ++ show (variation + 1)

dynamicRange :: Util.Dynamic -> (Int, Int)
dynamicRange = \case
    Util.PP -> (1, 31)
    Util.MP -> (32, 64)
    Util.MF -> (65, 108)
    Util.FF -> (109, 127)

articulationFile :: Articulation -> String
articulationFile = \case
    Mute -> "mute"
    LooseMute -> "loose"
    Open -> "open"
    Calung -> "calung"
    CalungMute -> "calung+mute"

variationsOf :: Articulation -> Util.Variation
variationsOf = \case
    Mute -> 8
    LooseMute -> 8
    Open -> 4
    Calung -> 3
    CalungMute -> 6

-- * implementation

data Instrument = Pemade | Kantilan deriving (Eq, Ord, Show, Enum, Bounded)

data Articulation = Mute | LooseMute | Open | CalungMute | Calung
    deriving (Eq, Ord, Enum, Bounded, Show)

findPitch :: Instrument -> Tuning -> Articulation
    -> Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch.NoteNumber, Pitch.NoteNumber, Midi.Key)
findPitch instrument tuning articulation symPitch = case symPitch of
    Left sym -> do
        (sampleNn, (key, _)) <- tryJust ("invalid pitch: " <> pretty sym) $
            List.find ((==sym) . snd . snd) $ Map.toList keys
        return (sampleNn, sampleNn, key)
    Right noteNn -> return (sampleNn, noteNn, key)
        where
        -- Only Nothing if keys was empty.
        Just (sampleNn, (key, _)) = Maps.lookup_closest noteNn keys
    where
    keys = keyMap instrument tuning articulation

keyMap :: Instrument -> Tuning -> Articulation
    -> Map Pitch.NoteNumber (Midi.Key, Pitch.Note)
keyMap instrument tuning articulation =
    fromMaybe (error "keyMap should have been complete") $
        Map.lookup (instrument, tuning, octaveOf articulation) maps
    where
    -- Memoize the key maps.  This should be floated to a CAF.
    maps = Map.fromList
        [ ((instrument, tuning, octave), makeKeyMap instrument tuning octave)
        | instrument <- Util.enumAll
        , tuning <- Util.enumAll
        , octave <- Seq.unique $ map octaveOf Util.enumAll
        ]
    octaveOf = \case
        Mute -> -2
        LooseMute -> -2
        CalungMute -> -2
        Open -> 2
        Calung -> 2

makeKeyMap :: Instrument -> Tuning -> Midi.Key
    -> Map Pitch.NoteNumber (Midi.Key, Pitch.Note)
makeKeyMap instrument tuning octave =
    Map.fromList $ zip nns (zip (map (octave*12+) keys) notes)
    where
    (keys, notes) = unzip $ wayangKeys $ case instrument of
        Pemade -> 3
        Kantilan -> 4
    nns = case (instrument, tuning) of
        (Pemade, Umbang) -> map fst pemadeTuning
        (Pemade, Isep) -> map snd pemadeTuning
        (Kantilan, Umbang) -> map fst kantilanTuning
        (Kantilan, Isep) -> map snd kantilanTuning

instrumentKeys :: Instrument -> Tuning -> Articulation
    -> [(Pitch.NoteNumber, (Midi.Key, Pitch.Note))]
instrumentKeys instrument tuning articulation =
    zip nns (zip (map (offset*12+) keys) notes)
    where
    (keys, notes) = unzip $ wayangKeys $ case instrument of
        Pemade -> 3
        Kantilan -> 4
    offset = case articulation of
        Mute -> -2
        LooseMute -> -2
        CalungMute -> -2
        Open -> 2
        Calung -> 2
    nns = case (instrument, tuning) of
        (Pemade, Umbang) -> map fst pemadeTuning
        (Pemade, Isep) -> map snd pemadeTuning
        (Kantilan, Umbang) -> map fst kantilanTuning
        (Kantilan, Isep) -> map snd kantilanTuning

wayangKeys :: Int -> [(Midi.Key, Pitch.Note)]
wayangKeys baseOct = take 10 $ drop 1
    [ convert (o + Midi.to_key baseOct) key sym
    | o <- [0..], (key, sym) <- baseKeys
    ]
    where
    convert oct key sym =
        (key + oct*12, Pitch.Note (showt (Midi.from_key oct) <> sym))
    baseKeys =
        [ (Key.e_1, "i")
        , (Key.f_1, "o")
        , (Key.a_1, "e")
        , (Key.b_1, "u")
        , (Key.c0, "a")
        ]

showPitchTable :: IO ()
showPitchTable = Text.IO.putStr $ Text.unlines $ Texts.columns 3 $
    Seq.rotate
    [ pemadeUmbang ++ repeat ""
    , replicate 5 "" ++ kantilanUmbang
    , pemadeIsep ++ repeat ""
    , replicate 5 "" ++ kantilanIsep
    ]
    where
    [pemadeUmbang, pemadeIsep, kantilanUmbang, kantilanIsep] =
        map (map (pp . fst))
            [ instrumentKeys inst tuning Open
            | inst <- [Pemade, Kantilan], tuning <- [Umbang, Isep]
            ]
    pp :: Pitch.NoteNumber -> Text
    pp = Text.replace "nn" "" . pretty

pemadeTuning :: [(Pitch.NoteNumber, Pitch.NoteNumber)]
pemadeTuning =
    [ (52.27, 52.94)
    , (54.55, 55.15)
    , (57.35, 57.90)
    , (59.85, 60.32)

    , (62.50, 63.00)
    , (64.45, 64.72)
    , (67.29, 67.60)
    , (69.25, 69.48)
    , (71.83, 72.11)
    , (74.66, 74.85)
    ]

kantilanTuning :: [(Pitch.NoteNumber, Pitch.NoteNumber)]
kantilanTuning =
    [ (64.31, 64.70)
    , (67.13, 67.45)
    , (69.22, 69.46)
    , (71.81, 72.00)

    , (74.57, 74.80)
    , (76.75, 76.88)
    , (79.37, 79.50)
    , (81.53, 81.65)
    , (84.02, 84.13)
    , (86.79, 86.90)
    ]

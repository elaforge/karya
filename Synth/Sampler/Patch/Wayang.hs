-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for the wayang instrument family.
module Synth.Sampler.Patch.Wayang (
    patches
    -- * interactive
    , verifyFilenames
    , showPitchTable
) where
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Wayang as Wayang

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.WayangCode as WayangCode
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


patches :: [Patch.Patch]
patches =
    map make
        [ (Pemade, Umbang), (Pemade, Isep)
        , (Kantilan, Umbang), (Kantilan, Isep)
        ]
    where
    make (inst, tuning) = Patch.Patch
        { _name = Text.toLower $
            Text.intercalate "-" ["wayang", showt inst, showt tuning]
        , _dir = "wayang"
        , _convert = convert inst tuning
        , _karyaPatch = ImInst.code #= WayangCode.code $
            setRange inst $ setScale tuning $
            ImInst.make_patch $ Im.Patch.patch
                { Im.Patch.patch_controls = mconcat
                    [ Control.supportPitch
                    , Control.supportDyn
                    , Control.supportVariation
                    ]
                , Im.Patch.patch_attribute_map = const () <$> attributeMap
                }
        }
    setRange inst = ImInst.range $ BaliScales.instrument_range $ case inst of
        Pemade -> Wayang.pemade
        Kantilan -> Wayang.kantilan
    setScale tuning = ImInst.default_scale Wayang.scale_id
        . ImInst.environ EnvKey.tuning (tuningVal tuning :: Text)
    tuningVal Umbang = "umbang"
    tuningVal Isep = "isep"

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

verifyFilenames :: IO [FilePath]
verifyFilenames = filterM (fmap not . exists) allFilenames
    where exists = Directory.doesFileExist . ("../data/sampler/wayang" </>)

-- * convert

{- |
    Convert Note.Note into Sample.Sample.
    Must be deterministic, which means the Variation has to be in the Note.

    * map Note.instrument to (Instrument, Tuning)
    * map attrs to Articulation
    * map dynamic
    This is select the next higher Dynamic, then scale down by the difference
    * pitch

    I can use duration, or can extend duration until the next mute, and add
    a silent mute.  If the latter, I have to do it as a preprocess step, since
    it affects overlap calculation.
-}
convert :: Instrument -> Tuning -> Note.Note
    -> Patch.ConvertM (RealTime, Sample.Sample)
convert instrument tuning note = do
    let articulation = convertArticulation $ Note.attributes note
    let (dyn, scale) = convertDynamic $ fromMaybe 0 $
            Note.initial Control.dynamic note
    symPitch <- if Text.null (Note.element note)
        then Right <$> tryJust "no pitch" (Note.initialPitch note)
        else return $ Left $ Pitch.Note (Note.element note)
    (dyn, scale) <- pure $ workaround instrument tuning articulation dyn scale
    (filename, noteNn, sampleNn) <- tryRight $
        toFilename instrument tuning articulation
        symPitch dyn (convertVariation note)
    Log.debug $ "note at " <> pretty (Note.start note) <> ": "
        <> pretty ((dyn, scale), (symPitch, sampleNn), convertVariation note)
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

-- | I'm missing these samples, so substitute some others.
workaround :: Instrument -> Tuning -> Articulation -> Dynamic -> Signal.Y
    -> (Dynamic, Signal.Y)
workaround Kantilan Umbang CalungMute FF scale = (MF, scale + 1)
workaround _inst _tuning _articulation dyn scale = (dyn, scale)

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.15

-- Since dyn signal is in dB, this is -x*96 dB.
dynFactor :: Signal.Y
dynFactor = 1 -- TODO adjust

convertArticulation :: Attrs.Attributes -> Articulation
convertArticulation = maybe Open snd . (`Common.lookup_attributes` attributeMap)

-- | Convert to (Dynamic, DistanceFromPrevDynamic)
convertDynamic :: Signal.Y -> (Dynamic, Signal.Y)
convertDynamic y = find 0 (Num.clamp 0 127 (round (y * 127))) rangeDynamics
    where
    find low val ((high, dyn) : rest)
        | null rest || val < high =
            (dyn, Num.normalize (int low) (int high) (int val))
        | otherwise = find high val rest
    find _ _ [] = error "empty rangeDynamics"
    int = fromIntegral
    rangeDynamics = Seq.key_on (snd . dynamicRange) enumAll

convertVariation :: Note.Note -> Variation
convertVariation = maybe 0 floor . Note.initial Control.variation

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
    to $symPitch-$dyn-$articulation-$var.flac.
-}
toFilename :: Instrument -> Tuning -> Articulation
    -> Either Pitch.Note Pitch.NoteNumber -> Dynamic -> Variation
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
    toDir = map Char.toLower . show
    panggul = case articulation of
        CalungMute -> "calung"
        Calung -> "calung"
        _ -> "normal"
    sampleName sampleKey = Seq.join "-"
        [show sampleKey, show lowVel, show highVel, group]
    (lowVel, highVel) = dynamicRange dyn
    group = articulationFile articulation
        ++ show (variation `mod` variationsOf articulation + 1)

allFilenames :: [FilePath]
allFilenames = map fst3 $ Either.rights
    [ toFilename instrument tuning articulation (Right nn) dyn variation
    | instrument <- [Pemade, Kantilan]
    , tuning <- [Umbang, Isep]
    , articulation <- enumAll
    , nn <- map fst $ instrumentKeys instrument tuning articulation
    , dyn <- enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]
    where fst3 (a, _, _) = a

dynamicRange :: Dynamic -> (Int, Int)
dynamicRange = \case
    PP -> (1, 31)
    MP -> (32, 64)
    MF -> (65, 108)
    FF -> (109, 127)

articulationFile :: Articulation -> String
articulationFile = \case
    Mute -> "mute"
    LooseMute -> "loose"
    Open -> "open"
    Calung -> "calung"
    CalungMute -> "calung+mute"

variationsOf :: Articulation -> Variation
variationsOf = \case
    Mute -> 8
    LooseMute -> 8
    Open -> 4
    Calung -> 3
    CalungMute -> 6

-- * implementation

data Instrument = Pemade | Kantilan deriving (Eq, Ord, Show)
data Tuning = Umbang | Isep deriving (Eq, Ord, Show)

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Enum, Bounded, Show)

data Articulation = Mute | LooseMute | Open | CalungMute | Calung
    deriving (Eq, Ord, Enum, Bounded, Show)

type Variation = Int

instance Pretty Dynamic where pretty = showt

findPitch :: Instrument -> Tuning -> Articulation
    -> Either Pitch.Note Pitch.NoteNumber
    -> Either Text (Pitch.NoteNumber, Pitch.NoteNumber, Midi.Key)
findPitch instrument tuning articulation symPitch = case symPitch of
    Left sym -> do
        (sampleNn, (key, _)) <- tryJust ("invalid pitch: " <> pretty sym) $
            List.find ((==sym) . snd . snd) keys
        return (sampleNn, sampleNn, key)
    Right noteNn -> return (sampleNn, noteNn, key)
        where (sampleNn, (key, _)) = findBelow fst noteNn keys
    where
    keys = instrumentKeys instrument tuning articulation

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

findBelow :: Ord k => (a -> k) -> k -> [a] -> a
findBelow _ _ [] = error "empty list"
findBelow key val (x:xs) = go x xs
    where
    go x0 (x:xs)
        | val < key x = x0
        | otherwise = go x xs
    go x0 [] = x0

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
showPitchTable = Text.IO.putStr $ Text.unlines $ TextUtil.formatColumns 3 $
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

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

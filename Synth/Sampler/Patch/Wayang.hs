-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for the wayang instrument family.
module Synth.Sampler.Patch.Wayang (patches) where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Text.Parsec as P

import qualified Util.Num as Num
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Im.Patch as Im.Patch
import qualified Perform.Pitch as Pitch
import qualified Synth.Sampler.Patch2 as Patch2
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


patches :: [Patch2.Patch]
patches = map make
    [ (Pemade, Umbang), (Pemade, Isep), (Kantilan, Umbang), (Kantilan, Isep) ]
    where
    make (inst, tuning) = Patch2.Patch
        { _name = Text.toLower $
            Text.intercalate "-" ["wayang", showt inst, showt tuning]
        , _load = fmap (first toError) . loadPatch (inst, tuning)
        , _convert = convert (inst, tuning)
        , _karyaPatch = ImInst.make_patch $ Im.Patch.Patch
            { patch_controls = mempty
            , patch_attribute_map = const () <$> attributeMap
            , patch_flags = mempty
            }
        }
    -- TODO add scale and tuning env like kontakt instrument
    toError fnames = "error parsing samples: "
        <> Text.intercalate ", " (map txt fnames)

-- load :: IO (Either [FilePath] Patch)
-- load = loadPatch sampleDir
--
-- sampleDir :: FilePath
-- sampleDir = "../data/sampler/wayang"

{-
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
convert :: (Instrument, Tuning) -> Patch -> Note.Note
    -> Either Text Sample.Sample
convert inst patch note = do
    let articulation = convertArticulation $ Note.attributes note
    let (dyn, scale) = convertDynamic $ fromMaybe 0 $
            Note.initial Control.dynamic note
    samples <- tryJust ("no sample for " <> showt (inst, dyn, articulation)) $
        Map.lookup (dyn, articulation) (_samples patch)
    let var = convertVariation note
    let sample = samples !! (var `mod` length samples)
    pitch <- tryJust "no pitch" $ Note.initialPitch note
        -- samples should be NonEmpty, so this shouldn't fail.
    let dyn = dynFactor * scale
    return $ Sample.Sample
        { filename = _filename sample
        , offset = 0
        , envelope = Signal.constant dyn
            <> Signal.from_pairs
                [(Note.end note, dyn), (Note.end note + muteTime, 0)]
        , ratio = Signal.constant $
            Sample.pitchToRatio (Pitch.nn_to_hz (_pitch sample)) pitch
        }

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.15

-- TODO This should be dB
dynFactor :: Signal.Y
dynFactor = 0.5

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

convertArticulation :: Attrs.Attributes -> Articulation
convertArticulation = maybe Open snd . (`Common.lookup_attributes` attributeMap)

-- | Convert to (Dynamic, DistanceFromPrevDynamic)
convertDynamic :: Signal.Y -> (Dynamic, Signal.Y)
convertDynamic y =
    find 0 (Num.clamp 0 127 (round (y * 127))) (Map.toAscList dynamicRanges)
    where
    find low val (((_, high), dyn) : rest)
        | null rest || val < high =
            (dyn, Num.normalize (int low) (int high) (int val))
        | otherwise = find high val rest
    find _ _ [] = error "empty dynamicRanges"
    int = fromIntegral

convertVariation :: Note.Note -> Variation
convertVariation = maybe 0 floor . Note.initial Control.variation

-- * implementation

data Patch = Patch {
    _samples :: Map (Dynamic, Articulation) [Sample]
    } deriving (Eq, Show)

data Instrument = Pemade | Kantilan deriving (Eq, Ord, Show)
data Tuning = Umbang | Isep deriving (Eq, Ord, Show)

data Sample = Sample {
    _filename :: !FilePath
    , _pitch :: !Pitch.NoteNumber
    , _dynamic :: !Dynamic
    , _articulation :: !Articulation
    , _variation :: !Variation
    } deriving (Eq, Show)

data Dynamic = PP | MP | MF | FF
    deriving (Eq, Ord, Show)

data Articulation = Mute | LooseMute | Open | CalungMute | Calung
    deriving (Eq, Ord, Show)

type Variation = Int

loadPatch :: (Instrument, Tuning) -> FilePath -> IO (Either [FilePath] Patch)
loadPatch instTuning rootDir = do
    (errs, samples) <- Either.partitionEithers <$> loadDir rootDir instTuning
    return $ if null errs
        then Right $ Patch $ Map.fromList $
            map (second (Seq.sort_on _variation)) $
            Seq.keyed_group_sort (\s -> (_dynamic s, _articulation s)) samples
        else Left errs

-- parseAll :: FilePath -> IO [Either FilePath ((Instrument, Tuning), [Sample])]
-- parseAll rootDir = mapM parse
--     [ (Pemade, Umbang)
--     , (Pemade, Isep)
--     , (Kantilan, Umbang)
--     , (Kantilan, Isep)
--     ]
--     where

loadDir :: FilePath -> (Instrument, Tuning) -> IO [Either FilePath Sample]
loadDir rootDir (inst, tuning) =
    (++) <$> parseDir (keyToNn nns) (rootDir </> dir </> "normal")
         <*> parseDir (keyToNn nns) (rootDir </> dir </> "calung")
    where
    (nns, dir) = case (inst, tuning) of
        (Pemade, Umbang) -> (pemadeUmbang, "pemade/umbang")
        (Pemade, Isep) -> (pemadeIsep, "pemade/isep")
        (Kantilan, Umbang) -> (kantilanUmbang, "kantilan/umbang")
        (Kantilan, Isep) -> (kantilanIsep, "kantilan/isep")

parseDir :: KeyToNn -> FilePath -> IO [Either FilePath Sample]
parseDir toNn dir =
    map (\fn -> maybe (Left fn) Right $ parseFilename toNn fn) <$>
        Directory.listDirectory dir

{- |
    sample structure:
    pemade/{isep,umbang}/normal/$key-$lowVel-$highVel-$group.wav
    group = mute{1..8} loose{1..8} open{1..4}

    100-1-31-calung{1..3}.wav
    29-1-31-calung+mute{1..6}.wav

    mute and loose start at Key.f0 (17)
    open starts at Key.f4 65
-}
parseFilename :: KeyToNn -> FilePath -> Maybe Sample
parseFilename toNn filename =
    parse . Text.split (=='-') . txt . FilePath.dropExtension $ filename
    where
    parse [key, lowVel, highVel, group] = do
        dyn <- flip Map.lookup dynamicRanges
            =<< (,) <$> pNat lowVel <*> pNat highVel
        (art, var) <- parseArticulation group
        nn <- toNn art . Midi.Key =<< pNat key
        return $ Sample
            { _filename = filename
            , _pitch = nn
            , _dynamic = dyn
            , _articulation = art
            , _variation = var
            }
    parse _ = Nothing

pNat :: Text -> Maybe Int
pNat = try . Parse.parse Parse.p_nat

try :: Either Text a -> Maybe a
try = either (const Nothing) return

parseArticulation :: Text -> Maybe (Articulation, Variation)
parseArticulation = try . Parse.parse p
    where
    p = (,) <$> pArticulation <*> (P.optional (P.string "+v") *> Parse.p_nat)
    match (str, val) = P.try (P.string str) *> pure val
    pArticulation = P.choice $ map match
        [ ("mute", Mute)
        , ("loose", LooseMute)
        , ("open", Open)
        , ("calung+mute", CalungMute)
        , ("calung", Calung)
        ]

dynamicRanges :: Map (Int, Int) Dynamic
dynamicRanges = Map.fromList
    [ ((1, 31), PP)
    , ((32, 64), MP)
    , ((65, 108), MF)
    , ((109, 127), FF)
    ]

type KeyToNn = Articulation -> Midi.Key -> Maybe Pitch.NoteNumber

keyToNn :: ((Instrument, a), [Pitch.NoteNumber]) -> KeyToNn
keyToNn ((inst, _), nns) art key = lookup key keyNns
    where
    keyNns = zip keys nns
    keys = wayangKeys $ (if inst == Kantilan then (+1) else id) $ case art of
        Mute -> 1
        LooseMute -> 1
        CalungMute -> 1
        Open -> 5
        Calung -> 5

wayangKeys :: Int -> [Midi.Key]
wayangKeys oct = map (Midi.to_key (oct * 12) +)
    (take 10 $ drop 1 [k + o*12 | o <- [0..], k <- baseKeys])
    where
    -- ding dong deng dung dang
    baseKeys = [Key.e_1, Key.f_1, Key.a_1, Key.b_1, Key.c0]

pemadeUmbang :: ((Instrument, Tuning), [Pitch.NoteNumber])
pemadeUmbang = ((Pemade, Umbang),) $ map toNN
    [ (Key.f3, 55)
    , (Key.g3, 43)
    , (Key.as3, 56)
    , (Key.c4, 20)
    , (Key.ds4, 54)
    , (Key.f4, 50)
    , (Key.gs4, 68)
    , (Key.as4, 69)
    , (Key.c5, 18)
    , (Key.ds5, 34)
    ]

pemadeIsep :: ((Instrument, Tuning), [Pitch.NoteNumber])
pemadeIsep = ((Pemade, Isep),) $ map toNN
    [ (Key.f3, 0)
    , (Key.g3, -7)
    , (Key.as3, 0)
    , (Key.c4, -36)
    , (Key.ds4, 0)
    , (Key.f4, 28)
    , (Key.gs4, 39)
    , (Key.as4, 51)
    , (Key.c5, -12)
    , (Key.ds5, 16)
    ]

kantilanUmbang :: ((Instrument, Tuning), [Pitch.NoteNumber])
kantilanUmbang = ((Kantilan, Umbang),) $ map toNN
    [ (Key.e4, -31)
    , (Key.g4, -13)
    , (Key.a4, -23)
    , (Key.c5, 20)
    , (Key.ds5, 44)
    , (Key.f5, 24)
    , (Key.g5, -36)
    , (Key.as5, 48)
    , (Key.c6, -1)
    , (Key.ds6, 21)
    ]

kantilanIsep :: ((Instrument, Tuning), [Pitch.NoteNumber])
kantilanIsep = ((Kantilan, Isep),) $ map toNN
    [ (Key.e4, 23)
    , (Key.gs4, 55)
    , (Key.as4, 55)
    , (Key.c5, -1)
    , (Key.ds5, 20)
    , (Key.f5, 12)
    , (Key.gs5, 50)
    , (Key.as5, 35)
    , (Key.c6, -13)
    , (Key.ds6, 11)
    ]

-- NN +cents to adjust to that NN
toNN :: (Midi.Key, Int) -> Pitch.NoteNumber
toNN (key, cents) = Pitch.key_to_nn key - Pitch.nn cents / 100

{-
    Use this to get absolute pitches for samples:

    pemade umbang 12
        start f2, midi 53
        f+.55 g+.43 a#+.56 c+.2 d#+.54 f+.50 g#+.68 a#+.69 c+.18 d#+.34
    pemade isep 12
        start f2, midi 53
        f+0 g-.07 a#+0 c-.36 d#+0 f+.28 g#+.39 a#+.51 c-.12 d#+.16
        down to prev c, up to next g
    kantilan umbang 12
        start e3, midi 64
        e-.31 g-.13 a-.23 c+.2 d#+.44 f+.24 g-.36 a#+.48 c-.01 d#+.21
        down to prev c, up to next g
    kantilan isep 12
        start e3, midi 64
        e+.23 g#+.55 a#+.55 c-.01 d#+.2 f+.12 g#+.5 a#+.35 c-.13 d#+11
        down to prev c, up to next g
-}

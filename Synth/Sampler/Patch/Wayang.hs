-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Definitions for the wayang instrument family.
module Synth.Sampler.Patch.Wayang (patches, verifyFilenames) where
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified System.Directory as Directory
import System.FilePath ((</>))

import qualified Util.Num as Num
import qualified Util.Seq as Seq
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
import qualified Synth.Sampler.Patch2 as Patch2
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Types


patches :: [Patch2.Patch]
patches =
    map make
        [ (Pemade, Umbang), (Pemade, Isep)
        , (Kantilan, Umbang), (Kantilan, Isep)
        ]
    where
    make (inst, tuning) = Patch2.Patch
        { _name = Text.toLower $
            Text.intercalate "-" ["wayang", showt inst, showt tuning]
        , _convert = convert inst tuning
        , _karyaPatch = setRange inst $ setScale tuning $ ImInst.make_patch $
            Im.Patch.Patch
                { patch_controls = mconcat
                    [ Control.supportPitch
                    , Control.supportDyn
                    , Control.supportVariation
                    ]
                , patch_attribute_map = const () <$> attributeMap
                , patch_flags = mempty
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

-- kantilan/umbang/calung/52-109-127-calung+mute1.wav
-- kantilan/umbang/calung/52-109-127-calung+mute2.wav
-- kantilan/umbang/calung/52-109-127-calung+mute3.wav
-- kantilan/umbang/calung/52-109-127-calung+mute4.wav
-- kantilan/umbang/calung/52-109-127-calung+mute5.wav
-- kantilan/umbang/calung/52-109-127-calung+mute6.wav
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
convert :: Instrument -> Tuning -> Note.Note -> Either Text Sample.Sample
convert instrument tuning note = do
    let articulation = convertArticulation $ Note.attributes note
    let (dyn, scale) = convertDynamic $ fromMaybe 0 $
            Note.initial Control.dynamic note
    pitch <- tryJust "no pitch" $ Note.initialPitch note
    let (filename, sampleNn) =
            toFilename instrument tuning articulation pitch dyn
                (convertVariation note)
    let dyn = dynFactor * scale
    return $ Sample.Sample
        { filename = filename
        , offset = 0
        , envelope = Signal.from_pairs
            [ (Note.start note, dyn), (Note.end note, dyn)
            , (Note.end note + muteTime, 0)
            ]
        , ratio = Signal.constant $
            Sample.pitchToRatio (Pitch.nn_to_hz sampleNn) pitch
        }

-- | Time to mute at the end of a note.
muteTime :: RealTime
muteTime = 0.15

-- Since dyn signal is in dB, this is -x*96 dB.
dynFactor :: Signal.Y
dynFactor = 0.5

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
    >     $key-$lowVel-$highVel-$group.wav
    > normal groups = mute{1..8} loose{1..8} open{1..4}
    > calung groups = calung{1..3} calung+mute{1..6}
    > mute and loose start at Key.f0 (17)
    > open starts at Key.f4 65
-}
toFilename :: Instrument -> Tuning -> Articulation -> Pitch.NoteNumber
    -> Dynamic -> Variation -> (FilePath, Pitch.NoteNumber)
toFilename instrument tuning articulation nn dyn variation =
    ( toDir instrument </> toDir tuning </> panggul </> sampleName ++ ".wav"
    , sampleNn
    )
    where
    toDir :: Show a => a -> FilePath
    toDir = map Char.toLower . show
    panggul = case articulation of
        CalungMute -> "calung"
        Calung -> "calung"
        _ -> "normal"
    sampleName = Seq.join "-"
        [show sampleKey, show lowVel, show highVel, group]
    (lowVel, highVel) = dynamicRange dyn
    group = articulationFile articulation
        ++ show (variation `mod` variationsOf articulation + 1)
    (sampleNn, Midi.Key sampleKey) = findPitch instrument tuning articulation nn

allFilenames :: [FilePath]
allFilenames =
    [ fst $ toFilename instrument tuning articulation nn dyn variation
    | instrument <- [Pemade, Kantilan]
    , tuning <- [Umbang, Isep]
    , articulation <- enumAll
    , nn <- map fst $ instrumentKeys instrument tuning articulation
    , dyn <- enumAll
    , variation <- [0 .. variationsOf articulation - 1]
    ]

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

findPitch :: Instrument -> Tuning -> Articulation -> Pitch.NoteNumber
    -> (Pitch.NoteNumber, Midi.Key)
findPitch instrument tuning articulation nn =
    findBelow fst nn (instrumentKeys instrument tuning articulation)

instrumentKeys :: Instrument -> Tuning -> Articulation
    -> [(Pitch.NoteNumber, Midi.Key)]
instrumentKeys instrument tuning articulation = zip nns keys
    where
    keys = wayangKeys $ (if instrument == Kantilan then (+1) else id) $
        case articulation of
            Mute -> 1
            LooseMute -> 1
            CalungMute -> 1
            Open -> 5
            Calung -> 5
    nns = case (instrument, tuning) of
        (Pemade, Umbang) -> pemadeUmbang
        (Pemade, Isep) -> pemadeIsep
        (Kantilan, Umbang) -> kantilanUmbang
        (Kantilan, Isep) -> kantilanIsep

findBelow :: Ord k => (a -> k) -> k -> [a] -> a
findBelow _ _ [] = error "empty list"
findBelow key val (x:xs) = go x xs
    where
    go x0 (x:xs)
        | val < key x = x0
        | otherwise = go x xs
    go x0 [] = x0

wayangKeys :: Int -> [Midi.Key]
wayangKeys oct = map (Midi.to_key (oct * 12) +)
    (take 10 $ drop 1 [k + o*12 | o <- [0..], k <- baseKeys])
    where
    -- ding dong deng dung dang
    baseKeys = [Key.e_1, Key.f_1, Key.a_1, Key.b_1, Key.c0]

pemadeUmbang :: [Pitch.NoteNumber]
pemadeUmbang = map toNN
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

pemadeIsep :: [Pitch.NoteNumber]
pemadeIsep = map toNN
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

kantilanUmbang :: [Pitch.NoteNumber]
kantilanUmbang = map toNN
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

kantilanIsep :: [Pitch.NoteNumber]
kantilanIsep = map toNN
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

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound .. maxBound]

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

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BohlenPierce where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Num as Num
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Pitches as Pitches
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales = [absolute_scale]

scale_id :: Pitch.ScaleId
scale_id = "bp"

absolute_scale :: Scale.Scale
absolute_scale =
    (ChromaticScales.make_scale scale_map scale_id
        "Bohlen-Pierce is for when you want a scale which is systematically\
        \ different in every way from all those other scales, but with its\
        \ own peculiar internal consistency. TODO: MIDI input is\
        \ not properly implemented.\n")
    { Scale.scale_note_to_call = note_to_call scale_map
    , Scale.scale_input_to_nn = Util.computed_input_to_nn
        (ChromaticScales.input_to_note scale_map) (note_to_call scale_map)
    }
    -- TODO that's pretty awkward, ChromaticScales should provide a way to make
    -- a scale but specify my own pitch aspect.

scale_map :: ChromaticScales.ScaleMap
scale_map = ChromaticScales.scale_map layout fmt all_keys default_key

pscale :: PitchSignal.Scale
pscale = Pitches.scale absolute_scale

fmt :: TheoryFormat.Format
fmt = TheoryFormat.make_absolute_format
        (TheoryFormat.make_pattern degrees) degrees
        TheoryFormat.ascii_accidentals
    where
    degrees = TheoryFormat.make_degrees
        ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

note_to_call :: ChromaticScales.ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call smap note =
    case TheoryFormat.read_pitch (ChromaticScales.smap_fmt smap) note of
        Left _ -> ScaleDegree.scale_degree_interval pscale named_intervals note
        Right pitch ->
            Just $ scale_degree
                (ChromaticScales.pitch_nn smap degree_to_nn pitch)
                (ChromaticScales.pitch_note smap pitch)

-- TODO frac should always be 0, right?
degree_to_nn :: TrackLang.Environ -> Score.ControlValMap -> Double
    -> Pitch.NoteNumber
degree_to_nn env controls degreef = Pitch.hz_to_nn $ Num.scale hz1 hz2 frac
    where
    hz1 = degree_to_hz base_hz tonic (Pitch.Degree degree)
    hz2 = degree_to_hz base_hz tonic (Pitch.Degree (degree + 1))
    (degree, frac) = properFraction degreef
    base_hz = Map.findWithDefault default_base_hz just_base_control controls
    tonic = Theory.note_to_semis layout $ Theory.key_tonic key
    key = fromMaybe default_key $ do
        key <- Util.lookup_key env
        Map.lookup key all_keys

degree_to_hz :: Pitch.Hz -> Theory.Semi -> Pitch.Degree -> Pitch.Hz
degree_to_hz base_hz tonic (Pitch.Degree abs_degree) = oct_base * ratio
    where
    oct_base = base_hz * 3 ^^ (octave - Pitch.middle_octave)
    ratio = index_mod just_ratios (degree - tonic)
    (octave, degree) = abs_degree `divMod` semis_per_octave

index_mod :: Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

middle_a :: Pitch.Degree
middle_a = Pitch.Degree 60 -- Pitch.middle_degree

default_base_hz :: Pitch.Hz
default_base_hz = Pitch.middle_c_hz

just_base_control :: Score.Control
just_base_control = "just-base"

scale_degree :: Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree = ScaleDegree.scale_degree_just pscale named_intervals 1

-- **

-- | Keyboard layout where the lambda mode in A is analogous to C-major:
--
-- > A - B C D - E F - G H - I A
-- > 1   2 3 4   5 6   7 8   9 1
-- >
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ █ │ │ █ │ █ │ █ │ │
-- > │ │ │ │ │ │ │ │ │ │ │
-- > └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
-- >  1 2 3 4 5 6 7 8 9 A
-- >  A B C D E F G H I A
layout :: Theory.Layout
layout = Theory.layout lambda_intervals

lambda_intervals :: [Int]
lambda_intervals = [2, 1, 1, 2, 1, 2, 1, 2, 1]

-- 1, 7, 11, or in lambda: A, E, G -- 1 5 7
just_ratios :: Vector.Vector Double
just_ratios = Vector.fromList (map realToFrac bp_ratios)

bp_ratios :: [Ratio.Rational]
bp_ratios =
    [ 1, 27 % 25, 25 % 21, 9 % 7, 7 % 5, 75 % 49, 5 % 3
    , 9 % 5, 49 % 25, 15 % 7, 7 % 3, 63 % 25, 25 % 9
    ]

-- | Display scale degrees and the intervals between them.
print_intervals :: Text
print_intervals =
    Text.unwords $ interleave (map (("("<>) . (<>")")) notes) intervals
    where
    interleave (x:xs) (y:ys) = x : y : interleave xs ys
    interleave xs [] = xs
    interleave [] ys = ys
    notes = TheoryFormat.key_notes default_key fmt
    intervals = zipWith interval bp_ratios (drop 1 bp_ratios)
    interval low high = fromMaybe ("no interval: " <> showt (high/low)) $
        Map.lookup (high / low) names
    names = Map.fromList [(v, k) | (k, v) <- Map.toList named_intervals]

-- | Display the keys and their signatures.
print_scales :: IO ()
print_scales = mapM_ (putStrLn . untxt) $
    map (TheoryFormat.show_key_signature fmt) $
    filter ((== Theory.Note 0 0) . Theory.key_tonic) $ Map.elems all_keys

pc_per_octave :: Theory.PitchClass
pc_per_octave = length lambda_intervals

semis_per_octave :: Int
semis_per_octave = sum lambda_intervals

all_keys :: ChromaticScales.Keys
all_keys = ChromaticScales.make_keys fmt $ concatMap (uncurry make_keys) modes

default_key :: Theory.Key
Just default_key = Map.lookup (Pitch.Key "a-lambda") all_keys

-- | BP modes, as documented on <http://www.huygens-fokker.org/bpsite/>
--
-- The only difference is that I wrap the octave at A, rather than C.
--
-- This has keys rooted at every chromatic step, but BP modes rooted on
-- accidentals tend to wind up with crazy key signatures, e.g. with triple
-- sharps, probably due to the lack of a circle-of-fifths organization.
modes :: [(Text, [Theory.Semi])]
modes = make_modes lambda_names lambda_intervals
    ++ make_modes gamma_names gamma_intervals
    where
    make_modes names intervals =
        [ (name, take pc_per_octave (drop n (cycle intervals)))
        | (n, name) <- zip [0..] names
        ]

-- | Lambda family, never has 2 whole steps in a row.
lambda_names :: [Text]
lambda_names =
    [ "lambda", "walker-a", "moll-2", "walker-1", "harmonic", "walker-2"
    , "dur-1", "moll-1", "walker-b"
    ]

-- | Gamma family.  The x modes are considered not very useful.
gamma_names :: [Text]
gamma_names = ["gamma", "x1", "x2", "x3", "x4", "x5", "x6", "dur-2", "x7"]
gamma_intervals :: [Int]
gamma_intervals = [1, 2, 1, 2, 1, 1, 2, 2, 1]

make_keys :: Text -> [Theory.Semi] -> [Theory.Key]
make_keys name intervals =
    [Theory.key tonic name intervals layout
        | tonic <- all_notes, abs (Theory.note_accidentals tonic) <= 1]

all_notes :: [Theory.Note]
all_notes =
    [Theory.Note pc accs | pc <- [0 .. pc_per_octave - 1], accs <- [-1..1]]

named_intervals :: ScaleDegree.NamedIntervals
named_intervals = Map.fromList
    [ ("d", minor)
    , ("D", major)
    , ("m-2", small)
    , ("m2", small * minor)
    , ("M2", small * major)
    , ("M+2", small * minor * major)
    ]
    where
    small = 27 % 25
    minor = 245 % 243 -- minor diesis
    major = 3125 % 3087 -- major diesis

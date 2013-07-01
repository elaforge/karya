-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | A version of a just intonation diatonic scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Num as Num
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales =
    [ make_scale (Pitch.ScaleId "just") absolute_format
    , make_scale (Pitch.ScaleId "just-r") relative_format
    ]

absolute_format :: TheoryFormat.Format
absolute_format = TheoryFormat.absolute_c

relative_format :: TheoryFormat.Format
relative_format =
    TheoryFormat.sargam (fmap key_tonic . lookup_key) 0
        TheoryFormat.show_note_diatonic TheoryFormat.adjust_diatonic

-- | Each accidental adds or subtracts this interval.
accidental_interval :: Double
accidental_interval = 16 / 15

make_scale :: Pitch.ScaleId -> TheoryFormat.Format -> Scale.Scale
make_scale scale_id fmt = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = TheoryFormat.fmt_pattern fmt
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose fmt
    , Scale.scale_enharmonics = enharmonics fmt
    , Scale.scale_note_to_call = note_to_call fmt
    , Scale.scale_input_to_note = input_to_note (TheoryFormat.show_pitch fmt)
    , Scale.scale_input_to_nn =
        Util.computed_input_to_nn
            (input_to_note (TheoryFormat.show_pitch fmt))
            (note_to_call fmt)
    , Scale.scale_call_doc = Util.annotate_call_doc Util.standard_transposers
        ("Just scales are tuned by ratios from a base frequency.\
        \ That frequency is taken from the `%just-base` control and the key.\
        \ For example, `%just-base = 440 | key = a-maj` means that A in the\
        \ middle octave is 440hz and is considered 1/1, and uses the `maj`\
        \ set of ratios.  If the base hz isn't given, it defaults to the\
        \ 12TET tuning of the key.\
        \ Just scales support accidentals, but are inherently\
        \ diatonic, so chromatic transposition is the same as diatonic\
        \ transposition."
        ) [] (Util.scale_degree_doc (scale_degree 0))
    }

read_note :: TheoryFormat.Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Theory.Pitch
read_note fmt key =
    TheoryFormat.fmt_adjust fmt key <=< TheoryFormat.read_pitch fmt


-- * input_to_note

enharmonics :: TheoryFormat.Format -> Derive.Enharmonics
enharmonics fmt key note = do
    pitch <- read_note fmt key note
    return $ map (TheoryFormat.show_pitch fmt key) $
        Theory.enharmonics_of layout pitch

input_to_note :: TheoryFormat.ShowPitch -> Maybe Pitch.Key -> Pitch.InputKey
    -> Maybe Pitch.Note
input_to_note show_pitch _key (Pitch.InputKey nn) =
    -- I ignore the key on input.  Otherwise, a InputKey on middle C will be
    -- adjusted for the key, but I want middle C to be the sa of a relative
    -- scale.
    Just $ show_pitch Nothing $ Theory.semis_to_pitch_sharps layout $
        Theory.nn_to_semis $ floor nn

layout :: Theory.Layout
layout = Theory.layout TheoryFormat.absolute_c_intervals

-- | Since just scales don't really have key signatures or even accidentals,
-- this only even produces sharps.
degree_note :: TheoryFormat.ShowPitch -> Maybe Pitch.Key -> Double -> Pitch.Note
degree_note show_pitch key degreef =
    -- TODO pitch_expr makes a call to scale_degree, so I think this is wrong!
    Pitch.Note $ ScaleDegree.pitch_expr note frac
    where
    (degree, frac) = properFraction degreef
    (octave, pc) = degree `divMod` pc_per_octave
    note = show_pitch key $ Theory.Pitch octave (Theory.Note pc 0)

-- | Unused, but still here in case I don't like accidentals.
input_to_note_no_acc :: TheoryFormat.ShowPitch -> Maybe Pitch.Key
    -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note_no_acc show_pitch key (Pitch.InputKey nn) =
    case (degree1, degree2) of
        (Just d1, Just d2) ->
            Just $ degree_note show_pitch key (Num.scale d1 d2 frac)
        _ -> Nothing
    where
    degree1 = nn_to_degree Vector.!? input_degree
    degree2 = nn_to_degree Vector.!? (input_degree + 1)
    (input_degree, frac) = properFraction nn

nn_to_degree :: Vector.Vector Double
nn_to_degree = Vector.fromList $ take 127 $
    to_steps (zip [0..] (cycle TheoryFormat.absolute_c_intervals))
    where
    to_steps [] = []
    to_steps ((n, step) : steps)
        | step == 2 = n : n + 0.5 : to_steps steps
        | otherwise = n : to_steps steps


-- * transpose

transpose :: TheoryFormat.Format -> Maybe Pitch.Key -> Pitch.Octave
    -> Pitch.Transpose -> Pitch.Note -> Either Scale.ScaleError Pitch.Note
transpose fmt key oct transpose note = do
    pitch <- read_note fmt key note
    let steps = floor $ case transpose of
            Pitch.Chromatic steps -> steps
            Pitch.Diatonic steps -> steps
    Right $ TheoryFormat.show_pitch fmt key $
        Theory.transpose_pitch pc_per_octave (oct * pc_per_octave + steps) pitch

pc_per_octave :: Theory.PitchClass
pc_per_octave = Theory.layout_max_pc layout

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
note_to_call :: TheoryFormat.Format -> Pitch.Note -> Maybe Derive.ValCall
note_to_call fmt note = case TheoryFormat.read_pitch fmt note of
    Left _ -> ScaleDegree.scale_degree_interval named_intervals note
    Right pitch_ ->
        let pitch = pitch_ { Theory.pitch_note = (Theory.pitch_note pitch_)
                { Theory.note_accidentals = 0 } }
            accs = Theory.pitch_accidentals pitch_
        in Just $ scale_degree (accidental_interval ^^ accs)
            (pitch_nn fmt pitch) (pitch_note fmt pitch)

pitch_nn :: TheoryFormat.Format -> Theory.Pitch -> Scale.PitchNn
pitch_nn fmt pitch env controls =
    Util.scale_to_pitch_error chromatic diatonic $ do
        key <- read_key env
        pitch <- TheoryFormat.fmt_adjust fmt (Util.lookup_key env) pitch
        let hz = transpose_to_hz base_hz key (chromatic + diatonic) pitch
            nn = Pitch.hz_to_nn hz
        if Num.in_range 0 127 nn then Right nn
            else Left Scale.InvalidTransposition
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls
    base_hz = Map.lookup just_base_control controls

pitch_note :: TheoryFormat.Format -> Theory.Pitch -> Scale.PitchNote
pitch_note fmt pitch env controls =
    Util.scale_to_pitch_error chromatic diatonic $ do
        let key = Util.lookup_key env
        pitch <- TheoryFormat.fmt_adjust fmt key pitch
        let transposed = Theory.transpose_pitch pc_per_octave
                (round (chromatic + diatonic)) pitch
        Right $ TheoryFormat.show_pitch fmt key transposed
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

scale_degree :: Pitch.Hz -> Scale.PitchNn -> Scale.PitchNote -> Derive.ValCall
scale_degree = ScaleDegree.scale_degree_just named_intervals

just_base_control :: Score.Control
just_base_control = Score.Control "just-base"

-- ** relative interval

-- ** implementation

transpose_to_hz :: Maybe Pitch.Hz -> Key -> Double -> Theory.Pitch -> Pitch.Hz
transpose_to_hz base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    (steps, frac) = properFraction frac_steps
    pitch1 = Theory.transpose_pitch pc_per_octave steps pitch
    hz1 = degree_to_hz pc_per_octave (key_ratios key) base_hz key pitch1
    hz2 = degree_to_hz pc_per_octave (key_ratios key) base_hz key
        (Theory.transpose_pitch pc_per_octave 1 pitch1)

-- | Given a Key, convert a pitch in that key to its hz value.
-- If the base hz is given, this is the frequency of the key's tonic.
-- Otherwise, the base is taken from the 12TET value.
degree_to_hz :: Theory.Degree -> Ratios -> Maybe Pitch.Hz
    -> Key -> Theory.Pitch -> Pitch.Hz
degree_to_hz per_oct ratios maybe_base_hz key pitch = base * ratio
    where
    base = base_hz * 2 ^^ Theory.pitch_octave degree
    base_hz = Pitch.nn_to_hz $ normalize_octave $
        maybe (key_tonic_nn key) Pitch.hz_to_nn maybe_base_hz
    ratio = index_mod ratios (Theory.note_pc (Theory.pitch_note degree))
    degree = Theory.transpose_pitch per_oct (- key_tonic key) pitch

-- | Normalize a NoteNumber to lie within the first 'Theory.Pitch' octave.
normalize_octave :: Pitch.NoteNumber -> Pitch.NoteNumber
normalize_octave = (`Num.fmod` 12)

-- * key

data Key = Key {
    -- | PitchClass starts at A, not C.
    key_tonic :: !Theory.PitchClass
    -- | NoteNumber in the bottom octave as returned by 'normalize_octave'.
    , key_tonic_nn :: !Pitch.NoteNumber
    , key_ratios :: !Ratios
    } deriving (Show)

-- | Number of degrees in an octave.
key_octave :: Key -> Theory.PitchClass
key_octave = Vector.length . key_ratios

all_keys :: Map.Map Pitch.Key Key
all_keys = Map.fromList
    [ (Pitch.Key $ degree <> "-" <> name, Key pc nn ratios)
    | (name, ratios) <- all_key_ratios
    , (degree, pc, nn) <- zip3 TheoryFormat.absolute_c_degrees [0..6] nns
    ]
    where nns = scanl (+) 0 (cycle [2, 2, 1, 2, 2, 2, 1])

default_key :: Key
Just default_key = Map.lookup (Pitch.Key "c-maj") all_keys

read_key :: TrackLang.Environ -> Either Scale.ScaleError Key
read_key = Util.read_environ (\k -> Map.lookup (Pitch.Key k) all_keys)
    default_key Environ.key

lookup_key :: Maybe Pitch.Key -> Either Scale.ScaleError Key
lookup_key Nothing = Right default_key
lookup_key (Just key@(Pitch.Key txt)) =
    maybe (Left $ Scale.UnparseableEnviron Environ.key txt) Right $
        Map.lookup key all_keys

-- * ratios

index_mod :: Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

type Ratios = Vector.Vector Double

all_key_ratios :: [(Text, Ratios)]
all_key_ratios = map (second Vector.fromList)
    [ ("maj", [1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8])
    , ("min", [1, 9/8, 6/5, 4/3, 3/2, 8/5, 9/5])
    , ("legong", [1, 10/9, 6/5, 4/3, 3/2, 25/16, 9/5])
    , ("hemavathi", [1, 10/9, 6/5, (3/2) / (16/15), 3/2, 5/3, 9/5])
    ]

named_intervals :: ScaleDegree.NamedIntervals
named_intervals = Map.fromList
    [ ("m2-", 25 % 24) -- 71, 5-limit minor half-step
    , ("m2", 16 % 15) -- 112, 5-limit major half-step
    , ("M2-", 10 % 9) -- 182, minor whole tone
    , ("M2", 9 % 8) -- 204, 5-limit major second
    , ("m3", 6 % 5) -- 316, 5-limit minor third
    , ("M3", 5 % 4) -- 386, 5-limit major third
    , ("P4", 4 % 3) -- 498, perfect fourth
    , ("tt11", 11 % 8) -- 551, undecimal tritone
    , ("tt7-", 7 % 5) -- 583, septimal tritone
    , ("tt", 45 % 32) -- 590, high 5-limit tritone
    , ("tt7+", 10 % 7) -- 618, septimal tritone
    , ("wolf", 40 % 27) -- 681, wolf 5-limit 5th
    , ("P5", 3 % 2) -- 702, perfect fifth
    , ("m6", 8 % 5) -- 5-limit minor sixth
    , ("M6", 5 % 3) -- 5-limit major sixth
    , ("m7", 9 % 5) -- 5-limit large minor seventh
    , ("M7", 15 % 8) -- 5-limit major seventh
    ]


{- Retuning scales:

    build :: Ratios -> Pitch.Degree -> Pitch.Degree -> Pitch.Hz -> [Pitch.Hz]
    build ratios per_oct degree base =
        List.sort $ map calc (zip [degree..] (Vector.toList ratios))
        where
        calc (d, rat)
            | d >= per_oct = f / 2
            | otherwise = f
            where f = base * realToFrac rat

    interpolated ratios per_oct degree from_base to_base frac =
        where
        v1 = build ratios per_oct degree from_base
        v2 = build ratios per_oct degree to_base

             A       B       C       D       E       F       G       A
    A440     440     495     550     586.6   660     773.3   825     880
    C550     458.3   515.625 550     618.75  687.5   733.3   825     916.6
    A458.3   458.3   515.587 527.875 611.06  687.45  763.83  859.312 970.6

    So if I'm going A->C I can't go through B.  I have to figure out the
    frequencies for C550 and then interpolate to those.

    The tuning pitch must be [(C, A, 0.5)].  So it starts A440, then has C.
    C maps to ratio scale of 5:4, so we set
    (-base = 440, -from = 'a', -to = 550, %-frac = %)
    (-base = 550, -from = 'c', -to = 458.3, %-frac = %)

    What I really need is base_from and base_to, where base is always A.  So
    if the tuning pitch says C then I have to figure out the A of C, based on
    the previous frequency.  I think I can do this if

    But how could this work if pitches are moving up and down?

    f0 = build major_ratios 7
    b1 = f0 0 440
    b2 = f0 2 550
    b3 = f0 0 (458 + 1/3)
-}

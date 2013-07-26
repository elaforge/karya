module Derive.Scale.JustScales where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch


type Ratios = Vector.Vector (Ratio.Ratio Int)

-- | Hz frequency of 1/1.
just_base_control :: Score.Control
just_base_control = Score.Control "just-base"

-- | Bundle up data needed to construct a just scale.
data ScaleMap = ScaleMap {
    smap_fmt :: TheoryFormat.Format
    , smap_keys :: Keys
    , smap_default_key :: Key
    -- | Previously I would default to the 12TET of the tonic when just-base
    -- isn't set, but that doesn't work when the scale doesn't use 12TET names.
    , smap_default_base_hz :: Pitch.Hz
    , smap_named_intervals :: ScaleDegree.NamedIntervals
    -- | Sharps multiply by this, flats divide by this.
    --
    -- While these scales recognize sharps and flats, and 'input_to_note' will
    -- create them, they are still considered diatonic scales, and the
    -- accidentals are not considered part of the scale.  So chromatic
    -- transposition is treated the same as diatonic transposition, and you
    -- can't root a key on a note with an accidental.
    , smap_accidental_interval :: Double
    }

type Keys = Map.Map Pitch.Key Key

-- | Make a just scale with the given set of keys.  A \"key\" in a just scale
-- is the set of ratios and the tonic.  The number of ratios should be the same
-- as the number of scale degrees as defined by the 'TheoryFormat.Format'.  If
-- there are too many, the extras will never be reached, if too few, they'll
-- wrap around.
scale_map :: Keys -> Key -> TheoryFormat.Format -> ScaleMap
scale_map keys default_key fmt = ScaleMap
    { smap_fmt = fmt
    , smap_keys = keys
    , smap_default_key = default_key
    , smap_default_base_hz = Pitch.nn_to_hz NN.middle_c
    , smap_named_intervals = named_intervals
    , smap_accidental_interval = 16 / 15
    }

-- | TODO this should be 'repeat 2', but as long as this is also controlling
-- the input layout, it has to be piano layout, or it won't work properly with
-- kbd and midi entry.
layout :: Theory.Layout
layout = TheoryFormat.piano_layout

make_scale :: Pitch.ScaleId -> ScaleMap -> Text -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = TheoryFormat.fmt_pattern fmt
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose fmt
    , Scale.scale_enharmonics = enharmonics layout fmt
    , Scale.scale_note_to_call = note_to_call smap
    , Scale.scale_input_to_note = input2note
    , Scale.scale_input_to_nn =
        Util.computed_input_to_nn input2note (note_to_call smap)
    , Scale.scale_call_doc =
        Util.annotate_call_doc Util.standard_transposers (doc <> just_doc)
            keys_doc dummy_call
    }
    where
    dummy_call = Util.scale_degree_doc $
        ScaleDegree.scale_degree_just (smap_named_intervals smap) 0
    input2note = input_to_note (TheoryFormat.show_pitch fmt)
    fmt = smap_fmt smap
    keys_doc =
        [ (name, show_ratios (key_ratios key))
        | (name, key) <- ChromaticScales.group_keys $
            Map.toList (smap_keys smap)
        ]

show_ratios :: Ratios -> Text
show_ratios = Text.intercalate ", " . map Pretty.prettytxt . Vector.toList

just_doc :: Text
just_doc =
    "Just scales are tuned by ratios from a base frequency.\
    \ That frequency is taken from the `%just-base` control and the key.\
    \ For example, `%just-base = 440 | key = a-maj` means that A in the\
    \ middle octave is 440hz and is considered 1/1, and uses the `maj`\
    \ set of ratios.\
    \\nJust scales recognize accidentals as an offset by a fixed ratio,\
    \ but are inherently diatonic, so chromatic transposition is the same\
    \ as diatonic transposition."

read_note :: TheoryFormat.Format -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Theory.Pitch
read_note fmt key =
    TheoryFormat.fmt_to_absolute fmt key <=< TheoryFormat.read_pitch fmt

-- * input_to_note

enharmonics :: Theory.Layout -> TheoryFormat.Format -> Derive.Enharmonics
enharmonics layout fmt key note = do
    pitch <- read_note fmt key note
    return $ map (TheoryFormat.show_pitch fmt key) $
        Theory.enharmonics_of layout pitch

-- TODO the layout here is the input kbd layout!  So it should be piano_layout
-- until I get that sorted out.
input_to_note :: TheoryFormat.ShowPitch -> Maybe Pitch.Key
    -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note show_pitch _key (Pitch.InputKey nn) =
    -- I ignore the key on input.  Otherwise, a InputKey on middle C will be
    -- adjusted for the key, but I want middle C to be the sa of a relative
    -- scale.
    Just $ show_pitch Nothing $
        Theory.semis_to_pitch_sharps TheoryFormat.piano_layout $
        Theory.nn_to_semis $ floor nn

-- ** input_to_note_no_acc

-- | Unused, but still here in case I don't like accidentals.
input_to_note_no_acc :: TheoryFormat.Format -> Maybe Pitch.Key
    -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note_no_acc fmt key (Pitch.InputKey nn) =
    case (degree1, degree2) of
        (Just d1, Just d2) -> Just $ degree_note fmt key (Num.scale d1 d2 frac)
        _ -> Nothing
    where
    degree1 = nn_to_degree Vector.!? input_degree
    degree2 = nn_to_degree Vector.!? (input_degree + 1)
    (input_degree, frac) = properFraction nn

-- | Since just scales don't really have key signatures or even accidentals,
-- this only even produces sharps.
degree_note :: TheoryFormat.Format -> Maybe Pitch.Key -> Double -> Pitch.Note
degree_note fmt key degreef =
    -- TODO pitch_expr makes a call to scale_degree, so I think this is wrong!
    Pitch.Note $ ScaleDegree.pitch_expr note frac
    where
    (degree, frac) = properFraction degreef
    (octave, pc) = degree `divMod` TheoryFormat.fmt_pc_per_octave fmt
    note = TheoryFormat.show_pitch fmt key $
        Theory.Pitch octave (Theory.Note pc 0)

nn_to_degree :: Vector.Vector Double
nn_to_degree = Vector.fromList $ take 127 $
    to_steps (zip [0..] (cycle TheoryFormat.piano_intervals))
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
        Theory.transpose_pitch per_oct (oct * per_oct + steps) pitch
    where per_oct = TheoryFormat.fmt_pc_per_octave fmt

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g-maj@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
note_to_call :: ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call smap note =
    case TheoryFormat.read_pitch fmt note of
        Left _ ->
            ScaleDegree.scale_degree_interval (smap_named_intervals smap) note
        Right pitch_ ->
            let pitch = pitch_
                    { Theory.pitch_note = (Theory.pitch_note pitch_)
                        { Theory.note_accidentals = 0 }
                    }
                accs = Theory.pitch_accidentals pitch_
            in Just $ ScaleDegree.scale_degree_just
                (smap_named_intervals smap)
                (smap_accidental_interval smap ^^ accs)
                (pitch_nn smap pitch) (pitch_note fmt pitch)
    where fmt = smap_fmt smap

pitch_nn :: ScaleMap -> Theory.Pitch -> Scale.PitchNn
pitch_nn smap pitch env controls =
    Util.scale_to_pitch_error chromatic diatonic $ do
        key <- read_key smap env
        pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap)
            (Util.lookup_key env) pitch
        let hz = transpose_to_hz
                (TheoryFormat.fmt_pc_per_octave (smap_fmt smap)) base_hz
                key (chromatic + diatonic) pitch
            nn = Pitch.hz_to_nn hz
        if Num.in_range 0 127 nn then Right nn
            else Left Scale.InvalidTransposition
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls
    base_hz = Map.findWithDefault (smap_default_base_hz smap)
        just_base_control controls

pitch_note :: TheoryFormat.Format -> Theory.Pitch -> Scale.PitchNote
pitch_note fmt pitch env controls =
    Util.scale_to_pitch_error chromatic diatonic $ do
        let key = Util.lookup_key env
        pitch <- TheoryFormat.fmt_to_absolute fmt key pitch
        let transposed = Theory.transpose_pitch
                (TheoryFormat.fmt_pc_per_octave fmt)
                (round (chromatic + diatonic)) pitch
        Right $ TheoryFormat.show_pitch fmt key transposed
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

transpose_to_hz :: Theory.PitchClass -> Pitch.Hz -> Key -> Double
    -> Theory.Pitch -> Pitch.Hz
transpose_to_hz per_oct base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    (steps, frac) = properFraction frac_steps
    pitch1 = Theory.transpose_pitch per_oct (steps - key_tonic key) pitch
    pitch2 = Theory.transpose_pitch per_oct 1 pitch1
    hz1 = degree_to_hz (key_ratios key) base_hz pitch1
    hz2 = degree_to_hz (key_ratios key) base_hz pitch2

-- | Given a Key, convert a pitch in that key to its hz value, calculated as
-- a ratio from the given base hz.
degree_to_hz :: Ratios -> Pitch.Hz
    -> Theory.Pitch -- ^ should be relative to scale's tonic
    -> Pitch.Hz
degree_to_hz ratios base_hz pitch = base * realToFrac ratio
    where
    base = adjusted_base * 2 ^^ Theory.pitch_octave pitch
    -- Normalize the base_hz to lie within the first octave.
    adjusted_base = Pitch.nn_to_hz $ Pitch.hz_to_nn base_hz `Num.fmod` 12
    ratio = index_mod ratios $ Theory.note_pc (Theory.pitch_note pitch)

index_mod :: Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

-- * key

data Key = Key {
    key_tonic :: TheoryFormat.Tonic
    , key_ratios :: Ratios
    } deriving (Show)

read_key :: ScaleMap -> TrackLang.Environ -> Either Scale.ScaleError Key
read_key smap = Util.read_environ
    (\k -> Map.lookup (Pitch.Key k) (smap_keys smap))
    (smap_default_key smap) Environ.key

make_keys :: [Text] -> [(Text, Ratios)] -> Keys
make_keys degrees key_ratios = Map.fromList
    [ (Pitch.Key $ degree <> "-" <> name, Key tonic ratios)
    | (name, ratios) <- key_ratios
    , (degree, tonic) <- zip degrees [0..]
    ]

-- * format

make_relative_fmt :: Keys -> Key
    -> TheoryFormat.RelativeFormat TheoryFormat.Tonic
make_relative_fmt keys default_key = TheoryFormat.RelativeFormat
    { TheoryFormat.rel_acc_fmt = TheoryFormat.ascii_accidentals
    , TheoryFormat.rel_parse_key = fmap key_tonic . lookup_key
    , TheoryFormat.rel_default_key = 0
    , TheoryFormat.rel_show_note = TheoryFormat.show_note_diatonic
    , TheoryFormat.rel_to_absolute = TheoryFormat.diatonic_to_absolute
    }
    where
    lookup_key Nothing = Right default_key
    lookup_key (Just key) = Util.maybe_key key (Map.lookup key keys)

-- * named intervals

-- | Default named intervals.
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

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JustScales where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch


type Ratios = Vector.Vector (Ratio.Ratio Int)

-- | Hz frequency of 1/1.
just_base_control :: Score.Control
just_base_control = "just-base"

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
layout = Theory.piano_layout

make_scale :: Pitch.ScaleId -> ScaleMap -> Text -> [(Text, Text)] -> Scale.Scale
make_scale scale_id smap doc doc_fields = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = TheoryFormat.fmt_pattern fmt
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Scales.standard_transposers
    , Scale.scale_read = TheoryFormat.read_pitch fmt
    , Scale.scale_show = \key -> Right . TheoryFormat.show_pitch fmt key
    , Scale.scale_layout =
        Scale.diatonic_layout $ TheoryFormat.fmt_pc_per_octave fmt
    , Scale.scale_transpose = transpose fmt
    , Scale.scale_enharmonics = enharmonics layout fmt
    , Scale.scale_note_to_call = note_to_call scale smap
    , Scale.scale_input_to_note = input_to_note smap
    , Scale.scale_input_to_nn =
        Scales.computed_input_to_nn (input_to_note smap)
            (note_to_call scale smap)
    , Scale.scale_call_doc =
        Scales.annotate_call_doc Scales.standard_transposers (doc <> just_doc)
            doc_fields dummy_call
    }
    where
    scale = PitchSignal.Scale scale_id Scales.standard_transposers
    dummy_call = Scales.scale_degree_doc $ \scale ->
        ScaleDegree.scale_degree_just scale (smap_named_intervals smap) 0
    fmt = smap_fmt smap

-- | Group keys and format them into fields suitable to pass to 'make_scale'.
-- The 'Key's are expected to be relative, so their 'key_tonic's are ignored.
group_relative_keys :: [(Pitch.Key, Key)] -> [(Text, Text)]
group_relative_keys = mapMaybe fmt . Seq.group_eq_on snd
    where
    fmt ((name, key) :| rest) =
        Just (fmt_names (name : map fst rest), show_ratios (key_ratios key))
    fmt_names = Text.intercalate ", " . map Pitch.key_text

show_ratios :: Ratios -> Text
show_ratios = Text.intercalate ", " . map prettyt . Vector.toList

just_doc :: Text
just_doc =
    "\nJust scales are tuned by ratios from a base frequency, taken from the\
    \ `%just-base` control, as hz. Typically the \"key\" will select the set\
    \ of ratios used, dependent on the scale.\
    \\nJust scales recognize accidentals as an offset by a fixed ratio,\
    \ but are inherently diatonic, so chromatic transposition is the same\
    \ as diatonic transposition."

-- * input_to_note

enharmonics :: Theory.Layout -> TheoryFormat.Format -> Derive.Enharmonics
enharmonics layout fmt key note = do
    pitch <- TheoryFormat.read_pitch fmt key note
    return $ map (TheoryFormat.show_pitch fmt key) $
        Theory.enharmonics_of layout pitch

input_to_note :: ScaleMap -> Maybe Pitch.Key -> Pitch.Input -> Maybe Pitch.Note
input_to_note smap maybe_key (Pitch.Input kbd pitch _frac) = do
    pitch <- Scales.kbd_to_scale kbd pc_per_octave tonic pitch
    return $ TheoryFormat.show_pitch (smap_fmt smap) Nothing pitch
    where
    pc_per_octave = TheoryFormat.fmt_pc_per_octave (smap_fmt smap)
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    tonic = key_tonic $ fromMaybe (smap_default_key smap) $
        flip Map.lookup (smap_keys smap) =<< maybe_key


-- * transpose

transpose :: TheoryFormat.Format -> Derive.Transpose
transpose fmt _transposition _key steps pitch =
    return $ Pitch.add_pc per_oct steps pitch
    where per_oct = TheoryFormat.fmt_pc_per_octave fmt

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g-maj@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
note_to_call :: PitchSignal.Scale -> ScaleMap -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call scale smap note =
    case TheoryFormat.read_unadjusted_pitch fmt note of
        Left _ -> ScaleDegree.scale_degree_interval
            scale (smap_named_intervals smap) note
        Right pitch_ ->
            let pitch = pitch_
                    { Pitch.pitch_degree = (Pitch.pitch_degree pitch_)
                        { Pitch.degree_accidentals = 0 }
                    }
                accs = Pitch.pitch_accidentals pitch_
            in Just $ ScaleDegree.scale_degree_just
                scale (smap_named_intervals smap)
                (smap_accidental_interval smap ^^ accs)
                (pitch_nn smap pitch) (pitch_note fmt pitch)
    where fmt = smap_fmt smap

pitch_nn :: ScaleMap -> Pitch.Pitch -> Scale.PitchNn
pitch_nn smap pitch (PitchSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error chromatic diatonic $ do
        key <- read_key smap env
        pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap)
            (Scales.lookup_key env) pitch
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

pitch_note :: TheoryFormat.Format -> Pitch.Pitch -> Scale.PitchNote
pitch_note fmt pitch (PitchSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error chromatic diatonic $ do
        let key = Scales.lookup_key env
        pitch <- TheoryFormat.fmt_to_absolute fmt key pitch
        let transposed = Pitch.add_pc
                (TheoryFormat.fmt_pc_per_octave fmt)
                (round (chromatic + diatonic)) pitch
        Right $ TheoryFormat.show_pitch fmt key transposed
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

transpose_to_hz :: Pitch.PitchClass -> Pitch.Hz -> Key -> Double
    -> Pitch.Pitch -> Pitch.Hz
transpose_to_hz per_oct base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    (steps, frac) = properFraction frac_steps
    pitch1 = Pitch.add_pc per_oct (steps - key_tonic key) pitch
    pitch2 = Pitch.add_pc per_oct 1 pitch1
    hz1 = degree_to_hz (key_ratios key) base_hz pitch1
    hz2 = degree_to_hz (key_ratios key) base_hz pitch2

-- | Given a Key, convert a pitch in that key to its hz value, calculated as
-- a ratio from the given base hz.
degree_to_hz :: Ratios -> Pitch.Hz
    -> Pitch.Pitch -- ^ should be relative to scale's tonic
    -> Pitch.Hz
degree_to_hz ratios base_hz pitch = base * realToFrac ratio
    where
    base = adjusted_base * 2 ^^ Pitch.pitch_octave pitch
    -- Normalize the base_hz to lie within the first octave.
    -- Add an octave because of NOTE [middle-c].
    adjusted_base = Pitch.nn_to_hz $ Pitch.hz_to_nn base_hz `Num.fmod` 12 + 12
    ratio = index_mod ratios $ Pitch.degree_pc (Pitch.pitch_degree pitch)

index_mod :: Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

-- * key

data Key = Key {
    key_tonic :: TheoryFormat.Tonic
    , key_ratios :: Ratios
    } deriving (Eq, Show)

read_key :: ScaleMap -> TrackLang.Environ -> Either Scale.ScaleError Key
read_key smap = Scales.read_environ
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
    , TheoryFormat.rel_show_degree = TheoryFormat.show_degree_diatonic
    , TheoryFormat.rel_to_absolute = TheoryFormat.diatonic_to_absolute
    , TheoryFormat.rel_key_tonic = id
    }
    where
    lookup_key Nothing = Right default_key
    lookup_key (Just key) = Scales.maybe_key key (Map.lookup key keys)

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

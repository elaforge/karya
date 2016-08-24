-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JustScales where
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global


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

make_scale :: Pitch.ScaleId -> ScaleMap -> Derive.Doc
    -> [(ShowVal.Doc, ShowVal.Doc)] -> Scale.Scale
make_scale scale_id smap doc doc_fields = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = TheoryFormat.fmt_pattern fmt
    , scale_symbols = []
    , scale_transposers = Scales.standard_transposers
    , scale_read = TheoryFormat.read_pitch fmt . Scales.environ_key
    , scale_show = TheoryFormat.scale_show_pitch fmt . Scales.environ_key
    , scale_bottom = Theory.semis_to_pitch_sharps layout $ Theory.nn_to_semis 0
    , scale_layout = Scale.diatonic_layout $ TheoryFormat.fmt_pc_per_octave fmt
    , scale_transpose = transpose fmt
    , scale_enharmonics = enharmonics layout fmt
    , scale_note_to_call = note_to_call scale smap
    , scale_input_to_note = input_to_note smap
    , scale_input_to_nn = Scales.computed_input_to_nn (input_to_note smap)
        (note_to_call scale smap)
    , scale_call_doc = Scales.annotate_call_doc Scales.standard_transposers
        (doc <> just_doc) doc_fields dummy_call
    }
    where
    scale = PSignal.Scale scale_id Scales.standard_transposers
    dummy_call = Scales.scale_degree_doc $ \scale ->
        ScaleDegree.scale_degree_just scale (smap_named_intervals smap) 0
    fmt = smap_fmt smap

-- | Group keys and format them into fields suitable to pass to 'make_scale'.
-- The 'Key's are expected to be relative, so their 'key_tonic's are ignored.
group_relative_keys :: [(Pitch.Key, Key)] -> [(ShowVal.Doc, ShowVal.Doc)]
group_relative_keys = mapMaybe fmt . Seq.group_stable snd
    where
    fmt ((name, key) :| rest) =
        Just (fmt_names (name : map fst rest),
            ShowVal.Doc $ show_ratios (key_ratios key))
    fmt_names = TextUtil.join ", " . map ShowVal.doc

show_ratios :: Ratios -> Text
show_ratios = Text.intercalate ", " . map pretty . Vector.toList

just_doc :: Derive.Doc
just_doc =
    "\nJust scales are tuned by ratios from a base frequency, taken from the "
    <> ShowVal.doc just_base_control
    <> " control, as hz. Typically the \"key\" will select the set\
    \ of ratios used, dependent on the scale.\
    \\nJust scales recognize accidentals as an offset by a fixed ratio,\
    \ but are inherently diatonic, so chromatic transposition is the same\
    \ as diatonic transposition."

-- * input_to_note

enharmonics :: Theory.Layout -> TheoryFormat.Format -> Derive.Enharmonics
enharmonics layout fmt env note = do
    let key = Scales.environ_key env
    pitch <- TheoryFormat.read_pitch fmt key note
    return $ map (TheoryFormat.show_pitch fmt key) $
        Theory.enharmonics_of layout pitch

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap env (Pitch.Input kbd pitch _frac) = do
    key <- Scales.get_key (smap_default_key smap) (smap_keys smap)
        (Scales.environ_key env)
    pitch <- Scales.kbd_to_scale kbd pc_per_octave (key_tonic key) pitch
    return $ TheoryFormat.show_pitch (smap_fmt smap) Nothing pitch
    where pc_per_octave = TheoryFormat.fmt_pc_per_octave (smap_fmt smap)


-- * transpose

transpose :: TheoryFormat.Format -> Derive.Transpose
transpose fmt _transposition _key steps pitch =
    return $ Pitch.add_pc per_oct steps pitch
    where per_oct = TheoryFormat.fmt_pc_per_octave fmt

-- * note_to_call

-- | To modulate to another scale: @just-base = (hz (4g)) | key = g-maj@
-- The order is important, so the @(hz (4g))@ happens in the context of the old
-- key.
note_to_call :: PSignal.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale smap note =
    case TheoryFormat.read_relative_pitch fmt note of
        Left _ -> ScaleDegree.scale_degree_interval
            scale (smap_named_intervals smap) note
        Right relative -> Just $ ScaleDegree.scale_degree_just
                scale (smap_named_intervals smap)
                (smap_accidental_interval smap ^^ acc)
                (pitch_nn smap stripped) (pitch_note fmt stripped)
            where
            (acc, stripped) = case relative of
                TheoryFormat.RelativePitch oct pc acc ->
                    (fromMaybe 0 acc,
                        TheoryFormat.RelativePitch oct pc (Just 0))
    where fmt = smap_fmt smap

pitch_nn :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNn
pitch_nn smap relative (PSignal.PitchConfig env controls) = do
    let maybe_key = Scales.environ_key env
    key <- read_key smap maybe_key
    pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap) maybe_key relative
    let hz = transpose_to_hz per_octave base_hz key
            (octave * fromIntegral per_octave + chromatic + diatonic) pitch
    return $ Pitch.hz_to_nn hz
    where
    per_octave = TheoryFormat.fmt_pc_per_octave (smap_fmt smap)
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    base_hz = Map.findWithDefault (smap_default_base_hz smap)
        just_base_control controls

pitch_note :: TheoryFormat.Format -> TheoryFormat.RelativePitch
    -> Scale.PitchNote
pitch_note fmt relative (PSignal.PitchConfig env controls) = do
    let maybe_key = Scales.environ_key env
    -- TODO wait, shouldn't I display the relative pitch?
    pitch <- TheoryFormat.fmt_to_absolute fmt maybe_key relative
    let transposed = Pitch.add_pc
            (TheoryFormat.fmt_pc_per_octave fmt)
            (round (chromatic + diatonic)) pitch
    Right $ TheoryFormat.show_pitch fmt maybe_key transposed
    where
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

transpose_to_hz :: Pitch.PitchClass -> Pitch.Hz -> Key -> Double
    -> Pitch.Pitch -> Pitch.Hz
transpose_to_hz per_oct base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    -- The 'frac' must be positive for it to fall between pitch1 and pitch2.
    (steps, frac) = split_fraction frac_steps
    pitch1 = Pitch.add_pc per_oct (steps - key_tonic key) pitch
    pitch2 = Pitch.add_pc per_oct 1 pitch1
    hz1 = degree_to_hz (key_ratios key) base_hz pitch1
    hz2 = degree_to_hz (key_ratios key) base_hz pitch2

-- | Like 'properFraction', but the fraction is always positive.
split_fraction :: (RealFrac a, Integral b) => a -> (b, a)
split_fraction frac
    | f < 0 = (i - 1, f + 1)
    | otherwise = (i, f)
    where (i, f) = properFraction frac

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

instance Pretty.Pretty Key where
    pretty (Key tonic ratios) =
        "(Key " <> showt tonic <> " [" <> show_ratios ratios <> "])"

read_key :: ScaleMap -> Maybe Pitch.Key -> Either BaseTypes.PitchError Key
read_key smap = Scales.get_key (smap_default_key smap) (smap_keys smap)

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
    { rel_config = TheoryFormat.default_config
    , rel_key_config = TheoryFormat.KeyConfig
        { key_parse = fmap key_tonic . Scales.get_key default_key keys
        , key_default = 0
        }
    , rel_show_degree = TheoryFormat.show_degree_diatonic
    , rel_to_absolute = TheoryFormat.diatonic_to_absolute
    }

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

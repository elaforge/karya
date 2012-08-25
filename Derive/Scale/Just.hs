-- | A version of a just intonation 12 note scale that is tuned based on
-- a pitch signal.
module Derive.Scale.Just where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector

import qualified Util.Num as Num
import qualified Ui.Track as Track
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scales :: [Scale.Scale]
scales = [just_major, just_minor]

just_major :: Scale.Scale
just_major = make_scale (Pitch.ScaleId "just-major") major_ratios

just_minor :: Scale.Scale
just_minor = make_scale (Pitch.ScaleId "just-minor") minor_ratios

make_scale :: Pitch.ScaleId -> Ratios -> Scale.Scale
make_scale scale_id ratios = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-g]"
    , Scale.scale_map = Track.make_scale_map scale_map
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose
    , Scale.scale_enharmonics = Util.no_enharmonics
    , Scale.scale_note_to_call = note_to_call ratios
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn =
        Util.input_to_nn input_to_note (note_to_call ratios)
    }

scale_map :: [(Pitch.Note, Pitch.Degree)]
scale_map = [(pitch_note p, n) | (n, p) <- zip [0..] pitches]
    where
    notes = [Theory.Note pc 0 | pc <- [0..6]]
    pitches = [Theory.Pitch oct note | oct <- [-1..9], note <- notes]

-- * input_to_note

input_to_note :: Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note _key (Pitch.InputKey key_nn) = case (degree1, degree2) of
    (Just d1, Just d2) -> Just $ degree_note (Num.scale d1 d2 frac)
    _ -> Nothing
    where
    degree1 = nn_to_degree Vector.!? input_degree
    degree2 = nn_to_degree Vector.!? (input_degree + 1)
    (input_degree, frac) = properFraction key_nn

degree_note :: Double -> Pitch.Note
degree_note degreef = Pitch.Note $ Call.Pitch.note_expr note frac
    where
    (degree, frac) = properFraction degreef
    (octave, pc) = degree `divMod` pc_per_octave
    note = pitch_note $ Theory.Pitch octave (Theory.Note pc 0)

pitch_note :: Theory.Pitch -> Pitch.Note
pitch_note = Pitch.Note . Theory.show_pitch "" "" "" ""

-- | NoteNumber 0 is -1c, and becomes 2.
nn_to_degree :: Vector.Vector Double
nn_to_degree = Vector.fromList $ take 127 $
    -- NN 0 is C, so start from C.
    to_steps (zip [2..] (cycle [2, 2, 1, 2, 2, 2, 1]))
    where
    to_steps [] = []
    to_steps ((n, step) : steps)
        | step == 2 = n : n + 0.5 : to_steps steps
        | otherwise = n : to_steps steps


-- * transpose

transpose :: Maybe Pitch.Key -> Pitch.Octave -> Pitch.Transpose
    -> Pitch.Note -> Either Scale.ScaleError Pitch.Note
transpose _key oct transpose note = do
    pitch <- read_pitch note
    let steps = floor $ case transpose of
            Pitch.Chromatic steps -> steps
            Pitch.Diatonic steps -> steps
    Right $ pitch_note $ Theory.transpose_pitch pc_per_octave
        (oct * pc_per_octave + steps) pitch

pc_per_octave :: Theory.PitchClass
pc_per_octave = 7

read_pitch :: Pitch.Note -> Either Scale.ScaleError Theory.Pitch
read_pitch note = case Theory.parse_pitch (Pitch.note_text note) of
    Just pitch | valid_pitch pitch -> Right pitch
    _ -> Left Scale.UnparseableNote

valid_pitch :: Theory.Pitch -> Bool
valid_pitch pitch = Theory.note_accidentals note == 0
        && Theory.note_pc note >= 0 && Theory.note_pc note < pc_per_octave
    where note = Theory.pitch_note pitch


-- * note_to_call

-- | TODO currently there's no way to set the base freq, so modulating from one
-- just scale to another is not too useful.  Since it'll use 12TET as the base,
-- all the keys will change tuning.  If I can set base hz, it's still awkward,
-- but doable given a few extra val calls:
--
-- Set to 440.  Then if I modulate to C, set to
-- "key = 'c'", "base = * 5:4 (hz (4c))"
note_to_call :: Ratios -> Pitch.Note -> Maybe Derive.ValCall
note_to_call ratios note = case read_pitch note of
    Left _ -> Nothing
    Right pitch ->
        -- TODO need to augment note_call to get the base_hz from the environ
        Just $ Call.Pitch.note_call note (note_number pitch)
    where
    note_number :: Theory.Pitch -> Scale.GetNoteNumber
    note_number pitch chromatic diatonic maybe_str_key = do
        key <- maybe (Right default_key) read_key maybe_str_key
        let hz = transpose_to_hz ratios Nothing key (chromatic+diatonic) pitch
            nn = Pitch.hz_to_nn hz
        if Num.in_range 0 127 nn then Right nn
            else Left Scale.InvalidTransposition

transpose_to_hz :: Ratios -> Maybe Pitch.Hz -> Key -> Double
    -> Theory.Pitch -> Pitch.Hz
transpose_to_hz ratios base_hz key frac_steps pitch = Num.scale hz1 hz2 frac
    where
    (steps, frac) = properFraction frac_steps
    pitch1 = Theory.transpose_pitch pc_per_octave steps pitch
    hz1 = degree_to_hz pc_per_octave ratios base_hz key pitch1
    hz2 = degree_to_hz pc_per_octave ratios base_hz key
        (Theory.transpose_pitch pc_per_octave 1 pitch1)

-- | Given a Key, convert a pitch in that key to its hz value.
-- If the base hz is given, this is the frequency of the key's tonic.
-- Otherwise, the base is taken from the 12TET value.
degree_to_hz :: Theory.Degree -> Ratios -> Maybe Pitch.Hz
    -> Key -> Theory.Pitch -> Pitch.Hz
degree_to_hz per_oct ratios maybe_base_hz key pitch = base * ratio
    where
    base = base_hz * 2 ^^ (Theory.pitch_octave degree)
    base_hz = Pitch.nn_to_hz $ normalize_octave $
        maybe (key_tonic_nn key) Pitch.hz_to_nn maybe_base_hz
    ratio = index_mod ratios (Theory.note_pc (Theory.pitch_note degree))
    degree = Theory.transpose_pitch per_oct (- key_tonic key) pitch

-- | Normalize a NoteNumber to lie within the first 'Theory.Pitch' octave.
-- Since Pitches start at A and NoteNumbers start at C, it's -3--9 rather than
-- 0--12.
normalize_octave :: Pitch.NoteNumber -> Pitch.NoteNumber
normalize_octave nn
    | nn > -3 + 12 = normalize_octave (nn - 12)
    | otherwise = nn

-- * key

data Key = Key {
    -- | PitchClass starts at A, not C.
    key_tonic :: !Theory.PitchClass
    -- | NoteNumber in the bottom octave as returned by 'normalize_octave',
    -- e.g. -3--9.
    , key_tonic_nn :: !Pitch.NoteNumber
    } deriving (Show)

all_keys :: Map.Map Char Key
all_keys =
    Map.fromList $ take 7 [(char, Key pc nn)
        | (char, pc, nn) <- zip3 ['a'..] [0..] nns]
    where
    -- Start at A which is nn -3.  This is like 'nn_to_degree', but the other
    -- way.  Pesky pesky conversions.
    nns = scanl (+) (-3) (cycle [2, 1, 2, 2, 2, 1, 2])

default_key :: Key
Just default_key = Map.lookup 'c' all_keys

read_key :: Pitch.Key -> Either Scale.ScaleError Key
read_key (Pitch.Key [key]) =
    maybe (Left Scale.UnparseableKey) Right $ Map.lookup key all_keys
read_key _ = Left Scale.UnparseableKey

-- * ratios

index_mod :: (Vector.Unbox a) => Vector.Vector a -> Int -> a
index_mod v i = Vector.unsafeIndex v (i `mod` Vector.length v)

type Ratios = Vector.Vector Double

-- | 5-limit diatonic, with just major triads.
major_ratios :: Ratios
major_ratios = Vector.fromList [1/1, 9/8, 5/4, 4/3, 3/2, 5/3, 15/8]

-- | 5-limit diatonic, with just minor triads.
minor_ratios :: Ratios
minor_ratios = Vector.fromList [1/1, 9/8, 6/5, 4/3, 3/2, 9/5, 9/5]


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

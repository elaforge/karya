{- | The western equal tempered 12 note scale, aka 12TET.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".  General to specific is more
    aesthetically appealing.

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.

    This, along with pengisep and pengumbang, will probably require that
    scale_note_to_nn and scale_input_to_note be passed performance and input
    context respectively.  And I'll need a flip enharmonic command here.

    4c is middle C, and the range is limited to the midi range.  Since
    'Pitch.NoteNumber's also use midi numbering, conversions are trivial.

    > nn 127 = 9g
    > nn 120 = 9c
    > middle c = nn 60 = 4c
    > nn 24 = 1c
    > nn 12 = 0c
    > nn 0 = -1c
-}
module Derive.Scale.Twelve where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Unboxed as V

import qualified Util.Map as Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Ui.Track as Track
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import Derive.Scale.Theory (PitchClass(..))
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale = Scale.Scale {
    Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-g]#?"
    , Scale.scale_map =
        Track.make_scale_map [(Pitch.note_text n, fromIntegral d)
            | (n, (_, d)) <- Map.toList note_to_degree]
    , Scale.scale_symbols = [] -- later maybe I can use fancy sharps and flats
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn = input_to_nn
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

transpose :: Derive.Transpose
transpose octaves degrees note = do
    (_, d) <- Map.lookup note note_to_degree
    -- TODO do a real transpose with the key
    Map.lookup (d + Pitch.Degree (octaves*12) + degrees) degree_to_note

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = case Map.lookup note note_to_degree of
        Nothing -> Nothing
        Just (pitch, degree) ->
            Just $ Call.Pitch.note_call note
                (note_number (Theory.pitch_class pitch) degree)
    where
    note_number :: PitchClass -> Pitch.Degree -> Scale.GetNoteNumber
    note_number pc (Pitch.Degree degree) chromatic diatonic maybe_str_key = do
        dsteps <- if diatonic == 0 then Right 0 else do
            str_key <- maybe (Left Scale.KeyNeeded) Right maybe_str_key
            key <- maybe (Left Scale.UnparseableKey) Right (parse_key str_key)
            return $ transpose_diatonic key pc diatonic
        let nn = Pitch.NoteNumber $ fromIntegral degree + chromatic + dsteps
        if Num.in_range 1 127 nn then Right nn
            else Left Scale.InvalidTransposition

-- | Parse a Key string into a Key as defined here.  Keys look like:
-- c-maj, c#-min, d-dorian
parse_key :: Pitch.Key -> Maybe Key
parse_key (Pitch.Key key) = do
    let (base, mode) = break (=='-') key
    i <- Map.lookup (drop 1 mode) modes
    (pc, accs) <- Theory.parse_pitch base
    return $ Key pc accs i standard_table standard_intervals

default_key :: Key
default_key = Key C 0 0 standard_table standard_intervals

-- | The intervals of the standard church modes, starting at C major.
standard_intervals :: [Int]
standard_intervals = [2, 2, 1, 2, 2, 2, 1]

standard_table :: V.Vector Int
standard_table = make_table standard_intervals

modes :: Map.Map String Int
modes = Map.fromList $ zip
    ["maj", "dorian", "phrygian", "lydian", "mixolydian", "min", "locrian"]
    [0..]

-- | Given a Key and base Degree, turn diatonic transposition into chromatic
-- transposition.
transpose_diatonic :: Key -> PitchClass -> Double -> Double
transpose_diatonic key pc steps =
    Num.scale (transpose isteps) (transpose (isteps+1)) frac
    where
    (isteps, frac) = properFraction steps
    transpose = fromIntegral . key_transpose key pc

-- | A Key describes what diatonic transposition means.
--
-- For the purposes of transposition, it's a map from a PitchClass to the
-- number of chromatic steps of a given amount of diatonic transposition.
data Key = Key {
    -- | Tonic.
    key_base :: !PitchClass
    -- | This is actually unneeded for transposition, but it seems weird
    -- to not distinguish between C and C#.  It's needed in any case for
    -- 'Theory.transpose' and I should use the same type for both.
    , key_accidentals :: !Theory.Accidentals
    -- | The church modes are all offsets of the same pattern of whole and
    -- semi tones.  This lets me reuse the same key_table for all of them.
    , key_mode_offset :: !Int
    -- | Precalculated scan of the intervals up to a certain point.
    , key_table :: !(V.Vector Int)
    -- | The intervals in case the table doesn't have the needed transposition
    -- precalculated.
    , key_intervals :: ![Int]
    } deriving (Show)

-- Precalculate: 6 for each scale degree to start on, then 10 for up to an
-- octave plus a bit of transposition after that.  Plus 16 more for fun.
make_table :: [Int] -> V.Vector Int
make_table = V.fromList . take 32 . scanl (+) 0 . cycle

key_transpose :: Key -> PitchClass -> Int -> Int
key_transpose (Key base _ offset table intervals) pc steps =
    case table V.!? (steps + degree) of
        Nothing -> sum $ take steps $ drop degree $ cycle intervals
        Just val -> val - table V.! degree
    where
    -- C in C is 0, but if it were C minor it should be 5.
    degree = (Theory.scale_degree base pc + offset) `mod` 7

input_to_note :: Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note maybe_key (Pitch.InputKey key_nn) = do
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    let key = Maybe.fromMaybe default_key $ parse_key =<< maybe_key
    let (degree, cents) = properFraction key_nn
    note <- lookup_note key (Pitch.Degree degree)
    return $ Pitch.Note $ Call.Pitch.note_expr note cents

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

-- * constants

middle_c :: Pitch.Degree
middle_c = c4

c3, d3, e3, f3, g3, a3, b3 :: Pitch.Degree
(c3, d3, e3, f3, g3, a3, b3) = (48, 50, 52, 53, 55, 57, 59)

c4, d4, e4, f4, g4, a4, b4 :: Pitch.Degree
(c4, d4, e4, f4, g4, a4, b4) = (60, 62, 64, 65, 67, 69, 71)

c5, d5, e5, f5, g5, a5, b5 :: Pitch.Degree
(c5, d5, e5, f5, g5, a5, b5) = (72, 74, 76, 77, 79, 81, 83)

c6, d6, e6, f6, g6, a6, b6 :: Pitch.Degree
(c6, d6, e6, f6, g6, a6, b6) = (84, 86, 88, 89, 91, 93, 95)


-- * implementation

note_to_degree :: Map.Map Pitch.Note (Theory.Pitch, Pitch.Degree)
note_to_degree = Map.fromList $ filter in_range $ concat $
    [[note "#" "x" "b" "bb" p, note "`#`" "`##`" "`b`" "`bb`" p]
        | p <- pitches]
    where
    note s s2 f f2 p = (Pitch.Note $ Theory.show_pitch s s2 f f2 p,
        (p, Theory.pitch_degree p))
    pitches = [Theory.Pitch oct pc acc
        | oct <- [-1..9], pc <- [C .. B], acc <- [-2..2]]
    in_range = Num.in_range 1 128 . snd . snd

-- | What note should a degree in a particular key map to?
-- This determines the choice of sharp or flat.
--
-- What about double sharps?
--
-- c-maj a# -> a#
-- f-maj a# -> bb
lookup_note :: Key -> Pitch.Degree -> Maybe Pitch.Note
lookup_note key degree = Map.lookup degree degree_to_note
    -- notes <- Map.lookup degree degree_to_note
    -- let cs = filter ((== sharps) . (>=0) . Theory.pitch_accidentals . fst)
    --         notes
    -- undefined
    -- where
    -- sharps = Theory.accidentals_at_key (key_base key) (key_accidentals key)
    --     >= 0
    -- I'm assuming the usual order of sharps and flats, but it means I can't
    -- use exotic key signatures.

-- TODO should pick an accidental based on the key
degree_to_note :: Map.Map Pitch.Degree Pitch.Note
degree_to_note = Map.fromList
    [(d, snd (head (Seq.sort_on simple ns)))
        | (d, ns) <- Map.toList degree_to_notes]
    where
    -- select only the naturals and sharps
    simple (p, _) = if Theory.pitch_accidentals p < 0 then 999
        else Theory.pitch_accidentals p

degree_to_notes :: Map.Map Pitch.Degree [(Theory.Pitch, Pitch.Note)]
degree_to_notes =
    Map.multimap [(d, (p, n)) | (n, (p, d)) <- Map.toList note_to_degree]
    -- filter out or in ``s if I don't want those
    -- look up for d, then filter pitch with the same sign as accs

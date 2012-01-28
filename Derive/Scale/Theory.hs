-- | Functions for symbolic manipulations of pitches according to standard
-- western music theory.
--
-- TODO very incomplete
module Derive.Scale.Theory where
import qualified Data.Char as Char

import qualified Util.Pretty as Pretty
import qualified Perform.Pitch as Pitch


data Pitch = Pitch {
    pitch_octave :: Octave
    , pitch_class :: PitchClass
    , pitch_accidentals :: Accidentals
    } deriving (Eq, Show)
data PitchClass = C | D | E | F | G | A | B deriving (Enum, Ord, Show, Eq)
type Accidentals = Int -- positive for sharps, negative for flats
type Octave = Int

type ScaleDegree = Int -- 0--7

instance Pretty.Pretty Pitch where
    pretty = show_pitch "#" "x" "b" "bb"

show_pitch :: String -> String -> String -> String -> Pitch -> String
show_pitch sharp sharp2 flat flat2 (Pitch oct pc acc) =
    show oct ++ map Char.toLower (show pc) ++ accs
    where
    accs
        | acc == 0 = ""
        | acc < 0 = concat $ replicate (-x) flat2 ++ replicate s flat
        | otherwise = concat $ replicate x sharp2 ++ replicate s sharp
        where (x, s) = acc `divMod` 2

-- | TODO merge with Twelve.Key.
data Key = Key PitchClass Accidentals Mode deriving (Show)
data Mode = Major | Minor deriving (Show)


-- * Pitch

add_pc :: Int -> PitchClass -> PitchClass
add_pc n = toEnum . (`mod` 7) . (+n) . fromEnum

-- | Get the scale degree of a pitch in a certain key.  Note that accidentals
-- don't matter: Any C is 0 in any key of C, by definition.
scale_degree :: PitchClass -> PitchClass -> ScaleDegree
scale_degree key pc = (fromEnum pc - fromEnum key) `mod` 7

-- | This puts 4c at 60, -1c at 0.
pitch_degree :: Pitch -> Pitch.Degree
pitch_degree (Pitch oct pc acc) =
    Pitch.Degree $ (oct+1) * 12 + pitch_class_steps pc + acc

pitch_class_steps :: PitchClass -> Int
pitch_class_steps pc = case pc of
    C -> 0; D -> 2; E -> 4; F -> 5; G -> 7; A -> 9; B -> 11

parse_pitch :: String -> Maybe (PitchClass, Accidentals)
parse_pitch [] = Nothing
parse_pitch (p:naccs) = do
    pc <- case p of
        'a' -> Just A; 'b' -> Just B; 'c' -> Just C; 'd' -> Just D;
        'e' -> Just E; 'f' -> Just F; 'g' -> Just G;
        _ -> Nothing
    accs <- case naccs of
        "" -> Just 0
        "#" -> Just 1
        "x" -> Just 2
        "b" -> Just (-1)
        "bb" -> Just (-2)
        _ -> Nothing
    return (pc, accs)

-- * transpose

-- | In a certain key, transpose a pitch a given number of diatonic steps.
--
-- It's correct, I think, but probably slow.  Can't I do a more efficient
-- version for Twelve?
transpose :: Key -> Int -> Pitch -> Pitch
transpose key steps (Pitch oct pc acc) =
    Pitch (oct + oct2) (toEnum pc2) (acc + accidentals_at key (toEnum pc2))
    where (oct2, pc2) = (fromEnum pc + steps) `divMod` 7

relative_major :: Key -> Key
relative_major key@(Key _ _ Major) = key
relative_major (Key pc acc Minor) = Key (add_pc 2 pc) acc Major

-- Diatonic transposition always increases or decreases the letter, so
-- in CMaj, E# becomes F#.  In DMaj, then, E# must become F##.  Enharmonics
-- cannot be simplified: if E# were made F, then it "one step" would take it
-- to G.  And of course in a non equal tempered scale, E# and F have different
-- frequencies.

-- | How many accidentals does the given pc have in the given key?
accidentals_at :: Key -> PitchClass -> Accidentals
accidentals_at key = case relative_major key of
    Key pc acc _ -> accidentals_at_pitch (accidentals_at_key pc acc)

-- | How many sharps or flats should a given key have?
accidentals_at_key :: PitchClass -> Accidentals -> Accidentals
accidentals_at_key pc acc
    | acc >= 0 && pc /= F = acc*7 + case pc of
        C -> 0; G -> 1; D -> 2; A -> 3; E -> 4; B -> 5; F -> 6
    | otherwise = nflats*7 - case pc of
        C -> 0; F -> 1; B -> 2; E -> 3; A -> 4; D -> 5; G -> 6
    where nflats = if pc == C || pc == F then acc else acc + 1

-- | Given a key signature with n sharps or -n flats, how many sharps or flats
-- should the PitchClass have?
accidentals_at_pitch :: Accidentals -> PitchClass -> Accidentals
accidentals_at_pitch acc pc
    | acc >= 0 = wrapped + if pc `elem` take under7 sharps then 1 else 0
    | otherwise = wrapped + if pc `elem` take under7 flats then 1 else 0
    where
    (wrapped, under7) = acc `divMod` 7
    sharps = [F, C, G, D, A, E, B]
    flats = [B, E, A, D, G, C, F]

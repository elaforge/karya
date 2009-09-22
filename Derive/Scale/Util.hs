{-# LANGUAGE PatternGuards #-}
-- | Utilities to construct scales.
module Derive.Scale.Util where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Parse as Parse
import qualified Perform.Pitch as Pitch


-- | A number between 0 and 1, representing the portion of the way between two
-- scale degrees.  I could have used \"Cents\" for this, but that implies equal
-- temperedness.
type Frac = Double

type InputMap = Map.Map Pitch.InputKey (Pitch.NoteNumber, String)
-- | Map a scale degree to the previous nn, matching nn, and following nn.
type DegreeMap = Map.Map String
    (Maybe Pitch.NoteNumber, Pitch.NoteNumber, Maybe Pitch.NoteNumber)

note_to_nn :: DegreeMap -> Pitch.Note -> Maybe Pitch.NoteNumber
note_to_nn degree_map note = do
    (degree, frac, hz) <- split_note note
    (prev, nn, next) <- Map.lookup degree degree_map
    make_nn prev nn next frac hz

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Frac -> Int -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac hz =
    fmap (Pitch.add_hz (fromIntegral hz)) nn2
    where
    nn2
        | frac == 0 = Just nn
        | frac > 0 = fmap (\n -> interpolate nn n) mnext
        | otherwise = fmap (\p -> interpolate p nn) mprev
    interpolate low high = Num.scale low high frac

input_to_note :: InputMap -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note input_map input = flip fmap (lookup_input input input_map) $
    \(_, degree, frac) -> join_note degree frac 0

input_to_nn :: InputMap -> Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn input_map input =
    fmap (\(nn, _, _) -> nn) (lookup_input input input_map)

lookup_input :: Pitch.InputKey -> InputMap
    -> Maybe (Pitch.NoteNumber, String, Frac)
lookup_input input input_map
    | Just (nn, degree) <- at = Just (nn, degree, 0)
    | Map.null pre || Map.null post = Nothing
    | otherwise =
        let (prev_input, (prev_nn, prev_degree)) = Map.findMax pre
            (next_input, (next_nn, next_degree)) = Map.findMin post
            dist = Num.normalize (i prev_input) (i next_input) (i input)
            scaled_nn = Num.scale prev_nn next_nn dist
        in if dist > 0.5
            then Just (scaled_nn, next_degree, (dist-1) * 100)
            else Just (scaled_nn, prev_degree, dist * 100)
    | otherwise = Nothing
    where
    (pre, at, post) = Map.splitLookup input input_map
    i (Pitch.InputKey nn) = nn

-- | strip hz, convert to generic pitch (just a double), add n, put hz
-- back on
transpose :: Map.Map String Int -> Map.Map Int String -> Pitch.Transposer
transpose degree_to_num num_to_degree n note = do
    (degree, frac, hz) <- maybe (Left Pitch.NotInScale) Right (split_note note)
    num <- maybe (Left Pitch.NotInScale) Right (Map.lookup degree degree_to_num)
    let (num2, frac2) = properFraction $ n + fromIntegral num + frac
    degree2 <- maybe (Left Pitch.OutOfRange) Right
        (Map.lookup num2 num_to_degree)
    return $ join_note degree2 frac2 hz

-- I use + and - for fractional offset, and ',' for an absolute hz offset.
-- This means that scales that use these utils can't use those chars themselves
-- or the parser will get confused.
--
-- As a special case for scales that start with possibly negative number, the
-- degree may start with a + or -, since a null degree isn't very useful.

-- 4.+32,10
-- 3.,-10
join_note :: String -> Frac -> Int -> Pitch.Note
join_note degree frac hz = Pitch.Note $ degree ++ frac_s ++ hz_s
    where
    hz_s
        | hz == 0 = ""
        | otherwise = hz_char : show hz
    frac_s
        | frac == 0 = ""
        | frac > 0 = '+':s
        | otherwise = s
        where s = show (round (frac*100))

split_note :: Pitch.Note -> Maybe (String, Frac, Int)
split_note note = do
    let (degree0, rest0) = break (`elem` "-+") (Pitch.note_text note)
        (degree, rest) = if null degree0 then (rest0, "") else (degree0, rest0)
    let (frac_s, hz_s) = break (==hz_char) (drop 1 rest)
    frac <- if null frac_s then Just 0 else Parse.int frac_s
    hz <- if null hz_s then Just 0 else Parse.int (drop 1 hz_s)
    return (degree, fromIntegral frac / 100, hz)

hz_char :: Char
hz_char = ','

-- * misc

-- | Symbolic names for input keys.
[i_c, i_cs, i_d, i_ds, i_e, i_f, i_fs, i_g, i_gs, i_a, i_as, i_b]
    = [0..11] :: [Double]

{-# LANGUAGE PatternGuards #-}
-- | Utilities to construct scales.
module Derive.Scale.Util where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq
import qualified Perform.Pitch as Pitch


-- | A number between -1 and 1, representing the portion of the way between two
-- scale degrees.  I could have used \"Cents\" for this, but that implies equal
-- temperedness.
type Frac = Double

-- | This isn't Pitch.Degree because I want to force that they are integral.
-- The whole point of Degree is that it has an integral relationship with
-- notes, minus the fractional offset.
type IntDegree = Int

-- | Map a scale degree to the previous nn, matching nn, and following nn.
data ScaleMap = ScaleMap {
    smap_note_to_degree :: Map.Map Pitch.Note IntDegree
    , smap_note_to_nn :: Map.Map Pitch.Note Pitch.NoteNumber
    , smap_degree_to_nn :: Map.Map IntDegree
        (Maybe Pitch.NoteNumber, Pitch.NoteNumber, Maybe Pitch.NoteNumber)
    , smap_input_to_note :: InputMap
    } deriving (Show)

scale_map :: [Pitch.Note] -> [Pitch.InputKey] -> [Pitch.NoteNumber]
    -> [IntDegree] -> ScaleMap
scale_map notes inputs nns degrees = ScaleMap
    (Map.fromList (zip notes degrees))
    (Map.fromList (zip notes nns))
    (Map.fromList (zip degrees (Seq.zip_neighbors nns)))
    (Map.fromList (zip inputs (zip nns notes)))

type InputMap = Map.Map Pitch.InputKey (Pitch.NoteNumber, Pitch.Note)

note_to_degree :: ScaleMap -> Pitch.Note -> Maybe Pitch.Degree
note_to_degree smap note = do
    (step, frac) <- split_note note
    degree <- Map.lookup (Pitch.Note step) (smap_note_to_degree smap)
    -- TODO Degree has no field for absolute offset, I can add it if it would
    -- be useful
    return $ Pitch.Degree (fromIntegral degree + frac)

input_to_note :: ScaleMap -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note smap input = flip fmap (lookup_input input input_map) $
    \(_, step, frac) -> join_note (Pitch.note_text step) frac
    where input_map = smap_input_to_note smap

input_to_nn :: ScaleMap -> Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn smap input =
    fmap (\(nn, _, _) -> nn) (lookup_input input input_map)
    where input_map = smap_input_to_note smap

lookup_input :: Pitch.InputKey -> InputMap
    -> Maybe (Pitch.NoteNumber, Pitch.Note, Frac)
lookup_input input input_map
    | Just (nn, step) <- at = Just (nn, step, 0)
    | Map.null pre || Map.null post = Nothing
    | otherwise =
        let (prev_input, (prev_nn, prev_degree)) = Map.findMax pre
            (next_input, (next_nn, next_degree)) = Map.findMin post
            dist = Num.normalize (i prev_input) (i next_input) (i input)
            scaled_nn = Num.scale prev_nn next_nn (Pitch.NoteNumber dist)
        in if dist > 0.5
            then Just (scaled_nn, next_degree, dist - 1)
            else Just (scaled_nn, prev_degree, dist)
    | otherwise = Nothing
    where
    -- TODO use zip_neighbors?
    (pre, at, post) = Map.splitLookup input input_map
    i (Pitch.InputKey nn) = nn

degree_to_nn :: ScaleMap -> Pitch.Degree -> Maybe Pitch.NoteNumber
degree_to_nn smap (Pitch.Degree degree) = do
    let (int, frac) = properFraction degree
    (prev, nn, next) <- Map.lookup int (smap_degree_to_nn smap)
    make_nn prev nn next frac

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Frac -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high (Pitch.NoteNumber frac)

-- I use + and - for fractional offset.  This means that scales that use these
-- utils can't use those chars themselves or the parser will get confused.
--
-- As a special case for scales that start with possibly negative number, the
-- step may start with a + or -, since a null step isn't very useful.
join_note :: String -> Frac -> Pitch.Note
join_note step frac = Pitch.Note $ step ++ frac_s
    where
    frac_s
        | frac == 0 = ""
        | frac > 0 = '+':s
        | otherwise = s
        where s = show (round (frac*100))

-- | Examples: @\"4+32\" -> (4, 0.32)@, @\"-1c#-12\" -> (\"-1c#\", -0.12)@.
split_note :: Pitch.Note -> Maybe (String, Frac)
split_note (Pitch.Note note) = case frac of
        Just f -> Just (step, fromIntegral f / 100)
        Nothing -> Nothing
    where
    (degree0, rest0) = break (`elem` "-+") (drop 1 note)
    step = take 1 note ++ degree0
    frac = if null rest0 then Just 0 else Parse.int rest0

-- * misc

-- | Symbolic names for input keys.
[i_c, i_cs, i_d, i_ds, i_e, i_f, i_fs, i_g, i_gs, i_a, i_as, i_b]
    = [0..11] :: [Double]

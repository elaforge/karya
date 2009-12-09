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

-- | This isn't Pitch.Generic because I want to force that they are integral.
-- The whole point of Generic is that it has an integral relationship with
-- notes, minus the fractional offset.
type IntGeneric = Int

-- | Map a scale degree to the previous nn, matching nn, and following nn.
data ScaleMap = ScaleMap {
    smap_note_to_generic :: Map.Map Pitch.Note IntGeneric
    , smap_note_to_nn :: Map.Map Pitch.Note Pitch.NoteNumber
    , smap_generic_to_nn :: Map.Map IntGeneric
        (Maybe Pitch.NoteNumber, Pitch.NoteNumber, Maybe Pitch.NoteNumber)
    , smap_input_to_note :: InputMap
    } deriving (Show)

scale_map :: [Pitch.Note] -> [Pitch.InputKey] -> [Pitch.NoteNumber]
    -> [IntGeneric] -> ScaleMap
scale_map notes inputs nns generics = ScaleMap
    (Map.fromList (zip notes generics))
    (Map.fromList (zip notes nns))
    (Map.fromList (zip generics (Seq.zip_neighbors nns)))
    (Map.fromList (zip inputs (zip nns notes)))

type InputMap = Map.Map Pitch.InputKey (Pitch.NoteNumber, Pitch.Note)

note_to_generic :: ScaleMap -> Pitch.Note -> Maybe Pitch.Generic
note_to_generic smap note = do
    (degree, frac) <- split_note note
    generic <- Map.lookup (Pitch.Note degree) (smap_note_to_generic smap)
    -- TODO Generic has no field for absolute offset, I can add it if it would
    -- be useful
    return $ Pitch.Generic (fromIntegral generic + frac)

input_to_note :: ScaleMap -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note smap input = flip fmap (lookup_input input input_map) $
    \(_, degree, frac) -> join_note (Pitch.note_text degree) frac
    where input_map = smap_input_to_note smap

input_to_nn :: ScaleMap -> Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn smap input =
    fmap (\(nn, _, _) -> nn) (lookup_input input input_map)
    where input_map = smap_input_to_note smap

lookup_input :: Pitch.InputKey -> InputMap
    -> Maybe (Pitch.NoteNumber, Pitch.Note, Frac)
lookup_input input input_map
    | Just (nn, degree) <- at = Just (nn, degree, 0)
    | Map.null pre || Map.null post = Nothing
    | otherwise =
        let (prev_input, (prev_nn, prev_degree)) = Map.findMax pre
            (next_input, (next_nn, next_degree)) = Map.findMin post
            dist = Num.normalize (i prev_input) (i next_input) (i input)
            scaled_nn = Num.scale prev_nn next_nn dist
        in if dist > 0.5
            then Just (scaled_nn, next_degree, dist - 1)
            else Just (scaled_nn, prev_degree, dist)
    | otherwise = Nothing
    where
    -- TODO use zip_neighbors?
    (pre, at, post) = Map.splitLookup input input_map
    i (Pitch.InputKey nn) = nn


generic_to_nn :: ScaleMap -> Pitch.Generic -> Maybe Pitch.NoteNumber
generic_to_nn smap (Pitch.Generic generic) = do
    let (int, frac) = properFraction generic
    (prev, nn, next) <- Map.lookup int (smap_generic_to_nn smap)
    make_nn prev nn next frac

make_nn :: Maybe Pitch.NoteNumber -> Pitch.NoteNumber -> Maybe Pitch.NoteNumber
    -> Frac -> Maybe Pitch.NoteNumber
make_nn mprev nn mnext frac
    | frac == 0 = Just nn
    | frac > 0 = fmap (\next -> interpolate nn next) mnext
    | otherwise = fmap (\prev -> interpolate prev nn) mprev
    where
    interpolate low high = Num.scale low high frac

-- I use + and - for fractional offset.  This means that scales that use these
-- utils can't use those chars themselves or the parser will get confused.
--
-- As a special case for scales that start with possibly negative number, the
-- degree may start with a + or -, since a null degree isn't very useful.
join_note :: String -> Frac -> Pitch.Note
join_note degree frac = Pitch.Note $ degree ++ frac_s
    where
    frac_s
        | frac == 0 = ""
        | frac > 0 = '+':s
        | otherwise = s
        where s = show (round (frac*100))

-- | Examples: @"4+32" -> (4, 0.32)@, @"-1c#-12" -> ("-1c#", -0.12)@.
split_note :: Pitch.Note -> Maybe (String, Frac)
split_note (Pitch.Note note) = case frac of
        Just f -> Just (degree, fromIntegral f / 100)
        Nothing -> Nothing
    where
    (degree0, rest0) = break (`elem` "-+") (drop 1 note)
    degree = take 1 note ++ degree0
    frac = if null rest0 then Just 0 else Parse.int rest0

-- * misc

-- | Symbolic names for input keys.
[i_c, i_cs, i_d, i_ds, i_e, i_f, i_fs, i_g, i_gs, i_a, i_as, i_b]
    = [0..11] :: [Double]

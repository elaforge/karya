-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Notation for Carnatic solkattu.
--
-- This is actually a separate library that's independent of Derive.  The only
-- connection is that its final output can be stroke names for some instrument
-- and thus easily inserted into a track.
module Derive.Solkattu.Solkattu where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Global


type Sequence stroke = [Note stroke]
type Error = Text

data Note stroke =
    -- | A Sollu can carry an explicit stroke, for exceptions to the usual
    -- inference.  The concrete stroke type depends on the instrument it's
    -- being realized on.
    Sollu Sollu (Maybe stroke)
    | Rest
    -- | Set pattern with the given duration.
    | Pattern Matras
    | Alignment Alignment
    | TimeChange TimeChange
    deriving (Eq, Ord, Show)

map_stroke :: (Maybe a -> Maybe b) -> Note a -> Note b
map_stroke f n = case n of
    Sollu sollu stroke -> Sollu sollu (f stroke)
    Rest -> Rest
    Pattern a -> Pattern a
    Alignment a -> Alignment a
    TimeChange a -> TimeChange a

data Alignment = Akshara Aksharas | Arudi
    deriving (Eq, Ord, Show)

data TimeChange = Speed Speed | Nadai Matras
    deriving (Eq, Ord, Show)

-- | Each speed increase doubles the number of 'Matras' per akshara.  As
-- documented in 'Matras', this is a nonstandard use of the term.
data Speed = S1 | S2 | S3 | S4 deriving (Eq, Ord, Show, Bounded, Enum)

instance Pretty.Pretty Speed where pretty = showt

speed_factor :: Speed -> Double
speed_factor s = case s of
    S1 -> 1
    S2 -> 2
    S3 -> 4
    S4 -> 8

factor_speed :: Int -> Maybe Speed
factor_speed n = case n of
    1 -> Just S1
    2 -> Just S2
    4 -> Just S3
    8 -> Just S4
    _ -> Nothing

instance Pretty.Pretty stroke => Pretty.Pretty (Note stroke) where
    pretty n = case n of
        Sollu s stroke -> maybe (pretty s)
            (\stroke -> (pretty s <> "!" <> pretty stroke)) stroke
        Rest -> "__"
        Pattern d -> "p" <> showt d
        Alignment (Akshara n) -> "@" <> showt n
        Alignment Arudi -> "@X"
        TimeChange change -> pretty change

instance Pretty.Pretty TimeChange where
    pretty (Speed s) = "speed " <> showt s
    pretty (Nadai s) = "nadai " <> showt s

data Sollu =
    Dheem | Dhom | Di | Din | Dit
    | Ga | Gin | Ka | Ki | Ku | Lang
    | Mi | Na | Nam | Ri | Ta | Tam | Tang
    | Tat | Tha | Thom | Ti
    deriving (Eq, Ord, Show)

instance Pretty.Pretty Sollu where
    pretty = Text.toLower . showt

-- | An akshara is one count of the talam.
type Aksharas = Int

-- | A matra is an akshara divided by the nadai divided by the 'speed_factor'.
-- It corresponds to a single sollu.
--
-- This is nonstandard usage since an actual matra doesn't depend on speed, so
-- it can be fractional when >S1, but it's more convenient for me to have
-- a variable time unit corresponding to a single sollu.
type Matras = Int

duration_of :: Sequence stroke -> Matras
duration_of = sum . map note_duration

note_duration :: Note stroke -> Matras
note_duration n = case n of
    Sollu {} -> 1
    Rest -> 1
    Pattern dur -> dur
    Alignment {} -> 0
    TimeChange {} -> 0

data Tala = Tala {
    tala_aksharas :: !Aksharas
    , tala_arudi :: !Aksharas
    , tala_nadai :: !Matras
    } deriving (Show)

instance Pretty.Pretty Tala where
    format (Tala aksharas arudi nadai) = Pretty.record "Tala"
        [ ("aksharas", Pretty.format aksharas)
        , ("arudi", Pretty.format arudi)
        , ("nadai", Pretty.format nadai)
        ]

adi_tala :: Matras -> Tala
adi_tala = Tala 8 4

-- | Keep track of timing and tala position.
data State = State {
    state_avartanam :: !Int
    , state_akshara :: !Aksharas
    -- | This is not 'Matras' because it's actual fractional matras, rather
    -- than sollu-durations.
    , state_matra :: !Double
    , state_speed :: !Speed
    , state_nadai :: !Int
    } deriving (Show)

initial_state :: Tala -> State
initial_state tala = State 0 0 0 S1 (tala_nadai tala)

-- | Verify that the notes start and end at Sam, and the given Alignments
-- fall where expected.
verify_alignment :: Pretty.Pretty stroke => Tala -> [Note stroke]
    -> Either [Text] [Note stroke]
verify_alignment tala =
    verify_result . filter not_empty . map_time tala verify
        . (Alignment (Akshara 0) :) . (++[Alignment (Akshara 0)])
    where
    not_empty (Left err) | Text.null err = False
    not_empty _ = True
    verify state note = second (:[]) $ case note of
        Sollu {} -> (Right 1, Right note)
        Rest -> (Right 1, Right note)
        Pattern matras -> (Right (fromIntegral matras), Right note)
        Alignment align -> (Right 0, verify_align state align)
        TimeChange change -> (Left change, Right note)
    verify_result vals
        | null errs = Right ok
        | otherwise = Left $
            map (either id (Text.unwords . map pretty)) (group_rights vals)
        where (errs, ok) = Either.partitionEithers vals
    verify_align state align
        | state_akshara state == expected && state_matra state == 0 = Left ""
        | otherwise = Left $ "expected " <> showt align
            <> ", but at avartanam " <> showt (state_avartanam state + 1)
            <> ", akshara " <> showt (state_akshara state)
            <> ", matra " <> showt (state_matra state)
        where
        expected = case align of
            Akshara n -> n
            Arudi -> tala_arudi tala

time_change :: TimeChange -> State -> State
time_change change state = case change of
    Speed s -> state { state_speed = s }
    Nadai s -> state { state_nadai = s }

-- | Map over notes, keeping track of the position in the talam.
map_time :: Tala -> (State -> a -> (Either TimeChange Double, [b]))
    -- ^ (either a time change or matras to advance, results)
    -> [a] -> [b]
map_time tala f = concat . snd . List.mapAccumL process (initial_state tala)
    where
    process state note =
        (either time_change (advance_state tala) advance state, vals)
        where (advance, vals) = f state note

advance_state :: Tala -> Double -> State -> State
advance_state tala matras state = state
    { state_avartanam = state_avartanam state + akshara_carry
    , state_akshara = akshara
    , state_matra = matra
    }
    where
    (akshara_carry, akshara) =
        (state_akshara state + matra_carry) `divMod` tala_aksharas tala
    (matra_carry, matra) = (state_matra state + matras * factor)
        `Num.fDivMod` fromIntegral (state_nadai state)
    factor = 1 / speed_factor (state_speed state)

-- * transform

-- | Drop a number of matras from the Sequence.  Patterns will be shortened.
dropM :: Matras -> Sequence stroke -> Sequence stroke
dropM matras ns = case ns of
    [] -> []
    (n:ns)
        | matras <= 0 -> (n:ns)
        | otherwise -> case n of
            Pattern dur
                | dur > matras -> Pattern (dur - matras) : ns
                | otherwise -> dropM (matras - dur) ns
            _ -> dropM (matras - note_duration n) ns

takeM :: Matras -> Sequence stroke -> Sequence stroke
takeM _ [] = []
takeM matras _ | matras <= 0 = []
takeM matras (n:ns) = case n of
    Sollu {} -> n : takeM (matras-1) ns
    Rest {} -> n : takeM (matras-1) ns
    Pattern dur
        | dur > matras -> n : takeM (matras-dur) ns
        | otherwise -> [Pattern (dur - matras)]
    Alignment {} -> takeM matras ns
    TimeChange {} -> takeM matras ns

rdropM :: Matras -> Sequence stroke -> Sequence stroke
rdropM matras = reverse . dropM matras . reverse

-- | Reduce three times, with a separator.
reduce3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduce3 n sep = List.intercalate sep . take 3 . iterate (dropM n)

-- | Reduce by a duration until a final duration.
reduceTo :: CallStack.Stack => Matras -> Matras -> Sequence stroke
    -> Sequence stroke
reduceTo by to seq
    | (duration_of seq - to) `mod` by /= 0 =
        errorStack $ showt (duration_of seq) <> " can't reduce by " <> showt by
            <> " to " <> showt to
    | otherwise = mconcat $ takeWhile ((>=to) . duration_of) $
        iterate (dropM by) seq

-- | Reduce by dropping the end.
reduceR :: Matras -> Sequence stroke -> [Sequence stroke]
reduceR n = iterate (rdropM n)

reduceR3 :: Matras -> Sequence stroke -> Sequence stroke -> Sequence stroke
reduceR3 dur sep = List.intercalate sep . take 3 . reduceR dur

-- | Start fully reduced, and expand to the given sequence.
expand :: Int -> Matras -> Sequence stroke -> [Sequence stroke]
expand times dur = reverse . take times . iterate (dropM dur)

-- ** vary

data PatternFamily = A | B | C | D | E | F | G deriving (Eq, Ord, Show)
type Variations = [(Matras, Matras, Matras)]

-- | Variation means replacing a triad of patterns of the same duration with a
-- an increasing or decreasing sequence.  For instance, 666 can become 567,
-- 765, or 777 can become 678 or 579 or their inverses.
--
-- TODO Variation on a higher order is also possible, so for instance 777, 777,
-- 777 may become 666, 777, 888
--
-- TODO Also we have 5, 55, 555 -> 55, 55, 55 -> 555, 55, 5.  This actually
-- applies to more than just Patterns, e.g. 3 as tadin_.  I think this is
-- orthogonal and could get a different function.
vary :: (Matras -> Variations) -- ^ variations allowed for this duration
    -> Sequence stroke -> [Sequence stroke]
vary allowed_variations notes
    | null modification_groups = [notes]
    | otherwise = map apply modification_groups
    where
    -- List of sets of permutations.
    modification_groups = permute_fst allowed_variations (find_triads notes)
    -- Apply a set of permutations to the original input.
    apply mods = apply_modifications (\_ matras -> Pattern matras)
        (concatMap extract mods) notes
    extract ((m1, m2, m3), (i1, i2, i3)) = [(i1, m1), (i2, m2), (i3, m3)]

variations :: [(Matras, Matras, Matras) -> Bool] -> (Matras -> Variations)
variations filters = filter (\v -> all ($v) filters) . all_variations

ascending, descending, standard :: (Matras, Matras, Matras) -> Bool
ascending (m1, m2, m3) = m1 < m2 && m2 < m3
descending (m1, m2, m3) = m1 > m2 && m2 > m3
standard (m1, m2, m3) =
    m1 == m2 && m2 == m3
    || List.sort [m1, m2, m3] `elem` [[5, 6, 7], [6, 7, 8], [5, 7, 9]]

all_variations :: Matras -> Variations
all_variations matras = concatMap vars [0 .. max 1 (matras - min_duration)]
    where
    vars d
        | d == 0 = [(matras, matras, matras)]
        | otherwise =
            [ (matras - d, matras, matras + d)
            , (matras + d, matras, matras - d)
            ]
    min_duration = 3

-- | Find triples of Patterns with the same length and return their indices.
-- The indices are in ascending order.
find_triads :: [Note stroke] -> [(Matras, (Int, Int, Int))]
find_triads notes =
    [ (matras, triad)
    | (matras, indices) <- Seq.group_fst
        [(matras, i) | (i, Pattern matras) <- zip [0..] notes]
    , triad <- triads indices
    ]
    where
    triads (x1:x2:x3:xs) = (x1, x2, x3) : triads xs
    triads _ = []

-- * misc

check :: CallStack.Stack => Either Error a -> a
check = either errorStack id

check_msg :: CallStack.Stack => Text -> Either Error a -> a
check_msg msg = either (errorStack . ((msg <> ": ") <>)) id

-- * util

splits :: [a] -> [([a], [a])]
splits xs = drop 1 $ zip (List.inits xs) (List.tails xs)

-- | Round the first argument up to the next multiple of the second.
round_up :: Integral a => a -> a -> a
round_up a b = b * ceiling (fromIntegral a / fromIntegral b)

-- | Split when the function returns Just, and pair that value with the
-- subsequent elements.
split_just :: (a -> Maybe b) -> b -> [a] -> [(b, [a])]
split_just f initial xs = go [] initial (zip (map f xs) xs)
    where
    go accum key ((mb, a) : rest) = case mb of
        Nothing -> go (a : accum) key rest
        Just b -> (key, reverse accum) : go [a] b rest
    go accum key [] = [(key, reverse accum)]

-- | Group consecutive Rights.
group_rights :: [Either a b] -> [Either a [b]]
group_rights xs = case rest of
    [] -> cons []
    Left x : xs -> cons $ Left x : group_rights xs
    Right x : xs -> Right [x] : group_rights xs
    where
    (rights, rest) = Seq.span_while (either (const Nothing) Just) xs
    cons = if null rights then id else (Right rights :)

apply_modifications :: (a -> mod -> a) -> [(Int, mod)]
    -- ^ modifications along with their indices, in ascending order
    -> [a] -> [a]
apply_modifications apply mods = go mods . zip [0..]
    where
    go [] xs = map snd xs
    go _ [] = []
    go ((i1, mod) : mods) ((i2, x) : xs)
        | i1 < i2 = go mods ((i2, x) : xs)
        | i1 == i2 = apply x mod : go mods xs
        | otherwise = x : go ((i1, mod) : mods) xs

permute_fst :: (a -> [b]) -> [(a, x)] -> [[(b, x)]]
permute_fst _ [] = []
permute_fst permutations ((k, x) : xs)
    | null xs = [[(p, x)] | p <- permutations k]
    | otherwise =
        [(p, x) : rest | p <- permutations k, rest <- go xs]
    where go = permute_fst permutations

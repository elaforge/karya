-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Notation for Carnatic solkattu.
--
-- This is actually a separate library that's independent of Derive.  The only
-- connection is that its final output can be stroke names for some instrument
-- and thus easily inserted into a track.
module Derive.Solkattu.Solkattu where
import qualified Data.List as List
import qualified Data.Ratio as Ratio
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Global


type Sequence stroke = [Note stroke]
type Error = Text

data Note stroke =
    -- | A Sollu can carry an explicit stroke, for exceptions to the usual
    -- inference.  The concrete stroke type depends on the instrument it's
    -- being realized on.
    Sollu Sollu Karvai (Maybe stroke)
    | Rest
    -- | Set pattern with the given duration.
    | Pattern Matras
    | Alignment Alignment
    | TimeChange TimeChange
    deriving (Eq, Ord, Show)

-- | If it's a karvai stroke, and it's followed by a rest, it will replace the
-- rest.  Otherwise, it will be replaced by a note.
data Karvai = Karvai | NotKarvai deriving (Eq, Ord, Show)

map_stroke :: (Maybe a -> Maybe b) -> Note a -> Note b
map_stroke f n = case n of
    Sollu sollu karvai stroke -> Sollu sollu karvai (f stroke)
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

speed_factor :: Speed -> Duration
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
        Sollu sollu karvai stroke -> mconcat $ filter (not . Text.null)
            [ pretty sollu
            , if karvai == Karvai then "(k)" else ""
            , maybe "" (("!"<>) . pretty) stroke
            ]
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

-- | A matra is an akshara divided by the nadai.  It corresponds to a single
-- sollu in first speed.  'Duration' is used for absolute, Nadai and Speed
-- independent duration.
type Matras = Int

-- | A single Duration unit is equivalent to 1 Akshara.
newtype Duration = Duration Ratio.Rational
    deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac)
    -- deriving Real produces:
    -- <no location info>: Warning:
    -- Call of toRational :: Rational -> Rational
    --   can probably be omitted
-- type Duration = Ratio.Rational

instance Pretty.Pretty Duration where
    pretty (Duration dur) =
        pretty (whole :: Int) <> if frac == 0 then "" else " " <> pretty frac
        where (whole, frac) = properFraction dur

durations_of :: Sequence stroke -> [Duration]
durations_of = snd . List.mapAccumL dur (S1, 4)
    where
    dur (speed, nadai) n = case n of
        TimeChange change -> case change of
            Speed speed -> ((speed, nadai), 0)
            Nadai nadai -> ((speed, nadai), 0)
        _ -> ((speed, nadai), fromIntegral (note_matras n)
            / speed_factor speed / fromIntegral nadai)

duration_of :: Sequence stroke -> Duration
duration_of = sum . durations_of

matras_of :: Sequence stroke -> Matras
matras_of = sum . map note_matras
    -- TODO take speed changes into account

note_matras :: Note stroke -> Matras
note_matras n = case n of
    Sollu {} -> 1
    Rest -> 1
    Pattern dur -> dur
    Alignment {} -> 0
    TimeChange {} -> 0

data Tala = Tala {
    tala_aksharas :: !Aksharas
    , tala_arudi :: !Aksharas
    , tala_nadai :: !Matras
    } deriving (Eq, Show)

instance Pretty.Pretty Tala where
    format (Tala aksharas arudi nadai) = Pretty.record "Tala"
        [ ("aksharas", Pretty.format aksharas)
        , ("arudi", Pretty.format arudi)
        , ("nadai", Pretty.format nadai)
        ]

-- tala_matras :: Tala -> Matras
-- tala_matras tala = tala_aksharas tala * tala_nadai tala

adi_tala :: Matras -> Tala
adi_tala = Tala 8 4

-- | Keep track of timing and tala position.
data State = State {
    state_avartanam :: !Int
    , state_akshara :: !Aksharas
    -- | Time through this akshara, so this is always < 1.
    -- TODO actually this is not matras, but fraction of the way through the
    -- akshara.  Is there a better term?
    , state_matra :: !Duration
    -- | How many nadai in this akshara.  This is different from 'state_nadai'
    -- because if nadai changes in the middle of an akshara, that akshara will
    -- have an irregular number of matra in it.  For instance, if you change
    -- from nadai 4 to 3 at matra 2, then you have a 2+3 = 5 matra akshara.
    , state_akshara_nadai :: !Int
    , state_speed :: !Speed
    , state_nadai :: !Int
    } deriving (Show)

instance Pretty.Pretty State where
    format (State avartanam akshara matra akshara_nadai speed nadai) =
        Pretty.record "State"
            [ ("avartanam", Pretty.format avartanam)
            , ("akshara", Pretty.format akshara)
            , ("matra", Pretty.format matra)
            , ("akshara_nadai", Pretty.format akshara_nadai)
            , ("speed", Pretty.format speed)
            , ("nadai", Pretty.format nadai)
            ]

initial_state :: Tala -> State
initial_state tala = State
    { state_avartanam = 0
    , state_akshara = 0
    , state_matra = 0
    , state_akshara_nadai = tala_nadai tala
    , state_speed = S1
    , state_nadai = tala_nadai tala
    }

-- | A Karvai Sollu followed by a Rest will replace the rest, if followed by
-- a Sollu, the Karvai will be dropped.
cancel_karvai :: [Note stroke] -> [Note stroke]
cancel_karvai = go
    where
    go (Sollu sollu Karvai stroke : rest) = case drop_next_rest rest of
        (True, rest) -> Sollu sollu Karvai stroke : go rest
        (False, rest) -> go rest
    go (n:ns) = n : go ns
    go [] = []

drop_next_rest :: [Note stroke] -> (Bool, [Note stroke])
drop_next_rest (n:ns) = case n of
    Rest -> (True, ns)
    Sollu {} -> (False, n:ns)
    _ -> (n:) <$> drop_next_rest ns
drop_next_rest [] = (False, [])

-- | Verify that the notes start and end at Sam, and the given Alignments
-- fall where expected.
verify_alignment :: Pretty.Pretty stroke => Tala -> [Note stroke]
    -> ([Note stroke], [Text])
    -- ^ Return warnings in addition to notes, rather than just failing with
    -- a Left.  This is because the misaligned notes are easier to read if I
    -- realize them down to strokes.
verify_alignment tala =
    verify_result . filter (not . is_empty) . map flatten . map_time tala verify
        . (Alignment (Akshara 0) :) . (++[Alignment (Akshara 0)])
    where
    flatten = either Left (either Left Right)
    verify state note = second (:[]) $ case note of
        Sollu {} -> (Right 1, Right note)
        Rest -> (Right 1, Right note)
        Pattern matras -> (Right (fromIntegral matras), Right note)
        Alignment align -> (Right 0, verify_align state align)
        TimeChange change -> (Left change, Right note)
    -- Stop at the first error, since if I'm already wrong then later errors
    -- are probably just noise.
    verify_result vals = case right_until_left vals of
        (notes, Nothing) -> (notes, [])
        (notes, Just err) -> (notes, [Text.unwords (map pretty notes), err])
    is_empty (Left err) = Text.null err
    is_empty _ = False
    verify_align state align
        | state_akshara state == expected && state_matra state == 0 = Left ""
        | otherwise = Left $ "expected akshara " <> showt expected
            <> ", but at " <> show_position state
        where
        expected = case align of
            Akshara n -> n
            Arudi -> tala_arudi tala

-- | Map over notes, keeping track of the position in the talam.
map_time :: Tala -> (State -> a -> (Either TimeChange Matras, [b]))
    -- ^ return (either a time change or Matras to advance, results)
    -- The Matras here are assuming 'S1', so they still need to be divided by
    -- speed to get Duration.
    -> [a] -> [Either Text b]
map_time tala f = concat . snd . List.mapAccumL process (initial_state tala)
    where
    process state note = case change_advance of
        Left change -> case time_change change state of
            Right state -> (state, map Right vals)
            Left err -> (state, [Left err])
        Right advance -> (advance_state tala advance state, map Right vals)
        where (change_advance, vals) = f state note

time_change :: TimeChange -> State -> Either Text State
time_change change state = case change of
    Speed speed -> Right $ state { state_speed = speed }
    Nadai nadai -> do
        akshara_nadai <- nadai_change nadai state
        Right $ state
            { state_nadai = nadai
            , state_akshara_nadai = akshara_nadai
            }

nadai_change :: Matras -> State -> Either Text Int
nadai_change new_nadai state
    | frac == 0 = Right nadai
    | otherwise = Left $ show_position state
        <> ": can't change nadai " <> showt old_nadai <> "->" <> showt new_nadai
        <> " at " <> pretty (state_matra state) <> " akshara, would be a "
        <> pretty pre <> " + " <> pretty post
        <> " = " <> pretty (pre + post) <> " matra akshara"
    where
    (nadai, frac) = properFraction (pre + post)
    pre = (1 - ratio) * fromIntegral new_nadai
    post = ratio * fromIntegral old_nadai
    -- Ratio of the way through the akshara.
    ratio = state_matra state -- / fromIntegral old_nadai
    old_nadai = state_nadai state

advance_state :: Tala -> Matras -> State -> State
advance_state tala matras state = state
    { state_avartanam = state_avartanam state + akshara_carry
    , state_akshara = akshara
    , state_matra = matra
    , state_akshara_nadai = if matra_carry > 0
        then state_nadai state else state_akshara_nadai state
    }
    where
    advance = fromIntegral matras / speed_factor (state_speed state)
        / fromIntegral (state_nadai state)
    (matra_carry, matra) = properFraction $ state_matra state + advance
    (akshara_carry, akshara) = (state_akshara state + matra_carry)
        `divMod` tala_aksharas tala

show_position :: State -> Text
show_position state =
    "avartanam " <> showt (state_avartanam state + 1)
    <> ", akshara " <> showt (state_akshara state)
    <> ", matra "
    <> pretty (state_matra state * fromIntegral (state_nadai state))


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

-- | Collect Rights until I hit a Left.
right_until_left :: [Either a b] -> ([b], Maybe a)
right_until_left = go
    where
    go [] = ([], Nothing)
    go (Left a : _) = ([], Just a)
    go (Right b : xs) = first (b:) (go xs)

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

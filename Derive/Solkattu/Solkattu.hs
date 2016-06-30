-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams #-}
-- | Notation for Carnatic solkattu.
--
-- This is actually a separate library that's independent of Derive.  The only
-- connection is that its final output can be stroke names for some instrument
-- and thus easily inserted into a track.
module Derive.Solkattu.Solkattu where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Global


type Sequence stroke = [Note stroke]

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
    deriving (Eq, Show)

data Alignment = Akshara Aksharas | Arudi
    deriving (Eq, Show)

data TimeChange = Speed Speed | Nadai Matras
    deriving (Eq, Show)

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
    | Mi | Na | Ri | Ta | Tam | Tang
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
    -- | This is not 'Matras' because it's actual fraction matras, rather than
    -- sollu-durations.
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
    verify_result . filter not_empty
        . snd . List.mapAccumL verify (initial_state tala)
        . (Alignment (Akshara 0) :) . (++[Alignment (Akshara 0)])
    where
    not_empty (Left err) | Text.null err = False
    not_empty _ = True
    verify state note = case note of
        Sollu {} -> (advance 1, Right note)
        Rest -> (advance 1, Right note)
        Pattern matras -> (advance (fromIntegral matras), Right note)
        Alignment align -> (state, verify_align state align)
        TimeChange change -> (time_change change state, Right note)
        where advance n = advance_state tala n state
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

-- * misc

check :: Log.Stack => Either Text a -> a
check = either (errorStack . untxt) id

check_msg :: Log.Stack => String -> Either Text a -> a
check_msg msg = either (errorStack . ((msg <> ": ") <>) . untxt) id

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

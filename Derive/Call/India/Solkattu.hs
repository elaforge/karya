-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams, LambdaCase #-}
-- | Notate Carnatic solkattu and realize to mridangam fingering.
module Derive.Call.India.Solkattu where
import qualified Data.Either as Either
import qualified Data.Fixed as Fixed
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Global


type Sequence = [Note]

data Note =
    Sollu Sollu (Maybe Stroke)
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

speed_factor :: Speed -> Double
speed_factor s = case s of
    S1 -> 1
    S2 -> 2
    S3 -> 4
    S4 -> 8

instance Pretty.Pretty Note where
    pretty n = case n of
        Sollu s stroke -> maybe (pretty s)
            (\stroke -> ("st " <> pretty stroke <> " " <> pretty s)) stroke
        Rest -> "__"
        Pattern d -> "p" <> showt d
        Alignment (Akshara n) -> "@" <> showt n
        Alignment Arudi -> "@X"
        TimeChange change -> pretty change

instance Pretty.Pretty TimeChange where
    pretty (Speed s) = "speed " <> showt s
    pretty (Nadai s) = "nadai " <> showt s

data Sollu = Ta | Di | Ki | Thom -- ta di ki ta thom
    | Na | Ka | Ti | Ku | Ri -- nakatikutari
    | Din | Gin -- ta din gin na tom
    | Dit | Dheem
    | Tam | Tang | Lang -- generally means chapu
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

duration_of :: Sequence -> Matras
duration_of = sum . map note_duration

note_duration :: Note -> Matras
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
verify_alignment :: Tala -> [Note] -> Either [Text] [Note]
verify_alignment tala =
    check . filter (/= Left "")
        . snd . List.mapAccumL verify (initial_state tala)
        . (Alignment (Akshara 0) :) . (++[Alignment (Akshara 0)])
    where
    verify state note = case note of
        Sollu {} -> (advance 1, Right note)
        Rest -> (advance 1, Right note)
        Pattern matras -> (advance (fromIntegral matras), Right note)
        Alignment align -> (state, verify_align state align)
        TimeChange change -> (time_change change state, Right note)
        where advance n = advance_state tala n state
    check vals
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
        `fDivMod` fromIntegral (state_nadai state)
    factor = 1 / speed_factor (state_speed state)

-- * transform

-- | Drop a number of matras from the Sequence.  Patterns will be shortened.
dropM :: Matras -> Sequence -> Sequence
dropM matras ns = case ns of
    [] -> []
    (n:ns)
        | matras <= 0 -> (n:ns)
        | otherwise -> case n of
            Pattern dur
                | dur > matras -> Pattern (dur - matras) : ns
                | otherwise -> dropM (matras - dur) ns
            _ -> dropM (matras - note_duration n) ns

takeM :: Matras -> Sequence -> Sequence
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

-- * realize

data Korvai = Korvai {
    korvai_sequence :: Sequence
    , korvai_mridangam :: StrokeMap
    , korvai_tala :: Tala
    } deriving (Show)

-- | [Sollu] and Strokes should be the same length.  This is enforced in the
-- constructor 'stroke_map'.
newtype StrokeMap = StrokeMap (Map.Map [Sollu] [Stroke])
    deriving (Show, Pretty.Pretty, Monoid.Monoid)

stroke_map :: [(Sequence, [MNote])] -> Either Text StrokeMap
stroke_map = unique <=< mapM check
    where
    check (sollus, strokes) = do
        let throw = Left
                . (("mridangam map " <> pretty (sollus, strokes) <> ": ") <>)
        sollus <- forM sollus $ \case
            Sollu s _ -> Right s
            s -> throw $ "should only have plain sollus: " <> pretty s
        strokes <- forM strokes $ \case
            MNote s -> Right s
            s -> throw $ "should have plain strokes: " <> showt s
        unless (length sollus == length strokes) $
            throw "sollus and strokes have differing lengths"
        return (sollus, strokes)
    unique pairs
        | null dups = Right (StrokeMap smap)
        | otherwise = Left $ "duplicate mridangam keys: " <> pretty dups
        where (smap, dups) = Util.Map.unique2 pairs

-- | Matras should equal length [MNote].  This is enforced in the constructor
-- 'patterns'.
newtype Patterns = Patterns (Map.Map Matras [MNote])
    deriving (Show)

patterns :: [(Matras, [MNote])] -> Either Text Patterns
patterns pairs
    | null wrong = Right $ Patterns $ Map.fromList pairs
    | otherwise = Left $ Text.intercalate "; " wrong
    where
    wrong =
        [ "matras should match notes: " <> showt matras <> " /= " <> pretty ns
        | (matras, ns) <- pairs
        , matras /= length ns
        ]

data MNote = MNote Stroke | MRest | MTimeChange TimeChange
    deriving (Show)

data Stroke = Thoppi !Thoppi | Valantalai Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Show)
data Thoppi = MTha | MThom
    deriving (Eq, Show)
data Valantalai = MKi | MTa | MNam | MDin | MChapu | MDheem
    deriving (Eq, Show)

instance Pretty.Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = case t of
        MTha -> case v of
            MKi -> "P"
            MTa -> "X"
            MNam -> "A"
            MDin -> "O"
            MChapu -> "pu" -- These are pretty rare.
            MDheem -> "pi"
        MThom -> Text.toUpper (pretty v)
instance Pretty.Pretty MNote where
    pretty MRest = "-"
    pretty (MNote s) = pretty s
    pretty (MTimeChange change) = pretty change

instance Pretty.Pretty Thoppi where
    pretty n = case n of
        MThom -> "o"
        MTha -> "p"
instance Pretty.Pretty Valantalai where
    pretty n = case n of
        MKi -> "k"
        MTa -> "t"
        MNam -> "n"
        MDin -> "d"
        MChapu -> "u"
        MDheem -> "i"

instance Pretty.Pretty Korvai where
    format (Korvai sequence mridangam tala) = Pretty.record "Korvai"
        [ ("sequence", Pretty.format sequence)
        , ("mridangam", Pretty.format mridangam)
        , ("tala", Pretty.format tala)
        ]

-- | Check for errors and construct a 'Korvai'.
korvai :: Tala -> [(Sequence, [MNote])] -> Sequence -> Either Text Korvai
korvai tala mridangam sequence = do
    smap <- stroke_map mridangam
    return $ Korvai
        { korvai_sequence = sequence
        , korvai_mridangam = smap <> standard_stroke_map
        , korvai_tala = tala
        }

standard_stroke_map :: StrokeMap
standard_stroke_map = StrokeMap $ Map.fromList
    [ ([Thom], [Thoppi MThom])
    , ([Tam], [Valantalai MChapu])
    , ([Tang], [Valantalai MChapu])
    , ([Lang], [Valantalai MChapu])
    ]

-- | Realize a Korvai in mridangam strokes.
realize_korvai :: Patterns -> Korvai -> Either Text [MNote]
realize_korvai patterns korvai = first Text.unlines $ do
    rnotes <- verify_alignment (korvai_tala korvai) (korvai_sequence korvai)
    realize_mridangam patterns (korvai_mridangam korvai) rnotes

realize_mridangam :: Patterns -> StrokeMap -> [Note] -> Either [Text] [MNote]
realize_mridangam (Patterns patterns) smap =
    format_error . go
    where
    go :: [Note] -> ([[MNote]], Maybe (Text, [Note]))
    go [] = ([], Nothing)
    go (n : ns) = case n of
        Pattern dur -> case Map.lookup dur patterns of
            Nothing ->
                ([], Just ("no pattern with duration " <> showt dur, n:ns))
            Just mseq -> first (mseq:) (go ns)
        Rest -> first ([MRest] :) (go ns)
        Sollu _ (Just stroke) -> first ([MNote stroke] :) (go ns)
        Sollu sollu Nothing -> case find_mridangam_sequence smap sollu ns of
            Right (strokes, rest) -> first (strokes:) (go rest)
            Left err -> ([], Just (err, n:ns))
        Alignment {} -> go ns
        TimeChange change -> first ([MTimeChange change] :) (go ns)
    format_error (result, Nothing) = Right (concat result)
    format_error (pre, Just (err, post)) = Left $
        [ Text.intercalate " / " $ map pretty_strokes pre
        , "*** " <> err
        , Text.unwords (map pretty post)
        ]

-- | Find the longest matching sequence until the sollus are consumed or
-- a sequence isn't found.
find_mridangam_sequence :: StrokeMap -> Sollu -> [Note]
    -> Either Text ([MNote], [Note])
find_mridangam_sequence (StrokeMap smap) sollu notes =
    case longest_match (sollu : sollus) of
        Nothing -> Left $ "sequence not found: " <> pretty (sollu : sollus)
        Just strokes ->
            Right $ insert_rests strokes (Sollu sollu Nothing : notes)
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = fst $ first Maybe.catMaybes $ Seq.span_while is_sollu notes
    is_sollu (Sollu s Nothing) = Just (Just s)
    is_sollu (Rest {}) = Just Nothing
    is_sollu _ = Nothing
    longest_match = Seq.head . mapMaybe (flip Map.lookup smap) . reverse
        . drop 1 . List.inits

-- | Match each stroke to its Note, and insert rests where the
-- RealizedNotes has them.
insert_rests :: [Stroke] -> [Note] -> ([MNote], [Note])
insert_rests [] ns = ([], ns)
insert_rests (stroke : strokes) (n : ns) = case n of
    Rest -> first (MRest :) $ insert_rests (stroke : strokes) ns
    Sollu {} -> first (MNote stroke :) $ insert_rests strokes ns
    -- These shouldn't happen because the strokes are from the result of
    -- Seq.span_while is_sollu.
    Pattern {} -> skip
    Alignment {} -> skip
    TimeChange {} -> skip
    where
    skip = insert_rests (stroke : strokes) ns
insert_rests (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

-- | Format the notes according to the tala.
pretty_strokes_tala :: Tala -> [MNote] -> Text
pretty_strokes_tala tala =
    Text.stripStart . mconcat . Maybe.catMaybes . snd
        . List.mapAccumL format (initial_state tala)
    where
    format state note = second (fmap decorate) $ case note of
        MRest -> (advance, Just "-")
        MNote n -> (advance, Just (pretty n))
        MTimeChange change -> (time_change change state, Nothing)
        where
        decorate = (newline<>) . add_emphasis . pad
        add_emphasis s
            | not (Text.null s) && matra == 0 = emphasize s
            | otherwise = s
        newline
            | matra == 0 && state_akshara state == 0 = "\n\n"
            | matra == 0 && state_akshara state == tala_arudi tala = "\n"
            | otherwise = ""
        -- TODO look for the highest speed, and normalize to that
        pad = Text.justifyLeft (if state_speed state == S1 then 2 else 0) ' '

        matra = state_matra state
        advance = advance_state tala 1 state
    emphasize txt = "\ESC[1m" <> txt <> "\ESC[0m"

pretty_strokes :: [MNote] -> Text
pretty_strokes = Text.unwords . map (Text.justifyLeft 2 ' ' . pretty)

-- | Pretty reproduces the SolkattuScore syntax, which has to be haskell
-- syntax, so it can't use +, and I have to put thoppi first to avoid the
-- keyword @do@.  It would be nice if I could make the tracklang syntax
-- consistent, but maybe not a huge deal at the moment.
stroke_to_call :: Stroke -> Text
stroke_to_call s = case s of
    Thoppi t -> thoppi t
    Valantalai v -> pretty v
    Both t v -> pretty v <> thoppi t
    where
    thoppi t = case t of
        MThom -> "o"
        MTha -> "+"

-- * misc

check :: Log.Stack => Either Text a -> a
check = either (errorStack . untxt) id

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

fDivMod :: Double -> Double -> (Int, Double)
fDivMod a b = (floor (a / b), Fixed.mod' a b)

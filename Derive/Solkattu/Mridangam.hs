-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
-- | Realize an abstract solkattu 'S.Sequence' to concrete mridangam 'Note's.
module Derive.Solkattu.Mridangam where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Solkattu.Solkattu as S
import Global


type SNote = S.Note Stroke
type Sequence = S.Sequence Stroke

-- | Sollus and Strokes should be the same length.  This is enforced in the
-- constructor 'stroke_map'.  Nothing is a rest, which applies to longer
-- sequences like dinga.
newtype StrokeMap = StrokeMap (Map.Map [S.Sollu] [Maybe Stroke])
    deriving (Show, Pretty.Pretty, Monoid.Monoid)

stroke_map :: [(Sequence, [Note])] -> Either Text StrokeMap
stroke_map = unique <=< mapM verify
    where
    verify (sollus, strokes) = do
        let throw = Left
                . (("mridangam map " <> pretty (sollus, strokes) <> ": ") <>)
        sollus <- fmap Maybe.catMaybes $ forM sollus $ \case
            S.Sollu s _ -> Right (Just s)
            S.Rest -> Right Nothing
            s -> throw $ "should only have plain sollus: " <> pretty s
        strokes <- forM strokes $ \case
            Note s -> Right (Just s)
            Rest -> Right Nothing
            s -> throw $ "should have plain strokes: " <> showt s
        unless (length sollus == length strokes) $
            throw "sollus and strokes have differing lengths after removing\
                \ sollu rests"
        return (sollus, strokes)
    unique pairs
        | null dups = Right (StrokeMap smap)
        | otherwise = Left $ "duplicate mridangam keys: " <> pretty dups
        where (smap, dups) = Util.Map.unique2 pairs

-- | Matras should equal length [Note].  This is enforced in the constructor
-- 'patterns'.
newtype Patterns = Patterns (Map.Map S.Matras [Note])
    deriving (Show, Pretty.Pretty, Monoid.Monoid)

patterns :: [(S.Matras, [Note])] -> Either Text Patterns
patterns pairs
    | null wrong = Right $ Patterns $ Map.fromList pairs
    | otherwise = Left $ Text.intercalate "; " wrong
    where
    wrong =
        [ "matras should match notes: " <> showt matras <> " /= " <> pretty ns
        | (matras, ns) <- pairs
        , matras /= length ns
        ]

data Note = Note Stroke | Rest | TimeChange S.TimeChange
    deriving (Show)

instance Pretty.Pretty Note where
    pretty Rest = "__"
    pretty (Note s) = pretty s
    pretty (TimeChange change) = pretty change

data Stroke = Thoppi !Thoppi | Valantalai !Valantalai | Both !Thoppi !Valantalai
    deriving (Eq, Show)
data Thoppi = Tha | Thom
    deriving (Eq, Show)
data Valantalai = Ki | Ta | Nam | Din | Chapu | Dheem
    deriving (Eq, Show)

instance Pretty.Pretty Stroke where
    pretty (Thoppi t) = pretty t
    pretty (Valantalai v) = pretty v
    pretty (Both t v) = case t of
        Tha -> case v of
            Ki -> "P"
            Ta -> "X"
            Nam -> "A"
            Din -> "O"
            Chapu -> "pu" -- These are pretty rare.
            Dheem -> "pi"
        Thom -> Text.toUpper (pretty v)

instance Pretty.Pretty Thoppi where
    pretty n = case n of
        Thom -> "o"
        Tha -> "p"
instance Pretty.Pretty Valantalai where
    pretty n = case n of
        Ki -> "k"
        Ta -> "t"
        Nam -> "n"
        Din -> "d"
        Chapu -> "u"
        Dheem -> "i"

-- | Sollu to mridangam stroke mapping.
data Mridangam = Mridangam {
    mridangam_stroke_map :: StrokeMap
    , mridangam_patterns :: Patterns
    } deriving (Show)

instance Monoid.Monoid Mridangam where
    mempty = Mridangam mempty mempty
    mappend (Mridangam a1 b1) (Mridangam a2 b2) = Mridangam (a1<>a2) (b1<>b2)

instance Pretty.Pretty Mridangam where
    format (Mridangam stroke_map patterns) = Pretty.record "Mridangam"
        [ ("stroke_map", Pretty.format stroke_map)
        , ("patterns", Pretty.format patterns)
        ]

mridangam :: [(Sequence, [Note])] -> Patterns -> Either Text Mridangam
mridangam strokes patterns = do
    smap <- stroke_map strokes
    return $ Mridangam
        { mridangam_stroke_map = smap <> standard_stroke_map
        , mridangam_patterns = patterns
        }

standard_stroke_map :: StrokeMap
standard_stroke_map = StrokeMap $ Map.fromList
    [ ([S.Thom], [Just $ Thoppi Thom])
    , ([S.Tam], [Just $ Valantalai Chapu])
    , ([S.Tang], [Just $ Valantalai Chapu])
    , ([S.Lang], [Just $ Valantalai Chapu])
    , ([S.Dheem], [Just $ Valantalai Dheem])
    ]

realize :: Mridangam -> [SNote] -> Either [Text] [Note]
realize (Mridangam smap (Patterns patterns)) = format_error . go
    where
    go :: [SNote] -> ([[Note]], Maybe (Text, [SNote]))
    go [] = ([], Nothing)
    go (n : ns) = case n of
        S.Pattern dur -> case Map.lookup dur patterns of
            Nothing ->
                ([], Just ("no pattern with duration " <> showt dur, n:ns))
            Just mseq -> first (mseq:) (go ns)
        S.Rest -> first ([Rest] :) (go ns)
        S.Sollu sollu stroke ->
            case find_sequence smap sollu stroke ns of
                Right (strokes, rest) -> first (strokes:) (go rest)
                Left err -> ([], Just (err, n:ns))
        S.Alignment {} -> go ns
        S.TimeChange change -> first ([TimeChange change] :) (go ns)
    format_error (result, Nothing) = Right (concat result)
    format_error (pre, Just (err, post)) = Left $
        [ Text.intercalate " / " $ map pretty_strokes pre
        , "*** " <> err
        , Text.unwords (map pretty post)
        ]

-- | Find the longest matching sequence until the sollus are consumed or
-- a sequence isn't found.
find_sequence :: StrokeMap -> S.Sollu -> Maybe Stroke -> [SNote]
    -> Either Text ([Note], [SNote])
find_sequence (StrokeMap smap) sollu stroke notes =
    case longest_match (sollu : sollus) of
        Nothing -> Left $ "sequence not found: " <> pretty (sollu : sollus)
        Just strokes ->
            Right $ replace_strokes strokes (S.Sollu sollu stroke : notes)
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = Maybe.catMaybes $ fst $ Seq.span_while is_sollu notes
    is_sollu (S.Sollu s _) = Just (Just s)
    is_sollu (S.Rest {}) = Just Nothing
    is_sollu _ = Nothing
    longest_match = Seq.head . mapMaybe (flip Map.lookup smap) . reverse
        . drop 1 . List.inits

-- | Match each stroke to its 'SNote', and insert rests where the SNotes have
-- one.
replace_strokes :: [Maybe Stroke] -> [SNote] -> ([Note], [SNote])
replace_strokes [] ns = ([], ns)
replace_strokes (stroke : strokes) (n : ns) = case n of
    S.Rest -> first (Rest :) skip
    S.Sollu _ explicit_stroke ->
        first (maybe (maybe Rest Note stroke) Note explicit_stroke :) $
            replace_strokes strokes ns
    -- These shouldn't happen because the strokes are from the result of
    -- Seq.span_while is_sollu.
    S.Pattern {} -> skip
    S.Alignment {} -> skip
    S.TimeChange {} -> skip
    where
    skip = replace_strokes (stroke : strokes) ns
replace_strokes (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

-- | Format the notes according to the tala.
pretty_strokes_tala :: S.Tala -> [Note] -> Text
pretty_strokes_tala tala =
    Text.stripStart . mconcat . Maybe.catMaybes . snd
        . List.mapAccumL format (S.initial_state tala)
    where
    format state note = second (fmap decorate) $ case note of
        Rest -> (advance, Just "-")
        Note n -> (advance, Just (pretty n))
        TimeChange change -> (S.time_change change state, Nothing)
        where
        decorate = (newline<>) . add_emphasis . pad
        add_emphasis s
            | not (Text.null s) && matra == 0 = emphasize s
            | otherwise = s
        newline
            | matra == 0 && S.state_akshara state == 0 = "\n\n"
            | matra == 0 && S.state_akshara state == S.tala_arudi tala = "\n"
            | otherwise = ""
        -- TODO look for the highest speed, and normalize to that
        pad = Text.justifyLeft (if S.state_speed state == S.S1 then 2 else 0)
            ' '

        matra = S.state_matra state
        advance = S.advance_state tala 1 state
    emphasize txt = "\ESC[1m" <> txt <> "\ESC[0m"

pretty_strokes :: [Note] -> Text
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
        Thom -> "o"
        Tha -> "+"

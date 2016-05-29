-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ImplicitParams, LambdaCase #-}
-- | Notate Carnatic solkattu and realize to mridangam fingering.
module Derive.Call.India.Solkattu where
import qualified Data.Either as Either
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

data Note = Sollu Sollu Karvai (Maybe Stroke)
    | Rest
    -- | Set pattern with the given duration.
    | Pattern Matras Karvai
    | Alignment Alignment
    deriving (Eq, Show)

data Alignment = Sam | Arudi
    deriving (Eq, Show)

instance Pretty.Pretty Note where
    pretty n = case n of
        Sollu s karvai stroke ->
            maybe sollu (\s -> ("st " <> pretty s <> " " <> sollu)) stroke
            where sollu = pretty s <> k karvai
        Rest -> "__"
        Pattern d karvai -> "p" <> showt d <> k karvai
        Alignment Sam -> "at0"
        Alignment Arudi -> "atX"
        where
        k Karvai = "_"
        k NoKarvai = ""

data Karvai = Karvai | NoKarvai
    deriving (Eq, Show)

-- | Mid-level representation, lower level than 'Note', but higher than
-- 'MNote'.
data RealizedNote =
    RRest !Matras | RKarvai !Matras | RSollu !Sollu !(Maybe Stroke)
    | RPattern !Matras
    deriving (Show)

instance Pretty.Pretty RealizedNote where
    pretty n = case n of
        RRest dur -> "__" <> if dur == 1 then "" else showt dur
        RKarvai dur -> "__k" <> if dur == 1 then "" else showt dur
        RSollu s stroke ->
            pretty s <> maybe "" ((" ("<> ) . (<>")") . pretty) stroke
        RPattern dur -> "p" <> showt dur

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

-- | A matra is an akshara divided by the nadai.
type Matras = Int

duration_of :: Sequence -> Matras
duration_of = sum . map note_duration

note_duration :: Note -> Matras
note_duration n = case n of
    Sollu {} -> 1
    Rest -> 1
    Pattern dur _ -> dur
    Alignment {} -> 0

data Tala = Tala {
    tala_aksharas :: !Aksharas
    , tala_arudi :: !Aksharas
    , tala_nadai :: !Matras
    } deriving (Show)

tala_matras :: Tala -> Matras
tala_matras tala = tala_aksharas tala * tala_nadai tala

instance Pretty.Pretty Tala where
    format (Tala aksharas arudi nadai) = Pretty.record "Tala"
        [ ("aksharas", Pretty.format aksharas)
        , ("arudi", Pretty.format arudi)
        , ("nadai", Pretty.format nadai)
        ]

adi_tala :: Matras -> Tala
adi_tala = Tala 8 4

-- | Realize karvai and verify alignment.
realize_tala :: Tala -> Sequence -> Either [Text] [RealizedNote]
realize_tala tala =
    fmap realize_notes . check_errors . map apply_karvai
        . verify_alignment tala . split_just is_alignment Sam
    where
    is_alignment (Alignment align) = Just align
    is_alignment _ = Nothing
    apply_karvai = either Left (uncurry realize_karvai)
    check_errors groups
        | any Either.isLeft groups = Left $ map (either id pretty) groups
        | otherwise = Right $ concatMap (either (const []) id) groups

realize_notes :: [Either Matras Note] -- ^ Either a karvai gap or a Note.
    -> [RealizedNote]
realize_notes ns =
    (if not (null rests) then (RRest (length rests) :) else id) $
        case non_rests of
            [] -> []
            Left karvai : ns -> RKarvai karvai : realize_notes ns
            Right (Sollu s _ stroke) : ns -> RSollu s stroke : realize_notes ns
            Right (Pattern d _) : ns -> RPattern d : realize_notes ns
            _ : ns -> realize_notes ns
    where (rests, non_rests) = span (== Right Rest) ns

-- | Divide available time among notes with karvai.  Error if there are no
-- karvai, or if the time doesn't divide evenly.  Otherwise, replace karvai
-- with Lefts.
realize_karvai :: Matras -> [Note] -> Either Text [Either Matras Note]
realize_karvai extra notes
    | extra == 0 = Right (map Right notes)
    | karvais <= 0 =
        Left $ "no karvai but there's unfilled space: " <> showt extra
            <> ": " <> pretty notes
    | remainder /= 0 =
        Left $ "uneven division: " <> showt karvais <> " karvais into "
            <> showt extra <> " matras"
    | otherwise = Right $ concatMap replace notes
    where
    karvais = Seq.count has_karvai notes
    (per_karvai, remainder) = extra `divMod` karvais
    replace (Sollu x Karvai stroke) =
        [Right $ Sollu x NoKarvai stroke, Left per_karvai]
    replace (Pattern x Karvai) = [Right $ Pattern x NoKarvai, Left per_karvai]
    replace x = [Right x]

has_karvai :: Note -> Bool
has_karvai (Sollu _ Karvai _) = True
has_karvai (Pattern _ Karvai) = True
has_karvai _ = False

-- | If the notes have a duration that's longer than the the time from the
-- previous alignment, then error.  Also error if it's shorter and there are no
-- karvai.
--
-- Arudi must be followed by Sam.  If Sam is followed by Sam, it can insert
-- enough integral avartanams to make the duration long enough.
verify_alignment :: Tala -> [(Alignment, [Note])]
    -- ^ Notes divided into alignment groups.
    -> [Either Text (Matras, [Note])] -- ^ (extra_matras, notes)
verify_alignment tala notes =
    map (verify . second (maybe Sam fst)) $
        Seq.zip_next $ drop_initial notes
    where
    -- If the sequence started with Sam, I'll get an extra here.
    drop_initial ((Sam, []) : ns@((Sam, _) : _)) = ns
    drop_initial ns = ns
    verify ((align, notes), next) = case (align, next) of
        (Arudi, Arudi) -> Left "arudi must be followed by sam"
        (Arudi, Sam) -> transition "Arudi->Sam"
            (tala_nadai tala * (tala_aksharas tala - tala_arudi tala))
        (Sam, Arudi) -> transition "Sam->Arudi"
            (tala_nadai tala * tala_arudi tala)
        (Sam, Sam) -> Right (until - dur, notes)
            where until = round_up dur (tala_matras tala)
        where
        dur = duration_of notes
        transition name until
            | dur > until =
                Left $ name <> " transition should have <= "
                    <> showt until <> " matras, but has " <> showt dur
            | otherwise = Right (until - dur, notes)

-- * transform

-- | Drop a number of matras from the Sequence.  Patterns will be shortened.
dropM :: Matras -> Sequence -> Sequence
dropM matras ns = case ns of
    [] -> []
    (n:ns)
        | matras <= 0 -> (n:ns)
        | otherwise -> case n of
            Pattern dur karvai
                | dur > matras -> Pattern (dur - matras) karvai : ns
                | otherwise -> dropM (matras - dur) ns
            _ -> dropM (matras - note_duration n) ns

takeM :: Matras -> Sequence -> Sequence
takeM _ [] = []
takeM matras _ | matras <= 0 = []
takeM matras (n:ns) = case n of
    Sollu {} -> n : takeM (matras-1) ns
    Rest {} -> n : takeM (matras-1) ns
    Pattern dur karvai
        | dur > matras -> n : takeM (matras-dur) ns
        | otherwise -> [Pattern (dur - matras) karvai]
    Alignment {} -> takeM matras ns

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
            Sollu s NoKarvai _ -> Right s
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

data MNote = MNote Stroke | MRest
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
    pretty (Both t v) = pretty t <> pretty v
instance Pretty.Pretty MNote where
    pretty MRest = "-"
    pretty (MNote s) = pretty s

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
realize_korvai :: Patterns -> Patterns -> Korvai -> Either Text [MNote]
realize_korvai patterns karvai_patterns korvai = first Text.unlines $ do
    rnotes <- realize_tala (korvai_tala korvai) (korvai_sequence korvai)
    realize_mridangam patterns karvai_patterns (korvai_mridangam korvai) rnotes

realize_mridangam :: Patterns -> Patterns -> StrokeMap -> [RealizedNote]
    -> Either [Text] [MNote]
realize_mridangam (Patterns patterns) (Patterns karvai_patterns) smap =
    format_error . go
    where
    go :: [RealizedNote] -> ([[MNote]], Maybe (Text, [RealizedNote]))
    go [] = ([], Nothing)
    go (n : ns) = case n of
        RPattern dur -> case Map.lookup dur patterns of
            Nothing ->
                ([], Just ("no pattern with duration " <> showt dur, n:ns))
            Just mseq -> first (mseq:) (go ns)
        RKarvai dur -> case Map.lookup dur karvai_patterns of
            Nothing -> first (replicate dur MRest :) (go ns)
            Just mseq -> first (mseq:) (go ns)
        RRest dur -> first (replicate dur MRest :) (go ns)
        RSollu _ (Just stroke) -> first ([MNote stroke] :) (go ns)
        RSollu sollu Nothing -> case find_mridangam_sequence smap sollu ns of
            Right (strokes, rest) -> first (strokes:) (go rest)
            Left err -> ([], Just (err, n:ns))
    format_error (result, Nothing) = Right (concat result)
    format_error (pre, Just (err, post)) = Left $
        [ Text.intercalate " / " $ map pretty_strokes pre
        , "*** " <> err
        , Text.unwords (map pretty post)
        ]

-- | Find the longest matching sequence until the sollus are consumed or
-- a sequence isn't found.
find_mridangam_sequence :: StrokeMap -> Sollu -> [RealizedNote]
    -> Either Text ([MNote], [RealizedNote])
find_mridangam_sequence (StrokeMap smap) sollu notes =
    case longest_match (sollu : sollus) of
        Nothing -> Left $ "sequence not found: " <> pretty (sollu : sollus)
        Just strokes ->
            Right $ insert_rests strokes (RSollu sollu Nothing : notes)
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = fst $ first Maybe.catMaybes $ Seq.span_while is_sollu notes
    is_sollu (RSollu s Nothing) = Just (Just s)
    is_sollu (RRest {}) = Just Nothing
    is_sollu _ = Nothing
    longest_match = Seq.head . mapMaybe (flip Map.lookup smap) . reverse
        . drop 1 . List.inits

-- | Match each stroke to its RealizedNote, and insert rests where the
-- RealizedNotes has them.
insert_rests :: [Stroke] -> [RealizedNote] -> ([MNote], [RealizedNote])
insert_rests [] ns = ([], ns)
insert_rests (stroke : strokes) (n : ns) = case n of
    RRest dur ->
        first (replicate dur MRest ++) $ insert_rests (stroke : strokes) ns
    RSollu {} -> first (MNote stroke :) $ insert_rests strokes ns
    -- These shouldn't happen because the strokes are from the result of
    -- Seq.span_while is_sollu.
    RPattern {} -> skip
    RKarvai {} -> skip
    where
    skip = insert_rests (stroke : strokes) ns
insert_rests (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

-- | Format the notes according to the tala.
pretty_strokes_tala :: Tala -> [MNote] -> Text
pretty_strokes_tala tala =
    Text.stripEnd . Text.unlines . map pretty_avartanam
        . Seq.chunked (tala_matras tala) . zip [0..]
    where
    pretty_avartanam = Text.unlines . map (Text.unwords . map stroke1) . split
        where
        split xs = [pre, post]
            where (pre, post) = splitAt (tala_arudi tala * tala_nadai tala) xs
    stroke1 (i, stroke) =
        (if beat then emphasize else id) $
            Text.justifyLeft 2 ' ' (pretty stroke)
        where beat = i `mod` tala_nadai tala == 0
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

group_rights :: [Either a b] -> [Either a [b]]
group_rights xs = case rest of
    [] -> cons []
    Left x : xs -> cons $ Left x : group_rights xs
    Right x : xs -> Right [x] : group_rights xs
    where
    (rights, rest) = Seq.span_while (either (const Nothing) Just) xs
    cons = if null rights then id else (Right rights :)

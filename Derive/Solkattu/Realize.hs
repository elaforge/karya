-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Realize an abstract solkattu 'S.Sequence' to concrete instrument-dependent
-- 'Note's.
module Derive.Solkattu.Realize where
import qualified Data.List as List
import qualified Data.String as String
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Doc as Doc
import qualified Util.Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala

import Global


type SNote stroke = S.Note (Note stroke)

-- | The 'Solkattu.Sollu's have been reduced to concrete strokes.
data Note stroke = Note (Stroke stroke) | Rest | Pattern !Solkattu.Pattern
    deriving (Eq, Show, Functor)

data Stroke stroke = Stroke {
    _emphasis :: !Emphasis
    , _stroke :: !stroke
    } deriving (Eq, Ord, Show, Functor)

instance Pretty stroke => Pretty (Stroke stroke) where
    pretty (Stroke emphasis stroke) = case emphasis of
        -- This makes the output ambiguous since some strokes are already
        -- capitalized.  But since Pretty is used for 'format', which will
        -- doesn't understand about two-char strokes, I'll try the ambiguity
        -- and make format smarter (and take twice the width) if necessary.
        Heavy -> Text.toUpper $ pretty stroke
        Normal -> pretty stroke
        Light -> pretty stroke

stroke :: stroke -> Stroke stroke
stroke = Stroke Normal

-- There's no general ToCall instance for Stroke because individual instruments
-- may have special cases.

-- | The emphasis will be propagated to the underlying stroke.
data Emphasis = Light | Normal | Heavy deriving (Eq, Ord, Show)

instance Monoid Emphasis where
    mempty = Normal
    mappend = max

instance Pretty Emphasis where
    pretty Light = "^"
    pretty Normal = ""
    pretty Heavy = "v"

note_matras :: Note stroke -> S.Matra
note_matras n = case n of
    Note {} -> 1
    Rest -> 1
    Pattern p -> Solkattu.pattern_matras p

instance Pretty stroke => Pretty (Note stroke) where
    pretty Rest = "_"
    pretty (Note s) = pretty s
    pretty (Pattern p) = pretty p

-- | This maps a 'Pattern' of a certain duration to a realization.  The
-- 'S.Matra's should the same duration as the the list in the default tempo.
-- This is enforced in the constructor 'patterns'.
newtype Patterns stroke = Patterns (Map Solkattu.Pattern [SNote stroke])
    deriving (Eq, Show, Pretty, Monoid)

-- | Make a Patterns while checking that the durations match.
patterns :: Pretty stroke =>
    [(Solkattu.Pattern, [SNote stroke])] -> Either Text (Patterns stroke)
patterns pairs
    | null errors = Right $ Patterns $ Map.fromList pairs
    | otherwise = Left $ Text.intercalate "; " errors
    where
    errors = mapMaybe check pairs
    check (p, notes)
        | notes_matras /= fromIntegral p_matras =
            Just $ "pattern matras " <> pretty p_matras
                <> " /= realization matras " <> pretty notes_matras
                <> " for " <> showt p
        | otherwise = Nothing
        where
        p_matras = Solkattu.pattern_matras p
        notes_matras = notes_duration / S.matra_duration S.default_tempo
        notes_duration = sum $ map (S.note_duration note_matras S.default_tempo)
            notes

lookup_pattern :: Solkattu.Pattern -> Patterns stroke -> Maybe [SNote stroke]
lookup_pattern p (Patterns pmap) = Map.lookup p pmap

map_patterns :: ([SNote stroke] -> [SNote stroke]) -> Patterns stroke
    -> Patterns stroke
map_patterns f (Patterns p) = Patterns (f <$> p)

-- | Sollus and Strokes should be the same length.  This is enforced in the
-- constructor 'stroke_map'.  Nothing is a rest, which applies to longer
-- sequences like dinga.
newtype StrokeMap stroke =
    StrokeMap (Map [Solkattu.Sollu] [Maybe (Stroke stroke)])
    deriving (Eq, Show, Pretty, Monoid)

-- | Directly construct a StrokeMap from strokes.
simple_stroke_map :: [([Solkattu.Sollu], [Maybe stroke])] -> StrokeMap stroke
simple_stroke_map = StrokeMap .  fmap (fmap (fmap stroke)) . Map.fromList

stroke_map :: Pretty stroke =>
    [([S.Note (Solkattu.Note stroke)], [SNote stroke])]
    -> Either Text (StrokeMap stroke)
stroke_map = unique <=< mapM verify
    where
    verify (sollus, strokes) = do
        let throw = Left
                . (("stroke map " <> pretty (sollus, strokes) <> ": ") <>)
        sollus <- fmap Maybe.catMaybes $ forM sollus $ \case
            S.Note (Solkattu.Note note) -> Right (Just (Solkattu._sollu note))
            S.Note Solkattu.Rest -> Right Nothing
            s -> throw $ "should only have plain sollus: " <> pretty s
        strokes <- forM strokes $ \case
            S.Note (Note s) -> Right (Just s)
            S.Note Rest -> Right Nothing
            s -> throw $ "should have plain strokes: " <> pretty s
        unless (length sollus == length strokes) $
            throw "sollus and strokes have differing lengths after removing\
                \ sollu rests"
        return (sollus, strokes)
    unique pairs
        | null dups = Right (StrokeMap smap)
        | otherwise = Left $ "duplicate StrokeMap keys: " <> pretty dups
        where (smap, dups) = Util.Map.unique2 pairs

-- | Sollu to instrument stroke mapping.
data Instrument stroke = Instrument {
    inst_stroke_map :: StrokeMap stroke
    , inst_patterns :: Patterns stroke
    } deriving (Eq, Show)

instance Monoid (Instrument stroke) where
    mempty = Instrument mempty mempty
    mappend (Instrument a1 b1) (Instrument a2 b2) = Instrument (a1<>a2) (b1<>b2)

instance Pretty stroke => Pretty (Instrument stroke) where
    format (Instrument stroke_map patterns) = Pretty.record "Instrument"
        [ ("stroke_map", Pretty.format stroke_map)
        , ("patterns", Pretty.format patterns)
        ]

instrument :: Pretty stroke => StrokeMap stroke
    -> [([S.Note (Solkattu.Note stroke)], [SNote stroke])]
    -> Patterns stroke -> Either Text (Instrument stroke)
instrument defaults strokes patterns = do
    smap <- stroke_map strokes
    return $ Instrument
        { inst_stroke_map = smap <> defaults
        , inst_patterns = patterns
        }

-- * realize

type Event stroke = (S.Duration, Solkattu.Note stroke)

realize :: forall stroke. Pretty stroke =>
    StrokeMap stroke -> [(S.Tempo, Solkattu.Note (Stroke stroke))]
    -> Either Text [(S.Tempo, Note stroke)]
realize smap = format_error . go
    where
    go [] = ([], Nothing)
    go ((tempo, note) : rest) = case note of
        Solkattu.Alignment {} -> go rest
        Solkattu.Rest -> first ((tempo, Rest) :) (go rest)
        -- Patterns are realized separately with 'realize_patterns'.
        Solkattu.Pattern p -> first ((tempo, Pattern p) :) (go rest)
        Solkattu.Note (Solkattu.NoteT {_sollu, _stroke}) ->
            case find_sequence smap tempo _sollu _stroke rest of
                Left err -> case _stroke of
                    Nothing -> ([], Just err)
                    -- If it's not part of a sequence, but has a hardcoded
                    -- stroke then I know what to do with it already.
                    Just stroke -> first ((tempo, Note stroke) :) (go rest)
                Right (strokes, rest) -> first (strokes++) (go rest)
    format_error (result, Nothing) = Right result
    format_error (pre, Just err) = Left $
        TextUtil.joinWith "\n" (pretty_words (map snd pre)) ("*** " <> err)

tempo_to_duration :: [(S.Tempo, Note stroke)] -> [(S.Duration, Note stroke)]
tempo_to_duration = S.tempo_to_duration note_matras

pretty_words :: Pretty a => [a] -> Text
pretty_words = Text.unwords . map (Text.justifyLeft 2 ' ' . pretty)

-- | Find the longest matching sequence and return the match and unconsumed
-- notes.
find_sequence :: StrokeMap stroke -> a -> Solkattu.Sollu
    -> Maybe (Stroke stroke) -> [(a, Solkattu.Note (Stroke stroke))]
    -> Either Text ([(a, Note stroke)], [(a, Solkattu.Note (Stroke stroke))])
find_sequence (StrokeMap smap) a sollu stroke notes =
    case longest_match (sollu : sollus) of
        Nothing -> Left $ "sequence not found: " <> pretty (sollu : sollus)
        Just strokes -> Right $ replace_sollus strokes $
            (a, Solkattu.Note (Solkattu.note sollu stroke)) : notes
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = Maybe.catMaybes $ fst $ Seq.span_while (is_sollu . snd) notes
    is_sollu (Solkattu.Note note) = Just (Just (Solkattu._sollu note))
    is_sollu Solkattu.Rest = Just Nothing
    is_sollu (Solkattu.Alignment {}) = Just Nothing
    is_sollu _ = Nothing
    longest_match = Seq.head . mapMaybe (flip Map.lookup smap) . reverse
        . drop 1 . List.inits

-- | Match each stroke to a Sollu, copying over Rests without consuming
-- a stroke.
replace_sollus :: [Maybe (Stroke stroke)]
    -> [(a, Solkattu.Note (Stroke stroke))]
    -> ([(a, Note stroke)], [(a, Solkattu.Note (Stroke stroke))])
replace_sollus [] ns = ([], ns)
replace_sollus (stroke : strokes) ((a, n) : ns) = case n of
    Solkattu.Note snote -> first ((a, rnote) :) (replace_sollus strokes ns)
        where
        rnote = maybe (maybe Rest Note stroke) Note (Solkattu._stroke snote)
    Solkattu.Rest -> first ((a, Rest) :) next
    Solkattu.Alignment {} -> next
    -- This shouldn't happen because Seq.span_while is_sollu should have
    -- stopped when it saw this.
    Solkattu.Pattern {} -> next
    where
    next = replace_sollus (stroke : strokes) ns
replace_sollus (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

realize_patterns :: Pretty stroke =>
    Patterns stroke -> [(S.Tempo, Solkattu.Note (Stroke stroke))]
    -> Either Text [(S.Tempo, Solkattu.Note (Stroke stroke))]
realize_patterns pmap = format_error . concatMap realize
    where
    realize (tempo, n) = case n of
        Solkattu.Pattern p -> case lookup_pattern p pmap of
            Just notes ->
                map Right $ S.flatten_with tempo $ map (fmap to_solkattu) notes
            Nothing -> [Left $ "no pattern for " <> pretty p]
        _ -> [Right (tempo, n)]
    format_error xs = case S.first_left xs of
        Right vals -> Right vals
        Left (vals, err) ->
            Left $ TextUtil.joinWith "\n" (pretty_words (map snd vals)) err

to_solkattu :: Note stroke -> Solkattu.Note (Stroke stroke)
to_solkattu n = case n of
    Note stroke -> Solkattu.Note $ Solkattu.note Solkattu.NoSollu (Just stroke)
    Rest -> Solkattu.Rest
    Pattern matras -> Solkattu.Pattern matras


-- * format text

-- | Format the notes according to the tala.
--
-- The line breaking for rulers is a bit weird in that if the line is broken,
-- I only emit the first part of the ruler.  Otherwise I'd have to have
-- a multiple line ruler too, which might be too much clutter.  I'll have to
-- see how it works out in practice.
format :: Pretty stroke => Maybe Int -> Int -> Tala.Tala
    -> [(S.Tempo, Note stroke)] -> Text
format override_stroke_width width tala notes =
    Text.stripEnd $ attach_ruler ruler_avartanams
    where
    ruler_avartanams =
        [ (infer_ruler_text tala stroke_width (head lines),
            Text.unlines $ map format_line lines)
        | lines <- avartanam_lines
        ]
    (avartanam_lines, stroke_width) = case override_stroke_width of
        Just n -> (format_lines n width tala notes, n)
        Nothing -> case format_lines 1 width tala notes of
            ([line] : _)
                | sum (map (Text.length . snd) line) <= width `div` 2 ->
                    (format_lines 2 width tala notes, 2)
            result -> (result, 1)
    format_line :: [(S.State, Text)] -> Text
    format_line = Text.stripEnd . mconcat . map snd
        . map_with_fst emphasize_akshara . thin_rests
    emphasize_akshara state word
        | should_emphasize aksharas state = emphasize word
        | otherwise = word
        where aksharas = akshara_set tala

should_emphasize :: Set Tala.Akshara -> S.State -> Bool
should_emphasize aksharas state =
    S.state_matra state == 0 && Set.member (S.state_akshara state) aksharas

akshara_set :: Tala.Tala -> Set Tala.Akshara
akshara_set = Set.fromList . scanl (+) 0 . Tala.tala_aksharas

-- | Drop single character rests on odd columns, to make the output look less
-- cluttered.
thin_rests :: (Eq str, String.IsString str) => [(a, str)] -> [(a, str)]
thin_rests = zipWith thin [0..]
    where
    thin column (state, stroke)
        | stroke == "_" && odd column = (state, " ")
        | otherwise = (state, stroke)

-- | Break into [avartanam], where avartanam = [line].
format_lines :: Pretty stroke => Int -> Int -> Tala.Tala
    -> [(S.Tempo, Note stroke)] -> [[[(S.State, Text)]]]
format_lines stroke_width width tala =
    map (break_line width) . break_avartanams
        . map combine . Seq.zip_prev
        . map (second show_stroke)
        . S.normalize_speed note_matras tala
    where
    combine (prev, (state, text)) = (state, Text.drop overlap text)
        where overlap = maybe 0 (subtract stroke_width . Text.length . snd) prev
    show_stroke s = case s of
        S.Attack a -> Text.justifyLeft stroke_width ' ' (pretty a)
        S.Sustain -> Text.replicate stroke_width "-"
        S.Rest -> Text.justifyLeft stroke_width ' ' "_"

break_avartanams :: [(S.State, a)] -> [[(S.State, a)]]
break_avartanams = dropWhile null . Seq.split_with (is_sam . fst)
    where is_sam state = S.state_matra state == 0 && S.state_akshara state == 0

-- | Strip duplicate rulers and attach to the notation lines.  Avartanams which
-- were broken due to width are separated with two newlines to make that
-- visible.
attach_ruler :: [(Text, Text)] -> Text
attach_ruler = mconcatMap merge . map (second Text.stripEnd)
    . map strip_duplicate . Seq.zip_prev
    where
    merge (ruler, line) = maybe "" (<>"\n") ruler <> line
        <> if "\n" `Text.isInfixOf` line then "\n\n" else "\n"
    strip_duplicate (prev, (ruler, line))
        | maybe False ((==ruler) . fst) prev = (Nothing, line)
        | otherwise = (Just ruler, line)

-- | Like 'second', but also give fst as an argument.
map_with_fst :: (a -> b -> c) -> [(a, b)] -> [(a, c)]
map_with_fst f xs = [(a, f a b) | (a, b) <- xs]

-- | If the text goes over the width, break at the middle akshara, or the
-- last one before the width if there isn't a middle.
break_line :: Int -> [(S.State, Text)] -> [[(S.State, Text)]]
break_line max_width notes
    | width <= max_width = [notes]
    | even aksharas = break_at (aksharas `div` 2) notes
    | otherwise = break_before max_width notes
    where
    width = sum $ map (Text.length . snd) notes
    aksharas = Seq.count at_akshara notes
    break_at akshara =
        pair_to_list . break ((==akshara) . S.state_akshara . fst)
    pair_to_list (a, b) = [a, b]

-- | Yet another word-breaking algorithm.  I must have 3 or 4 of these by now.
break_before :: Int -> [(S.State, Text)] -> [[(S.State, Text)]]
break_before max_width = go . dropWhile null . Seq.split_with at_akshara
    where
    go aksharas =
        case break_fst (>max_width) (zip (running_width aksharas) aksharas) of
            ([], []) -> []
            (pre, []) -> [concat pre]
            ([], post:posts) -> post : go posts
            (pre, post) -> concat pre : go post
    -- drop 1 so it's the width at the end of each section.
    running_width = drop 1 . scanl (+) 0 . map (sum . map (Text.length . snd))

break_fst :: (key -> Bool) -> [(key, a)] -> ([a], [a])
break_fst f = (map snd *** map snd) . break (f . fst)

infer_ruler_text :: Tala.Tala -> Int -> [(S.State, Text)] -> Text
infer_ruler_text tala stroke_width = mconcatMap fmt . infer_ruler tala
    where
    fmt (label, spaces) = Text.justifyLeft (spaces * stroke_width) ' ' label

infer_ruler :: Tala.Tala -> [(S.State, a)] -> [(Text, Int)]
infer_ruler tala = zip (Tala.tala_labels tala ++ ["|"])
    . (++[0]) . map length . dropWhile null
    . Seq.split_with at_akshara

at_akshara :: (S.State, a) -> Bool
at_akshara = (==0) . S.state_matra . fst

emphasize :: Text -> Text
emphasize word
    -- A bold _ looks the same as a non-bold one, so put a bar to make it
    -- more obvious.
    | word == "_ " = emphasize "_|"
    | otherwise = "\ESC[1m" <> pre <> "\ESC[0m" <> post
    where (pre, post) = Text.break (==' ') word

-- * format html

write_html :: Pretty stroke => FilePath -> Tala.Tala
    -> [(S.Tempo, Note stroke)] -> IO ()
write_html fname tala = Text.IO.writeFile fname . Doc.un_html . format_html tala

format_html :: Pretty stroke => Tala.Tala -> [(S.Tempo, Note stroke)]
    -> Doc.Html
format_html tala notes = to_table 30 (map Doc.html ruler) (map (map snd) body)
    where
    ruler = maybe [] (concatMap akshara . infer_ruler tala) (Seq.head body)
    akshara (n, spaces) = n : replicate (spaces-1) ""
    body = map thin_rests $ format_table tala notes

format_table :: Pretty stroke => Tala.Tala -> [(S.Tempo, Note stroke)]
    -> [[(S.State, Doc.Html)]]
format_table tala =
    break_avartanams
        . map_with_fst emphasize_akshara
        . map (second show_stroke)
        . S.normalize_speed note_matras tala
    where
    show_stroke s = case s of
        S.Attack a -> Doc.html (pretty a)
        S.Sustain -> "&mdash;"
        S.Rest -> "_"
    emphasize_akshara state word
        | should_emphasize aksharas state = "<b>" <> word <> "</b>"
        | otherwise = word
    aksharas = akshara_set tala

to_table :: Int -> [Doc.Html] -> [[Doc.Html]] -> Doc.Html
to_table col_width header rows = mconcatMap (<>"\n") $
    [ "<table cellpadding=0 cellspacing=0 style=\"table-layout: fixed\">"
    , "<tr>" <> mconcatMap th header <> "</tr>\n"
    ] ++ map row rows
    ++ ["</table>"]
    where
    th col = Doc.tag_attrs "th" [("width", showt col_width), ("align", "left")]
        (Just col)
    row cols = "<tr>" <> mconcatMap (Doc.tag "td") cols <> "</tr>"

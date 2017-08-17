-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase, ScopedTypeVariables, DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Realize abstract solkattu 'S.Note's to concrete instrument-dependent
-- 'Note's.
module Derive.Solkattu.Realize where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.MultiSet as MultiSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified Util.Doc as Doc
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.Expr as Expr
import qualified Derive.Solkattu.Sequence as S
import qualified Derive.Solkattu.Solkattu as Solkattu
import qualified Derive.Solkattu.Tala as Tala
import qualified Derive.Solkattu.Terminal as Terminal
import qualified Derive.Symbols as Symbols

import Global


type Error = Text

-- | The group is () because I don't need groups in the stroke map keys.
type SNote stroke = S.Note () (Note stroke)

-- | The 'Solkattu.Sollu's have been reduced to concrete strokes.
data Note stroke =
    Note !(Stroke stroke)
    | Space !Solkattu.Space
    | Pattern !Solkattu.Pattern
    deriving (Eq, Show, Functor)

instance DeepSeq.NFData (Note stroke) where
    rnf _ = ()

data Stroke stroke = Stroke {
    _emphasis :: !Emphasis
    , _stroke :: !stroke
    } deriving (Eq, Ord, Show, Functor)

to_expr :: Expr.ToExpr a => Stroke a -> Expr.Expr Text
to_expr (Stroke emphasis stroke) = case emphasis of
    Normal -> Expr.to_expr stroke
    Light -> Expr.with Symbols.weak stroke
    Heavy -> Expr.with Symbols.accent stroke

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

rest :: SNote stroke
rest = S.Note (Space Solkattu.Rest)

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

instance S.HasMatras (Note stroke) where
    matras_of n = case n of
        Note {} -> 1
        Space {} -> 1
        Pattern p -> S.matras_of p
    has_duration n = case n of
        Note {} -> False
        Space {} -> True
        Pattern {} -> True

instance Pretty stroke => Pretty (Note stroke) where
    pretty (Space Solkattu.Rest) = "_"
    pretty (Space Solkattu.Sarva) = "="
    pretty (Note s) = pretty s
    pretty (Pattern p) = pretty p

-- | This maps a 'Pattern' of a certain duration to a realization.  The
-- 'S.Matra's should the same duration as the the list in the default tempo.
-- This is enforced in the constructor 'patterns'.
newtype Patterns stroke = Patterns (Map Solkattu.Pattern [SNote stroke])
    deriving (Eq, Show, Pretty, Monoid)

-- | Make a Patterns while checking that the durations match.
patterns :: [(Solkattu.Pattern, [SNote stroke])]
    -> Either Error (Patterns stroke)
patterns pairs
    | null errors = Right $ Patterns $ Map.fromList pairs
    | otherwise = Left $ Text.intercalate "; " errors
    where
    errors = mapMaybe check pairs
    check (p, notes)
        | notes_matras /= fromIntegral (S.matras_of p) =
            Just $ "pattern matras " <> pretty (S.matras_of p)
                <> " /= realization matras " <> pretty notes_matras
                <> " for " <> showt p
        | otherwise = Nothing
        where
        notes_matras = notes_duration / S.matra_duration S.default_tempo
        notes_duration = sum $ map (S.note_duration S.default_tempo)
            notes

lookup_pattern :: Solkattu.Pattern -> Patterns stroke -> Maybe [SNote stroke]
lookup_pattern p (Patterns pmap) = Map.lookup p pmap

map_patterns :: ([SNote stroke] -> [SNote stroke]) -> Patterns stroke
    -> Patterns stroke
map_patterns f (Patterns p) = Patterns (f <$> p)

-- ** StrokeMap

-- | Sollus and Strokes should be the same length.  This is enforced in the
-- constructor 'stroke_map'.  Nothing is a rest, which applies to longer
-- sequences like dinga.
newtype StrokeMap stroke = StrokeMap
    (Map (Maybe Solkattu.Tag, [Solkattu.Sollu]) [Maybe (Stroke stroke)])
    deriving (Eq, Show, Pretty, Monoid)

-- | Directly construct a StrokeMap from strokes.
simple_stroke_map :: [([Solkattu.Sollu], [Maybe stroke])] -> StrokeMap stroke
simple_stroke_map = StrokeMap .  fmap (fmap (fmap stroke)) . Map.fromList
    . map (first (Nothing,))

stroke_map :: Pretty stroke =>
    [([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])]
    -> Either Error (StrokeMap stroke)
stroke_map =
    fmap (StrokeMap . Map.fromList)
        . mapM (verify . first (map (S.map_group (const ()))))
    where
    verify (sollus, strokes) = do
        let throw = Left
                . (("stroke map " <> pretty sollus <> " to " <> pretty strokes
                    <> ": ") <>)
        (tags, sollus) <- fmap (unzip . Maybe.catMaybes) $
            -- Allow but ignore TempoChanges.  This makes it convenient to use
            -- a sequence like 'nakataka = su (na.ka.ta.ka)' in both notation
            -- and the stroke map.
            forM (S.notes sollus) $ \case
                Solkattu.Note note ->
                    Right $ Just (Solkattu._tag note, Solkattu._sollu note)
                Solkattu.Space {} -> Right Nothing
                s -> throw $ "should only have plain sollus: " <> pretty s
        strokes <- forM strokes $ \case
            S.Note (Note s) -> Right (Just s)
            S.Note (Space {}) -> Right Nothing
            s -> throw $ "should have plain strokes: " <> pretty s
        unless (length sollus == length strokes) $
            throw $ "sollus and strokes have differing lengths after removing\
                \ rests: sollus " <> showt (length sollus)
                <> " /= strokes " <> showt (length strokes)
        -- TODO warn if there are inconsistent tags?
        return ((Seq.head (Maybe.catMaybes tags), sollus), strokes)

-- ** Instrument

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

instrument :: Pretty stroke =>
    [([S.Note g (Solkattu.Note Solkattu.Sollu)], [SNote stroke])]
    -> Patterns stroke -> Either Error (Instrument stroke)
instrument strokes patterns = do
    smap <- stroke_map strokes
    return $ Instrument
        { inst_stroke_map = smap
        , inst_patterns = patterns
        }

-- * realize

type RealizePattern tempo stroke =
    tempo -> Solkattu.Pattern -> Either Error [(tempo, Note stroke)]

-- | Don't realize Patterns, just pass them through.
keep_pattern :: RealizePattern tempo stroke
keep_pattern tempo pattern = Right [(tempo, Pattern pattern)]

realize_pattern :: Patterns stroke -> RealizePattern S.Tempo stroke
realize_pattern pmap tempo pattern = case lookup_pattern pattern pmap of
    Nothing -> Left $ "no pattern for " <> pretty pattern
    Just notes -> Right $ map (first S._tempo) $ S.flatten_with tempo notes

type Meta sollu = S.Meta (Solkattu.Group sollu)

realize :: forall stroke sollu. (Pretty stroke, Pretty sollu) =>
    RealizePattern S.Tempo stroke -> GetStroke sollu stroke
    -> [(Meta sollu, Solkattu.Note sollu)]
    -> Either Error [(Meta sollu, Note stroke)]
realize realize_pattern get_stroke =
    fmap strip . realize_notes . map (first Just)
    where
    realize_notes = format_error . first concat . map_until_left realize1
    realize1 ((Just (S.Meta (Just (S.GroupMark count group)) tempo)), note)
            notes
        | not (null (Solkattu._dropped group)) = (, post) <$>
            realize_group group ((Just stripped, note) : pre)
        where
        (pre, post) = splitAt (count-1) notes
        -- I want to keep the group for the output, but if I leave it as-is
        -- I'll recurse endlessly.  Since realize_group will add the dropped
        -- sollus on, I can strip them from the group, and prevent it from
        -- happening again.
        stripped = S.Meta (Just g) tempo
        g = S.GroupMark count (group { Solkattu._dropped = [] })
    realize1 (meta, note) notes = case note of
        Solkattu.Alignment {} -> Right ([], notes)
        Solkattu.Space space -> Right ([(meta, Space space)], notes)
        Solkattu.Pattern p -> case meta of
            Just m -> (,notes) . map (first (Just . add_meta)) <$>
                realize_pattern (S._tempo m) p
            -- This shouldn't be possible because Nothing meta is only created
            -- by 'extra' below.
            Nothing -> Left "Pattern with Nothing meta"
        Solkattu.Note {} -> find_sequence get_stroke ((meta, note) : notes)

    format_error (result, Nothing) = Right result
    format_error (pre, Just err) = Left $
        TextUtil.joinWith "\n" (pretty_words (map snd pre)) err

    realize_group :: Solkattu.Group sollu
        -> [(Maybe (Meta sollu), Solkattu.Note sollu)]
        -> Either Error [(Maybe (Meta sollu), Note stroke)]
    realize_group (Solkattu.Group dropped side) =
        first ("group: "<>) . realize_notes . add
        where
        -- Add the 'dropped' notes back on to the sequence to find matches.
        -- Since they are the only ones with Nothing meta, I can strip them off
        -- afterwards by filtering out Nothing meta.
        add notes = case side of
            Solkattu.Front -> extra ++ notes
            Solkattu.Back -> notes ++ extra
            where
            extra = map ((Nothing,) . Solkattu.Note . Solkattu.note) dropped
    strip notes = [(meta, n) | (Just meta, n) <- notes]

-- | Patterns just have (tempo, stroke), no groups, so I add empty groups to
-- merge their result into 'realize' output.
add_meta :: S.Tempo -> S.Meta g
add_meta tempo = S.Meta { _mark = Nothing, _tempo = tempo }

-- | Apply the function until it returns Left.  The function can consume a
-- variable number of elements.
map_until_left :: (a -> [a] -> Either err (b, [a])) -> [a] -> ([b], Maybe err)
map_until_left f = go
    where
    go [] = ([], Nothing)
    go (x:xs) = case f x xs of
        Left err -> ([], Just err)
        Right (val, rest) -> first (val:) (go rest)

-- | Find the longest matching sequence and return the match and unconsumed
-- notes.
find_sequence :: Pretty sollu => GetStroke sollu stroke
    -> [(tempo, Solkattu.Note sollu)]
    -> Either Error ([(tempo, Note stroke)], [(tempo, Solkattu.Note sollu)])
find_sequence get_stroke notes = case best_match tag sollus get_stroke of
    Nothing -> Left $ "sequence not found: " <> pretty sollus
    Just strokes -> Right $ replace_sollus strokes notes
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = Maybe.catMaybes $ fst $ Seq.span_while (is_sollu . snd) notes
    is_sollu (Solkattu.Note note) = Just (Just (Solkattu._sollu note))
    is_sollu (Solkattu.Space {}) = Just Nothing
    is_sollu (Solkattu.Alignment {}) = Just Nothing
    is_sollu _ = Nothing
    tag = Solkattu._tag =<< Seq.head (mapMaybe (Solkattu.note_of . snd) notes)

-- | Match each stroke to a Sollu, copying over Rests without consuming
-- a stroke.
replace_sollus :: [Maybe (Stroke stroke)]
    -> [(tempo, Solkattu.Note sollu)]
    -> ([(tempo, Note stroke)], [(tempo, Solkattu.Note sollu)])
replace_sollus [] ns = ([], ns)
replace_sollus (stroke : strokes) ((tempo, n) : ns) = case n of
    Solkattu.Note _ -> first ((tempo, rnote) :) (replace_sollus strokes ns)
        where rnote = maybe (Space Solkattu.Rest) Note stroke
    Solkattu.Space space -> first ((tempo, Space space) :) next
    Solkattu.Alignment {} -> next
    -- This shouldn't happen because Seq.span_while is_sollu should have
    -- stopped when it saw this.
    Solkattu.Pattern {} -> next
    where
    next = replace_sollus (stroke : strokes) ns
replace_sollus (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

pretty_words :: Pretty a => [a] -> Text
pretty_words = Text.unwords . map (justify_left 2 ' ' . pretty)

-- ** GetStroke

-- | Int is the longest [sollu] key, so I know when to give up looking for the
-- longest prefix.
type GetStroke sollu stroke =
    (Int, Maybe Solkattu.Tag -> [sollu] -> Maybe [Maybe (Stroke stroke)])

realize_stroke :: GetStroke (Stroke stroke) stroke
realize_stroke = (1, const $ Just . map Just)

realize_simple_stroke :: GetStroke stroke stroke
realize_simple_stroke = (1, const $ Just . map (Just . stroke))

realize_sollu :: StrokeMap stroke -> GetStroke Solkattu.Sollu stroke
realize_sollu (StrokeMap smap) =
    ( fromMaybe 0 $ Seq.maximum (map (length . snd) (Map.keys smap))
    , \tag sollus -> Map.lookup (tag, sollus) smap
    )

best_match :: Maybe Solkattu.Tag -> [sollu] -> GetStroke sollu stroke
    -> Maybe [Maybe (Stroke stroke)]
best_match tag sollus (longest_key, get_stroke) =
    -- Try with the specific tag, otherwise fall back to no tag.
    Seq.head (find tag prefixes) <|> Seq.head (find Nothing prefixes)
    where
    find tag = mapMaybe (\s -> get_stroke tag s)
    prefixes = reverse $ drop 1 $ List.inits $ take longest_key sollus

exact_match :: Maybe Solkattu.Tag -> [sollu] -> GetStroke sollu stroke
    -> Maybe [Maybe (Stroke stroke)]
exact_match tag sollus (_, get_stroke) =
    get_stroke tag sollus <|> get_stroke Nothing sollus


-- * format text

-- | Format the notes according to the tala.
--
-- The line breaking for rulers is a bit weird in that if the line is broken,
-- I only emit the first part of the ruler.  Otherwise I'd have to have
-- a multiple line ruler too, which might be too much clutter.  I'll have to
-- see how it works out in practice.
format :: Pretty stroke => Maybe Int -> Int -> Tala.Tala
    -> [(S.Meta (), Note stroke)] -> Text
format override_stroke_width width tala notes =
    Text.stripEnd $ attach_ruler ruler_avartanams
    where
    ruler_avartanams =
        [ (infer_ruler_text tala stroke_width (head lines),
            Text.unlines $ map (format_line . map snd) lines)
        | lines <- avartanam_lines
        ]
    (avartanam_lines, stroke_width) = case override_stroke_width of
        Just n -> (format_lines n width tala notes, n)
        Nothing -> case format_lines 1 width tala notes of
            ([line] : _)
                | sum (map (text_length . _text . snd) line) <= width `div` 2 ->
                    (format_lines 2 width tala notes, 2)
            result -> (result, 1)
    format_line :: [Symbol] -> Text
    format_line = Terminal.fix_for_iterm
        . Text.stripEnd . mconcat . map format_symbol . thin_rests

-- | Drop single character rests on odd columns, to make the output look less
-- cluttered.
thin_rests :: [Symbol] -> [Symbol]
thin_rests = snd . List.mapAccumL thin 0
    where
    thin column sym
        | Text.all (=='_') (_text sym) =
            let (column2, stroke2) = Text.mapAccumL clear column (_text sym)
            in (column2, sym { _text = stroke2 })
        | otherwise = (column + text_length (_text sym), sym)
    clear column _ = (column+1, if even column then '_' else ' ')

-- | If the final non-rest is at sam, drop trailing rests, and don't wrap it
-- onto the next line.
format_final_avartanam :: [[[(a, Symbol)]]] -> [[[(a, Symbol)]]]
format_final_avartanam avartanams = case reverse avartanams of
    [final : rests] : penultimate : prevs
        | not (is_rest (snd final)) && all (is_rest . snd) rests ->
            reverse $ (Seq.map_last (++[final]) penultimate) : prevs
        | otherwise -> avartanams
    _ -> avartanams
    where
    -- This should be (== Space Rest), but I have to show_stroke first to break
    -- lines.
    is_rest = (=="_") . Text.strip . _text

-- | Break into [avartanam], where avartanam = [line].
format_lines :: Pretty stroke => Int -> Int -> Tala.Tala
    -> [(S.Meta (), Note stroke)] -> [[[(S.State, Symbol)]]]
format_lines stroke_width width tala =
    format_final_avartanam . map (break_line width) . break_avartanams
        . map combine . Seq.zip_prev
        . map_with_fst make_symbol
        . annotate_groups
        . S.normalize_speed tala
    where
    combine (prev, (state, sym)) = (state, text (Text.drop overlap) sym)
        where
        overlap = maybe 0 (subtract stroke_width . text_length . _text . snd)
            prev
    make_symbol state (bounds, s) = make bounds $ case s of
        S.Attack a -> justify_left stroke_width ' ' (pretty a)
        S.Sustain a -> Text.replicate stroke_width $ case a of
            Pattern {} -> "-"
            _ -> pretty a
        S.Rest -> justify_left stroke_width ' ' "_"
        where
        make bounds text = Symbol text (should_emphasize aksharas state) bounds
    aksharas = akshara_set tala

-- | Put StartEnd on the strokes to mark group boundaries.
annotate_groups :: [(Maybe (S.GroupMark g), (S.State, S.Stroke a))]
    -> [(S.State, ([StartEnd], S.Stroke a))]
annotate_groups = snd . List.mapAccumL go mempty . zip [0..]
    where
    go ends (i, (g, (state, stroke))) = case g of
        Nothing -> (ends, (state, (if_end, stroke)))
        Just g ->
            ( MultiSet.insert (i + S._count g - 1) ends
            , (state, (Start : if_end, stroke))
            )
        where if_end = replicate (MultiSet.occur i ends) End

data StartEnd = Start | End deriving (Eq, Show)

should_emphasize :: Set Tala.Akshara -> S.State -> Bool
should_emphasize aksharas state =
    S.state_matra state == 0 && Set.member (S.state_akshara state) aksharas

akshara_set :: Tala.Tala -> Set Tala.Akshara
akshara_set = Set.fromList . scanl (+) 0 . Tala.tala_aksharas

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
break_line :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
break_line max_width notes
    | width <= max_width = [notes]
    | even aksharas = break_at (aksharas `div` 2) notes
    | otherwise = break_before max_width notes
    where
    width = sum $ map (text_length . _text . snd) notes
    aksharas = Seq.count at_akshara notes
    break_at akshara =
        pair_to_list . break ((==akshara) . S.state_akshara . fst)
    pair_to_list (a, b) = [a, b]

-- | Yet another word-breaking algorithm.  I must have 3 or 4 of these by now.
break_before :: Int -> [(S.State, Symbol)] -> [[(S.State, Symbol)]]
break_before max_width = go . dropWhile null . Seq.split_with at_akshara
    where
    go aksharas =
        case break_fst (>max_width) (zip (running_width aksharas) aksharas) of
            ([], []) -> []
            (pre, []) -> [concat pre]
            ([], post:posts) -> post : go posts
            (pre, post) -> concat pre : go post
    -- drop 1 so it's the width at the end of each section.
    running_width =
        drop 1 . scanl (+) 0 . map (sum . map (text_length . _text . snd))

break_fst :: (key -> Bool) -> [(key, a)] -> ([a], [a])
break_fst f = (map snd *** map snd) . break (f . fst)

infer_ruler_text :: Tala.Tala -> Int -> [(S.State, a)] -> Text
infer_ruler_text tala stroke_width =
    -- A final stroke will cause a trailing space, so stripEnd.
    Text.stripEnd . mconcatMap fmt . infer_ruler tala
    where
    fmt (label, spaces) = justify_left (spaces * stroke_width) ' ' label

infer_ruler :: Tala.Tala -> [(S.State, a)] -> [(Text, Int)]
infer_ruler tala = zip (Tala.tala_labels tala ++ ["|"])
    . (++[0]) . map length . dropWhile null
    . Seq.split_with at_akshara

at_akshara :: (S.State, a) -> Bool
at_akshara = (==0) . S.state_matra . fst

justify_left :: Int -> Char -> Text -> Text
justify_left n c text
    | len >= n = text
    | otherwise = text <> Text.replicate (n - len) (Text.singleton c)
    where len = text_length text

-- ** formatting

data Symbol = Symbol {
    _text :: !Text
    , _emphasize :: !Bool
    , _bounds :: ![StartEnd]
    } deriving (Eq, Show)

symbol :: Text -> Symbol
symbol text = Symbol text False []

text :: (Text -> Text) -> Symbol -> Symbol
text f sym = sym { _text = f (_text sym) }

format_symbol :: Symbol -> Text
format_symbol (Symbol text emph bounds) = mconcat
    [ Text.replicate (Seq.count (==End) bounds) Terminal.bg_default
    , Text.replicate (Seq.count (==Start) bounds)
        (Terminal.set_bg Terminal.Normal Terminal.White)
    , (if emph then emphasize  else id) text
    ]

emphasize :: Text -> Text
emphasize word
    -- A bold _ looks the same as a non-bold one, so put a bar to make it
    -- more obvious.
    | word == "_ " = emphasize "_|"
    | otherwise = Terminal.bold_on <> pre <> Terminal.bold_off <> post
    where (pre, post) = Text.break (==' ') word

text_length :: Text -> Int
text_length = sum . map len . untxt
    where
    -- Combining characters don't contribute to the width.  I'm sure it's way
    -- more complicated than this, but for the moment this seems to work.
    len c
        | Char.isMark c = 0
        | otherwise = 1

-- * format html

write_html :: Pretty stroke => FilePath -> Tala.Tala
    -> [[(S.Meta (), Note stroke)]] -> IO ()
write_html fname tala =
    Text.IO.writeFile fname . Text.intercalate "\n<hr>\n"
    . map (Doc.un_html . format_html tala)

format_html :: Pretty stroke => Tala.Tala -> [(S.Meta (), Note stroke)]
    -> Doc.Html
format_html tala notes = to_table 30 (map Doc.html ruler) body
    where
    ruler = maybe [] (concatMap akshara . infer_ruler tala)
        (Seq.head avartanams)
    akshara (n, spaces) = n : replicate (spaces-1) ""
    body = map (thin . map snd) avartanams
    thin = map (Doc.Html . _text) . thin_rests . map (symbol . Doc.un_html)
    avartanams = format_table tala notes

format_table :: Pretty stroke => Tala.Tala -> [(S.Meta (), Note stroke)]
    -> [[(S.State, Doc.Html)]]
format_table tala =
    break_avartanams
        . map_with_fst emphasize_akshara
        . map (second show_stroke)
        . annotate_groups
        . S.normalize_speed tala
    where
    -- TODO use bounds to colorize groups
    show_stroke (bounds, s) = case s of
        S.Attack a -> Doc.html (pretty a)
        S.Sustain a -> case a of
            Pattern {} -> "&mdash;"
            _ -> Doc.html $ pretty a
        S.Rest -> "_"
    emphasize_akshara state word
        | should_emphasize aksharas state = "<b>" <> word <> "</b>"
        | otherwise = word
    aksharas = akshara_set tala

to_table :: Int -> [Doc.Html] -> [[Doc.Html]] -> Doc.Html
to_table col_width header rows = mconcatMap (<>"\n") $
    -- TODO the idea is that the cell widths should all be fixed, but it
    -- doesn't work, because I don't understand HTML.  Fix this some day.
    [ "<table cellpadding=0 cellspacing=0 style=\"table-layout: fixed\">"
    , "<tr>" <> mconcatMap th header <> "</tr>\n"
    ] ++ map row rows
    ++ ["</table>"]
    where
    th col = Doc.tag_attrs "th" [("width", showt col_width), ("align", "left")]
        (Just col)
    row cols = "<tr>" <> mconcatMap (Doc.tag "td") cols <> "</tr>"

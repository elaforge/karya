-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase, DeriveFunctor #-}
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

-- * Note

-- | The 'Solkattu.Sollu's have been reduced to concrete strokes.
data Note stroke =
    Note !(Stroke stroke)
    | Space !Solkattu.Space
    | Pattern !Solkattu.Pattern
    -- | This is 'Solkattu.Alignment'.  It shouldn't be here, but since I now
    -- drop groups in realize via 'strip_groups', I have to do
    -- 'verify_alignment' on the output of 'realize', which means I need to
    -- preserve the Alignments.
    | Alignment !Tala.Akshara
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

instance Solkattu.Notation stroke => Solkattu.Notation (Stroke stroke) where
    notation (Stroke emphasis stroke) = case emphasis of
        -- This makes the output ambiguous since some strokes are already
        -- capitalized.  TODO use bold or something
        Heavy -> Text.toUpper $ Solkattu.notation stroke
        Normal -> Solkattu.notation stroke
        Light -> Solkattu.notation stroke

instance Pretty stroke => Pretty (Stroke stroke) where
    pretty (Stroke emphasis stroke) = (<> pretty stroke) $ case emphasis of
        Heavy -> "hv "
        Normal -> ""
        Light -> "lt "

note_of :: Note a -> Maybe (Stroke a)
note_of (Note stroke) = Just stroke
note_of _ = Nothing

stroke_of :: Note a -> Maybe a
stroke_of = fmap _stroke . note_of

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
        Alignment {} -> 0
    has_duration n = case n of
        Note {} -> False
        Space {} -> True
        Pattern {} -> True
        Alignment {} -> False

instance Solkattu.Notation stroke => Solkattu.Notation (Note stroke) where
    notation n = case n of
        Space Solkattu.Rest -> "_"
        Space Solkattu.Sarva -> "="
        Note s -> Solkattu.notation s
        Pattern p -> Solkattu.notation p
        Alignment _ -> "" -- this should be filtered out prior to render

instance Pretty stroke => Pretty (Note stroke) where
    pretty n = case n of
        Space Solkattu.Rest -> "_"
        Space Solkattu.Sarva -> "="
        Note s -> pretty s
        Pattern p -> pretty p
        Alignment n -> "@" <> showt n

note_duration :: S.Tempo -> Note stroke -> S.Duration
note_duration tempo = (* S.matra_duration tempo) . fromIntegral . S.matras_of

-- * verify_alignment

-- | Verify that the notes start and end at sam, and the given Alignments
-- fall where expected.
verify_alignment :: Tala.Tala -> [(S.Tempo, Note stroke)] -> Maybe (Int, Error)
    -- ^ (index where the error occured, error)
verify_alignment tala notes =
    msum (map verify (zip [0..] states)) <|> append_ends_on_sam
    where
    (final_state, states) = S.tempo_to_state tala notes
    -- Either final_state one is at 0, or the last non-rest note is.
    append_ends_on_sam
        | at_akshara 0 final_state || maybe False (at_akshara 0) final_note =
            Nothing
        | otherwise = Just
            ( length states
            , "korvai should end on or before sam: "
                <> S.show_position final_state
            )
        where
        final_note = fst <$> List.find (not . is_space . snd) (reverse states)
    verify (i, (state, Alignment akshara))
        | at_akshara akshara state = Nothing
        | otherwise = Just (i, "expected akshara " <> showt akshara
            <> ", but at " <> S.show_position state)
    verify _ = Nothing
    is_space (Space _) = True
    is_space _ = False
    at_akshara akshara state =
        S.state_akshara state == akshara && S.state_matra state == 0

-- * Patterns

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
    Just notes -> Right $ S.tempo_notes $ S.flatten_with tempo notes

type Realized stroke = S.Flat (Group (Stroke stroke)) (Note stroke)

-- | This is the realized version of 'Solkattu.Group'.  I retain the dropped
-- strokes so "Derive.Solkattu.Technique" can use them.
data Group stroke = Group { _dropped :: ![stroke], _side :: !Solkattu.Side }
    deriving (Eq, Ord, Show)

instance Pretty stroke => Pretty (Group stroke) where
    pretty (Group dropped side) = pretty (dropped, side)

realize :: forall stroke sollu. (Pretty sollu, Solkattu.Notation stroke)
    => RealizePattern S.Tempo stroke -> GetStroke sollu stroke
    -> [S.Flat Solkattu.Group (Solkattu.Note sollu)]
    -> Either Error [Realized stroke]
realize realize_pattern get_stroke =
    collect_groups <=< format_error . first concat . map_until_left realize1
    where
    realize1 note@(S.FNote tempo n) notes = case n of
        Solkattu.Alignment n -> Right ([S.FNote tempo (Alignment n)], notes)
        Solkattu.Space space -> Right ([S.FNote tempo (Space space)], notes)
        Solkattu.Pattern p -> do
            tempo_notes <- realize_pattern tempo p
            return (map (uncurry S.FNote) tempo_notes, notes)
        Solkattu.Note {} -> find_sequence get_stroke (note : notes)
    realize1 (S.FGroup tempo count g) notes = do
        let (pre, post) = splitAt count notes
        -- realize_pattern can change the length of the output, so I have to
        -- recompute the group count.  TODO this is error-prone, making FGroup
        -- not-flat would fix it, but likely make other things more
        -- complicated.
        realized <- format_error $ first concat $ map_until_left realize1 pre
        return (S.FGroup tempo (length realized) g : realized, post)

    format_error :: ([S.Flat g (Note stroke)], Maybe Text)
        -> Either Error [S.Flat g (Note stroke)]
    format_error (result, Nothing) = Right result
    format_error (pre, Just err) = Left $
        TextUtil.joinWith "\n" (error_notation (S.flattened_notes pre)) err
    error_notation = Text.unwords . map (justify_left 2 ' ' . Solkattu.notation)

{- | Given a group like

    > [S.FGroup 2 (Solkatttu.Group 1 Before), a, b, c]

    collect dropped strokes into a 'Group':

    > [S.FGroup 1 (Group [a] Before), b, c]
-}
collect_groups :: [S.Flat Solkattu.Group (Note stroke)]
    -> Either Error [Realized stroke]
collect_groups = go
    where
    go (S.FGroup tempo count g : notes) = do
        (group, rest) <- collect_group tempo g pre
        (group:) <$> go (rest ++ post)
        where (pre, post) = splitAt count notes
    go (S.FNote tempo note : notes) = (S.FNote tempo note :) <$> go notes
    go [] = Right []

collect_group :: S.Tempo -> Solkattu.Group
    -> [S.Flat Solkattu.Group (Note stroke)]
    -> Either Error (Realized stroke, [S.Flat Solkattu.Group (Note stroke)])
    -- ^ The updated Group, and the rest of the notes.  The rest are returned
    -- unmodified because there might be nested groups inside, and I'll need to
    -- call 'collect_group' on them too.
collect_group tempo g@(Solkattu.Group split side) notes = do
    -- Since 'split_strokes' will add the 'dropped' durations, 0 makes it split
    -- after all dropped strokes.
    -- Debug.tracepM "split" $ (split, S.fmatra_duration tempo split)
    (left, (pre, post)) <- first (("dropping group " <> pretty g <> ": ") <>) $
        split_strokes (S.fmatra_duration tempo split) notes
    unless (left == 0) $
        Left $ "split at " <> pretty split <> " is longer than the sequence, "
            <> pretty left <> " aksharas left over"
    let (kept, dropped) = case side of
            Solkattu.Before -> (post, pre)
            Solkattu.After -> (pre, post)
    let group = Group
            { _dropped = mapMaybe note_of $ S.flattened_notes dropped
            , _side = side
            }
    return (S.FGroup tempo (length kept) group, kept)

-- split_strokes2 :: S.Duration -> [S.Flat Solkattu.Group (Note stroke)]
--     -> Either Error (S.Duration,
--         ( [S.Flat Solkattu.Group (Note stroke)]
--         , [S.Flat Solkattu.Group (Note stroke)]
--         ))
-- split_strokes2 dur (note : notes) = case note of
--     S.FGroup tempo count (Solkattu.Group split side) -> do
--         (left, (before, after)) <-
--             split_strokes2 (S.fmatra_duration tempo split) pre
--         unless (left == 0) $
--             Left $ "split at " <> pretty split <> " is longer than the group, "
--                 <> pretty left <> " aksharas left over"
--         let (kept, dropped) = case side of
--                 Solkattu.Before -> (after, before)
--                 Solkattu.After -> (before, after)
--         let group = Group
--                 { _dropped = mapMaybe note_of $ S.flattened_notes dropped
--                 , _side = side
--                 }
--         return (S.FGroup tempo (length kept) group, kept)
--         where (pre, post) = splitAt count notes


split_strokes :: S.Duration -> [S.Flat Solkattu.Group (Note stroke)]
    -> Either Error (S.Duration,
        ( [S.Flat Solkattu.Group (Note stroke)]
        , [S.Flat Solkattu.Group (Note stroke)]
        ))
split_strokes dur [] = Right (dur, ([], []))
split_strokes dur (grp@(S.FGroup tempo _ (Solkattu.Group split side)) : notes) =
    add (grp:) $ case side of
        Solkattu.Before -> do
            -- Debug.tracepM "add Before" (dur*4, dropped_dur*4)
            split_strokes (dur + dropped_dur) notes
        Solkattu.After -> do
            (left, (pre, post)) <- split_strokes dur notes
            -- TODO I think this is wrong
            -- Debug.tracepM "add After" (left*4, dropped_dur*4)
            if left > 0
                then add (pre++) $ split_strokes (left + dropped_dur) post
                else return (left, (pre, post))
            -- dur=2
            -- split 2
            -- g (ta ka | din) na
    where
    dropped_dur = S.fmatra_duration tempo split
    add = fmap . second . first

split_strokes dur (note@(S.FNote tempo n) : notes)
    | dur <= 0 = Right (0, ([], note : notes))
    | note_dur <= dur = do
        -- Debug.tracepM "<=" (tempo, note_dur * 4, dur * 4)
        add (note:) $ split_strokes (dur - note_dur) notes
    | otherwise = case n of
        Note _ -> Left "can't split a stroke"
        Pattern p -> Left $ "can't split a pattern: " <> pretty p
        Alignment _ -> Left "can't split 0 duration alignment"
        Space space -> do
            let make = fmap (map (uncurry S.FNote)) . make_space tempo space
            pre <- make dur
            post <- make (dur - note_dur)
            return (0, (pre, post ++ notes))
    where
    note_dur = -- Debug.trace_retp "note_dur" (tempo, n, notes) $
        note_duration tempo n
    add = fmap . second . first

-- | Try to produce Spaces of the given Duration.  Based on Notation.spaceD.
make_space :: S.Tempo -> Solkattu.Space -> S.Duration
    -> Either Error [(S.Tempo, Note stroke)]
make_space tempo space dur = map make <$> S.decompose s0_matras
    where
    make speed = (tempo { S._speed = speed }, Space space)
    s0_matras = dur * fromIntegral (S._nadai tempo)

-- | Find the longest matching sequence and return the match and unconsumed
-- notes.
find_sequence :: Pretty sollu => GetStroke sollu stroke
    -> [S.Flat g (Solkattu.Note sollu)]
    -> Either Error ([S.Flat g (Note stroke)], [S.Flat g (Solkattu.Note sollu)])
find_sequence get_stroke notes =
    case best_match tag sollus get_stroke of
        Nothing -> Left $ "sequence not found: " <> pretty sollus
        Just strokes -> Right $ replace_sollus strokes notes
    where
    -- Collect only sollus and rests, and strip the rests.
    sollus = Maybe.catMaybes $ fst $
        Seq.span_while is_sollu (S.flattened_notes notes)
    is_sollu (Solkattu.Note n) = Just $ Just $ Solkattu._sollu n
    is_sollu (Solkattu.Space {}) = Just Nothing
    is_sollu (Solkattu.Alignment {}) = Just Nothing
    is_sollu _ = Nothing
    tag = Solkattu._tag
        =<< Seq.head (mapMaybe Solkattu.note_of (S.flattened_notes notes))

-- | Match each stroke to a Sollu, copying over Rests without consuming
-- a stroke.
replace_sollus :: [Maybe (Stroke stroke)]
    -> [S.Flat g (Solkattu.Note sollu)]
    -> ([S.Flat g (Note stroke)], [S.Flat g (Solkattu.Note sollu)])
replace_sollus [] ns = ([], ns)
replace_sollus (stroke : strokes) (n : ns) = case n of
    S.FGroup tempo count g -> first (S.FGroup tempo count g :) next
    S.FNote tempo note -> case note of
        Solkattu.Note _ ->
            first (S.FNote tempo rnote :) $ replace_sollus strokes ns
            where rnote = maybe (Space Solkattu.Rest) Note stroke
        Solkattu.Space space -> first (S.FNote tempo (Space space) :) next
        Solkattu.Alignment {} -> next
        -- This shouldn't happen because Seq.span_while is_sollu should have
        -- stopped when it saw this.
        Solkattu.Pattern {} -> next
    where
    -- Continue without consuming this stroke.
    next = replace_sollus (stroke : strokes) ns
replace_sollus (_:_) [] = ([], [])
    -- This shouldn't happen because strokes from the StrokeMap should be
    -- the same length as the RealizedNotes used to find them.

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
format :: Solkattu.Notation stroke => Maybe Int -> Int -> Tala.Tala
    -> [S.Flat g (Note stroke)] -> Text
format override_stroke_width width tala notes =
    Text.stripEnd $ Terminal.fix_for_iterm $ attach_ruler ruler_avartanams
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
    format_line = Text.stripEnd . mconcat . map format_symbol . thin_rests

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
format_lines :: Solkattu.Notation stroke => Int -> Int -> Tala.Tala
    -> [S.Flat g (Note stroke)] -> [[[(S.State, Symbol)]]]
format_lines stroke_width width tala =
    format_final_avartanam . map (break_line width) . break_avartanams
        . map combine . Seq.zip_prev
        . map make_symbol
        . annotate_groups
        . S.normalize_speed tala
    where
    combine (prev, (state, sym)) = (state, text (Text.drop overlap) sym)
        where
        overlap = maybe 0 (subtract stroke_width . text_length . _text . snd)
            prev
    make_symbol (start_ends, (state, note)) = (state,) $ make $ case note of
        S.Attack a -> justify_left stroke_width ' ' (Solkattu.notation a)
        S.Sustain a -> Text.replicate stroke_width $ case a of
            Pattern {} -> "-"
            _ -> Solkattu.notation a
        S.Rest -> justify_left stroke_width ' ' "_"
        where
        make text = Symbol text (should_emphasize aksharas state) start_ends
    aksharas = akshara_set tala

-- | Put StartEnd on the strokes to mark group boundaries.  This discards all
-- other group data.
annotate_groups :: [S.Flat g a] -> [([StartEnd], a)]
annotate_groups =
    Maybe.catMaybes . snd . List.mapAccumL go (mempty, 0) . zip [0..]
    where
    go (groups, starts) (i, S.FGroup _ count _) =
        ((MultiSet.insert (i + count) groups, starts + 1), Nothing)
    go (groups, starts) (i, S.FNote _ note) =
        ( (groups, 0)
        , Just (replicate starts Start ++ replicate ends End, note)
        )
        where ends = MultiSet.occur i groups

-- TODO these don't distinguish between different groups, but I'll probably
-- want to do that once I have fancier formatting.
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

write_html :: Solkattu.Notation stroke => FilePath -> Tala.Tala
    -> [[S.Flat g (Note stroke)]] -> IO ()
write_html fname tala =
    Text.IO.writeFile fname . Text.intercalate "\n<hr>\n"
    . map (Doc.un_html . format_html tala)

format_html :: Solkattu.Notation stroke => Tala.Tala
    -> [S.Flat g (Note stroke)] -> Doc.Html
format_html tala notes = to_table 30 (map Doc.html ruler) body
    where
    ruler = maybe [] (concatMap akshara . infer_ruler tala)
        (Seq.head avartanams)
    akshara (n, spaces) = n : replicate (spaces-1) ""
    body = map (thin . map snd) avartanams
    thin = map (Doc.Html . _text) . thin_rests . map (symbol . Doc.un_html)
    avartanams = format_table tala notes

format_table :: Solkattu.Notation stroke => Tala.Tala
    -> [S.Flat g (Note stroke)] -> [[(S.State, Doc.Html)]]
format_table tala =
    break_avartanams
        . map emphasize_akshara
        . map show_stroke
        . annotate_groups
        . S.normalize_speed tala
    where
    -- TODO use start_end to colorize groups
    show_stroke (start_end, (state, s)) = (state,) $ case s of
        S.Attack a -> Doc.html (Solkattu.notation a)
        S.Sustain a -> case a of
            Pattern {} -> "&mdash;"
            _ -> Doc.html $ Solkattu.notation a
        S.Rest -> "_"
    emphasize_akshara (state, word)
        | should_emphasize aksharas state = (state, "<b>" <> word <> "</b>")
        | otherwise = (state, word)
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

-- * util

span_until_just :: [(Maybe a, b)] -> ([b], [(Maybe a, b)])
span_until_just [] = ([], [])
span_until_just ((Nothing, b) : abs) = first (b:) (span_until_just abs)
span_until_just abs@((Just _, _) : _) = ([], abs)

-- | Apply the function until it returns Left.  The function can consume a
-- variable number of elements, ultimately because 'find_sequence' does.
map_until_left :: (a -> [a] -> Either err (b, [a])) -> [a] -> ([b], Maybe err)
map_until_left f = go
    where
    go [] = ([], Nothing)
    go (x:xs) = case f x xs of
        Left err -> ([], Just err)
        Right (val, rest) -> first (val:) (go rest)

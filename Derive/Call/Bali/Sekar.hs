-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | Pattern based derivation.
module Derive.Call.Bali.Sekar where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Ui.ScoreTime as ScoreTime
import qualified Derive.Args as Args
import qualified Derive.Call.Module as Module
import qualified Derive.Call.Post as Post
import qualified Derive.Call.Sub as Sub
import qualified Derive.Call.Tags as Tags
import qualified Derive.Derive as Derive
import qualified Derive.Flags as Flags
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("sekar", c_sekar_direct)
    , ("sekar-e", c_sekar_even)
    ]

module_ :: Module.Module
module_ = "bali" <> "sekar"

c_sekar_direct :: Derive.Generator Derive.Note
c_sekar_direct = Derive.generator module_ "sekar" (Tags.inst <> Tags.subs)
    "Arrange sub-notes according to a pattern.\
    \\nIn the direct substitution style, each note retains its relative\
    \ duration as it is rearranged by the pattern. A rest is considered a\
    \ note, but just one note, so you can't have two rests in a row."
    $ Sig.call ((,)
    <$> Sig.required "pattern" ("If there is a list of patterns, they are for\
        \ different numbers of notes, starting with 1. A single pattern is\
        \ applied to all numbers though. " <> pattern_doc)
    <*> arrive_env
    ) $ \(pattern_text, arrive) args -> do
        patterns <- mapM make_pattern pattern_text
        patterns <- Derive.require_right id $
            check_patterns (zip patterns pattern_text)
        let range = Args.range args
        let derive
                | arrive = sekar_direct_arrive range patterns
                | otherwise = sekar_direct range patterns
        mconcatMap derive =<< Sub.sub_rest_events arrive True args

c_sekar_even :: Derive.Generator Derive.Note
c_sekar_even = Derive.generator module_ "sekar" (Tags.inst <> Tags.subs)
    "Arrange sub-notes according to a pattern.\
    \\nIn the even subdivision style, the range is divided evenly based\
    \ on the highest index of the pattern (so `abcac` would divide into 3\
    \ parts). The melody is sampled at those points for note attacks,\
    \ sustains, and rests, which are then rearranged by the pattern.\
    \ Thus, the output is always notes in a regular tempo determined by the\
    \ length of the pattern."
    $ Sig.call ((,)
    <$> Sig.required "pattern" pattern_doc
    <*> arrive_env
    ) $ \(pattern, arrive) args -> do
        pattern <- make_pattern pattern
        let derive = sekar_even arrive (Args.range args) pattern
        mconcatMap derive =<< Sub.sub_rest_events arrive True args

arrive_env :: Sig.Parser Bool
arrive_env = Sig.environ "arrive" Sig.Prefixed True $
    "If true, the last note of the pattern is aligned to the end of the event,\
    \ and given " <> ShowVal.pretty Flags.infer_duration <> "."

pattern_doc :: Derive.Doc
pattern_doc =
    "The pattern is letters from a-z, where `a` is the first note and `z` is\
    \ the 26th. Capital letters replace that note with a rest. Gaps in the\
    \ input notes count as rest notes."

check_patterns :: [(Pattern, Text)] -> Either Text (NonEmpty Pattern)
check_patterns [(pattern, _)] = Right (pattern :| [])
check_patterns patterns = do
    mapM_ check (zip [1..] patterns)
    case NonEmpty.nonEmpty (map fst patterns) of
        Nothing -> Left "require at least one pattern"
        Just ps -> Right ps
    where
    check (n, (pattern, ptext))
        | pattern_length pattern /= n = Left $
            "expected pattern of length " <> showt n <> " but got "
            <> showt ptext
        | otherwise = return ()

-- ** even subdivision

data DivNote a = DivNote !a | DivRest | DivContinue
    deriving (Eq, Show, Functor)

sekar_even :: Bool -> (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar_even arrive (start, end) pattern events =
    Sub.derive $ (if arrive then nudge else id) $ map (Sub.at start) $
        div_realize dur notes pattern
    where
    notes = div_extract events samples
    samples = (if arrive then drop 1 else id) $ Seq.range start end ndur
    ndur = (end - start) / fromIntegral (pattern_length pattern)
    dur = (end - start) / fromIntegral (length pattern)
    nudge = Seq.map_last (fmap add_last_note_flags) . map (Sub.at dur)

div_realize :: ScoreTime -> [DivNote a] -> Pattern -> [Sub.GenericEvent a]
div_realize dur notes = combine . zip (Seq.range_ 0 dur) . map resolve
    where
    resolve (i, element) = case element of
        Rest -> DivRest
        Note -> fromMaybe DivRest $ Seq.at notes i
    combine ((start, note) : notes) = case note of
        DivRest -> continue
        DivContinue -> continue
        DivNote d -> Sub.Event start (dur * fromIntegral cs) d : continue
            where cs = length (takeWhile is_continue notes) + 1
        where continue = combine notes
    combine [] = []
    is_continue (_, DivContinue) = True
    is_continue _ = False

-- | Convert Sub.Events to DivNotes at the given times.
div_extract :: [Sub.GenericEvent (Maybe a)] -> [ScoreTime] -> [DivNote a]
div_extract events = snd . List.mapAccumL go events
    where
    go events t = case drop_until_next (past t) events of
        events@(event : _) -> (,) events $ case Sub.event_note event of
            Nothing -> DivRest
            Just d
                | t ScoreTime.== Sub.event_start event -> DivNote d
                | t ScoreTime.> Sub.event_end event -> DivRest
                | otherwise -> DivContinue
        [] -> ([], DivRest)
    past t = (ScoreTime.> t) . Sub.event_start

-- | Drop until the predicate is true for the next event.  This is like
-- @dropWhile (not . f)@, but you get the element before the predicate becomes
-- false.
drop_until_next :: (a -> Bool) -> [a] -> [a]
drop_until_next f xs = case xs of
    xs@(_ : x2 : _) | f x2 -> xs
    [x] -> [x]
    _ : xs -> drop_until_next f xs
    [] -> []

-- ** direct substitution

-- | Like 'sekar_direct', but expect sub-events excluding the start and
-- including the end, and align the last note to the end of the call.
sekar_direct_arrive :: (ScoreTime, ScoreTime) -> NonEmpty Pattern
    -> [Sub.RestEvent] -> Derive.NoteDeriver
sekar_direct_arrive range patterns events_ =
    Sub.derive $ add_flags $ align $ map (Sub.stretch factor) $
        Sub.strip_rests realized
    where
    -- The first event should be a rest, since I passed end_bias=True to
    -- Sub.sub_rest_events.  The stretch factor assumes the event durations add
    -- up to 'range'.  For that to be true, I have to give the initial rest
    -- duration to the destination note.
    events = case events_ of
        [] -> []
        rest : events -> case Seq.viewr events of
            Nothing -> events
            Just (initial, final)
                | Sub.event_start final >= snd range ->
                    initial ++ [final { Sub.event_duration = dur }]
                | otherwise -> events ++ [Sub.Event (snd range) dur Nothing]
                where dur = Sub.event_duration rest

    realized = realize_groups patterns events
    factor = sum_duration events / sum_duration realized
    -- Align notes to the end of the range.
    align es = case Seq.last es of
        Nothing -> []
        Just e -> map (Sub.at (snd range - Sub.event_start e)) es
    add_flags = Seq.map_last $ fmap add_last_note_flags

add_last_note_flags :: Derive.NoteDeriver -> Derive.NoteDeriver
add_last_note_flags = fmap $ Post.emap1_ $ Score.add_flags $
    Flags.infer_duration <> Flags.strong

sum_duration :: [Sub.GenericEvent a] -> ScoreTime
sum_duration = sum . map Sub.event_duration

-- | Sekaran derivation via direct substitution of the sub-events.
sekar_direct :: (ScoreTime, ScoreTime) -> NonEmpty Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar_direct range patterns events =
    Sub.derive $ map (Sub.place (fst range) factor) $ Sub.strip_rests realized
    where
    realized = realize_groups patterns events
    factor = sum_duration events / sum_duration realized

realize_groups :: NonEmpty Pattern -> [Sub.RestEvent] -> [Sub.RestEvent]
realize_groups _ [] = []
realize_groups patterns events@(event:rest) =
    go (Sub.event_start event) $
        split_groups (pattern_length pattern) events
    where
    pattern = index_with (event :| rest) patterns
    go _ [] = []
    go start (group : groups) = notes ++ go (start + sum_duration notes) groups
        where notes = realize start group pattern

index_with :: NonEmpty a -> NonEmpty b -> b
index_with (x :| xs) (y :| ys) = go x xs y ys
    where
    go _ (x:xs) _ (y:ys) = go x xs y ys
    go _ [] y _ = y
    go _ _ y [] = y

split_groups :: Int -> [a] -> [[a]]
split_groups n notes = case splitAt n notes of
    ([], _) -> []
    (pre, post) -> pre : split_groups n post

type Pattern = [(Index, Element)]
-- | Index into melody notes.
type Index = Int
data Element = Note | Rest deriving (Show)

pattern_length :: Pattern -> Int
pattern_length = maximum . (0:) . map ((+1) . fst)

make_pattern :: Text -> Derive.Deriver Pattern
make_pattern pattern = do
    when (Text.null pattern || Text.any (not . a_to_z . Char.toLower) pattern) $
        Derive.throw $ "pattern chars must be a-z: " <> showt pattern
    return
        [ (fromEnum (Char.toLower c) - fromEnum 'a',
            if Char.isUpper c then Rest else Note)
        | c <- Text.unpack pattern
        ]
    where a_to_z c = 'a' <= c && c <= 'z'

-- | Apply the pattern to the events.
realize :: ScoreTime -> [Sub.RestEvent] -> Pattern -> [Sub.RestEvent]
realize start events = map place . add_starts . mapMaybe resolve
    where
    resolve (i, element) = resolve1 element <$> Seq.at events i
    -- Rests have a duration, but no deriver.
    resolve1 element (Sub.Event _ dur d) = case d of
        Nothing -> (dur, Nothing)
        Just deriver -> (,) dur $ case element of
            Note -> Just deriver
            Rest -> Nothing
    add_starts events = zip (scanl (+) start (map fst events)) events
    place (start, (dur, maybe_deriver)) = Sub.Event start dur maybe_deriver

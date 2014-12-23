-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | Pattern based derivation.
--
-- TODO This is merely a proof of concept.  Sekaran could be implemented a lot
-- of ways, but I'll have to wait to see which ones are most compositionally
-- useful.  It would definitely be more natural, though, if the notation marked
-- the seleh instead of the first note.  But that would require either
-- a preproc pass or deriving the notes backwards.
module Derive.Call.Bali.Sekar where
import qualified Data.Char as Char
import qualified Data.List as List
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
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Sig as Sig

import qualified Perform.Pitch as Pitch
import Global
import Types


note_calls :: Derive.CallMaps Derive.Note
note_calls = Derive.generator_call_map
    [ ("sekar", c_sekar False direct_doc)
    , ("sekar-e", c_sekar True even_doc)
    ]

module_ :: Module.Module
module_ = "bali" <> "sekar"

c_sekar :: Bool-> Text -> Derive.Generator Derive.Note
c_sekar even doc = Derive.make_call module_ "sekar" (Tags.inst <> Tags.subs)
    ("Arrange sub-notes according to a pattern.\n" <> doc)  $ Sig.call ((,)
    <$> Sig.required "pattern"
        "Apply this pattern to the encompassed notes. The pattern is\
        \ letters from a-z, where `a` is the first note and `z` is the 26th.\
        \ Capital letters replace that note with a rest. Gaps in the input\
        \ notes count as rest notes."
    <*> Sig.environ "arrive" Sig.Prefixed True ("If true, the last note of the\
        \ pattern is aligned to the end of the event, and given "
        <> ShowVal.doc_val Flags.infer_duration <> ".")
    ) $ \(pattern, arrive) args -> do
        pattern <- make_pattern pattern
        let derive
                | even = sekar_even arrive
                | arrive = sekar_direct_arrive
                | otherwise = sekar_direct
        mconcatMap (derive (Args.range args) pattern)
            =<< Sub.sub_rest_events arrive True args

even_doc :: Text
even_doc =
    "In the even subdivision style, the range is divided evenly based\
    \ on the highest index of the pattern (so `abcac` would divide into 3\
    \ parts). The melody is sampled at those points for note attacks,\
    \ sustains, and rests, which are then rearranged by the pattern.\
    \ Thus, the output is always notes in a regular tempo determined by the\
    \ length of the pattern."

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
                | ScoreTime.eq t (Sub.event_start event) -> DivNote d
                | t `ScoreTime.gt` Sub.event_end event -> DivRest
                | otherwise -> DivContinue
        [] -> ([], DivRest)
    past t = (`ScoreTime.gt` t) . Sub.event_start

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

direct_doc :: Text
direct_doc =
    "In the direct substitution style, each note retains its relative \
    \ duration as it is rearranged by the pattern.  A rest is considered a\
    \ note, but just one note, so you can't have two rests in a row."

apply f (Sub.Event s d n) = Sub.Event s d <$> f n

showr :: Maybe Derive.NoteDeriver -> Derive.Deriver Text
showr = maybe (return "-") showd

showd :: Derive.NoteDeriver -> Derive.Deriver Text
showd d = do
    es <- LEvent.events_of <$> d
    return $ maybe "?" Pitch.note_text $ Score.initial_note =<< Seq.head es

sekar_direct_arrive :: (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar_direct_arrive range pattern events_ = do
    -- Debug.traceM "events" =<< mapM (apply showr) events
    -- Debug.traceM "realized" =<< mapM (apply showr) realized
    Sub.derive $ add_flags $ align $ map (Sub.stretch factor) $
        Sub.strip_rests $ realized
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

    realized = realize_groups pattern events
    factor = sum_duration events / sum_duration realized
    -- Align notes to the end of the range.
    align es = case Seq.last es of
        Nothing -> []
        Just e -> map (Sub.at (snd range - Sub.event_start e)) es
    add_flags = Seq.map_last $ fmap $ add_last_note_flags

add_last_note_flags :: Derive.NoteDeriver -> Derive.NoteDeriver
add_last_note_flags = fmap $ Post.emap1_ $ Score.add_flags $
    Flags.infer_duration <> Flags.cancel_next

sum_duration :: [Sub.GenericEvent a] -> ScoreTime
sum_duration = sum . map Sub.event_duration

sekar_direct :: (ScoreTime, ScoreTime) -> Pattern -> [Sub.RestEvent]
    -> Derive.NoteDeriver
sekar_direct range pattern events =
    Sub.derive $ map (Sub.place (fst range) factor) $ Sub.strip_rests realized
    where
    realized = realize_groups pattern events
    factor = sum_duration events / sum_duration realized

realize_groups :: Pattern -> [Sub.RestEvent] -> [Sub.RestEvent]
realize_groups _ [] = []
realize_groups pattern events@(event:_) =
    go (Sub.event_start event) $ split_groups (pattern_length pattern) events
    where
    go _ [] = []
    go start (group : groups) =
        notes ++ go (start + sum_duration notes) groups
        where notes = realize start group pattern

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
        Derive.throw $ "pattern chars must be a-z: " ++ show pattern
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

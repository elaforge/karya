-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | The 'Events' type contains the events of a track.

    This is the largest part of the score and also the part most often
    modified, so there is a plethora of access functions.
-}
module Ui.Events (
    -- * events
    Events
    , Range(..), range
    , empty, null, length, time_begin, time_end

    -- ** list conversion
    , singleton, from_list
    , ascending, descending

    -- ** transformation
    , map_events, clip

    -- ** insert / remove
    , insert, remove, remove_at
    , merge

    -- ** lookup
    , at, overlapping, head, last

    -- ** split
    -- *** events
    , split_range, split, split_exclude
    , in_range
    , around
    -- *** List [Event]
    , split_lists
    , at_after, after, before
    , split_at_before
    , check_invariants
    , clip_negative_events

#ifdef TESTING
    , module Ui.Events
#endif
) where
import qualified Prelude
import Prelude hiding (head, last, length, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map

import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Ui.Event as Event
import qualified Ui.ScoreTime as ScoreTime
import Global
import Types


data Range =
    Inclusive ScoreTime ScoreTime
    | Positive ScoreTime ScoreTime
    | Negative ScoreTime ScoreTime
    deriving (Eq, Show)

instance Pretty.Pretty Range where
    pretty r = case r of
        Inclusive s e -> pretty s <> "++" <> pretty e
        Positive s e -> pretty s <> "+-" <> pretty e
        Negative s e -> pretty s <> "-+" <> pretty e
        -- the [1, 2) notation looks really confusing in context.

range :: Event.Orientation -> ScoreTime -> ScoreTime -> Range
range orientation start end
    | start == end = Inclusive start end
    | orientation == Event.Positive = Positive (min start end) (max start end)
    | otherwise = Negative (min start end) (max start end)

range_times :: Range -> (ScoreTime, ScoreTime)
range_times r = case r of
    Inclusive s e -> (s, e)
    Positive s e -> (s, e)
    Negative s e -> (s, e)

-- * events

empty :: Events
empty = Events Map.empty

null :: Events -> Bool
null (Events m) = Map.null m

length :: Events -> Int
length = Map.size . get

time_begin :: Events -> ScoreTime
time_begin = maybe 0 Event.start . head

time_end :: Events -> ScoreTime
time_end = maybe 0 Event.end . last

-- ** list conversion

singleton :: Event.Event -> Events
singleton event = Events $ Map.singleton (Event.start event) event

from_list :: [Event.Event] -> Events
from_list evts = insert evts empty

-- | Get all events in ascending order.
ascending :: Events -> [Event.Event]
ascending = to_asc_list . get

descending :: Events -> [Event.Event]
descending = to_desc_list . get

-- ** transformation

-- | Map a function across the events in Events.
map_events :: (Event.Event -> Event.Event) -> Events -> Events
map_events f = from_list . map f . ascending

-- | Clip off the events after the given end time.  Also shorten the last
-- event so it doesn't cross the end, if necessary.
clip :: Bool -> ScoreTime -> [Event.Event] -> [Event.Event]
clip _ _ [] = []
clip allow_zero at (event : events)
    | allow_zero && Event.start event > at = []
    | not allow_zero && Event.start event >= at = []
    | Event.end event > at =
        [Event.modify_duration (\d -> min d (at - Event.start event)) event]
    | otherwise = event : clip allow_zero at events

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces important invariants.
insert :: [Event.Event] -> Events -> Events
insert [] events = events
insert new_events_ (Events events) =
    Events $ Map.unions [pre, overlapping, post]
    where
    new_events = map round_event (Seq.sort_on Event.start new_events_)
    start = Event.start (Prelude.head new_events)
    end = Event.end (Prelude.last new_events)
    (pre, within, post) = _split_overlapping start end events
    overlapping = from_ascending $ clip_events $
        Seq.merge_on Event.start (Map.elems within) new_events

-- | Round event times as described in 'ScoreTime.round'.
round_event :: Event.Event -> Event.Event
round_event event =
    Event.place (ScoreTime.round (Event.start event))
        (ScoreTime.round (Event.duration event)) event

-- | Remove events in the range.
remove :: Range -> Events -> Events
remove range events = case range of
    Inclusive start end | start == end -> emap (Map.delete start) events
    _ -> case split_range range events of
        (_, Events within, _) -> emap (`Map.difference` within) events

-- | Remove an event if it occurs exactly at the given pos.
remove_at :: ScoreTime -> Events -> Events
remove_at t = remove (Inclusive t t)

-- ** lookup

-- | An event exactly at the given pos, or Nothing.
at :: ScoreTime -> Events -> Maybe Event.Event
at pos = Map.lookup pos . get

-- | Like 'at', but return an event that overlaps the given pos.
overlapping :: ScoreTime -> Events -> Maybe Event.Event
overlapping pos events
    | next : _ <- post, Event.start next == pos || Event.end next < pos =
        Just next
    | prev : _ <- pre, Event.end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = (descending *** ascending) $ split pos events

head :: Events -> Maybe Event.Event
head (Events events) = snd <$> Map.min events

-- | Final event, if there is one.
last :: Events -> Maybe Event.Event
last (Events events) = snd <$> Map.max events

-- ** split

-- *** events

split_range :: Range -> Events -> (Events, Events, Events)
split_range range (Events events) = (Events pre, Events within, Events post)
    where
    (pre, within, post) = adjust_end $ adjust_start $
        Map.split3 start end events
    (start, end) = range_times range
    adjust_start (pre, within, post)
        | not want_start, Just (t, evt) <- Map.min within, t == start =
            (Map.insert t evt pre, Map.delete t within, post)
        | otherwise = (pre, within, post)
    adjust_end (pre, within, post)
        | want_end, Just (t, evt) <- Map.min post, t == end =
            (pre, Map.insert t evt within, Map.delete t post)
        | otherwise = (pre, within, post)
    want_start = case range of
        Negative {} -> False
        _ -> True
    want_end = case range of
        Positive {} -> False
        _ -> True

-- | Split at the given time.  An event that starts at the give time will
-- appear in the above events.
split :: ScoreTime -> Events -> (Events, Events)
split pos (Events events) = (Events pre, Events post)
    where (pre, post) = Map.split2 pos events

-- | Like 'split', but an event that matches exactly is excluded from the
-- result.
split_exclude :: ScoreTime -> Events -> (Events, Events)
split_exclude pos (Events events) = (Events pre, Events post)
    where (pre, post) = Map.split pos events

-- | Like 'split_range', but only return the middle part.
in_range :: Range -> Events -> Events
in_range range events = within
    where (_, within, _) = split_range range events

-- | Get events in the given range, plus surrounding.  If there is no event at
-- 'start', the previous event will be included.  The event after 'end' is
-- always included.
around :: ScoreTime -> ScoreTime -> Events -> Events
around start end = emap (split_around start end)
    where
    split_around start end events = above (below within)
        where
        (pre, within, post) = Map.split3 start end events
        below m
            | Just (lowest, _) <- Map.min within, lowest == start = m
            | otherwise = maybe m (\(pos, evt) -> Map.insert pos evt m)
                (Map.max pre)
        above m = maybe m (\(pos, evt) -> Map.insert pos evt m)
            (Map.min post)

-- *** List [Event]

split_lists :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_lists pos = (descending *** ascending) . split pos

-- | Events whose start is at or after @pos@.
at_after :: ScoreTime -> Events -> [Event.Event]
at_after pos = snd . split_lists pos

-- | Events whose start is strictly after @pos@.
after :: ScoreTime -> Events -> [Event.Event]
after pos events = case at_after pos events of
    next : rest | Event.start next == pos -> rest
    events -> events

-- | Events whose start before @pos@.
before :: ScoreTime -> Events -> [Event.Event]
before pos = fst . split_lists pos

-- | This is like 'split', but if there isn't an event exactly at the pos then
-- put the previous one in the post list.
split_at_before :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
split_at_before pos events
    | next : _ <- post, Event.start next == pos = (pre, post)
    | before : prepre <- pre = (prepre, before:post)
    | otherwise = (pre, post)
    where (pre, post) = split_lists pos events

-- * implementation

-- | This is the underlying storage for a sequence of events.  The invariant
-- is that events start + duration don't overlap.
--
-- This type should remain abstract, and you should manipulate events using
-- functions in this module.
newtype Events = Events EventMap
    deriving (DeepSeq.NFData, Eq, Show, Read)

-- | The ScoreTime is redundant since it's also stored in the Event itself.
-- I used to have them separate, but then I had to pass (ScoreTime, Event)
-- pairs around everywhere.
type EventMap = Map.Map ScoreTime Event.Event

-- | This assumes the input is already sorted!
from_ascending :: [Event.Event] -> EventMap
from_ascending = Map.fromAscList . Seq.key_on Event.start

to_asc_list :: EventMap -> [Event.Event]
to_asc_list = map snd . Map.toAscList

to_desc_list :: EventMap -> [Event.Event]
to_desc_list = map snd . Map.toDescList

instance Pretty.Pretty Events where
    format = Pretty.format . map event . ascending
        where
        event e = Pretty.text $
            o <> pretty (Event.start e, Event.duration e, Event.text e)
            where o = if Event.orientation e == Event.Positive then "" else "-"

instance Monoid Events where
    mempty = empty
    mappend = merge

get :: Events -> EventMap
get (Events evts) = evts

emap :: (EventMap -> EventMap) -> Events -> Events
emap f (Events evts) = Events (f evts)

-- | Put events that overlap the range into within.
_split_overlapping :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_split_overlapping start end events = (pre2, within2, post)
    where
    (pre, within, post) = Map.split3 start end events
    (pre2, within2) = case Map.max pre of
        Just (pos, evt) | Event.overlaps start evt ->
            (Map.delete pos pre, Map.insert pos evt within)
        _ -> (pre, within)

{- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
    clipped so they don't overlap.  If events occur simultaneously, the event
    from @evts1@ wins.

    The strategy is to extract the overlapping section and clip only that, then
    merge it back into the input maps, before merging them.  So in the common
    cases of non-overlapping maps or a small narrow map inserted into a large
    wide one this should traverse only a small portion of the large one, and it
    should do so in one pass.  However, if the small map straddles the large
    one, it will force an unnecessary traversal of the large one.  In that
    case, I'd be better off merging each event individually.
-}
merge :: Events -> Events -> Events
merge (Events evts1) (Events evts2)
    | Map.null evts1 = Events evts2
    | Map.null evts2 = Events evts1
    | otherwise = Events $ from_ascending $ clip_events $
        Seq.merge_on Event.start (Map.elems evts1) (Map.elems evts2)
    -- Previously I would extract the overlapping sections and clip only those,
    -- but I moved that to 'insert'.  Perhaps it's a bit more elegant here, but
    -- I think I'm never really merging large Events, just inserting small
    -- lists into a large EventMap.  And in any case, EventMaps never get very
    -- big.  Also, putting it in 'insert' avoids having to clip_events an extra
    -- time to create the new Events.

{- | Clip overlapping event durations.  If two event positions coincide, the
    last prevails.  An event with duration overlapping another event will be
    clipped.

    The precondition is that the input events are sorted, the postcondition is
    that they are still sorted and no [pos .. pos+dur) ranges will overlap.
-}
clip_events :: [Event.Event] -> [Event.Event]
clip_events = mapMaybe clip . Seq.zip_next
    where
    clip (cur, Nothing) = Just cur
    clip (cur, Just next)
        | Event.start next <= Event.start cur = Nothing
        | Event.start next < Event.end cur =
            Just $ Event.set_duration (Event.start next - Event.start cur) cur
        | otherwise = Just cur

-- | Like 'clip_events', except that negative events will clip their starts
-- instead of their ends.  If a positive duration event is followed by
-- a negative duration event, the duration of the positive one will clip the
-- negative one.
clip_negative_events :: [Event.Event] -> [Event.Event]
clip_negative_events =
    Seq.sort_on Event.start . mapMaybe clip_duration . Seq.zip_neighbors
    where
    -- Why is this so complicated and irregular?
    clip_duration (maybe_prev, cur, maybe_next)
        | Event.is_positive cur = case positive_clip cur maybe_next of
            Nothing -> Just cur
            Just p
                -- If two events have the same start, the second one wins.
                | p <= Event.start cur -> Nothing
                | otherwise ->
                    Just $ Event.set_duration (p - Event.start cur) cur
        | otherwise = case negative_clip maybe_prev cur maybe_next of
            Nothing -> Just cur
            Just p
                | p > Event.end cur -> Nothing
                | otherwise -> Just $ Event.set_start p cur
    -- Get the clip point for a positive event.
    positive_clip cur maybe_next = maybe_next >>= \next ->
        if Event.trigger next < Event.end cur
            then Just (Event.trigger next) else Nothing
    negative_clip maybe_prev cur maybe_next = case (maybe_prev, maybe_next) of
        (_, Just next)
            | Event.is_positive next && Event.start next < Event.end cur ->
                Just (Event.end next)
        (Just prev, _)
            | Event.end prev > Event.start cur -> Just (Event.end prev)
        _ -> Nothing

check_invariants :: Events -> [Text]
check_invariants (Events events) =
    [ "key /= event start: " <> showt (t, event)
    | (t, event) <- Map.toAscList events, t /= Event.start event
    ] ++
    [ "overlapping: " <> showt evt1 <> " and " <> showt evt2
    | (evt1, evt2) <- find_overlaps events
    ]

find_overlaps :: EventMap -> [(Event.Event, Event.Event)]
find_overlaps = mapMaybe check . Seq.zip_next . map snd . Map.toAscList
    where
    check (cur, Just next)
        | Event.end cur > Event.start next = Just (cur, next)
        | otherwise = Nothing
    check _ = Nothing

-- * serialize

instance Serialize.Serialize Events where
    put (Events a) = Serialize.put_version 3 >> Serialize.put a
    get = do
        v <- Serialize.get_version
        case v of
            3 -> do
                events :: Map.Map ScoreTime Event.Event <- Serialize.get
                return $ Events events
            _ -> Serialize.bad_version "Events" v

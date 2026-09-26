-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | The 'Events' type contains the events of a track.

    This is the largest part of the score and also the part most often
    modified, so there is a plethora of access functions.
-}
module Ui.Events (
    -- * range
    Range(..)
    , selectionRange, eventRange
    , rangeTimes, rangeStart, rangeEnd, rangeDuration

    -- * events
    , Events
    , empty, null, length, timeBegin, timeEnd

    -- ** list conversion
    , singleton, fromList
    , ascending, descending

    -- ** transformation
    , mapEvents, move, clip, clipList

    -- ** insert / remove
    , insert, remove
    , merge

    -- ** lookup
    , at, overlapping, head, last

    -- ** split
    -- *** events
    , splitRange, split, splitExclude
    , inRange
    , around
    -- *** List [Event]
    , splitLists
    , atAfter, after, before, atBefore
    , splitAtBefore

#ifdef TESTING
    , module Ui.Events
#endif
) where
import qualified Prelude
import           Prelude hiding (head, last, length, null)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Serialize as Serialize

import qualified Ui.Event as Event
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types

import           Global
import           Types


-- * Range

data Range
    -- | A range between the given points.  It will select a positive event at
    -- the start time, or a negative one at the end time.  Effectively it's
    -- half-open from the start for Positive events, and half-open from the end
    -- for Negative ones.
    --
    -- Start should be <= end.
    = Range !TrackTime !TrackTime
    -- | Select an event at exactly the given time and orientation.
    | Point !TrackTime !Types.Orientation
    deriving (Eq, Show)

instance Pretty Range where
    pretty r = case r of
        Range s e -> pretty s <> "--" <> pretty e
        Point p orient ->
            "@" <> pretty p <> (if orient == Types.Positive then "+" else "-")

selectionRange :: Sel.Selection -> Range
selectionRange sel
    | start == end = Point start $ case Sel.orientation sel of
        Sel.Positive -> Types.Positive
        Sel.Negative -> Types.Negative
        -- The event selection shouldn't be None so this shouldn't happen.
        Sel.None -> Types.Positive
    | otherwise = Range start end
    where (start, end) = Sel.range sel

eventRange :: Event.Event -> Range
eventRange event
    | Event.duration event == 0 =
        Point (Event.start event) (Event.orientation event)
    | otherwise = uncurry Range (Event.range event)

rangeTimes :: Range -> (TrackTime, TrackTime)
rangeTimes (Range s e) = (s, e)
rangeTimes (Point s _) = (s, s)

rangeStart :: Range -> TrackTime
rangeStart = fst . rangeTimes

rangeEnd :: Range -> TrackTime
rangeEnd = snd . rangeTimes

rangeDuration :: Range -> TrackTime
rangeDuration range = end - start
    where (start, end) = rangeTimes range

-- * events

-- TODO Probably the ScoreTimes in here should be TrackTime.  This module dates
-- from before TrackTime existed.

empty :: Events
empty = Events Map.empty

null :: Events -> Bool
null (Events m) = Map.null m

length :: Events -> Int
length = Map.size . get

timeBegin :: Events -> ScoreTime
timeBegin = maybe 0 Event.min . head

timeEnd :: Events -> ScoreTime
timeEnd = maybe 0 Event.max . last

-- ** list conversion

singleton :: Event.Event -> Events
singleton event = Events $ Map.singleton (eventKey event) event

fromList :: [Event.Event] -> Events
fromList evts = insert evts empty

-- | Get all events in ascending order.
ascending :: Events -> [Event.Event]
ascending = toAscList

descending :: Events -> [Event.Event]
descending = toDescList . get

-- ** transformation

-- | Map a function across the events in Events.
mapEvents :: (Event.Event -> Event.Event) -> Events -> Events
mapEvents f = fromList . map f . ascending

-- | Move events by a constant amount.  It's more efficient than 'map_events'
-- because it doesn't have to sort and clip the events.
move :: ScoreTime -> Events -> Events
move delta (Events events) =
    Events . Map.fromAscList . map m . Map.toAscList $ events
    where
    m (Key t orient, event) =
        (Key (t+delta) orient, Event.start_ %= (+delta) $ event)

-- | Clip off the events after the given end time.  Also shorten the last
-- event so it doesn't cross the end, if necessary.
clip :: Bool -> ScoreTime -> Events -> Events
clip allowZero end = fromAscList . clipList allowZero end . toAscList

-- | Like 'clip', but works on a list.
clipList :: Bool -> ScoreTime -> [Event.Event] -> [Event.Event]
clipList _ _ [] = []
clipList allowZero end (event : events)
    | allowZero && Event.start event > end = []
    | Event.isNegative event && Event.start event > end = []
    | Event.isPositive event && Event.start event >= end && not allowZero = []
    | Event.end event > end = [Event.setEnd end event]
    | otherwise = event : clipList allowZero end events

-- ** insert / remove

-- | Merge events into the given Events.  Events that overlap will have
-- their tails clipped until they don't, and given events that start at the
-- same place as existing events will replace the existing ones.
--
-- This should be the the only way to create a 'Events', short of
-- debugging, since it enforces that events don't overlap.
insert :: [Event.Event] -> Events -> Events
insert [] events = events
insert unsortedEvents (Events events) =
    Events $ Map.unions [pre, get overlapping, post]
    where
    newEvents = Lists.sortOn fst $ Lists.keyOn eventKey $
        map Event.round unsortedEvents
    start = Event.min $ snd $ Prelude.head newEvents
    end = Event.max $ snd $ Prelude.last newEvents
    (pre, within, post) = _splitOverlapping start end events
    overlapping = mergeAndClip (Map.toAscList within) newEvents

-- | Remove events in the range.
remove :: Range -> Events -> Events
remove (Point t orient) events = emap (Map.delete (Key t orient)) events
remove range events = emap (`Map.difference` within) events
    where (_, Events within, _) = splitRange range events

-- ** lookup

-- | An event exactly at the given pos, or Nothing.
-- TODO this is just in_range (Point ...), merge them?
at :: ScoreTime -> Types.Orientation -> Events -> Maybe Event.Event
at pos orient = Map.lookup (Key pos orient) . get

-- | Like 'at', but return an event that overlaps the given pos.
overlapping :: ScoreTime -> Events -> Maybe Event.Event
overlapping pos events
    | next : _ <- post, Event.start next == pos || Event.end next < pos =
        Just next
    | prev : _ <- pre, Event.end prev > pos = Just prev
    | otherwise = Nothing
    where (pre, post) = bimap descending ascending $ split pos events

head :: Events -> Maybe Event.Event
head (Events events) = snd <$> Map.lookupMin events

-- | Final event, if there is one.
last :: Events -> Maybe Event.Event
last (Events events) = snd <$> Map.lookupMax events

-- ** split

-- *** events

splitRange :: Range -> Events -> (Events, Events, Events)
splitRange (Point t orient) (Events events) =
    (Events pre, maybe mempty singleton at, Events post)
    where (pre, at, post) = Map.splitLookup (Key t orient) events
splitRange (Range start end) (Events events) =
    (Events pre, Events within, Events post)
    where
    (pre, within, post) =
        Maps.split3 (Key start Types.Positive) (Key end Types.Positive) events

-- | Split at the given time.  A positive event that starts at the given time
-- will appear in the after events, a negative event in the previous events.
split :: ScoreTime -> Events -> (Events, Events)
split pos (Events events) = (Events pre, Events post)
    where (pre, post) = Maps.split2 (Key pos Types.Positive) events

-- | Like 'split', but a positive event that matches exactly is excluded from
-- the result.
splitExclude :: ScoreTime -> Events -> (Events, Events)
splitExclude pos (Events events) =
    (Events (Map.delete (Key pos Types.Negative) pre), Events post)
    where (pre, post) = Map.split (Key pos Types.Positive) events

-- | Like 'split_range', but only return the middle part.
inRange :: Range -> Events -> Events
inRange range events = within
    where (_, within, _) = splitRange range events

-- | Get events in the given range, plus surrounding.  If there is no event at
-- @start@, the previous event will be included.  The event after @end@ is
-- always included.
around :: ScoreTime -> ScoreTime -> Events -> Events
around start end events = Events $ above $ below within
    where
    (Events pre, Events within, Events post) =
        splitRange (Range start end) events
    below m
        | Just (Key lowest _, _) <- Map.lookupMin within, lowest == start = m
        | otherwise = maybe m (\(k, e) -> Map.insert k e m) (Map.lookupMax pre)
    above m = maybe m (\(k, e) -> Map.insert k e m) (Map.lookupMin post)

-- *** List [Event]

splitLists :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
splitLists pos = bimap descending ascending . split pos

-- | Events with start >= @pos@.
atAfter :: ScoreTime -> Events -> [Event.Event]
atAfter pos = snd . splitLists pos

-- | Events with start > @pos@.
after :: ScoreTime -> Events -> [Event.Event]
after pos events = case atAfter pos events of
    next : rest | Event.start next == pos -> rest
    events -> events

-- | Events with start < @pos@.
before :: ScoreTime -> Events -> [Event.Event]
before pos = fst . splitLists pos

-- | Events with start <= @pos@.
atBefore :: ScoreTime -> Events -> [Event.Event]
atBefore pos events
    | next : _ <- post, Event.start next == pos = next : pre
    | otherwise = pre
    where (pre, post) = splitLists pos events

-- | This is like 'split', but if there isn't an event exactly at the pos then
-- put the previous one in the post list.
splitAtBefore :: ScoreTime -> Events -> ([Event.Event], [Event.Event])
splitAtBefore pos events
    | next : _ <- post, Event.start next == pos = (pre, post)
    | before : prepre <- pre = (prepre, before : post)
    | otherwise = (pre, post)
    where (pre, post) = splitLists pos events

-- * implementation

-- | This is the underlying storage for a sequence of events.  The invariant
-- is that events start + duration don't overlap.
--
-- This type should remain abstract, and you should manipulate events using
-- functions in this module.
newtype Events = Events EventMap
    deriving (DeepSeq.NFData, Eq, Show)

type EventMap = Map Key Event.Event

-- | This determines event order, and which events can coexist.
--
-- Technically, since 'Event.start' is in here, it doesn't have to be in
-- 'Event.Event'.  I used to have them separate, but it was a pain to pass
-- (ScoreTime, Event) pairs around everywhere.
data Key = Key !TrackTime !Types.Orientation
    deriving (Eq, Ord, Show)

instance DeepSeq.NFData Key where rnf _ = ()

instance Pretty Key where
    pretty (Key t o) = pretty t <> case o of
        Types.Negative -> "-"
        Types.Positive -> "+"

eventKey :: Event.Event -> Key
eventKey event = Key (Event.start event) (Event.orientation event)

-- | This assumes the input is already sorted!
fromAscList :: [Event.Event] -> Events
fromAscList = Events . Map.fromAscList . Lists.keyOn eventKey

toAscList :: Events -> [Event.Event]
toAscList = map snd . Map.toAscList . get

toDescList :: EventMap -> [Event.Event]
toDescList = map snd . Map.toDescList

instance Pretty Events where
    format = Pretty.format . map event . ascending
        where
        event e = Pretty.text $
            pretty (Event.start e, Event.duration e, Event.text e)

instance Semigroup Events where (<>) = merge
instance Monoid Events where
    mempty = empty
    mappend = (<>)

get :: Events -> EventMap
get (Events evts) = evts

emap :: (EventMap -> EventMap) -> Events -> Events
emap f (Events evts) = Events (f evts)

-- | Put events that overlap the range into within.
_splitOverlapping :: ScoreTime -> ScoreTime -> EventMap
    -> (EventMap, EventMap, EventMap)
_splitOverlapping start end events = (pre2, within3, post2)
    where
    (Events pre, Events within, Events post) =
        splitRange (Range start end) (Events events)
    (pre2, within2) = case Map.lookupMax pre of
        Just (k, e) | Event.overlaps start e ->
            (Map.delete k pre, Map.insert k e within)
        _ -> (pre, within)
    (post2, within3) = case Map.lookupMin post of
        Just (k, e) | Event.overlaps end e ->
            (Map.delete k post, Map.insert k e within2)
        _ -> (post, within2)

{- | Merge @evts2@ into @evts1@.  Events that overlap other events will be
    clipped so they don't overlap.  If events occur simultaneously, the event
    from @evts1@ wins.
-}
merge :: Events -> Events -> Events
merge (Events evts1) (Events evts2)
    | Map.null evts1 = Events evts2
    | Map.null evts2 = Events evts1
    | otherwise =
        mergeAndClip (Map.toAscList evts2) (Map.toAscList evts1)
    -- Previously I would extract the overlapping sections and clip only those,
    -- but I moved that to 'insert'.  Perhaps it's a bit more elegant here, but
    -- I think I'm never really merging large Events, just inserting small
    -- lists into a large EventMap.  And in any case, EventMaps never get very
    -- big.  Also, putting it in 'insert' avoids having to clip_events an extra
    -- time to create the new Events.

mergeAndClip :: [(Key, Event.Event)] -> [(Key, Event.Event)] -> Events
mergeAndClip old new = fromAscList $ clipEvents $ map snd $
    Lists.mergeOn fst (map (first (,False)) old) (map (first (,True)) new)
    -- Lists.mergeOn should put elements from the first argument first, but
    -- it doesn't guarantee it, so let's be explicit.

{- | Clip overlapping event durations.  An event with duration overlapping
    another event will be clipped.  Positive events are clipped by following
    events, and negative ones are clipped by previous ones.  In the event of
    a conflict between positive and negative, they are both clipped to the
    middle of the overlap.  This may seem a bit weird, but it has the nice
    properties that I never clip an event to 0, and never have to drop an
    event due to clipping.  From an implementation point of view, it lets me
    write a single-pass algorithm.

    If there are multiple events with the same start and orientation, the last
    one wins.

    The precondition is that the input events are sorted by 'event_key', the
    postcondition is that they are still sorted and no [pos .. pos+dur) ranges
    will overlap.
-}
clipEvents :: [Event.Event] -> [Event.Event]
clipEvents =
    map clip . Lists.zipNeighbors
        . Lists.dropInitialDups (\e -> (Event.start e, Event.orientation e))
    where
    clip (maybePrev, cur, maybeNext)
        | Event.isNegative cur = case maybePrev of
            Nothing -> cur
            Just prev
                -- prev |--->
                -- cur    <---|
                | Event.isPositive prev -> if Event.end cur < Event.end prev
                    then Event.end_ #= midpoint prev cur $ cur
                    else cur
                -- prev <---|
                -- cur    <---|
                | Event.start prev > Event.end cur ->
                    Event.end_ #= Event.start prev $ cur
                | otherwise -> cur
        | otherwise = case maybeNext of
            Nothing -> cur
            Just next
                -- cur  |--->
                -- next    <---|
                | Event.isNegative next -> if Event.end next < Event.end cur
                    then Event.end_ #= midpoint cur next $ cur
                    else cur
                -- cur  |--->
                -- next    |--->
                | Event.start next < Event.end cur ->
                    Event.end_ #= Event.start next $ cur
                | otherwise -> cur
    midpoint pos neg =
        (max (Event.start pos) (Event.end neg)
            + min (Event.end pos) (Event.start neg))
        / 2

-- * serialize

instance Serialize.Serialize Events where
    put (Events a) = Serialize.putVersion 4 >> Serialize.put a
    get = do
        v <- Serialize.getVersion
        case v of
            3 -> do
                events :: Map ScoreTime Event.Event <- Serialize.get
                pure $ fromAscList $ Map.elems events
            4 -> do
                events :: Map Key Event.Event <- Serialize.get
                pure $ Events events
            _ -> Serialize.badVersion "Events" v

-- Key has no version because there are a lot of them and they're all the same
-- and Events has a version.
instance Serialize.Serialize Key where
    put (Key a b) = Serialize.put a >> Serialize.put b
    get = Key <$> Serialize.get <*> Serialize.get

-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | A Stream is a collection of 'LEvent.LEvent's which is hopefully sorted in
-- time order.
module Derive.Stream (
    Stream
    -- * construct
    , empty, fromLogs, fromEventLogs, fromEvents, fromSortedList
    , fromEvent, fromSortedEvents
    -- * extract
    , toList, writeLogs, partition, logsOf, eventsOf, length
    -- * transform
    , takeWhile, dropWhile, catMaybes
    , sort
    , mergeAscLists
    , mergeLog, mergeLogs
    , leventKey
    -- ** specific transformations
    , first, firstLast
    -- ** zip
    , zip, zipOn, zip3, zip3On, zip4
    -- * misc util
    , prettyShortEvents, shortEvents
) where
import Prelude hiding (length, dropWhile, takeWhile, zip, zip3)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists

import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import Global hiding (first)
import Types


{- | A list seems inefficient, since each call appends to the stream.  A block
    call will then append a bunch of events which need to be copied, and
    recopied as part of the larger chunk for the next block call up.  It's
    possible the head of the stream will also be copied every time something
    is appended to it, but I'm not sure about that.  It could also be that the
    number of events is low enough that all the inefficiency doesnt actually
    matter, but I'm not sure.  I need to understand profiling to tell.

    TODO one possibility is a MergeList:

    data MergeList a = Chunk [a] | Merge (MergeList a) (MergeList a)

    This way I don't need to copy large chunks multiple times.  Also, if I make
    sure there is no data dependency between the merge branches, I can evaluate
    them in parallel.

    Each call generates a chunk [Event], and the chunks are then joined with
    (<>).  This means every cons is copied once, but I think this is hard
    to avoid if I want to merge streams.

    TODO the Functor and Traversable can destroy the order, but this isn't
    checked.  Maybe I shouldn't have them?
-}
-- data Stream a = Stream !Sorted ![LEvent.LEvent a]
--     deriving (Show, Functor, Foldable.Foldable, Traversable.Traversable)

-- | Currently I don't actually track order, and just trust the callers.
newtype Stream a = Stream [LEvent.LEvent a]
    deriving (Show, Functor)

data Sorted = Unsorted | Sorted deriving (Show, Eq)

instance Semigroup Sorted where
    Sorted <> Sorted = Sorted
    _ <> _ = Unsorted
instance Monoid Sorted where
    mempty = Sorted
    mappend = (<>)

emap :: ([LEvent.LEvent a] -> [LEvent.LEvent b]) -> Stream a -> Stream b
emap f = fromSortedList . f . toList

instance Semigroup (Stream Score.Event) where
    s1 <> s2 =
        fromSortedList $ Lists.mergeOn leventKey (toList s1) (toList s2)
    -- Stream s1 e1 <> Stream s2 e2 = case (s1, s2) of
    --     (Sorted, Sorted) -> from_sorted_list $ Lists.mergeOn levent_key e1 e2
    --     _ -> from_list (e1 <> e2)
instance Monoid (Stream Score.Event) where
    mempty = fromSortedList mempty
    mappend = (<>)
    mconcat = fromSortedList . Lists.mergeLists leventKey . map toList
    -- mconcat streams = from_sorted_list groups
    --     where
    --     groups = Lists.mergeLists levent_key
    --         [events | Stream _ events <- map sort streams]

-- To my surprise, GHC will accept an overlapping instance Monoid (Stream a).
-- Maybe it's ok because it's unambiguous, but I still like specific instances.
-- This means calls have to use Stream.empty instead of mempty, but that's not
-- such a big deal.

instance Semigroup (Stream Signal.Control) where
    s1 <> s2 = fromSortedList (toList s1 <> toList s2)

-- | Signal.Control streams don't need sorted order.
instance Monoid (Stream Signal.Control) where
    mempty = empty
    mappend = (<>)

instance Semigroup (Stream PSignal.PSignal) where
    s1 <> s2 = fromSortedList (toList s1 <> toList s2)
instance Monoid (Stream PSignal.PSignal) where
    mempty = empty
    mappend = (<>)

instance DeepSeq.NFData a => DeepSeq.NFData (Stream a) where
    rnf = DeepSeq.rnf . toList

instance Pretty a => Pretty (Stream a) where
    -- format (Stream sorted events) =
    --     Pretty.text (showt sorted) Pretty.<+> Pretty.format events
    format (Stream events) = Pretty.format events

-- * construct

empty :: Stream a
empty = fromSortedList []

fromLogs :: [Log.Msg] -> Stream a
fromLogs = fromSortedList . map LEvent.Log

fromEventLogs :: a -> [Log.Msg] -> Stream a
fromEventLogs e logs = fromSortedList (LEvent.Event e : map LEvent.Log logs)

fromSortedList :: [LEvent.LEvent a] -> Stream a
fromSortedList = Stream -- Sorted

fromList :: [LEvent.LEvent a] -> Stream a
fromList = Stream -- Unsorted

fromEvents :: [a] -> Stream a
fromEvents = fromList . map LEvent.Event

fromEvent :: a -> Stream a
fromEvent = fromSortedList . (:[]) . LEvent.Event

-- | Promise that the stream is really sorted.
fromSortedEvents :: [a] -> Stream a
fromSortedEvents = fromSortedList . map LEvent.Event

-- * extract

toList :: Stream a -> [LEvent.LEvent a]
toList (Stream events) = events

writeLogs :: Log.LogMonad m => Stream a -> m [a]
writeLogs = LEvent.writeLogs . toList

partition :: Stream a -> ([a], [Log.Msg])
partition = LEvent.partition . toList

logsOf :: Stream a -> [Log.Msg]
logsOf = LEvent.logsOf . toList

eventsOf :: Stream a -> [a]
eventsOf = LEvent.eventsOf . toList

length :: Stream a -> Int
length = List.length . toList

-- * transform

takeWhile :: (a -> Bool) -> Stream a -> Stream a
takeWhile = emap . Prelude.takeWhile . LEvent.logOr

dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile = emap . Prelude.dropWhile . LEvent.logOr

-- | 'Data.Maybe.catMaybes' for Stream.
catMaybes :: Stream (Maybe a) -> Stream a
catMaybes = emap go
    where
    go [] = []
    go (x : xs) = case x of
        LEvent.Log log -> LEvent.Log log : go xs
        LEvent.Event (Just e) -> LEvent.Event e : go xs
        LEvent.Event Nothing -> go xs

sort :: Stream Score.Event -> Stream Score.Event
-- sort s@(Stream Sorted _) = s
-- sort (Stream Unsorted events) =
--     from_sorted_list $ Lists.sortOn levent_key events
sort (Stream events) = fromSortedList $ Lists.sortOn leventKey events

-- | Merge sorted lists of events.  If the lists themselves are also sorted,
-- I can produce output without scanning the entire input list, so this should
-- be more efficient for a large input list than (<>).
--
-- This assumes all the streams are sorted.  I could check first, but this
-- would destroy the laziness.  Instead, let it be out of order, and Convert
-- will complain about it.
mergeAscLists :: [Stream Score.Event] -> Stream Score.Event
mergeAscLists streams = fromSortedList $
    Lists.mergeAscLists leventKey (map toList streams)

-- | This will make logs always merge ahead of score events, but that should
-- be ok.
leventKey :: LEvent.LEvent Score.Event -> RealTime
leventKey (LEvent.Log _) = -1/0 -- -Infinity
leventKey (LEvent.Event event) = Score.eventStart event

mergeLog :: Log.Msg -> Stream a -> Stream a
mergeLog log = emap (LEvent.Log log :)

mergeLogs :: [Log.Msg] -> Stream e -> Stream e
mergeLogs logs = emap (map LEvent.Log logs ++)

-- ** specific transformations

-- | Apply to the first Event that matches the predicate.
first :: (a -> Bool) -> (a -> a) -> Stream a -> Stream a
first matches f = fromSortedList . go . toList
    where
    go [] = []
    go (e : es) = case e of
        LEvent.Event e | matches e -> LEvent.Event (f e) : es
        _ -> e : go es

-- | Apply to the first and last Event that matches the predicate.  If there
-- are fewer than 2 such events, do nothing.
firstLast :: (a -> Bool) -> (a -> a) -> (a -> a) -> Stream a -> Stream a
firstLast matches start end = fromSortedList . atStart . toList
    where
    atStart [] = []
    atStart (e : es) = case e of
        LEvent.Event e | matches e ->
            if any (LEvent.either matches (const False)) es
                then LEvent.Event (start e) : atEnd es
                else LEvent.Event e : es
        _ -> e : atStart es
    atEnd [] = []
    atEnd (e : es) = case e of
        LEvent.Event e | matches e
                && null (Prelude.dropWhile (LEvent.logOr (not . matches)) es) ->
            LEvent.Event (end e) : es
        _ -> e : atEnd es

-- ** zip

zip :: [a] -> Stream x -> Stream (a, x)
zip as = emap $ LEvent.zip as

zipOn :: ([a] -> [b]) -> Stream a -> Stream (b, a)
zipOn key = emap $ \xs -> LEvent.zip (key (LEvent.eventsOf xs)) xs

zip3 :: [a] -> [b] -> Stream x -> Stream (a, b, x)
zip3 as bs = emap $ LEvent.zip3 as bs

zip3On :: ([a] -> [b]) -> ([a] -> [c]) -> Stream a -> Stream (b, c, a)
zip3On key1 key2 = emap $ \xs ->
    LEvent.zip3 (key1 (LEvent.eventsOf xs)) (key2 (LEvent.eventsOf xs)) xs

zip4 :: [a] -> [b] -> [c] -> Stream x -> Stream (a, b, c, x)
zip4 as bs cs = emap $ LEvent.zip4 as bs cs

-- * misc util

-- | Like 'Score.short_events', but works on a Stream.
prettyShortEvents :: Stream Score.Event -> Text
prettyShortEvents = pretty . Pretty.formattedList '[' ']' . shortEvents

shortEvents :: Stream Score.Event -> [Text]
shortEvents = map (LEvent.either Score.shortEvent pretty) . toList

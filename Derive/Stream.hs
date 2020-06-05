-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor #-}
-- | A Stream is a collection of 'LEvent.LEvent's which is hopefully sorted in
-- time order.
module Derive.Stream (
    Stream
    -- * construct
    , empty, from_logs, from_event_logs, from_events, from_sorted_list
    , from_event, from_sorted_events
    -- * extract
    , to_list, write_logs, partition, logs_of, events_of, length
    -- * transform
    , take_while, drop_while, cat_maybes
    , sort
    , merge_asc_lists
    , merge_log, merge_logs
    , levent_key
    -- ** specific transformations
    , first, first_last
    -- ** zip
    , zip, zip_on, zip3, zip3_on, zip4
    -- * misc util
    , pretty_short_events, short_events
) where
import Prelude hiding (length, zip, zip3)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

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
emap f = from_sorted_list . f . to_list

instance Semigroup (Stream Score.Event) where
    s1 <> s2 =
        from_sorted_list $ Seq.merge_on levent_key (to_list s1) (to_list s2)
    -- Stream s1 e1 <> Stream s2 e2 = case (s1, s2) of
    --     (Sorted, Sorted) -> from_sorted_list $ Seq.merge_on levent_key e1 e2
    --     _ -> from_list (e1 <> e2)
instance Monoid (Stream Score.Event) where
    mempty = from_sorted_list mempty
    mappend = (<>)
    mconcat = from_sorted_list . Seq.merge_lists levent_key . map to_list
    -- mconcat streams = from_sorted_list groups
    --     where
    --     groups = Seq.merge_lists levent_key
    --         [events | Stream _ events <- map sort streams]

-- To my surprise, GHC will accept an overlapping instance Monoid (Stream a).
-- Maybe it's ok because it's unambiguous, but I still like specific instances.
-- This means calls have to use Stream.empty instead of mempty, but that's not
-- such a big deal.

instance Semigroup (Stream Signal.Control) where
    s1 <> s2 = from_sorted_list (to_list s1 <> to_list s2)

-- | Signal.Control streams don't need sorted order.
instance Monoid (Stream Signal.Control) where
    mempty = empty
    mappend = (<>)

instance Semigroup (Stream PSignal.PSignal) where
    s1 <> s2 = from_sorted_list (to_list s1 <> to_list s2)
instance Monoid (Stream PSignal.PSignal) where
    mempty = empty
    mappend = (<>)

instance DeepSeq.NFData a => DeepSeq.NFData (Stream a) where
    rnf = DeepSeq.rnf . to_list

instance Pretty a => Pretty (Stream a) where
    -- format (Stream sorted events) =
    --     Pretty.text (showt sorted) Pretty.<+> Pretty.format events
    format (Stream events) = Pretty.format events

-- * construct

empty :: Stream a
empty = from_sorted_list []

from_logs :: [Log.Msg] -> Stream a
from_logs = from_sorted_list . map LEvent.Log

from_event_logs :: a -> [Log.Msg] -> Stream a
from_event_logs e logs = from_sorted_list (LEvent.Event e : map LEvent.Log logs)

from_sorted_list :: [LEvent.LEvent a] -> Stream a
from_sorted_list = Stream -- Sorted

from_list :: [LEvent.LEvent a] -> Stream a
from_list = Stream -- Unsorted

from_events :: [a] -> Stream a
from_events = from_list . map LEvent.Event

from_event :: a -> Stream a
from_event = from_sorted_list . (:[]) . LEvent.Event

-- | Promise that the stream is really sorted.
from_sorted_events :: [a] -> Stream a
from_sorted_events = from_sorted_list . map LEvent.Event

-- * extract

to_list :: Stream a -> [LEvent.LEvent a]
to_list (Stream events) = events

write_logs :: Log.LogMonad m => Stream a -> m [a]
write_logs = LEvent.write_logs . to_list

partition :: Stream a -> ([a], [Log.Msg])
partition = LEvent.partition . to_list

logs_of :: Stream a -> [Log.Msg]
logs_of = LEvent.logs_of . to_list

events_of :: Stream a -> [a]
events_of = LEvent.events_of . to_list

length :: Stream a -> Int
length = List.length . to_list

-- * transform

take_while :: (a -> Bool) -> Stream a -> Stream a
take_while = emap . takeWhile . LEvent.log_or

drop_while :: (a -> Bool) -> Stream a -> Stream a
drop_while = emap . dropWhile . LEvent.log_or

-- | 'Data.Maybe.catMaybes' for Stream.
cat_maybes :: Stream (Maybe a) -> Stream a
cat_maybes = emap go
    where
    go [] = []
    go (x : xs) = case x of
        LEvent.Log log -> LEvent.Log log : go xs
        LEvent.Event (Just e) -> LEvent.Event e : go xs
        LEvent.Event Nothing -> go xs

sort :: Stream Score.Event -> Stream Score.Event
-- sort s@(Stream Sorted _) = s
-- sort (Stream Unsorted events) =
--     from_sorted_list $ Seq.sort_on levent_key events
sort (Stream events) = from_sorted_list $ Seq.sort_on levent_key events

-- | Merge sorted lists of events.  If the lists themselves are also sorted,
-- I can produce output without scanning the entire input list, so this should
-- be more efficient for a large input list than 'merge'.
--
-- This assumes all the streams are sorted.  I could check first, but this
-- would destroy the laziness.  Instead, let it be out of order, and Convert
-- will complain about it.
merge_asc_lists :: [Stream Score.Event] -> Stream Score.Event
merge_asc_lists streams = from_sorted_list $
    Seq.merge_asc_lists levent_key (map to_list streams)

-- | This will make logs always merge ahead of score events, but that should
-- be ok.
levent_key :: LEvent.LEvent Score.Event -> RealTime
levent_key (LEvent.Log _) = -1/0 -- -Infinity
levent_key (LEvent.Event event) = Score.event_start event

merge_log :: Log.Msg -> Stream a -> Stream a
merge_log log = emap (LEvent.Log log :)

merge_logs :: [Log.Msg] -> Stream e -> Stream e
merge_logs logs = emap (map LEvent.Log logs ++)

-- ** specific transformations

-- | Apply to the first Event that matches the predicate.
first :: (a -> Bool) -> (a -> a) -> Stream a -> Stream a
first matches f = from_sorted_list . go . to_list
    where
    go [] = []
    go (e : es) = case e of
        LEvent.Event e | matches e -> LEvent.Event (f e) : es
        _ -> e : go es

-- | Apply to the first and last Event that matches the predicate.  If there
-- are fewer than 2 such events, do nothing.
first_last :: (a -> Bool) -> (a -> a) -> (a -> a) -> Stream a -> Stream a
first_last matches start end = from_sorted_list . at_start . to_list
    where
    at_start [] = []
    at_start (e : es) = case e of
        LEvent.Event e | matches e ->
            if any (LEvent.either matches (const False)) es
                then LEvent.Event (start e) : at_end es
                else LEvent.Event e : es
        _ -> e : at_start es
    at_end [] = []
    at_end (e : es) = case e of
        LEvent.Event e | matches e
                && null (dropWhile (LEvent.log_or (not . matches)) es) ->
            LEvent.Event (end e) : es
        _ -> e : at_end es

-- ** zip

zip :: [a] -> Stream x -> Stream (a, x)
zip as = emap $ LEvent.zip as

zip_on :: ([a] -> [b]) -> Stream a -> Stream (b, a)
zip_on key = emap $ \xs -> LEvent.zip (key (LEvent.events_of xs)) xs

zip3 :: [a] -> [b] -> Stream x -> Stream (a, b, x)
zip3 as bs = emap $ LEvent.zip3 as bs

zip3_on :: ([a] -> [b]) -> ([a] -> [c]) -> Stream a -> Stream (b, c, a)
zip3_on key1 key2 = emap $ \xs ->
    LEvent.zip3 (key1 (LEvent.events_of xs)) (key2 (LEvent.events_of xs)) xs

zip4 :: [a] -> [b] -> [c] -> Stream x -> Stream (a, b, c, x)
zip4 as bs cs = emap $ LEvent.zip4 as bs cs

-- * misc util

-- | Like 'Score.short_events', but works on a Stream.
pretty_short_events :: Stream Score.Event -> Text
pretty_short_events = pretty . Pretty.formattedList '[' ']' . short_events

short_events :: Stream Score.Event -> [Text]
short_events = map (LEvent.either Score.short_event pretty) . to_list

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Events_test where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import Global
import Types


test_split_range = do
    let f start end evts = e_ranges $ Events.split_range start end
            (from_list [(p, d, showt p) | (p, d) <- evts])
    equal (f 1 2 [(0, 1), (1, 1), (2, 1), (3, 1)]) ([0], [1], [2, 3])
    equal (f 1 2 [(0, 1), (1, 0.5), (2, -0.5), (3, 1)]) ([0], [1, 1.5], [3])
    equal (f 1 2 [(0, 0), (1, 0), (2, 0), (3, 0)]) ([0], [1], [2, 3])

    -- A point selection divides into before and after, even for negative
    -- events.
    equal (f 1 1 [(1, 1)]) ([], [], [1])
    equal (f 1 1 [(1, -1)]) ([0], [], [])

test_split_range_or_point = do
    let f start evts = e_ranges $ Events.split_range_or_point start start
            (from_list [(p, d, showt p) | (p, d) <- evts])
    equal (f 1 [(0, 1), (2, 1), (4, 1)]) ([0], [], [2, 4])
    equal (f 2 [(0, 1), (2, 1), (4, 1)]) ([0], [2], [4])
    equal (f 1 [(0, 1), (3, -1), (4, 1)]) ([0], [], [2, 4])
    equal (f 2 [(0, 1), (3, -1), (4, 1)]) ([0], [2], [4])

e_ranges :: (Events.Events, Events.Events, Events.Events)
    -> ([TrackTime], [TrackTime], [TrackTime])
e_ranges (pre, within, post) =
    ( map Event.start (Events.descending pre)
    , map Event.start (Events.ascending within)
    , map Event.start (Events.ascending post)
    )

test_split_at_before = do
    let f pos = extract $ Events.split_at_before pos e1
        e1 = from_list [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
        extract = map Event.start *** map Event.start
    equal (f 0) ([], [0, 1, 2])
    equal (f 0.5) ([], [0, 1, 2])
    equal (f 1) ([0], [1, 2])
    equal (f 1.5) ([0], [1, 2])

test_from_list = do
    let f = extract . from_list
    equal (f [(0, 1, "a"), (2, 1, "c"), (1, 1, "b")])
        [(0, 1, "a"), (1, 1, "b"), (2, 1, "c")]

test_clip = do
    let f include_end at = map extract_event $
            Events.clip include_end at (pos_events [(0, 2, "a"), (2, 2, "b")])
    equal (map (f False) (Seq.range 0 5 1))
        [ []
        , [(0, 1, "a")]
        , [(0, 2, "a")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]
    equal (map (f True) (Seq.range 0 5 1))
        [ [(0, 0, "a")]
        , [(0, 1, "a")]
        , [(0, 2, "a"), (2, 0, "b")]
        , [(0, 2, "a"), (2, 1, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        , [(0, 2, "a"), (2, 2, "b")]
        ]

test_insert = do
    let f evts0 evts1 = extract $
            Events.insert (pos_events evts0) (from_list evts1)
    equal (f [(0, 1, "a0")] [(3, 1, "b0")])
        [(0, 1, "a0"), (3, 1, "b0")]
    equal (f [(0, 4, "a0"), (4, 4, "a1")] [(2, 4, "b0"), (6, 4, "b1")])
        [(0, 2, "a0"), (2, 2, "b0"), (4, 2, "a1"), (6, 4, "b1")]
    -- Inserting overlapping events are clipped.
    equal (f [(0, 4, "a0"), (2, 4, "a1")] []) [(0, 2, "a0"), (2, 4, "a1")]
    -- If the start is coincident, the existing events are replaced.
    equal (f [(0, 1, "a"), (2, 1, "b")] [(0, 0, "1"), (2, 0, "2")])
        [(0, 1, "a"), (2, 1, "b")]
    equal (f [(1, 0, "1"), (1.25, 0, "1.25"), (2, 0, "2")]
            [(0, 0, "0"), (0.25, 0, "0.25")])
        [(0, 0, "0"), (0.25, 0, "0.25"), (1, 0, "1"), (1.25, 0, "1.25"),
            (2, 0, "2")]

-- TODO unimplemented.  I should do this with quickcheck.
-- Not only to generate the events but also to report the failing input.
test_properties = do
    let checks = []
    forM_ checks $ \(events, inserts) -> do
        let result = Events.insert (pos_events inserts) (from_list events)
        equal [(k, e) | (k, e) <- Map.toList (Events.get result),
            k /= Event.start e] []
        equal (Events.find_overlaps result) []
        -- no events created or destroyed, except where start==start
        let start (s, _, _) = s
        equal (Events.length result)
            (length (Seq.unique_on start (events ++ inserts)))

test_clip_events = do
    let f = map extract_event .  Events.clip_events . pos_events
    equal (f [(0, 1, "a"), (0, 2, "b")]) [(0, 2, "b")]
    equal (f [(0, 1, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal (f [(0, 2, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal (f [(0, 1, "a"), (0, 2, "b")]) [(0, 2, "b")]
    equal (f [(0, 2, "a"), (0, 1, "b")]) [(0, 1, "b")]
    equal (f [(0, 0, "a"), (1, 0, "b")]) [(0, 0, "a"), (1, 0, "b")]

test_remove = do
    let events1 = from_list [(0, 0, "0"), (16, 1, "16")]
    -- able to remove 0 dur events
    equal (extract $ Events.remove_event 0 events1) [(16, 1, "16")]

    let f = Events.remove
    -- doesn't include end of range
    equal (extract $ f 0 16 events1) [(16, 1, "16")]
    -- get it all
    equal (extract $ f 0 17 events1) []
    -- missed entirely
    equal (extract $ f 4 10 events1) [(0, 0, "0"), (16, 1, "16")]

test_round_events = do
    let move events =
            Events.insert [next] $ Events.remove_event (Event.start e) events
            where
            e = Maybe.fromJust $ Events.head events
            next = Event.move (+ 1/3) e
    -- If round_event is doing its thing, this won't fall victim to
    -- imprecision.
    equal (to_list $ iterate move (from_list [(0, 1, "a")]) !! (20 * 3))
        [(20, 1, "a")]


-- * util

type Event = (ScoreTime, ScoreTime, Text)

from_list :: [Event] -> Events.Events
from_list = Events.from_list . pos_events

to_list :: Events.Events -> [Event]
to_list = map (\e -> (Event.start e, Event.duration e, Event.text e))
    . Events.ascending

pos_events :: [Event] -> [Event.Event]
pos_events = map (\(pos, dur, text) -> Event.event pos dur text)

extract :: Events.Events -> [(TrackTime, TrackTime, Text)]
extract = map extract_event . Events.ascending

extract_event :: Event.Event -> (TrackTime, TrackTime, Text)
extract_event e = (s, d, Event.text e)
    where (s, d) = Event.orientation_as_duration e

no_overlaps :: Events.Events -> IO Bool
no_overlaps = check . not . events_overlap

events_overlap :: Events.Events -> Bool
events_overlap events = any (uncurry overlaps)
    (zip (Events.ascending events) (drop 1 (Events.ascending events)))

overlaps :: Event.Event -> Event.Event -> Bool
overlaps evt1 evt2 =
    -- They don't overlap and they aren't simultaneous (the second condition is
    -- needed for zero duration events).
    Event.end evt1 > Event.start evt2 || Event.start evt1 >= Event.start evt2

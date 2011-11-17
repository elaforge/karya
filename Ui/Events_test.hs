module Ui.Events_test where
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import Types


-- TODO improve tests

test_split_range = do
    let extract (a, b, c) = (map fst (Events.descending a),
            map fst (Events.ascending b), map fst (Events.ascending c))
    let f start end evts = extract $ Events.split_range start end
            (from_list [(p, d, show p) | (p, d) <- evts])
    equal (f 1 2 [(0, 1), (1, 1), (2, 1), (3, 1)])
        ([0], [1], [2, 3])
    equal (f 1 2 [(0, 0.5), (1, -0.5), (2, 1), (3, 1)])
        ([1, 0], [], [2, 3])
    equal (f 1 2 [(0, 1), (1, 0.5), (2, -0.5), (3, 1)])
        ([0], [1, 2], [3])
    equal (f 1 1 [(1, 1)])
        ([], [], [1])
    equal (f 1 1 [(1, -1)])
        ([1], [], [])
    -- 0 dur events included at beginning, -0 included at end
    equal (f 1 2 [(0, 0), (1, 0), (2, 0), (3, 0)])
        ([0], [1], [2, 3])
    equal (f 1 2 [(0, -0), (1, -0), (2, -0), (3, -0)])
        ([1, 0], [2], [3])


test_split_at_before = do
    let e1 = from_list [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
    let f pos = let (pre, post) = Events.split_at_before pos e1
            in (map fst pre, map fst post)
    equal (f 0) ([], [0, 1, 2])
    equal (f 0.5) ([], [0, 1, 2])
    equal (f 1) ([0], [1, 2])
    equal (f 1.5) ([0], [1, 2])

test_insert_events = do
    let f evts0 evts1 = extract $
            Events.insert_events (pos_events evts0) (from_list evts1)

    equal (f [(0, 1, "a0")] [(3, 1, "b0")])
        [(0, 1, "a0"), (3, 1, "b0")]
    equal (f [(0, 4, "a0"), (4, 4, "a1")] [(2, 4, "b0"), (6, 4, "b1")])
        [(0, 2, "a0"), (2, 2, "b0"), (4, 2, "a1"), (6, 4, "b1")]
    -- Inserting overlapping events are clipped.
    equal (f [(0, 4, "a0"), (2, 4, "a1")] []) [(0, 2, "a0"), (2, 4, "a1")]
    -- If the start is coincident, the existing events are replaced.
    equal (f [(0, 1, "a"), (2, 1, "b")] [(0, 0, "1"), (2, 0, "2")])
        [(0, 1, "a"), (2, 1, "b")]

    -- this is really a place for quickcheck
    equal (f [(1, 0, "1"), (1.25, 0, "1.25"), (2, 0, "2")]
            [(0, 0, "0"), (0.25, 0, "0.25")])
        [(0, 0, "0"), (0.25, 0, "0.25"), (1, 0, "1"), (1.25, 0, "1.25"),
            (2, 0, "2")]

test_insert_negative_events = do
    let f evts0 evts1 = extract $
            Events.insert_events (pos_events evts0) (from_list evts1)
    equal (f
        [(1, -1, "a0")] [(2, -0.5, "b0")]) [(1, -1, "a0"), (2, -0.5, "b0")]
    equal (f [(1, -1, "a0")] [(2, -2, "b0")]) [(1, -1, "a0"), (2, -1, "b0")]
    equal (f [(0, 2, "a0"), (2, 2, "a1")] [(1, -1, "b0")])
        [(0, 1, "a0"), (1, 0, "b0"), (2, 2, "a1")]
    equal (f [(1.5, -0.5, "a0")] [(1, 0.5, "b0"), (2, 1, "b1")])
        [(1, 0.5, "b0"), (1.5, 0, "a0"), (2, 1, "b1")]
    equal (f [(1.25, 0.25, "a0")] [(2, -1, "b0")])
        [(1.25, 0.25, "a0"), (2, -0.5, "b0")]

test_clip_events = do
    let f = map extract_event .  Events.clip_events . pos_events
    equal (f [(0, 1, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal (f [(0, 2, "a"), (1, 1, "b")]) [(0, 1, "a"), (1, 1, "b")]
    equal (f [(1, -1, "a"), (2, -1, "b")]) [(1, -1, "a"), (2, -1, "b")]
    equal (f [(1, -1, "a"), (2, -2, "b")]) [(1, -1, "a"), (2, -1, "b")]
    equal (f [(0, 2, "a"), (1, -1, "b")]) [(0, 1, "a"), (1, 0, "b")]
    equal (f [(0, 1, "a"), (0, 2, "b")]) [(0, 2, "b")]

    equal (f [(0, 0.5, "a0"), (1, -5, "b0")]) [(0, 0.5, "a0"), (1, -0.5, "b0")]

test_remove_events = do
    let te1 = from_list [(0, 0, "0"), (16, 1, "16")]
    -- able to remove 0 dur events
    equal (extract $ Events.remove_event 0 te1) [(16, 1, "16")]

    let f = Events.remove_events
    -- doesn't include end of range
    equal (extract $ f 0 16 te1) [(16, 1, "16")]
    -- get it all
    equal (extract $ f 0 17 te1) []
    -- missed entirely
    equal (extract $ f 4 10 te1) [(0, 0, "0"), (16, 1, "16")]


-- * util

type Event = (ScoreTime, ScoreTime, String)

from_list :: [Event] -> Events.Events
from_list = Events.from_list . pos_events

pos_events :: [Event] -> [Events.PosEvent]
pos_events = map (\(pos, dur, text) -> (pos, Event.event text dur))

extract_text (_, event) = Event.event_string event
extract_event (pos, evt) =
    (pos, Event.event_duration evt, Event.event_string evt)
extract = map extract_event . Events.ascending

no_overlaps = check . not . events_overlap
events_overlap track = any (uncurry overlaps)
    (zip (Events.ascending track) (drop 1 (Events.ascending track)))

overlaps evt1 evt2 =
    -- They don't overlap and they aren't simultaneous (the second condition is
    -- needed for zero duration events).
    Events.end evt1 > fst evt2 || fst evt1 >= fst evt2

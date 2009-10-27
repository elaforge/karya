module Ui.Track_test where
import Util.Test

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track

-- TODO improve tests


test_events_at_before = do
    let e1 = track_events [(0, 1, "0"), (1, 1, "1"), (2, 1, "2")]
    let f pos = let (pre, post) = Track.events_at_before pos e1
            in (map extract_text pre, map extract_text post)
    equal (f 0) ([], ["0", "1", "2"])
    equal (f 0.5) ([], ["0", "1", "2"])
    equal (f 1) (["0"], ["1", "2"])
    equal (f 1.5) (["0"], ["1", "2"])

test_insert_events = do
    let f evts0 evts1 = extract $
            Track.insert_events (mkevents evts0) (track_events evts1)

    equal (f [(0, 1, "a0")] [(3, 1, "b0")])
        [(0, 1, "a0"), (3, 1, "b0")]
    equal (f [(0, 4, "a0"), (4, 4, "a1")] [(2, 4, "b0"), (6, 4, "b1")])
        [(0, 2, "a0"), (2, 2, "b0"), (4, 2, "a1"), (6, 4, "b1")]
    -- Inserting overlapping events are clipped.
    equal (f [(0, 4, "a0"), (2, 4, "a1")] []) [(0, 2, "a0"), (2, 4, "a1")]
    -- Negative durations are clipped to 0.
    equal (f [(0, -4, "a0")] []) [(0, 0, "a0")]

    -- If the start is coincident, the existing events are replaced.
    equal (f [(0, 1, "a"), (2, 1, "b")] [(0, 0, "1"), (2, 0, "2")])
        [(0, 1, "a"), (2, 1, "b")]

    -- this is really a place for quickcheck
    equal (f [(1, 0, "1"), (1.25, 0, "1.25"), (2, 0, "2")]
            [(0, 0, "0"), (0.25, 0, "0.25")])
        [(0, 0, "0"), (0.25, 0, "0.25"), (1, 0, "1"), (1.25, 0, "1.25"),
            (2, 0, "2")]

test_remove_events = do
    let te1 = track_events [(0, 0, "0"), (16, 0, "16")]
    -- able to remove 0 dur events
    equal (extract $ Track.remove_event (TrackPos 0) te1)
        [(16, 0, "16")]
    equal (extract $ Track.remove_event (TrackPos 16) te1)
        [(0, 0, "0")]

    -- doesn't include end of range
    equal (extract $ Track.remove_events (TrackPos 0) (TrackPos 16) te1)
        [(16, 0, "16")]
    -- get it all
    equal (extract $ Track.remove_events (TrackPos 0) (TrackPos 17) te1)
        []
    -- missed entirely
    equal (extract $ Track.remove_events (TrackPos 4) (TrackPos 10) te1)
        [(0, 0, "0"), (16, 0, "16")]


-- * util

track_events = Track.make_track_events . mkevents
mkevents =
    map (\(pos, dur, text) -> (TrackPos pos, Event.event text dur))

extract_text (_, event) = Event.event_string event
extract_event (pos, evt) =
    (pos, Event.event_duration evt, Event.event_string evt)
extract = map extract_event . Track.event_list

no_overlaps = check . not . events_overlap
events_overlap track = any (uncurry overlaps)
    (zip (Track.event_list track) (drop 1 (Track.event_list track)))

overlaps evt1 evt2 =
    -- They don't overlap and they aren't simultaneous (the second condition is
    -- needed for zero duration events).
    Track.event_end evt1 > fst evt2 || fst evt1 >= fst evt2

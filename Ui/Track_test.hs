module Ui.Track_test where
import qualified Data.Map as Map

import Util.Test

import Ui.Types
import qualified Ui.Event as Event
import qualified Ui.Track as Track
import qualified Ui.TestSetup as TestSetup

-- TODO improve tests

test_merge0 = do
    -- 0 dur events
    let te1 = merge Track.empty_events [(0, "0", 0), (16, "16", 0)]
    equal (extract te1) [(0, "0", 0), (16, "16", 0)]
    no_overlaps te1

    let te2 = merge te1 [(0, "0b", 0), (16, "16b", 0)]
    print (extract te2)
    equal (extract te2) [(0, "0b", 0), (16, "16b", 0)]
    print (merge_info te1 te2)

em1 = Track.event_map (merge Track.empty_events [(0, "0", 0), (16, "16", 0)])
em2 = Track.event_map
    (merge (Track.TrackEvents em1) [(0, "0b", 0), (16, "16b", 0)])

tm = Map.fromList [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]
t1 = Track.merge_range 2 3 tm

test_merge = do
    let te1 = merge Track.empty_events [(0, "0", 8), (16, "16", 8)]
    let te2 = merge te1 [(4, "0", 8), (20, "16", 8)]
    no_overlaps te2
    print (extract te2)
    print (merge_info te1 te2)

no_overlaps = check . not . events_overlap
events_overlap track = any (uncurry overlaps)
    (zip (Track.event_list track) (drop 1 (Track.event_list track)))

overlaps evt1 evt2 =
    -- They don't overlap and they aren't simultaneous (the second condition is
    -- needed for zero duration events).
    Track.event_end evt1 > fst evt2 || fst evt1 >= fst evt2

merge old_events evts = Track.insert_events (events evts) old_events

merge_info (Track.TrackEvents evts1) (Track.TrackEvents evts2) =
    ( (first_pos, last_pos)
    , (extractm (relevant evts1), extractm (relevant evts2))
    )
    where
    first_pos = max (fst (Map.findMin evts1)) (fst (Map.findMin evts2))
    last_pos = min (Track.event_end (Map.findMax evts2))
        (Track.event_end (Map.findMax evts2))
    relevant = Track.merge_range first_pos last_pos

extract (Track.TrackEvents fm) = extractm fm
extractm event_map = [(pos, Event.event_text evt, dur)
    | (TrackPos pos, evt@(Event.Event { Event.event_duration = TrackPos dur }))
        <- Map.toAscList event_map]



events =
    map (\(pos, text, dur) -> (TrackPos pos, TestSetup.event text dur))

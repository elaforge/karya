module Perform.Midi.Cache_test where
import qualified Data.Map as Map

import qualified Util.Ranges as Ranges
import Util.Test
import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Perform_test as Perform_test
import qualified Perform.Midi.Cache as Cache
import qualified Perform.Midi.Control as Control


test_cached_performance = do
    let sz = Timestamp.to_real_time Cache.cache_chunk_size
        events1 = [(0, 0.5), (sz, 0.5), (sz+1, 0.5), (sz*2, 0.5)]
        events2 = [(0, 0.5), (sz, 0.8), (sz+1, 0.5), (sz*2, 0.5)]

    -- Put the changed event in the middle of a chunk because otherwise the
    -- states definitely won't line up due to "last note" type state.
    equal (compare_cached events1 events2 [(sz, sz+10)])
        ([], [])

    -- The failure to change the note indicates that the cache is used when
    -- there is no damage.
    equal (compare_cached events1 events2 [])
        ([ (Left (4800, Midi.NoteOff 42 100))
        , (Right (4500, Midi.NoteOff 42 100))
        ], [])

    -- adding events to the end works, and skip a chunk
    equal (compare_cached [(0, 1)] [(0, 1), (sz*2, 1)] [(sz*2, sz*2+1)])
        ([], [])

    -- Deleting events works.  Splice fails on perform because there is now
    -- no addr allocation since I deleted the only note.
    equal (compare_cached [(0, 1), (sz, 1)] [(sz, 1)] [(0, 1)])
        ([], ["Perform cache: splice failed because of perform"])

    -- Splice succeeds this time because there is still a note in the previous
    -- chunk that sets up addr allocation.
    equal (compare_cached [(0, 1), (1, 1), (sz, 1)] [(1, 1), (sz, 1)] [(0, 1)])
        ([], [])

    -- This time the remaining event will establish the proper state before
    -- the next chunk.
    equal (compare_cached [(0, 1), (1, 1), (sz, 1)] [(1, 1), (sz, 1)] [(0, 1)])
        ([], [])

test_lazy_performance = do
    let inf = [(n * 0.5, 0.25) | n <- [0..]]
    let uncached = perform_uncached (mkevents inf)
    -- Make sure this only forces what performance is necessary.

    equal (take 3 (extract uncached))
        [ (0, Midi.PitchBend 0)
        , (0, Midi.NoteOn 42 100)
        , (250, Midi.NoteOff 42 100)
        ]

    -- Modifying an infinite sequence will reperform the modified parts,
    -- and splice back into the rest.
    let inf2 = (0, 0.4) : drop 1 inf
    let cached = Cache.perform uncached (mkdamage [(0, 0.25)]) (mkevents inf2)
    equal (take 3 (extract cached))
        [ (0, Midi.PitchBend 0)
        , (0, Midi.NoteOn 42 100)
        , (400, Midi.NoteOff 42 100)
        ]
    -- No warns means the splice worked.
    let logs = map DeriveTest.show_log $ LEvent.logs_of $ take 200 $
            Cache.cache_messages cached
    equal logs []

test_messages_from = do
    -- If the dur is always increasing then events don't all look the same.
    let cache = perform_uncached (mkevents [(n, (n+1)/16) | n <- [0..]])
        f ts = extract_msgs (Cache.messages_from ts cache)
    -- Extra pitch bend msgs are from the initialization.
    let pb0 = (0, Midi.PitchBend 0)
    equal (take 4 (f 0))
        [ pb0
        , (0, Midi.NoteOn 42 100)
        , (62, Midi.NoteOff 42 100)
        , (1000, Midi.NoteOn 42 100)
        ]
    -- Timestamps subtracted from events.
    equal (take 4 (f 60))
        [ pb0
        , (2, Midi.NoteOff 42 100)
        , (940, Midi.NoteOn 42 100)
        , (1065, Midi.NoteOff 42 100)
        ]
    -- Halfway into another chunk.
    equal (take 4 (f 6000))
        [ pb0
        , (0, Midi.NoteOn 42 100)
        , (438, Midi.NoteOff 42 100)
        , (1000, Midi.NoteOn 42 100)
        ]

test_state_initialization = do
    let events =
            [ set_control [(0, 0.5)] (mkevent (0, 1, 10))
            , set_control [(0, 0.5), (13, 0.75)] (mkevent (12, 1, 20))
            , set_control [(0, 0.5), (13, 0.75)] (mkevent (13, 1, 30))
            ]
    let cache = perform_uncached (map LEvent.Event events)
    -- pprint $ map (Perform.state_postproc . Cache.chunk_state)
    --     (Cache.cache_chunks cache))
    -- pprint $ map (Cache.chunk_messages) (Cache.cache_chunks cache)

    let f ts = extract_msgs (Cache.messages_from ts cache)
    -- Even though the cc was already set in a previous chunk, it should be set
    -- again explicitly if I skipped that chunk.
    check $ (0, Midi.ControlChange 1 63) `elem` f 12000
    -- And even if I am skipping notes.
    check $ (0, Midi.ControlChange 1 63) `elem` f 13000
    check $ (0, Midi.NoteOn 30 100) `elem` f 13000

set_control sig event = event { Perform.event_controls =
    Map.insert cont (Signal.signal sig) (Perform.event_controls event) }
    where cont = Control.Control "cc1"


type Events = [(RealTime, RealTime)]

compare_cached :: Events -> Events -> [(RealTime, RealTime)]
    -> ([Either Message Message], [String])
compare_cached initial modified damage =
    (diff_msgs uncached cached, extract_logs cached)
    where (_, cached, uncached) = perform_both initial modified damage

perform_both :: Events -> Events -> [(RealTime, RealTime)]
    -> (Cache.Cache, Cache.Cache, Cache.Cache)
perform_both initial modified damage = (initial_cache, cached, uncached)
    where
    initial_cache = perform_uncached (mkevents initial)
    cached = Cache.perform initial_cache (mkdamage damage) (mkevents modified)
    uncached = perform_uncached (mkevents modified)

perform_uncached :: [LEvent.LEvent Perform.Event] -> Cache.Cache
perform_uncached events =
    Cache.perform initial_cache (Cache.EventDamage Ranges.everything) events
    where initial_cache = Cache.cache Perform_test.midi_config1

mkdamage :: [(RealTime, RealTime)] -> Cache.EventDamage
mkdamage = Cache.EventDamage . Ranges.ranges

type Message = (Timestamp.Timestamp, Midi.ChannelMessage)

diff_msgs :: Cache.Cache -> Cache.Cache -> [Either Message Message]
diff_msgs msgs1 msgs2 = Seq.diff (==) (extract msgs1) (extract msgs2)

extract :: Cache.Cache -> [(Timestamp.Timestamp, Midi.ChannelMessage)]
extract = extract_msgs . Cache.cache_messages

extract_logs :: Cache.Cache -> [String]
extract_logs = map DeriveTest.show_log . LEvent.logs_of . Cache.cache_messages

extract_msgs :: [LEvent.LEvent Midi.WriteMessage]
    -> [(Timestamp.Timestamp, Midi.ChannelMessage)]
extract_msgs msgs =
    [(ts, cmsg) | Midi.WriteMessage _ ts (Midi.ChannelMessage _ cmsg)
        <- LEvent.events_of msgs]

mkevents :: [(RealTime, RealTime)] -> [LEvent.LEvent Perform.Event]
mkevents pairs =
    map LEvent.Event [mkevent (start, dur, 42) | (start, dur) <- pairs]

mkevent :: (RealTime, RealTime, Int) -> Perform.Event
mkevent (start, dur, pitch) =
    Perform.Event Perform_test.inst1
        (Timestamp.from_real_time start) (Timestamp.from_real_time dur)
        Map.empty (Signal.signal [(start, fromIntegral pitch)]) Stack.empty

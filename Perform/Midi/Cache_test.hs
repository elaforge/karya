module Perform.Midi.Cache_test where
import qualified Data.Map as Map

import qualified Util.Ranges as Ranges
import Util.Test
import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Derive.Stack as Stack

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Perform_test as Perform_test
import qualified Perform.Midi.Cache as Cache


test_cached_performance = do
    let sz = Cache.cache_chunk_size
        events1 = [(0, 0.5), (sz, 0.5), (sz+1, 0.5), (sz*2, 0.5)]
        events2 = [(0, 0.5), (sz, 0.8), (sz+1, 0.5), (sz*2, 0.5)]

    -- Put the changed event in the middle of a chunk because otherwise the
    -- states definitely won't line up due to "last note" type state.
    equal (compare_cached events1 events2 [(sz, sz+10)])
        ([], [])

    -- The failure to change the note indicates that the cache is used when
    -- there is no damage.
    equal (compare_cached events1 events2 [])
        ([ (False, (4800, Midi.NoteOff 42 100))
        , (True, (4500, Midi.NoteOff 42 100))
        ], [])

    -- adding events to the end works, and skip a chunk
    equal (compare_cached [(0, 1)] [(0, 1), (sz*2, 1)] [(sz*2, sz*2+1)])
        ([], [])

    -- Deleting events works.  State check fails on postproc because I deleted
    -- the pitchbend initialization.
    equal (compare_cached [(0, 1), (sz, 1)] [(sz, 1)] [(0, 1)])
        ([], ["splice failed: postproc"])

    -- This time the remaining event will establish the proper state before
    -- the next chunk.
    equal (compare_cached [(0, 1), (1, 1), (sz, 1)] [(1, 1), (sz, 1)] [(0, 1)])
        ([], [])

test_lazy_performance = do
    let inf = [(n * 0.5, 0.25) | n <- [0..]]
    let uncached = perform_uncached inf
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
    equal (take 5 (map Cache.chunk_warns (Cache.cache_chunks cached)))
        [[], [], [], [], []]


inst_lookup = Perform_test.inst_lookup
inst_config = Perform_test.inst_config1

type Events = [(RealTime, RealTime)]

compare_cached :: Events -> Events -> [(RealTime, RealTime)]
    -> ([(Bool, (Timestamp.Timestamp, Midi.ChannelMessage))], [String])
compare_cached initial modified damage = (diff_msgs uncached cached, warns)
    where
    (_, cached, uncached) = perform_both initial modified damage
    warns = map Warning.warn_msg
        (concatMap Cache.chunk_warns (Cache.cache_chunks cached))

perform_both :: Events -> Events -> [(RealTime, RealTime)]
    -> (Cache.Cache, Cache.Cache, Cache.Cache)
perform_both initial modified damage = (initial_cache, cached, uncached)
    where
    initial_cache = perform_uncached initial
    cached = Cache.perform initial_cache (mkdamage damage) (mkevents modified)
    uncached = perform_uncached modified

perform_uncached :: Events -> Cache.Cache
perform_uncached events =
    Cache.perform initial_cache (Cache.EventDamage Ranges.everything)
        (mkevents events)
    where
    initial_cache = Cache.cache Perform_test.inst_lookup
        Perform_test.inst_config1

mkdamage :: [(RealTime, RealTime)] -> Cache.EventDamage
mkdamage = Cache.EventDamage . Ranges.ranges

diff_msgs msgs1 msgs2 = simple_diff $
    Seq.diff (==) (extract msgs1) (extract msgs2)

extract :: Cache.Cache -> [(Timestamp.Timestamp, Midi.ChannelMessage)]
extract cache =
    [(ts, cmsg) | Midi.WriteMessage _ ts (Midi.ChannelMessage _ cmsg)
        <- Cache.cache_messages cache]

-- | True if the val was added, False if it was subtracted.
simple_diff :: [(Maybe a, Maybe a)] -> [(Bool, a)]
simple_diff = Seq.map_maybe f
    where
    f (Just a, Nothing) = Just (False, a)
    f (Nothing, Just a) = Just (True, a)
    f _ = Nothing

-- diff_events :: Result -> Result
--     -> Either Derive.DeriveError [(Bool, SimpleEvent)]
-- diff_events r1 r2 = do
--     e1 <- Derive.r_result r1
--     e2 <- Derive.r_result r2
--     return $ simple_diff $
--         Seq.diff (==) (map simple_event e1) (map simple_event e2)

-- cached_performance :: Cache -> EventDamage -> Perform.Events
--     -> (Cache, [Warning.Warning])

-- Give each one its own pitch
mkevents pairs = [mkevent (start, dur, 42) | (start, dur) <- pairs]

mkevent :: (RealTime, RealTime, Int) -> Perform.Event
mkevent (start, dur, pitch) =
    Perform.Event Perform_test.inst1 start dur Map.empty
        (Signal.signal [(start, fromIntegral pitch)]) Stack.empty

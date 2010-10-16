{-# LANGUAGE PatternGuards #-}
module Perform.Midi.Cache (
    Cache(..), cache, cache_length, cache_messages, messages_from
    , EventDamage(..)
    , cache_stats, is_splice_failure
    , Chunks, ChunkNum, to_chunknum, cache_chunk_size, Chunk(..)
    , perform
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Util.Ranges as Ranges

import qualified Midi.Midi as Midi

import Ui

import qualified Derive.Stack as Stack

import qualified Perform.Timestamp as Timestamp
import Perform.Timestamp (Timestamp)
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.Instrument as Instrument


data Cache = Cache {
    -- | If this changes, the cache map has to be discarded.
    cache_config :: Instrument.Config
    -- | Which chunks were reperformed due to damage, /not including/ the
    -- chunks rederived due to a failed splice.  The reason is laziness, see
    -- 'cache_stats'.
    , cache_damage :: Ranges.Ranges ChunkNum
    , cache_chunks :: Chunks
    } deriving (Show)

cache :: Instrument.Config -> Cache
cache config = Cache config Ranges.nothing []

-- | Return the length of time of the cached events.
cache_length :: Cache -> RealTime
cache_length cache = Timestamp.to_real_time $
    fromIntegral (length (cache_chunks cache)) * cache_chunk_size

cache_messages :: Cache -> Perform.Messages
cache_messages =
    Perform.merge_sorted_messages . map chunk_messages . cache_chunks

-- | Return messages starting from a certain timestamp.  Subtract that
-- timestamp from the message timestamps so they always start at 0.
--
-- TODO look at postproc state to initialize the controls properly
messages_from :: Timestamp.Timestamp -> Cache -> Perform.Messages
messages_from start cache = initialize_msgs ++
    map (Midi.add_timestamp (-start)) (Perform.merge_sorted_messages msgs)
    where
    start_chunk = fromIntegral $
        Timestamp.to_millis start `div` Timestamp.to_millis cache_chunk_size
    chunks = drop start_chunk (cache_chunks cache)
    msgs = case map chunk_messages chunks of
        [] -> []
        m : rest -> dropWhile ((<start) . Midi.wmsg_ts) m : rest
    initialize_msgs = case chunks of
        [] -> []
        chunk : _ -> state_initialization $
            Perform.state_postproc (chunk_state chunk)

state_initialization :: Perform.PostprocState -> Perform.Messages
state_initialization state = concatMap mkmsgs (Map.assocs state)
    where
    mkmsgs ((dev, chan), (pb, controls)) = map wmsg (pb_msg ++ control_msgs)
        where
        pb_msg = maybe [] ((:[]) . Midi.PitchBend) pb
        control_msgs = map mkcontrol (Map.assocs controls)
        wmsg msg = Midi.WriteMessage dev Timestamp.zero
            (Midi.ChannelMessage chan msg)
    mkcontrol (cc, val) = Midi.ControlChange cc val

-- | Figure out how much time of the performed MIDI messages were from
-- the cache and how much time was reperformed.
--
-- If a splice failed, take the chunknum at which it failed.  Since splices
-- can only fail once, every chunk beyond that will have been reperformed.
--
-- Since the splice failure is recorded in the log, and checking the logs
-- forces the chunk, this function accepts the splice failure as an argument
-- which is awkward but allows the caller to retain control over evaluation.
cache_stats :: Maybe ChunkNum -> Cache -> (RealTime, RealTime)
    -- ^ (time re-performed, total time)
cache_stats splice_failed_at cache = case splice_failed_at of
        Nothing -> stats (cache_damage cache)
        Just chunknum -> stats $ Monoid.mappend
            (Ranges.range chunknum nchunks) (cache_damage cache)
    where
    nchunks = fromIntegral (length (cache_chunks cache))
    time n = Timestamp.to_real_time (fromIntegral n * cache_chunk_size)
    stats ranges = (time (total ranges), time nchunks)
    total ranges = case Ranges.extract ranges of
        Nothing -> nchunks
        Just pairs -> sum (map (\(s, e) -> if e == s then 1 else e - s) pairs)

is_splice_failure :: Warning.Warning -> Bool
is_splice_failure warn =
    "splice failed: " `List.isPrefixOf` Warning.warn_msg warn

-- | Originally this was an IntMap which allows faster seeking to a a chunk,
-- but it's essential that the chunk list be spine-lazy.  Since each chunk is
-- a fixed period of time, the list should never get long enough for linear
-- search to take a noticeable amount of time anyway.
type Chunks = [Chunk]
type ChunkNum = Integer

to_chunknum :: RealTime -> ChunkNum
to_chunknum = floor . (/ Timestamp.to_real_time cache_chunk_size)

cache_chunk_size :: Timestamp
cache_chunk_size = Timestamp.seconds 4

data Chunk = Chunk {
    chunk_messages :: Perform.Messages
    , chunk_state :: Perform.State
    , chunk_warns :: [Warning.Warning]
    } deriving (Show)

newtype EventDamage = EventDamage (Ranges.Ranges RealTime)
    deriving (Show)

perform :: Cache -> EventDamage -> Perform.Events -> Cache
perform cache (EventDamage damage) events
    | null (cache_chunks cache) = set_map (everything, Ranges.everything)
    -- If the cache map is not null then it must be full except the areas under
    -- the damage.
    | otherwise = set_map $ case Ranges.extract damage of
        Nothing -> (everything, Ranges.everything)
        Just pairs -> let damage = chunk_damage pairs
            in (make_map damage, Ranges.ranges damage)
    where
    everything = perform_chunks config 0 Perform.initial_state events
    make_map damage = perform_cache config 0 (cache_chunks cache)
        Perform.initial_state events damage
    set_map (chunks, damage) =
        cache { cache_damage = damage, cache_chunks = chunks }
    config = cache_config cache

-- | Like the damage ranges, the chunk ranges are half-open.
chunk_damage :: [(RealTime, RealTime)] -> [(ChunkNum, ChunkNum)]
chunk_damage = map $ \(s, e) -> (floor (s / size), ceiling (e / size))
    where size = Timestamp.to_real_time cache_chunk_size

-- * implementation

-- | Run through the chunk cache, reperforming chunks that are within the
-- damage ranges.
--
-- After each reperformed range check if the final state is compatible with the
-- final state from the cache.  If so, the reperformed range can be spliced
-- into the result and the rest of the cache can be reused.  Otherwise, the
-- rest of the cache is invalidated since the state it was generated in respect
-- to has changed, and the entire rest of the performance must be recalculated.
-- So the cache may just save on the work done before the damage, but that's
-- still significant.
--
-- This function is too complicated, sorry about that.
perform_cache :: Instrument.Config -> ChunkNum -> [Chunk] -> Perform.State
    -> Perform.Events -> [(ChunkNum, ChunkNum)] -> [Chunk]
perform_cache config chunknum [] prev_state events _ =
    perform_chunks config chunknum prev_state events
    -- If there is no damage I can just reuse the chunks and don't have to look
    -- at the events.  This won't be true for the initially empty cache, but
    -- the case above should catch that.
perform_cache _ _ chunks _ _ [] = chunks
perform_cache config chunknum (cached : rest_cache) prev_state events
        damage@((start, end) : rest_damage)
    | chunknum < start =
        cached : perform_rest (chunk_state cached) events damage
    | chunknum+1 >= end = -- will be > for point damage, like (1, 1)
        -- Check the final state of a damaged range against its old value.
        -- If they're compatible, the cache afterwards is still valid.
        case incompatible (chunk_state cached) (chunk_state new_chunk) of
                -- Use the old state to hopefully minimize incompatibility.
            Nothing -> new_chunk
                : perform_rest (chunk_state cached) post_events rest_damage
            -- Putting this note in the warns is a bit of a hack, but it's nice
            -- to see when a splice failed and why.  The other other cache
            -- stats can be derived from the damage ranges.
            Just reason -> with_warn ("splice failed: " ++ reason) new_chunk
                : perform_chunks config (chunknum+1) (chunk_state new_chunk)
                    post_events
    | chunknum < end = new_chunk
        : perform_rest (chunk_state new_chunk) post_events damage
    -- This shouldn't happen, 'damage' should always be above 'chunknum'.
    | otherwise = perform_cache config chunknum (cached : rest_cache)
        prev_state events rest_damage
    where
    (new_chunk, post_events) = perform_chunk chunknum prev_state config events
    perform_rest = perform_cache config (chunknum+1) rest_cache
    with_warn msg chunk = chunk { chunk_warns = warn : chunk_warns chunk }
        where warn = Warning.warning msg Stack.empty Nothing

-- | Just keep performing chunks until I run out of events.
perform_chunks :: Instrument.Config -> ChunkNum -> Perform.State
    -> Perform.Events -> [Chunk]
perform_chunks config chunknum prev_state events =
    fst $ go chunknum prev_state events
    where
    go _ prev_state [] = ([], prev_state)
    go chunknum prev_state events = (chunk : chunks, final_state)
        where
        (chunk, post_events) = perform_chunk chunknum prev_state config events
        (chunks, final_state) = go (chunknum+1) (chunk_state chunk) post_events

-- | TODO this will be inefficient the first time it is called and has to
-- drop a lot of events.  Events should be a more efficiently seekable data
-- structure to avoid this.
trim_events :: ChunkNum -> Perform.Events -> Perform.Events
trim_events nchunk = dropWhile ((<start) . Perform.event_start)
    where start = fromIntegral nchunk * cache_chunk_size

perform_chunk :: ChunkNum -> Perform.State -> Instrument.Config
    -> Perform.Events -> (Chunk, Perform.Events)
perform_chunk chunknum state config events =
    (Chunk msgs (normalize_state end final_state) warns, post)
    where
    (pre, post) = break ((>=end) . Perform.event_start)
        (trim_events chunknum events)
    end = fromIntegral (chunknum+1) * cache_chunk_size
    (msgs, warns, final_state) = Perform.perform state config pre

-- | Compare to States and if they aren't compatible, return why not.
--
-- I could possibly trade some correctness for speed here by being more
-- lenient, but I'm not sure how much that would actually happen.
incompatible :: Perform.State -> Perform.State -> Maybe String
incompatible state1 state2
    -- The most likely to fail quickly go first.
    | neq Perform.state_postproc = Just "postproc"
    | neq Perform.state_perform = Just "perform"
    | neq Perform.state_allot = Just "allot"
    -- TODO this has to compare signals because that determines if an event
    -- can share, but is potentially too expensive.  Except if I clip the
    -- signals, I'm doing that, right?  Also, if I use signal IDs or
    -- fingerprints this could get cheaper.
    | neq Perform.state_channelize = Just "channelize"
    | otherwise = Nothing
    where
    neq f = f state1 /= f state2
    -- used while debugging to see how exactly they differ
    -- just f msg = Just $ msg ++ ": " ++ show (f state1, f state2)

-- | This is run on the state at the end of every chunk.  The idea is to
-- make it more likely that a reperformed state will be compatible, thus making
-- it more likely to be possible to splice a reperformed section into the
-- cache.  However, the normalization itself must be cheap or it defeats the
-- purpose.
normalize_state :: Timestamp -> Perform.State -> Perform.State
normalize_state now state = state {
    Perform.state_channelize =
        takeWhile overlapping (Perform.state_channelize state)
    }
    where
    overlapping = ((> now) . Perform.note_end . fst)
    -- The perform state records the note off times for each addr when it
    -- really only needs it to place the control lead time, so I could
    -- normalize note off time to (max (now-Perform.control_lead_time)), but
    -- allot state has the same problem and this seems to expensive anyway.

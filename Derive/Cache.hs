-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Derive.Cache (
    Cacheable(..)
    , block, track
    , get_control_damage, get_tempo_damage
    , is_cache_log, cache_hit_events, cache_miss_reason
    -- * debugging
    , pretty_cache

#ifdef TESTING
    , find_generator_cache
    , _extend_control_damage
#endif
) where
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Args as Args
import qualified Derive.Derive as Derive
import Derive.Derive (Cache(..), Cached(..), ScoreDamage(..), ControlDamage(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import Global
import Types


class Cacheable d where
    from_cache_entry :: Derive.CacheEntry -> Maybe (Derive.CallType d)
    to_cache_entry :: Derive.CallType d -> Derive.CacheEntry

instance Cacheable Score.Event where
    from_cache_entry (Derive.CachedEvents ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = Derive.CachedEvents

instance Cacheable Signal.Control where
    from_cache_entry (Derive.CachedControl ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = Derive.CachedControl

instance Cacheable PSignal.PSignal where
    from_cache_entry (Derive.CachedPitch ctype) = Just ctype
    from_cache_entry _ = Nothing
    to_cache_entry = Derive.CachedPitch

-- * block

-- | Unfortunately caching is not entirely general, and cache invalidation
-- works a bit differently for blocks and tracks.
data Type = Block !BlockId
    -- | Cache a track.  For a note track, the set is its TrackId along with
    -- its children, so it knows what sort of damage will invalidate the cache.
    -- If the track has children, it's assumed to be inverting, so that it
    -- depends on all of its children.  For a control track, the set should be
    -- empty, since control tracks are not invalidated by damage on their
    -- children.
    | Track !TrackId !(Set TrackId)
    deriving (Eq, Show)

-- | If the given generator has a cache entry, relevant derivation context is
-- the same as the cache entry's, and there is no damage under the generator,
-- I can reuse the cached values for it.  This is effectively a kind of
-- memoization.  If the generator is called, the results will be put in the
-- cache before being returned.
block :: BlockId -> (Derive.PassedArgs d -> Derive.NoteDeriver)
    -> (Derive.PassedArgs d -> Derive.NoteDeriver)
block block_id call args = caching_deriver (Block block_id) range (call args)
    where
    range = Range
        { _start = fst $ Args.range_on_track args
        , _end = snd $ Args.range_on_track args
        , _negative = Event.is_negative (Args.event args)
        }

data Range = Range {
    _start :: !TrackTime
    , _end :: !TrackTime
    -- | If True, these are the ranges of a negative event, which affects how
    -- control damage works.
    , _negative :: !Bool
    } deriving (Show)

-- | Cache a track, but only if it's not sliced and has a TrackId.
track :: Cacheable d => TrackTree.Track -> Set TrackId
    -- ^ Children, as documented in 'Track'.
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
track track children deriver = case should_cache track of
    Just track_id -> caching_deriver (Track track_id children) range deriver
    Nothing -> deriver
    where
    range = Range
        { _start = TrackTree.track_start track
        , _end = TrackTree.track_end track
        , _negative = False
        }

should_cache :: TrackTree.Track -> Maybe TrackId
should_cache track
    | TrackTree.track_sliced track == TrackTree.NotSliced =
        TrackTree.track_id track
    | otherwise = Nothing

caching_deriver :: Cacheable d => Type -> Range
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
caching_deriver typ range call = do
    st <- Derive.get
    let cdamage = Derive.state_control_damage (Derive.state_dynamic st)
        sdamage = Derive.state_score_damage (Derive.state_constant st)
        stack = Derive.state_stack (Derive.state_dynamic st)
    generate stack $ find_generator_cache typ (Derive.CacheKey stack) range
        sdamage cdamage (Derive.state_cache (Derive.state_constant st))
    where
    generate _ (Right (collect, cached)) = do
        Log.write $ cache_hit_msg cached
        stats <- hit_stats typ range
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Internal.merge_collect $
            mempty { Derive.collect_cache_stats = stats } <> collect
        return cached
    generate stack (Left (inflict_control_damage, reason)) = do
        (result, collect) <- with_collect inflict_control_damage call
        Log.write $ cache_miss_msg reason
        Internal.merge_collect $ mempty
            { Derive.collect_cache =
                make_cache (Derive.CacheKey stack) collect result
            }
        return result

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_collect inflict_control_damage deriver = do
        -- TODO Do I want to run deriver a sub derivation so I can put an
        -- empty cache if it failed?  Otherwise I think maybe a failed
        -- event will continue to produce its old value.
        (result, collect) <- with_empty_collect
            (is_block typ && inflict_control_damage) deriver
        Internal.merge_collect collect
        return (result, collect)
    is_block (Block {}) = True
    is_block _ = False

hit_stats :: Type -> Range -> Derive.Deriver Derive.CacheStats
hit_stats typ (Range start end _) = do
    start <- Derive.real start
    end <- Derive.real end
    return $ mempty
        { Derive.cstats_hits = [(id, (start, end))]
        }
    where
    id = case typ of
        Block block_id -> Left block_id
        Track track_id _ -> Right track_id

-- real_range :: Ranges.Ranges ScoreTime -> Derive.Deriver (RealTime, RealTime)
-- real_range ranges = case Ranges.extract ranges of
--     undefined

-- | Both track warps and local deps are used as dynamic return values (aka
-- modifying a variable to \"return\" something).  When evaluating a cached
-- generator, the caller wants to know the callee's track warps and local
-- deps, without getting them mixed up with its own warps and deps.  So run
-- a deriver in an empty environment, and restore it afterwards.
with_empty_collect :: Bool
    -- ^ If True, expand the ControlDamage to Ranges.everything.  If
    -- ControlDamage touches a block call then it likely invalidates everything
    -- within that block.
    -> Derive.Deriver a -> Derive.Deriver (a, Derive.Collect)
with_empty_collect inflict_control_damage deriver = do
    old <- Derive.get
    Derive.modify $ \st -> st
        { Derive.state_collect = mempty
        , Derive.state_dynamic = if inflict_control_damage
            then (Derive.state_dynamic st)
                { Derive.state_control_damage =
                    Derive.ControlDamage Ranges.everything
                }
            else Derive.state_dynamic st
        }
    result <- deriver
    collect <- Derive.gets Derive.state_collect
    Derive.put old
    return (result, collect)

-- | Find the cached value, or a reason why there is no cache entry.  This
-- is the function that determines whether you hit the cache or not.
find_generator_cache :: Cacheable d => Type -> Derive.CacheKey -> Range
    -> ScoreDamage -> ControlDamage -> Cache
    -> Either (Bool, Text) (Derive.Collect, Stream.Stream d)
    -- ^ on a miss, the Bool is 'with_empty_collect's inflict_control_damage arg
find_generator_cache typ key range score (ControlDamage control) (Cache cache)
        = do
    cached <- justErr (False, "not in cache") $ Map.lookup key cache
    -- Look for block damage before looking for Invalid, because if there is
    -- block damage I inflict control damage too.  This is because block damage
    -- means things like a block title change, or skeleton change, and those
    -- can invalidate all blocks called from this one.
    let stack = Stack.innermost (Derive.key_stack key)
    case typ of
        Block _block_id -> case msum (map Stack.block_of stack) of
            Just this_block
                | this_block `Set.member` sdamage_blocks score ->
                    Left (True, "direct block damage")
                | this_block `Set.member` sdamage_track_blocks score ->
                    Left (False, "track block damage")
            _ -> return ()
        Track _track_id children
            | any (`Set.member` children) (Map.keys (sdamage_tracks score)) ->
                Left (False, "track damage")
            | otherwise -> return ()
    Derive.CallType collect stream <- case cached of
        Invalid -> Left (False, "cache invalidated by score damage")
        Cached entry -> justErr (False, "cache has wrong type") $
            from_cache_entry entry
    let Derive.BlockDeps block_deps = Derive.collect_block_deps collect
    let damaged_blocks = Set.union
            (sdamage_track_blocks score) (sdamage_blocks score)
    unless (Set.null (Set.intersection damaged_blocks block_deps)) $
        Left (False, "sub-block damage")
    -- Negative duration indicates an arrival note.  The block deriver then
    -- takes the controls from the bottom of event (which is the start),
    -- see "Derive.Call.Block".
    let overlapping = if _negative range then Ranges.overlapping_closed
            else Ranges.overlapping
    when (overlapping control (Ranges.range (_start range) (_end range))) $
        Left (True, "control damage")
    return (collect, stream)

-- | Make a single cache entry.  This will go into 'Derive.collect_cache'
-- and be merged in with the rest.
make_cache :: Cacheable d => Derive.CacheKey -> Derive.Collect
    -> Stream.Stream d -> Cache
make_cache key collect stream = Cache $ Map.singleton key (Cached entry)
    where
    stripped = collect
        { Derive.collect_cache = mempty
        -- integration shouldn't happen if the cache is reused, since that
        -- means nothing changed.  So this reduces unnecessary reintegration.
        , Derive.collect_integrated = []
        -- Use Map.map instead of fmap, since fmap is lazy.
        , Derive.collect_track_dynamic =
            Map.map Derive.strip_dynamic (Derive.collect_track_dynamic collect)
        , Derive.collect_track_dynamic_inverted =
            Map.map Derive.strip_dynamic
                (Derive.collect_track_dynamic_inverted collect)
        }
    entry = to_cache_entry $ Derive.CallType stripped $
        Stream.from_sorted_list $ filter (not . cache_log) $
        Stream.to_list stream
    -- I do want a cached chunk to retain its log msgs, since those include
    -- errors deriving.  However, it's confusing if it also includes cache
    -- msgs because then it looks like it wasn't cached after all.
    -- It's unfortunate that I have to copy the chunk, but it's either this
    -- or a more complicated filtering scheme later on, which is bound to
    -- be just a filter too.  At least this way it only happens once.
    cache_log = LEvent.either (const False) is_cache_log

-- * logs

is_cache_log :: Log.Msg -> Bool
is_cache_log msg =
    Maybe.isJust (cache_hit_events msg) || Maybe.isJust (cache_miss_reason msg)

cache_hit :: Text
cache_hit = "cache-hit"

cache_hit_msg :: Stream.Stream a -> Log.Msg
cache_hit_msg cached =
    Log.with_int cache_hit events $ Log.msg Log.Debug Nothing $
        "using cache, " <> showt events <> " events, " <> showt logs <> " logs"
    where
    (events, logs) = foldl' count (0, 0) (Stream.to_list cached)
    count (!es, !ms) e = if LEvent.is_event e then (es+1, ms) else (es, ms+1)

-- | Get the number of cached events from a 'cache_hit_msg'.
cache_hit_events :: Log.Msg -> Maybe Int
cache_hit_events = Log.lookup_int cache_hit

cache_miss :: Text
cache_miss = "cache-miss"

cache_miss_msg :: Text -> Log.Msg
cache_miss_msg reason = Log.with_text cache_miss reason $
    Log.msg Log.Debug Nothing $ "rederived generator because of " <> reason

-- | Get the reason from a 'cache_miss_msg'.
cache_miss_reason :: Log.Msg -> Maybe Text
cache_miss_reason = Log.lookup_text cache_miss

-- * debugging

-- | Format the cache in a hopefully readable way.
pretty_cache :: Derive.Cache -> Text
pretty_cache (Derive.Cache cache) = Text.unlines $ concat
    [ Stack.pretty_ui_ stack <> ": " : map ("    "<>) (fmt cached) ++ [""]
    | (Derive.CacheKey stack, cached) <- Map.toList cache
    ]
    where
    fmt Derive.Invalid = ["Invalid"]
    fmt (Derive.Cached cached) = case cached of
        Derive.CachedEvents (Derive.CallType _ a) -> Stream.short_events a
        Derive.CachedControl (Derive.CallType _ a) -> [pretty a]
        Derive.CachedPitch (Derive.CallType _ a) -> [pretty a]


-- * get_control_damage

{- | ScoreDamage on the current track is converted into ControlDamage, and
    expanded to the neighbor events.  This is because control calls emit samples
    between their previous or next events.  Then this is merged with any
    ControlDamage inherited from callers.

    If a block call is touched by control damage, the the control damage expands
    to cover the entire block.

    Previously, this inherited damage would also be expanded to its neighbor
    events, under the rationale that controls can modify other controls.  While
    this is true, it causes an annoying situation where a control track with
    a single call under (say) a tempo track will cause any edits to the tempo
    track to invalidate the entire score.  This happens a lot in practice, and
    I've forgotten about this wrinkle a number of times

    The downside, of course, is that control damage will not cause a control
    call that lies before its previous event to rederive, even if it should
    have.  I'll have to see how annoying this is in practice.
-}
get_control_damage :: BlockId -> TrackId
    -> (ScoreTime, ScoreTime) -- ^ track_range must be passed explicitly
    -- because the event may have been sliced and shifted, but ControlDamage
    -- should be relative to the start of the track at ScoreTime 0.
    -> Derive.Deriver ControlDamage
get_control_damage block_id track_id track_range = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    (control<>) <$> extend_damage track_id track_range
        (score_to_control block_id track_id score)

{- | Since the warp is the integral of the tempo track, damage on the tempo
    track will affect all events after it.  Actually, the damage extends from
    the previous event to the end of the track, since interpolating calls extend
    from the previous event.

    It would be simpler to have any edit invalidate the whole track, but it
    seems like editing a score at the end is a common case, and it would be
    a shame to rederive the entire score on each edit when only the very end has
    changed.
-}
get_tempo_damage :: BlockId -> TrackId -> TrackTime
    -> Events.Events -> Derive.Deriver ControlDamage
get_tempo_damage block_id track_id track_end events = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    return $ extend $ control <> score_to_control block_id track_id score
    where
    extend (Derive.ControlDamage ranges) = Derive.ControlDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> case Events.split_lists s events of
                (prev : _, _) ->
                    Ranges.range (Event.start prev) track_end
                ([], _) -> Ranges.range s track_end

-- | Convert score damage directly to ControlDamage on a given track.
score_to_control :: BlockId -> TrackId -> ScoreDamage -> ControlDamage
score_to_control block_id track_id score = ControlDamage $
    (if block_id `Set.member` Derive.sdamage_blocks score
        then Ranges.everything else Ranges.nothing)
    <> Map.findWithDefault Ranges.nothing track_id (Derive.sdamage_tracks score)

-- | Extend the given ControlDamage as described in 'get_control_damage'.
-- Somewhat tricky because I also want to clip the damage to the track range,
-- if any.  This is so a sliced control track below an unsliced one won't
-- bother figuring out damage outside its range.
extend_damage :: TrackId -> (ScoreTime, ScoreTime) -> ControlDamage
    -> Derive.Deriver ControlDamage
extend_damage track_id track_range (ControlDamage damage)
    | damage == mempty = return mempty
    | otherwise = do
        events <- Track.track_events <$> Derive.get_track track_id
        return $ ControlDamage $ extend events
    where
    extend events
        | Events.null events = damage
        | otherwise = Ranges.intersection (uncurry Ranges.range track_range) $
            _extend_control_damage (snd track_range) events damage

_extend_control_damage :: ScoreTime -> Events.Events
    -> Ranges.Ranges ScoreTime -> Ranges.Ranges ScoreTime
_extend_control_damage track_end events = Ranges.pair_map (extend1 events)
    where
    extend1 events (s, e) = (event_at_before s events, event_after e events)
    event_at_before p events = case Events.split_lists p events of
        (_, at : _) | p == Event.start at -> p
        (prev : _, _) -> Event.start prev
        _ -> p
    event_after p events = maybe track_end Event.start $
        Seq.head (Events.after p events)

{-# LANGUAGE CPP #-}
module Derive.Cache (
    cached_transformer, caching_call
    , score_damage
    , get_control_damage, get_tempo_damage

#ifdef TESTING
    , find_generator_cache
#endif
) where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.Diff as Diff
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import Derive.Derive
       (CacheState(..), Cache(..), CacheEntry(..), CallType(..),
        GeneratorDep(..), TransformerType(..), ScoreDamage(..),
        DamageRanges, EventDamage(..), ControlDamage(..))
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack

import qualified Perform.RealTime as RealTime


-- * cached_transformer

-- | If the given transformer supports incremental recomputation and has cached
-- values, recompute based on the input events and EventDamage.  Regardless,
-- the new values will be be put in the cache before being returned.
--
-- This is called around each transformer, at the block level, track level, or
-- even at the evnt level.
--
-- EventDamage requires that you evaluate the the deriver to get the events and
-- damage out.  That's ok because incremental recomputation only applies to
-- postproc style transformers that evaluate the deriver anyway.
--
-- This doesn't use score or control damage like 'caching_call' since it
-- evaluates the underlying deriver regardless.
cached_transformer :: (Derive.Derived derived) => Cache -> Stack.Stack
    -> Derive.TransformerCall derived -> Derive.PassedArgs derived
    -> Derive.LogsDeriver derived
    -> Derive.Deriver (Derive.LogsDeriver derived, Maybe Cache)
cached_transformer _cache _stack (Derive.TransformerCall call _ttype)
        args deriver =
    return (call args deriver, Nothing)

{-
        -- NonIncremental -> return (call args deriver, Nothing)
        -- Incremental context -> recompute context
    where
    -- call the sub-deriver
    -- pull out EventDamage from the state
    -- add in ranges implied by any changed control deps
    -- recompute the transformer in those areas, expanded by its context
    recompute context = do
        -- Yes, this evaluates the deriver.  But by being NonIncremental, the
        -- transformer has asserted that it's going to do that anyway.
        -- This means that NonIncremental transformers should not modify the
        -- environment before they evaluate, otherwise the environment will
        -- lack control damage and some generators that should have been
        -- rederived may be retrieved from cache instead.
        sub_result <- deriver
        TransformerDep control_deps <- get_recorded_tdep
        (damage, cached) <- case lookup_transformer stack cache of
            Nothing -> return (entire_range sub_result, mempty)
            Just cached -> do
                local <- Derive.state_local_damage <$> Derive.get_cache_state
                cont <- control_damage control_deps
                return (expand_for_context context cached (local <> cont),
                    cached)
        result <- recompute_regions damage call args cached sub_result
        case result of
            Left err -> return (Left err, Nothing)
            Right new_deriver -> do
                -- Expanded modified area should replace existing damage.
                Derive.put_local_damage damage
                derived <- new_deriver
                let new_cache = put_transformer_cache stack derived cache
                return (Right (return derived), Just new_cache)

-- | Get the deps of the last evaluated call.  See 'Derive.collect_local_dep'
-- for details.
get_recorded_tdep :: Derive.Deriver TransformerDep
get_recorded_tdep = undefined
    -- gdep <- collect_local_dep <$> Derive.get_cache_state
    -- return $ TransformerDep (gdep_controls gdep)

lookup_transformer :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe derived
lookup_transformer stack cache = undefined

put_transformer_cache :: (Derive.Derived derived) => Stack.Stack -> derived
    -> Cache -> Cache
put_transformer_cache = undefined

entire_range :: (Derive.Derived derived) => derived -> EventDamage
entire_range = undefined

recompute_regions :: (Derive.Derived derived) =>
    EventDamage -> Derive.TransformerFunc derived
    -> Derive.PassedArgs derived -> derived -> derived
    -> Derive.Deriver (Either TrackLang.TypeError (Derive.Deriver derived))
recompute_regions damage call args cached derived = undefined

expand_for_context :: (Int, Int) -> derived -> EventDamage -> EventDamage
expand_for_context (pre, post) derived damage = undefined

-- | Compare the current state of the controls with the given control deps,
-- and return damage ranges for those that don't match.
control_damage :: ControlDeps -> Derive.Deriver EventDamage
control_damage = undefined
-}


-- * caching_call

-- | If the given generator has a cache entry, relevant derivation context is
-- the same as the cache entry's, and there is no damage under the generator,
-- I can reuse the cached values for it.  This is effectively a kind of
-- memoization.  If the generator is called, the results will be put in the
-- cache before being returned.
caching_call :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
caching_call call args = do
    (start, end) <- Derive.passed_real_range args
    cache <- Derive.gets Derive.state_cache_state
    stack <- Derive.gets Derive.state_stack
    generate stack $ find_generator_cache stack (Ranges.range start end)
        (state_score_damage cache) (state_control_damage cache)
        (state_cache cache)
    where
    generate _ (Right (collect, cached)) = do
        Log.debug $ "using cache (" ++ show (LEvent.length cached) ++ " vals)"
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Derive.modify_collect (collect <>)
        return cached
    generate stack (Left reason) = do
        (result, collect) <- with_collect (call args)
        cur_cache <- state_cache <$> Derive.get_cache_state
        Log.notice $ "rederived generator because of "
            -- This destroys laziness, though I'm not sure why since the
            -- log msg shouldn't be forced until the msgs already have been
            -- forced themselves.
            -- ++ show (LEvent.length stream) ++ " vals) because of "
            ++ reason
        Derive.modify_cache_state $ \st -> st { Derive.state_cache =
            insert_generator stack collect result cur_cache }
        return result

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_collect deriver = do
        -- TODO Do I want to run deriver a sub derivation so I can put an
        -- empty cache if it failed?  Otherwise I think maybe a failed
        -- event will continue to produce its old value.
        (result, collect) <- Derive.with_empty_collect deriver
        Derive.modify $ \st ->
            st { Derive.state_collect = collect <> Derive.state_collect st }
        return (result, collect)

find_generator_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Ranges.Ranges RealTime -> ScoreDamage -> ControlDamage
    -> Cache -> Either String (Derive.Collect, LEvent.LEvents derived)
find_generator_cache stack event_range score_damage
        (ControlDamage control_damage) cache = do
    (collect, stream) <- maybe (Left "not in cache") Right
        (lookup_generator stack cache)
    let Derive.GeneratorDep block_deps = Derive.collect_local_dep collect
    let damaged_blocks = Set.union
            (sdamage_track_blocks score_damage) (sdamage_blocks score_damage)
    unless (Set.null (Set.intersection damaged_blocks block_deps)) $
        Left "sub-block damage"
    when (Ranges.overlapping control_damage event_range) $
        Left "control damage"
    return (collect, stream)

lookup_generator :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (Derive.Collect, LEvent.LEvents derived)
lookup_generator stack cache = do
    ctype <- lookup_cache stack cache
    case ctype of
        CachedGenerator collect stream -> Just (collect, stream)
        _ -> Nothing

insert_generator :: (Derive.Derived derived) =>
    Stack.Stack -> Derive.Collect -> LEvent.LEvents derived -> Cache -> Cache
insert_generator stack collect stream (Cache cache) =
    Cache $ Map.insert stack entry cache
    where
    -- TODO clear out other bits of cache that this overlaps with
    -- TODO filter log msgs so I don't get logs about cache misses back with
    -- the cache hit.  This is unsatisfactory because it copies the stream.
    -- Better solution?
    entry = Derive.to_cache_entry
        (CachedGenerator collect (filter is_event stream))
    is_event (LEvent.Event _) = True
    is_event _ = False

-- * get_control_damage

-- | ControlDamage works in this manner:
--
-- ScoreDamage on a control track is expanded to include the previous to the
-- next event, since control calls generally generate samples based on their
-- previous event, and possibly the next one.  Since control tracks may depend
-- on other control tracks, controls beneath the damaged one will also expand
-- the damage to include previous and next events in the same way.
get_control_damage :: TrackId -> State.TrackRange
    -> Derive.Deriver ControlDamage
get_control_damage track_id range = do
    control <- Derive.state_control_damage <$> Derive.get_cache_state
    extend_damage track_id range =<< if control == mempty
        then score_to_control track_id range . Derive.state_score_damage
            =<< Derive.get_cache_state
        else return control

-- | Since the warp is the integral of the tempo track, damage on the tempo
-- track will affect all events after it.  The tempo track shouldn't be sliced
-- so TrackRange doesn't apply.
get_tempo_damage :: TrackId -> Derive.Deriver ControlDamage
get_tempo_damage track_id = do
    control <- Derive.state_control_damage <$> Derive.get_cache_state
    extend <$> if control == mempty
        then score_to_control track_id Nothing . Derive.state_score_damage
            =<< Derive.get_cache_state
        else return control
    where
    extend (Derive.ControlDamage ranges) = Derive.ControlDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> Ranges.range s RealTime.max

score_to_control :: TrackId -> State.TrackRange -> ScoreDamage
    -> Derive.Deriver ControlDamage
score_to_control track_id range score =
    ControlDamage <$> damage_to_real damage
    where
    damage = in_range $ maybe Ranges.nothing id $
        Map.lookup track_id (Derive.sdamage_tracks score)
    in_range = maybe id (Ranges.intersection . uncurry Ranges.range) range

-- | Extend the given ControlDamage as described in 'get_control_damage'.
-- Somewhat tricky because I also want to clip the damage to the track range,
-- if any.  This is so a sliced control track below an unsliced one won't
-- bother figuring out damage outside its range.
extend_damage :: TrackId -> State.TrackRange -> ControlDamage
    -> Derive.Deriver ControlDamage
extend_damage track_id range (ControlDamage damage)
    | damage == mempty = return (ControlDamage damage)
    | otherwise = do
        events <- Track.track_events <$> Derive.get_track track_id
        -- Empty tracks could not have contributed to further damage.
        if events == Track.empty_events
            then return (ControlDamage damage)
            else do
                sdamage <- damage_to_score damage
                Log.warn $ "extend from: " ++ show sdamage ++ " -> "
                    ++ show (extend range sdamage events)
                ControlDamage <$> damage_to_real (extend range sdamage events)
    where
    extend Nothing damage events = Ranges.fmap (Just . ext events) damage
    extend (Just range) damage events =
        Ranges.fmap (range_ext range events) damage
    range_ext track_range events range
        | in_range track_range range = Just (ext events range)
        | otherwise = Nothing
        where
        in_range (track_s, track_e) (s, e) = s >= track_s && e <= track_e
    ext events (s, e) = (event_at_before s events, event_after e events)

    event_at_before p events = case Track.split p events of
        (_, (at, _) : _) | p == at -> p
        ((prev, _) : _, _) -> prev
        _ -> p
    event_after p events = maybe Types.big_score fst $
        Seq.head (Track.events_after p events)

damage_to_real :: Ranges.Ranges ScoreTime
    -> Derive.Deriver (Ranges.Ranges RealTime)
damage_to_real r = case Ranges.extract r of
    Nothing -> return Ranges.everything
    Just rs -> Ranges.sorted_ranges <$>
        mapM (\(s, e) -> (,) <$> convert s <*> convert e) rs
    where
    convert t
        -- Ugh.  I don't really want to get the RealTime of Types.big_score.
        | t == Types.big_score = return RealTime.max
        | otherwise =  Derive.score_to_real t

damage_to_score :: Ranges.Ranges RealTime
    -> Derive.Deriver (Ranges.Ranges ScoreTime)
damage_to_score r = case Ranges.extract r of
    Nothing -> return Ranges.everything
    Just rs -> Ranges.sorted_ranges <$>
        mapM (\(s, e) -> (,) <$>
            Derive.real_to_score s <*> Derive.real_to_score e) rs

-- * types

-- when the types can be put in their own module, these should go there

-- | Constructor for ScoreDamage.
--
-- Updating a track will damage not only the track itself, but also the blocks
-- the track belongs to.
score_damage :: State.State -> State.State -> [Update.Update]
    -> Derive.ScoreDamage
score_damage ui_from ui_to updates =
    Derive.ScoreDamage tracks track_blocks blocks
    where
    -- When track title changes come from the UI they aren't emitted as
    -- Updates, but should still trigger a re-derive.
    track_updates = Diff.track_diff ui_from ui_to
    tracks = Map.fromListWith (<>) $
        Maybe.mapMaybe Update.track_changed (track_updates ++ updates)
    track_blocks = Set.fromList $ map fst $ State.find_tracks track_of_block
        (State.state_blocks ui_to)
    track_of_block (Block.TId tid _) = Map.member tid tracks
    track_of_block _ = False
    blocks = Set.fromList (Maybe.mapMaybe Update.block_changed updates)

lookup_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (CallType derived)
lookup_cache stack (Cache cache) =
    Derive.from_cache_entry =<< Map.lookup stack cache

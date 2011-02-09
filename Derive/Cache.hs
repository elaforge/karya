module Derive.Cache (
    cached_transformer, cached_generator
    , score_damage

    -- for testing
    , find_generator_cache
) where
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Diff as Diff
import qualified Ui.Update as Update

import qualified Derive.Derive as Derive
import Derive.Derive (
    CacheState(..), Cache(..), CacheEntry(..), CallType(..)
    , GeneratorDep(..), TransformerType(..)
    , ScoreDamage(..), DamageRanges, EventDamage(..), ControlDamage(..))
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack


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
-- This doesn't use score or control damage like 'cached_generator' since it
-- evaluates the underlying deriver regardless.
cached_transformer :: (Derive.Derived derived) => Cache -> Stack.Stack
    -> Derive.TransformerCall derived -> Derive.PassedArgs derived
    -> Derive.LogsDeriver derived
    -> Derive.Deriver (Derive.LogsDeriver derived, Maybe Cache)
cached_transformer cache stack (Derive.TransformerCall call ttype)
        args deriver =
    case ttype of
        _ -> return (call args deriver, Nothing)

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


-- * cached_generator

-- | If the given generator has a cache entry, relevant derivation context is
-- the same as the cache entry's, and there is no damage under the generator,
-- I can reuse the cached values for it.  This is effectively a kind of
-- memoization.  If the generator is called, the results will be put in the
-- cache before being returned.
--
-- This is called around each generator, or possibly a group of generators.
cached_generator :: (Derive.Derived derived) => CacheState -> Stack.Stack
    -> Derive.GeneratorCall derived -> Derive.PassedArgs derived
    -> Derive.Deriver (Derive.LogsDeriver derived, Maybe Cache)
cached_generator state stack (Derive.GeneratorCall func gtype _) args =
    case gtype of
        Derive.NonCachingGenerator ->
            non_caching =<< has_damage state stack
        Derive.CachingGenerator -> do
            start <- Derive.now
            end <- Derive.score_to_real 1
            generate $ find_generator_cache stack (Ranges.range start end)
                (state_score_damage state) (state_control_damage state)
                (state_cache state)
    where
    -- A non-caching generator should just be called normally.  However, I need
    -- to emit event damage if it was rederived because of score or control
    -- damage.  Otherwise, damage on a track of non-caching generators would
    -- never produce EventDamage.
    non_caching True = do
        derived <- func args
        -- In the case of samples, this range is not accurate, since the
        -- changed region actually extends to the *next* sample.  But
        -- I don't know what that is here, so rely on the control deriver
        -- to do that.  Ugh, not too pretty.
        Derive.insert_local_damage (EventDamage (derived_range derived))
        return (return derived, Nothing)
    non_caching False = return (func args, Nothing)
    generate (Right (collect, cached)) = do
        Log.debug $ "using cache (" ++ show (LEvent.length cached)
            ++ " vals)"
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Derive.modify_collect (collect <>)
        return (return cached, Nothing)
    generate (Left reason) = do
        (result, collect) <- with_collect (func args)
        cur_cache <- state_cache <$> Derive.get_cache_state
        let stream = either (const mempty) id result
        let new_cache = insert_generator stack collect stream cur_cache
        Log.debug $ "rederived generator because of "
            -- This destroys laziness, though I'm not sure why since the
            -- log msg shouldn't be forced until the msgs already have been
            -- forced themselves.
            -- ++ show (LEvent.length stream) ++ " vals) because of "
            ++ reason ++ case result of
                Left exc -> "  Raised: " ++ show exc
                Right _ -> ""
        case result of
            Left exc -> Error.throwError exc
            Right stream -> return (return stream, Just new_cache)

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_collect deriver = do
        (result, collect) <- Derive.with_empty_collect deriver
        Derive.modify $ \st ->
            st { Derive.state_collect = collect <> Derive.state_collect st }
        return (result, collect)

-- | Sort through the events for the derived chunks and figure out the range
-- they cover.
derived_range :: (Derive.Derived d) => LEvent.LEvents d
    -> Ranges.Ranges RealTime
derived_range events = case (Seq.head derived, Seq.last derived) of
        (Just hd, Just tl) -> Ranges.range
            (fst (Derive.derived_range hd)) (snd (Derive.derived_range tl))
        _ -> Ranges.nothing
    where
    derived = filter (not . Derive.derived_null) $
        Seq.map_maybe LEvent.event events

-- | Figure out if this event lies within damaged range, whether score or
-- control.
--
-- This is called on every non-caching generator, which is most of them, and
-- 'Derive.score_to_real' is already called too often, so I go to some effort
-- to only call it if I already know there isn't score damage.  TODO profile
-- and see if it actually makes a difference.  It's a lazy language, right?
--
-- This is never called for deleted events so it can't get damage for them,
-- but there's a hack for that: 'Derive.Derive.score_to_event_damage'.
has_damage :: CacheState -> Stack.Stack -> Derive.Deriver Bool
has_damage state stack
    | score = return True
    | no_control_damage = return False
    | otherwise = do
        start <- Derive.now
        end <- Derive.score_to_real 1
        return $ case Derive.state_control_damage state of
            ControlDamage dmg -> Ranges.overlapping (Ranges.range start end) dmg
    where
    no_control_damage = Derive.state_control_damage state
        == ControlDamage Ranges.nothing
    score
        -- Scan the stack for damaged tids before converting to UiFrames.
        -- TODO profile and see if this really helps.
        | not (any (`Map.member` damaged_tracks)
            [tid | Stack.Track tid <- Stack.innermost stack]) = False
        | otherwise = any overlapping (Stack.to_ui stack)
    overlapping (_, Just tid, Just (s, e)) =
        case Map.lookup tid damaged_tracks of
            Nothing -> False
            Just range -> Ranges.overlapping range (Ranges.range s e)
    overlapping _ = False
    damaged_tracks = sdamage_tracks (Derive.state_score_damage state)

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
        Seq.map_maybe Update.track_changed (track_updates ++ updates)
    track_blocks = Set.fromList $ map fst $ State.find_tracks track_of_block
        (State.state_blocks ui_to)
    track_of_block (Block.TId tid _) = Map.member tid tracks
    track_of_block _ = False
    blocks = Set.fromList (Seq.map_maybe Update.block_changed updates)

lookup_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (CallType derived)
lookup_cache stack (Cache cache) =
    Derive.from_cache_entry =<< Map.lookup stack cache

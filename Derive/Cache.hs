module Derive.Cache (
    cached_transformer, cached_generator

    -- for testing
    , find_generator_cache, lookup_prefix
) where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import Util.Control
import qualified Util.Map as Map
import qualified Util.Ranges as Ranges

import Ui

import qualified Derive.Derive as Derive
import Derive.Derive (
    CacheState(..), Cache(..), CacheEntry(..), CallType(..)
    , GeneratorDep(..), TransformerType(..)
    , ScoreDamage(..), DamageRanges, EventDamage(..), ControlDamage(..))
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import Util.Debug


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
    -> Derive.Deriver derived
    -> Derive.Deriver
        -- TODO this gets simpler once I integrate TypeError into DeriveError
        (Either TrackLang.TypeError (Derive.Deriver derived), Maybe Cache)
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
            Nothing -> return (entire_range sub_result, Monoid.mempty)
            Just cached -> do
                local <- Derive.state_local_damage <$> Derive.get_cache_state
                cont <- control_damage control_deps
                return (expand_for_context context cached
                    (Monoid.mappend local cont), cached)
        result <- recompute_regions damage call args cached sub_result
        case result of
            Left err -> return (Left err, Nothing)
            Right new_deriver -> do
                -- Expanded modified area should replace existing damage.
                Derive.put_local_damage damage
                derived <- new_deriver
                let new_cache = put_transformer_cache stack derived cache
                return (Right (return derived), Just new_cache)

-- | Get the deps of the last evaluated call.  See 'Derive.state_local_dep'
-- for details.
get_recorded_tdep :: Derive.Deriver TransformerDep
get_recorded_tdep = undefined
    -- gdep <- state_local_dep <$> Derive.get_cache_state
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
    -> Derive.Deriver
        (Either TrackLang.TypeError (Derive.Deriver derived), Maybe Cache)
cached_generator state stack (Derive.GeneratorCall func gtype) args =
    case gtype of
        Derive.NonCachingGenerator
            | range_damaged score_damage stack event_range -> case func args of
                Left err -> return (Left err, Nothing)
                Right deriver -> do
                    derived <- deriver
                    -- This damage is not totally accurate, since the changed
                    -- region actually extends to the *next* sample.  But I
                    -- don't know what that is here, so rely on the control
                    -- deriver to do that.  Ugh, not too pretty.
                    Derive.insert_local_damage
                        (EventDamage (Derive.derived_range derived))
                    return (Right (return derived), Nothing)
            | otherwise -> return (func args, Nothing)
            -- To get the actual damaged region, either the call needs to
            -- declare it, or I have to inspect the returned samples directly.
            -- This goes for CachingGenerator below as well, only 0-1 works
            -- because I expect to use it only for blocks and tracks.
            --
            -- Even so, this is going to wind up damaging everything unless I
            -- only emit damage for the control in the score damage range.
            -- That makes it cheaper too...
        Derive.CachingGenerator -> do
            start <- Derive.now
            end <- Derive.score_to_real 1
            Derive.with_msg "cached generator" $ generate $
                find_generator_cache stack (Ranges.range start end)
                    (state_score_damage state) (state_control_damage state)
                    (state_cache state)
    where
    score_damage = Derive.state_score_damage state
    event_range = uncurry Ranges.range $
        Derive.info_track_pos (Derive.passed_info args)
    generate (Right (gdep, cached)) = do
        Derive.debug $ "using cache (" ++ show (Derive.derived_length cached)
            ++ " vals)"
        -- The cached deriver still has the same deps as it would if it had
        -- been actually derived.
        Derive.modify_cache_state $ \st -> st { Derive.state_local_dep =
            Monoid.mappend gdep (Derive.state_local_dep st) }
        return (Right (return cached), Nothing)
    generate (Left reason) = case func args of
        Left err -> return (Left err, Nothing)
        Right deriver -> do
            (derived, local_dep) <- with_local_dep deriver
            cur_cache <- state_cache <$> Derive.get_cache_state
            let new_cache = insert_generator stack local_dep derived cur_cache
            Derive.debug $ "rederived generator ("
                ++ show (Derive.derived_length derived) ++ " vals) because of "
                ++ reason
            return (Right (return derived), Just new_cache)

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_local_dep deriver = do
        (derived, local_dep) <- Derive.with_local_dep $ do
            derived <- deriver
            -- It's essential that the evaluation happens above.  Not only do
            -- I need the derived to cache, I need it to put control deps in
            -- the cache so I can compute the new deps.
            local_dep <- state_local_dep <$> Derive.get_cache_state
            return (derived, local_dep)
        Derive.modify_cache_state $ \st -> st { Derive.state_local_dep =
            Monoid.mappend local_dep (Derive.state_local_dep st) }
        return (derived, local_dep)

range_damaged :: ScoreDamage -> Stack.Stack -> Ranges.Ranges ScoreTime -> Bool
range_damaged score_damage stack event_range = case track_ids of
    [] -> False
    tid : _ -> case Map.lookup tid (sdamage_tracks score_damage) of
        Nothing -> False
        Just range -> Ranges.overlapping event_range range
    where track_ids = [tid | Stack.Track tid <- Stack.innermost stack]

find_generator_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Ranges.Ranges RealTime -> ScoreDamage -> ControlDamage
    -> Cache -> Either String (GeneratorDep, derived)
find_generator_cache stack event_range (ScoreDamage _ damaged_blocks _)
        (ControlDamage control_damage) cache = do
    (gdep@(GeneratorDep block_deps), cached) <-
        maybe (Left "not in cache") Right (lookup_generator stack cache)
    unless (Set.null (Set.intersection damaged_blocks block_deps)) $
        Left "sub-block damage"
    when (Ranges.overlapping control_damage event_range) $
        Left "control damage"
    return (gdep, cached)

lookup_generator :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (GeneratorDep, derived)
lookup_generator stack cache = do
    ctype <- lookup_cache stack cache
    case ctype of
        CachedGenerator dep derived -> Just (dep, derived)
        _ -> Nothing

insert_generator :: (Derive.Derived derived) =>
    Stack.Stack -> GeneratorDep -> derived -> Cache -> Cache
insert_generator stack dep derived (Cache cache) =
    Cache $ Map.insert stack entry cache
    where
    -- TODO clear out other bits of cache that this overlaps with
    entry = Derive.to_cache_entry (CachedGenerator dep derived)

-- * types

-- when the types can be put in their own module, these should go there

lookup_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (CallType derived)
lookup_cache stack (Cache cache) =
    Derive.from_cache_entry =<< Map.lookup stack cache

lookup_prefix :: Stack.Stack -> Cache -> [([Stack.Frame], CacheEntry)]
lookup_prefix stack (Cache cache) =
    takeWhile ((frames `List.isPrefixOf`) . fst) $ map (first Stack.outermost) $
        Map.assocs $ snd $ Map.split2 stack cache
    where frames = Stack.outermost stack

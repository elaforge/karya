{-# LANGUAGE CPP #-}
module Derive.Cache (
    caching_call
    , get_control_damage, get_tempo_damage

#ifdef TESTING
    , find_generator_cache
#endif
) where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Events as Events
import qualified Ui.Track as Track

import qualified Derive.Derive as Derive
import Derive.Derive
       (Cache(..), CallType, ScoreDamage(..), ControlDamage(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack


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
    st <- Derive.get
    let cdamage = Derive.state_control_damage (Derive.state_dynamic st)
        sdamage = Derive.state_score_damage (Derive.state_constant st)
        stack = Derive.state_stack (Derive.state_dynamic st)
    generate stack $ find_generator_cache stack (Ranges.range start end)
        sdamage cdamage (Derive.state_cache (Derive.state_constant st))
    where
    generate _ (Right (collect, cached)) = do
        Log.debug $ "using cache (" ++ show (LEvent.length cached) ++ " vals)"
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Internal.merge_collect collect
        return cached
    generate stack (Left reason) = do
        (result, collect) <- with_collect (call args)
        Log.notice $ "rederived generator because of "
            -- This destroys laziness, though I'm not sure why since the
            -- log msg shouldn't be forced until the msgs already have been
            -- forced themselves.
            -- ++ show (LEvent.length stream) ++ " vals) because of "
            ++ reason
        Internal.merge_collect $
            mempty { Derive.collect_cache = make_cache stack collect result }
        return result

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_collect deriver = do
        -- TODO Do I want to run deriver a sub derivation so I can put an
        -- empty cache if it failed?  Otherwise I think maybe a failed
        -- event will continue to produce its old value.
        (result, collect) <- Internal.with_empty_collect deriver
        Derive.modify $ \st ->
            st { Derive.state_collect = collect <> Derive.state_collect st }
        return (result, collect)

find_generator_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Ranges.Ranges RealTime -> ScoreDamage -> ControlDamage
    -> Cache -> Either String (Derive.Collect, LEvent.LEvents derived)
find_generator_cache stack event_range score_damage
        (ControlDamage control_damage) cache = do
    (collect, stream) <- maybe (Left "not in cache") Right
        (lookup_cache stack cache)
    let Derive.GeneratorDep block_deps = Derive.collect_local_dep collect
    let damaged_blocks = Set.union
            (sdamage_track_blocks score_damage) (sdamage_blocks score_damage)
    unless (Set.null (Set.intersection damaged_blocks block_deps)) $
        Left "sub-block damage"
    when (Ranges.overlapping control_damage event_range) $
        Left "control damage"
    return (collect, stream)

make_cache :: (Derive.Derived d) => Stack.Stack -> Derive.Collect
    -> LEvent.LEvents d -> Cache
make_cache stack collect stream = Cache $ Map.singleton stack entry
    where
    -- TODO clear out other bits of cache that this overlaps with
    -- TODO filter log msgs so I don't get logs about cache misses back with
    -- the cache hit.  This is unsatisfactory because it copies the stream.
    -- Better solution?
    stripped = collect { Derive.collect_cache = mempty }
    entry = Derive.to_cache_entry (stripped, filter is_event stream)
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
get_control_damage :: TrackId -> (ScoreTime, ScoreTime)
    -> Derive.Deriver ControlDamage
get_control_damage track_id track_range = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    extend_damage track_id track_range =<< if control == mempty
        then score_to_control track_id track_range score
        else return control

-- | Since the warp is the integral of the tempo track, damage on the tempo
-- track will affect all events after it.
get_tempo_damage :: TrackId -> (ScoreTime, ScoreTime)
    -> Derive.Deriver ControlDamage
get_tempo_damage track_id track_range = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    extend =<< if control == mempty
        then score_to_control track_id track_range score
        else return control
    where
    extend (Derive.ControlDamage ranges) = Derive.ControlDamage <$>
        case Ranges.extract ranges of
            Nothing -> return Ranges.everything
            Just [] -> return Ranges.nothing
            Just ((s, _) : _) ->
                Ranges.range s <$> Derive.real (snd track_range)

score_to_control :: TrackId -> (ScoreTime, ScoreTime) -> ScoreDamage
    -> Derive.Deriver ControlDamage
score_to_control track_id track_range score =
    ControlDamage <$> damage_to_real damage
    where
    damage = in_range $ maybe Ranges.nothing id $
        Map.lookup track_id (Derive.sdamage_tracks score)
    in_range = Ranges.intersection $ uncurry Ranges.range track_range

-- | Extend the given ControlDamage as described in 'get_control_damage'.
-- Somewhat tricky because I also want to clip the damage to the track range,
-- if any.  This is so a sliced control track below an unsliced one won't
-- bother figuring out damage outside its range.
extend_damage :: TrackId -> (ScoreTime, ScoreTime) -> ControlDamage
    -> Derive.Deriver ControlDamage
extend_damage track_id (track_s, track_e) (ControlDamage damage)
    | damage == mempty = return (ControlDamage damage)
    | otherwise = do
        events <- Track.track_events <$> Derive.get_track track_id
        -- Empty tracks could not have contributed to further damage.
        if events == Events.empty
            then return (ControlDamage damage)
            else do
                sdamage <- damage_to_score damage
                ControlDamage <$> damage_to_real (extend sdamage events)
    where
    extend damage events = Ranges.fmap (extend1 events) damage
    extend1 events (s, e)
        | s >= track_s && e <= track_e =
            Just (event_at_before s events, event_after e events)
        | otherwise = Nothing
    event_at_before p events = case Events.split p events of
        (_, (at, _) : _) | p == at -> p
        ((prev, _) : _, _) -> prev
        _ -> p
    event_after p events = maybe track_e fst $ Seq.head (Events.after p events)

damage_to_real :: Ranges.Ranges ScoreTime
    -> Derive.Deriver (Ranges.Ranges RealTime)
damage_to_real r = case Ranges.extract r of
    Nothing -> return Ranges.everything
    Just rs -> Ranges.sorted_ranges <$>
        mapM (\(s, e) -> (,) <$> Derive.real s <*> Derive.real e) rs

damage_to_score :: Ranges.Ranges RealTime
    -> Derive.Deriver (Ranges.Ranges ScoreTime)
damage_to_score r = case Ranges.extract r of
    Nothing -> return Ranges.everything
    Just rs -> Ranges.sorted_ranges <$>
        mapM (\(s, e) -> (,) <$> Derive.score s <*> Derive.score e) rs

-- * types

lookup_cache :: (Derive.Derived derived) =>
    Stack.Stack -> Cache -> Maybe (CallType derived)
lookup_cache stack (Cache cache) =
    Derive.from_cache_entry =<< Map.lookup stack cache

{-# LANGUAGE CPP #-}
module Derive.Cache (
    cache_block, cache_track
    , get_control_damage, get_tempo_damage
    , is_cache_log

#ifdef TESTING
    , find_generator_cache
    , _extend_control_damage
#endif
) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track

import qualified Derive.Args as Args
import qualified Derive.Derive as Derive
import Derive.Derive
       (Cache(..), Cached(..), ScoreDamage(..), ControlDamage(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Stack as Stack

import Types


-- * cache_block

-- | Unfortunately caching is not entirely general, and cache invalidation
-- works a bit differently for blocks and tracks.
data Type = Block
    -- | Cache a track.  The set is its TrackId along with its children, so
    -- it knows what sort of damage will invalidate the cache.  If the track
    -- has children, it's assumed to be inverting, so that it depends on all of
    -- its children.
    | Track (Set.Set TrackId)
    deriving (Eq, Show)

-- | If the given generator has a cache entry, relevant derivation context is
-- the same as the cache entry's, and there is no damage under the generator,
-- I can reuse the cached values for it.  This is effectively a kind of
-- memoization.  If the generator is called, the results will be put in the
-- cache before being returned.
cache_block :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
cache_block call args = caching_deriver Block range (call args)
    where range = uncurry Ranges.range (Args.range_on_track args)

cache_track :: (Derive.Derived d) => Set.Set TrackId
    -> Derive.LogsDeriver d -> Derive.LogsDeriver d
cache_track children = caching_deriver (Track children) Ranges.everything

caching_deriver :: (Derive.Derived d) => Type -> Ranges.Ranges ScoreTime
    -> Derive.LogsDeriver d -> Derive.LogsDeriver d
caching_deriver typ range call = do
    st <- Derive.get
    let cdamage = Derive.state_control_damage (Derive.state_dynamic st)
        sdamage = Derive.state_score_damage (Derive.state_constant st)
        stack = Derive.state_stack (Derive.state_dynamic st)
    generate stack $ find_generator_cache typ stack range
        sdamage cdamage (Derive.state_cache (Derive.state_constant st))
    where
    generate _ (Right (collect, cached)) = do
        Log.debug $ cached_msg (LEvent.length cached)
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Internal.merge_collect collect
        return cached
    generate stack (Left (was_control_damage, reason)) = do
        (result, collect) <- with_collect was_control_damage call
        Log.debug $ rederived_msg reason
        Internal.merge_collect $
            mempty { Derive.collect_cache = make_cache stack collect result }
        return result

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    with_collect was_control_damage deriver = do
        -- TODO Do I want to run deriver a sub derivation so I can put an
        -- empty cache if it failed?  Otherwise I think maybe a failed
        -- event will continue to produce its old value.
        (result, collect) <- with_empty_collect
            (typ == Block && was_control_damage) deriver
        Derive.modify $ \st ->
            st { Derive.state_collect = collect <> Derive.state_collect st }
        return (result, collect)

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
with_empty_collect expand_control_damage deriver = do
    old <- Derive.get
    Derive.modify $ \st -> st
        { Derive.state_collect = mempty
        , Derive.state_dynamic = if expand_control_damage
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

find_generator_cache :: (Derive.Derived derived) => Type
    -> Stack.Stack -> Ranges.Ranges ScoreTime -> ScoreDamage -> ControlDamage
    -> Cache -> Either (Bool, String) (Derive.Collect, LEvent.LEvents derived)
find_generator_cache typ stack event_range score (ControlDamage control)
        (Cache cache) = do
    cached <- maybe (Left (False, "not in cache")) Right $
        Map.lookup stack cache
    Derive.CallType collect stream <- case cached of
        Invalid -> Left (False, "cache invalidated by score damage")
        Cached entry -> maybe (Left (False, "cache has wrong type")) Right $
            Derive.from_cache_entry entry
    let Derive.BlockDeps block_deps = Derive.collect_block_deps collect
    let damaged_blocks = Set.union
            (sdamage_track_blocks score) (sdamage_blocks score)
    case typ of
        Block -> case msum (map Stack.block_of (Stack.innermost stack)) of
            Just this_block | this_block `Set.member` damaged_blocks ->
                Left (False, "block damage")
            _ -> return ()
        Track children
            | any (`Set.member` children) (Map.keys (sdamage_tracks score)) ->
                Left (False, "track damage")
            | otherwise -> return ()
    unless (Set.null (Set.intersection damaged_blocks block_deps)) $
        Left (False, "sub-block damage")
    when (Ranges.overlapping control event_range) $
        Left (True, "control damage")
    return (collect, stream)

make_cache :: (Derive.Derived d) => Stack.Stack -> Derive.Collect
    -> LEvent.LEvents d -> Cache
make_cache stack collect stream = Cache $ Map.singleton stack (Cached entry)
    where
    stripped = collect
        { Derive.collect_cache = mempty
        -- Integration only happens for toplevel blocks, so there's no point
        -- returning it from a cached block call.  Also, integration shouldn't
        -- happen if the cache is reused, since that means nothing changed.
        -- So this reduces unnecessary integration as well.
        , Derive.collect_integrated = []
        }
    entry = Derive.to_cache_entry $
        Derive.CallType stripped $ filter (not . cache_log) stream
    -- I do want a cached chunk to retain its log msgs, since those include
    -- errors deriving.  However, it's confusing if it also includes cache
    -- msgs because then it looks like it wasn't cached after all.
    -- It's unfortunate that I have to copy the chunk, but it's either this
    -- or a more complicated filtering scheme later on, which is bound to
    -- be just a filter too.  At least this way it only happens once.
    cache_log = LEvent.either (const False) is_cache_log

cached_msg :: Int -> String
cached_msg ncached = "using cache, " ++ show ncached ++ " vals"

rederived_msg :: String -> String
rederived_msg reason = "rederived generator because of " ++ reason

-- | This is a terrible hack so the log msgs from caching can be treated
-- differently from other log msgs.  Perhaps log msgs should have a general
-- purpose field for tags like this?
is_cache_log :: Log.Msg -> Bool
is_cache_log msg = prefix "using cache, " || prefix "rederived generator "
    where prefix = (`Text.isPrefixOf` Log.msg_text msg)


-- * get_control_damage

-- | ControlDamage works in this manner:
--
-- The way the damage is calculated is complicated.  Firstly, a track with
-- no ControlDamage in scope has its ControlDamage calculated from the
-- ScoreDamage (this is guaranteed to be a control track simply because this
-- function is only called by control tracks).  Secondly, given some
-- ControlDamage, the range must be expanded to the neighbor events.  This is
-- because controls can depend on other controls, so a certain range of
-- ControlDamage may cause other controls to rederived.  Controls generally
-- generate samples based on their previous and next events.
--
-- TODO of course this isn't guaranteed, a control that depends on several
-- previous ones, of which there are many, will fail to expand the control
-- damage enough.  But so far no control calls depend on other controls, so
-- this doesn't come up in practice.  This area needs some thought.  One option
-- is to skip this sketchy expansion stuff, and just say control damage is just
-- a flag that causes everything underneath to rederive.  But that means that
-- editing any control track will be like tempo, i.e. the entire score derives.
-- The cache doesn't actually have to be 100% accurate, it can be mostly
-- accurate and still save lots of time.  But if a control is going to affect
-- another it's mostly likely that it's a parameter, e.g. vibrato speed, in
-- which case I think expansion is the right thing.
--
-- If a block call is touched by control damage, the the control damage expands
-- to cover the entire block.
get_control_damage :: TrackId
    -> (ScoreTime, ScoreTime) -- ^ track_range must be passed explicitly
    -- because the event may have been sliced and shifted, but ControlDamage
    -- should be relative to the start of the track at ScoreTime 0.
    -> Derive.Deriver ControlDamage
get_control_damage track_id track_range = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    extend_damage track_id track_range $
        control <> score_to_control track_id score

-- | Since the warp is the integral of the tempo track, damage on the tempo
-- track will affect all events after it.  Actually, the damage extends from
-- the previous event to the end of the track, since interpolating calls extend
-- from the previous event.
--
-- It would be simpler to have any edit invalidate the whole track, but it
-- seems like editing a score at the end is a common case, and it would be
-- a shame to rederive the entire score on each edit when only the very end has
-- changed.
get_tempo_damage :: TrackId -> (ScoreTime, ScoreTime) -> Events.Events
    -> Derive.Deriver ControlDamage
get_tempo_damage track_id track_range events = do
    st <- Derive.get
    let control = Derive.state_control_damage (Derive.state_dynamic st)
        score = Derive.state_score_damage (Derive.state_constant st)
    return $ extend $ control <> score_to_control track_id score
    where
    extend (Derive.ControlDamage ranges) = Derive.ControlDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> case Events.split s events of
                (prev : _, _) ->
                    Ranges.range (Event.start prev) (snd track_range)
                ([], _) -> Ranges.range s (snd track_range)

-- | Convert score damage directly to ControlDamage on a given track.
score_to_control :: TrackId -> ScoreDamage -> ControlDamage
score_to_control track_id score =
    ControlDamage $ fromMaybe Ranges.nothing $
        Map.lookup track_id (Derive.sdamage_tracks score)

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
_extend_control_damage track_end events = Ranges.fmap (extend1 events)
    where
    extend1 events (s, e) = (event_at_before s events, event_after e events)
    event_at_before p events = case Events.split p events of
        (_, at : _) | p == Event.start at -> p
        (prev : _, _) -> Event.start prev
        _ -> p
    event_after p events = maybe track_end Event.start $
        Seq.head (Events.after p events)

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
module Derive.Cache (
    Cacheable(..)
    , block, track
    , getControlDamage, getTempoDamage
    , isCacheLog, cacheHitEvents, cacheMissReason
    -- * debugging
    , prettyCache

#ifdef TESTING
    , findGeneratorCache
    , _extendControlDamage
#endif
) where
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Ranges as Ranges

import qualified Derive.Args as Args
import qualified Derive.Derive as Derive
import           Derive.Derive
    (Cache(..), Cached(..), ControlDamage(..), ScoreDamage(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream

import qualified Perform.Signal as Signal
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import           Global
import           Types


class Cacheable d where
    fromCacheEntry :: Derive.CacheEntry -> Maybe (Derive.CallType d)
    toCacheEntry :: Derive.CallType d -> Derive.CacheEntry

instance Cacheable Score.Event where
    fromCacheEntry (Derive.CachedEvents ctype) = Just ctype
    fromCacheEntry _ = Nothing
    toCacheEntry = Derive.CachedEvents

instance Cacheable Signal.Control where
    fromCacheEntry (Derive.CachedControl ctype) = Just ctype
    fromCacheEntry _ = Nothing
    toCacheEntry = Derive.CachedControl

instance Cacheable PSignal.PSignal where
    fromCacheEntry (Derive.CachedPitch ctype) = Just ctype
    fromCacheEntry _ = Nothing
    toCacheEntry = Derive.CachedPitch

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
block blockId call args = cachingDeriver (Block blockId) range (call args)
    where
    range = Range
        { _start = fst $ Args.rangeOnTrack args
        , _end = snd $ Args.rangeOnTrack args
        , _negative = Event.isNegative (Args.event args)
        }

data Range = Range
    { _start :: !TrackTime
    , _end :: !TrackTime
    -- | If True, these are the ranges of a negative event, which affects how
    -- control damage works.
    , _negative :: !Bool
    } deriving (Show)

-- | Cache a track, but only if it's not sliced and has a TrackId.
track :: Cacheable d => TrackTree.Track -> Set TrackId
    -- ^ Children, as documented in 'Track'.
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
track track children deriver = case shouldCache track of
    Just trackId -> cachingDeriver (Track trackId children) range deriver
    Nothing -> deriver
    where
    range = Range
        { _start = TrackTree.trackStart track
        , _end = TrackTree.trackEnd track
        , _negative = False
        }

shouldCache :: TrackTree.Track -> Maybe TrackId
shouldCache track
    | TrackTree.trackSliced track == TrackTree.NotSliced =
        TrackTree.trackId track
    | otherwise = Nothing

cachingDeriver :: Cacheable d => Type -> Range
    -> Derive.Deriver (Stream.Stream d) -> Derive.Deriver (Stream.Stream d)
cachingDeriver typ range call = do
    st <- Derive.get
    let cdamage = Derive.stateControlDamage (Derive.stateDynamic st)
        sdamage = Derive.stateScoreDamage (Derive.stateConstant st)
        stack = Derive.stateStack (Derive.stateDynamic st)
    generate stack $ findGeneratorCache typ (Derive.CacheKey stack) range
        sdamage cdamage (Derive.stateCache (Derive.stateConstant st))
    where
    generate _ (Right (collect, cached)) = do
        Log.write $ cacheHitMsg cached
        stats <- hitStats typ range
        -- The cached deriver must return the same collect as it would if it
        -- had been actually derived.
        Internal.mergeCollect $
            mempty { Derive.collectCacheStats = stats } <> collect
        pure cached
    generate stack (Left (inflictControlDamage, reason)) = do
        (result, collect) <- withCollect inflictControlDamage call
        Log.write $ cacheMissMsg reason
        Internal.mergeCollect $ mempty
            { Derive.collectCache =
                makeCache (Derive.CacheKey stack) collect result
            }
        pure result

    -- To get the deps of just the deriver below me, I have to clear out
    -- the local deps.  But this call is itself collecting deps for another
    -- call, so I have to merge the sub-deps back in before returning.
    withCollect inflictControlDamage deriver = do
        -- TODO Do I want to run deriver a sub derivation so I can put an
        -- empty cache if it failed?  Otherwise I think maybe a failed
        -- event will continue to produce its old value.
        (result, collect) <- withEmptyCollect
            (isBlock typ && inflictControlDamage) deriver
        Internal.mergeCollect collect
        pure (result, collect)
    isBlock (Block {}) = True
    isBlock _ = False

hitStats :: Type -> Range -> Derive.Deriver Derive.CacheStats
hitStats typ (Range start end _) = do
    start <- Derive.real start
    end <- Derive.real end
    pure $ mempty
        { Derive.cstatsHits = [(id, (start, end))]
        }
    where
    id = case typ of
        Block blockId -> Left blockId
        Track trackId _ -> Right trackId

-- real_range :: Ranges.Ranges ScoreTime -> Derive.Deriver (RealTime, RealTime)
-- real_range ranges = case Ranges.extract ranges of
--     undefined

-- | Both track warps and local deps are used as dynamic return values (aka
-- modifying a variable to \"return\" something).  When evaluating a cached
-- generator, the caller wants to know the callee's track warps and local
-- deps, without getting them mixed up with its own warps and deps.  So run
-- a deriver in an empty environment, and restore it afterwards.
withEmptyCollect :: Bool
    -- ^ If True, expand the ControlDamage to Ranges.everything.  If
    -- ControlDamage touches a block call then it likely invalidates everything
    -- within that block.
    -> Derive.Deriver a -> Derive.Deriver (a, Derive.Collect)
withEmptyCollect inflictControlDamage deriver = do
    old <- Derive.get
    Derive.modify $ \st -> st
        { Derive.stateCollect = mempty
        , Derive.stateDynamic = if inflictControlDamage
            then (Derive.stateDynamic st)
                { Derive.stateControlDamage =
                    Derive.ControlDamage Ranges.everything
                }
            else Derive.stateDynamic st
        }
    result <- deriver
    collect <- Derive.gets Derive.stateCollect
    Derive.put old
    pure (result, collect)

-- | Find the cached value, or a reason why there is no cache entry.  This
-- is the function that determines whether you hit the cache or not.
findGeneratorCache :: Cacheable d => Type -> Derive.CacheKey -> Range
    -> ScoreDamage -> ControlDamage -> Cache
    -> Either (Bool, Text) (Derive.Collect, Stream.Stream d)
    -- ^ on a miss, the Bool is 'with_empty_collect's inflict_control_damage arg
findGeneratorCache typ key range score (ControlDamage control) (Cache cache)
        = do
    cached <- justErr (False, "not in cache") $ Map.lookup key cache
    -- Look for block damage before looking for Invalid, because if there is
    -- block damage I inflict control damage too.  This is because block damage
    -- means things like a block title change, or skeleton change, and those
    -- can invalidate all blocks called from this one.
    let stack = Stack.innermost (Derive.keyStack key)
    case typ of
        Block _blockId -> case msum (map Stack.blockOf stack) of
            Just thisBlock
                | thisBlock `Set.member` sdamageBlocks score ->
                    Left (True, "direct block damage")
                | thisBlock `Set.member` sdamageTrackBlocks score ->
                    Left (False, "track block damage")
            _ -> pure ()
        Track _trackId children
            | any (`Set.member` children) (Map.keys (sdamageTracks score)) ->
                Left (False, "track damage")
            | otherwise -> pure ()
    Derive.CallType collect stream <- case cached of
        Invalid -> Left (False, "cache invalidated by score damage")
        Cached entry -> justErr (False, "cache has wrong type") $
            fromCacheEntry entry
    let Derive.BlockDeps blockDeps = Derive.collectBlockDeps collect
    let damagedBlocks = Set.union
            (sdamageTrackBlocks score) (sdamageBlocks score)
    unless (Set.null (Set.intersection damagedBlocks blockDeps)) $
        Left (False, "sub-block damage")
    -- Negative duration indicates an arrival note.  The block deriver then
    -- takes the controls from the bottom of event (which is the start),
    -- see "Derive.Call.Block".
    let overlapping = if _negative range then Ranges.overlappingClosed
            else Ranges.overlapping
    when (overlapping control (Ranges.range (_start range) (_end range))) $
        Left (True, "control damage")
    pure (collect, stream)

-- | Make a single cache entry.  This will go into 'Derive.collect_cache'
-- and be merged in with the rest.
makeCache :: Cacheable d => Derive.CacheKey -> Derive.Collect
    -> Stream.Stream d -> Cache
makeCache key collect stream = Cache $ Map.singleton key (Cached entry)
    where
    stripped = collect
        { Derive.collectCache = mempty
        -- integration shouldn't happen if the cache is reused, since that
        -- means nothing changed.  So this reduces unnecessary reintegration.
        , Derive.collectIntegrated = []
        -- Use Map.map instead of fmap, since fmap is lazy.
        , Derive.collectTrackDynamic =
            Map.map Derive.stripDynamic (Derive.collectTrackDynamic collect)
        , Derive.collectTrackDynamicInverted =
            Map.map Derive.stripDynamic
                (Derive.collectTrackDynamicInverted collect)
        }
    entry = toCacheEntry $ Derive.CallType stripped $
        Stream.fromSortedList $ filter (not . cacheLog) $
        Stream.toList stream
    -- I do want a cached chunk to retain its log msgs, since those include
    -- errors deriving.  However, it's confusing if it also includes cache
    -- msgs because then it looks like it wasn't cached after all.
    -- It's unfortunate that I have to copy the chunk, but it's either this
    -- or a more complicated filtering scheme later on, which is bound to
    -- be just a filter too.  At least this way it only happens once.
    cacheLog = LEvent.either (const False) isCacheLog

-- * logs

isCacheLog :: Log.Msg -> Bool
isCacheLog msg =
    Maybe.isJust (cacheHitEvents msg) || Maybe.isJust (cacheMissReason msg)

cacheHit :: Text
cacheHit = "cache-hit"

cacheHitMsg :: Stream.Stream a -> Log.Msg
cacheHitMsg cached =
    Log.withInt cacheHit events $ Log.msg Log.Debug Nothing $
        "using cache, " <> showt events <> " events, " <> showt logs <> " logs"
    where
    (events, logs) = foldl' count (0, 0) (Stream.toList cached)
    count (!es, !ms) e = if LEvent.isEvent e then (es+1, ms) else (es, ms+1)

-- | Get the number of cached events from a 'cache_hit_msg'.
cacheHitEvents :: Log.Msg -> Maybe Int
cacheHitEvents = Log.lookupInt cacheHit

cacheMiss :: Text
cacheMiss = "cache-miss"

cacheMissMsg :: Text -> Log.Msg
cacheMissMsg reason = Log.withText cacheMiss reason $
    Log.msg Log.Debug Nothing $ "rederived generator because of " <> reason

-- | Get the reason from a 'cache_miss_msg'.
cacheMissReason :: Log.Msg -> Maybe Text
cacheMissReason = Log.lookupText cacheMiss

-- * debugging

-- | Format the cache in a hopefully readable way.
prettyCache :: Derive.Cache -> Text
prettyCache (Derive.Cache cache) = Text.unlines $ concat
    [ Stack.prettyUi_ stack <> ": " : map ("    "<>) (fmt cached) ++ [""]
    | (Derive.CacheKey stack, cached) <- Map.toList cache
    ]
    where
    fmt Derive.Invalid = ["Invalid"]
    fmt (Derive.Cached cached) = case cached of
        Derive.CachedEvents (Derive.CallType _ a) -> Stream.shortEvents a
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
getControlDamage :: BlockId -> TrackId
    -> (ScoreTime, ScoreTime) -- ^ track_range must be passed explicitly
    -- because the event may have been sliced and shifted, but ControlDamage
    -- should be relative to the start of the track at ScoreTime 0.
    -> Derive.Deriver ControlDamage
getControlDamage blockId trackId trackRange = do
    st <- Derive.get
    let control = Derive.stateControlDamage (Derive.stateDynamic st)
        score = Derive.stateScoreDamage (Derive.stateConstant st)
    (control<>) <$> extendDamage trackId trackRange
        (scoreToControl blockId trackId score)

{- | Since the warp is the integral of the tempo track, damage on the tempo
    track will affect all events after it.  Actually, the damage extends from
    the previous event to the end of the track, since interpolating calls extend
    from the previous event.

    It would be simpler to have any edit invalidate the whole track, but it
    seems like editing a score at the end is a common case, and it would be
    a shame to rederive the entire score on each edit when only the very end has
    changed.
-}
getTempoDamage :: BlockId -> TrackId -> TrackTime
    -> Events.Events -> Derive.Deriver ControlDamage
getTempoDamage blockId trackId trackEnd events = do
    st <- Derive.get
    let control = Derive.stateControlDamage (Derive.stateDynamic st)
        score = Derive.stateScoreDamage (Derive.stateConstant st)
    pure $ extend $ control <> scoreToControl blockId trackId score
    where
    extend (Derive.ControlDamage ranges) = Derive.ControlDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> case Events.splitLists s events of
                (prev : _, _) ->
                    Ranges.range (Event.start prev) trackEnd
                ([], _) -> Ranges.range s trackEnd

-- | Convert score damage directly to ControlDamage on a given track.
scoreToControl :: BlockId -> TrackId -> ScoreDamage -> ControlDamage
scoreToControl blockId trackId score = ControlDamage $
    (if blockId `Set.member` Derive.sdamageBlocks score
        then Ranges.everything else Ranges.nothing)
    <> Map.findWithDefault Ranges.nothing trackId (Derive.sdamageTracks score)

-- | Extend the given ControlDamage as described in 'get_control_damage'.
-- Somewhat tricky because I also want to clip the damage to the track range,
-- if any.  This is so a sliced control track below an unsliced one won't
-- bother figuring out damage outside its range.
extendDamage :: TrackId -> (ScoreTime, ScoreTime) -> ControlDamage
    -> Derive.Deriver ControlDamage
extendDamage trackId trackRange (ControlDamage damage)
    | damage == mempty = pure mempty
    | otherwise = do
        events <- Track.trackEvents <$> Derive.getTrack trackId
        pure $ ControlDamage $ extend events
    where
    extend events
        | Events.null events = damage
        | otherwise = Ranges.intersection (uncurry Ranges.range trackRange) $
            _extendControlDamage (snd trackRange) events damage

_extendControlDamage :: ScoreTime -> Events.Events
    -> Ranges.Ranges ScoreTime -> Ranges.Ranges ScoreTime
_extendControlDamage trackEnd events = Ranges.pairMap (extend1 events)
    where
    extend1 events (s, e) = (eventAtBefore s events, eventAfter e events)
    eventAtBefore p events = case Events.splitLists p events of
        (_, at : _) | p == Event.start at -> p
        (prev : _, _) -> Event.start prev
        _ -> p
    eventAfter p events = maybe trackEnd Event.start $
        Lists.head (Events.after p events)

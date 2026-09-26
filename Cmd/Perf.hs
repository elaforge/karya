-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This module is for Cmds that want to peek into the derived score for
    whatever reason.  Normally the flow of information goes from Cmd -> Derive
    -> Perform, but this module is for those cases when Cmd wants to know
    about the results of later stages.
-}
module Cmd.Perf where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Trees as Trees

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.C.Prelude.Block as Prelude.Block
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Eval as Eval
import qualified Derive.EvalTrack as EvalTrack
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.Note
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stream as Stream
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Typecheck as Typecheck

import qualified Midi.Midi as Midi
import qualified Perform.Pitch as Pitch
import qualified Perform.Transport as Transport
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig

import           Global
import           Types


get :: Cmd.M m => BlockId -> m Cmd.Performance
get blockId = Cmd.require ("no performance for " <> pretty blockId)
    =<< Cmd.lookupPerformance blockId

getCurrent :: Cmd.M m => BlockId -> m Cmd.Performance
getCurrent blockId =
    Cmd.require ("no current performance for " <> pretty blockId)
        =<< Map.lookup blockId <$>
            Cmd.gets (Cmd.stateCurrentPerformance . Cmd.statePlay)

lookupRoot :: Cmd.M m => m (Maybe Cmd.Performance)
lookupRoot = justm Ui.lookupRootId Cmd.lookupPerformance

getRoot :: Cmd.M m => m Cmd.Performance
getRoot = Cmd.require "no root performance" =<< lookupRoot

-- * derive

-- | Specialized version of 'derive_expr' for note calls with no arguments.
deriveNoteCall :: Cmd.M m => BlockId -> TrackId -> TrackTime
    -> Expr.Symbol -> m (Either Text [Score.Event], [Log.Msg])
deriveNoteCall blockId trackId pos call =
    deriveExpr blockId trackId pos (Expr.generator0 call)

-- | Derive an expression.
deriveExpr :: (Cmd.M m, Derive.CallableExpr d) => BlockId -> TrackId
    -> TrackTime -> DeriveT.Expr -> m (Either Text [d], [Log.Msg])
deriveExpr blockId trackId pos expr = do
    (result, logs) <- deriveAt blockId trackId
        (Eval.evalOneAt False pos 1 expr)
    pure $ case result of
        Left err -> (Left err, logs)
        Right stream -> (Right events, deriveLogs ++ logs)
            where (events, deriveLogs) = Stream.partition stream

-- | Run an ad-hoc derivation in the context of the given track.
deriveAt :: Cmd.M m => BlockId -> TrackId
    -> Derive.Deriver a -> m (Either Text a, [Log.Msg])
deriveAt blockId trackId =
    fmap (first (first pretty)) . deriveAtExc blockId trackId

deriveAtExc :: Cmd.M m => BlockId -> TrackId
    -> Derive.Deriver a -> m (Either Derive.Error a, [Log.Msg])
deriveAtExc blockId trackId deriver = do
    uiState <- Ui.get
    (constant, aliases) <- PlayUtil.getConstant uiState mempty mempty
    dynamic <- fromMaybe (PlayUtil.initialDynamic aliases) <$>
        findDynamic (blockId, Just trackId)
    let (val, _, logs) = PlayUtil.runWithConstant constant dynamic deriver
    pure (val, logs)

-- | Like 'derive_at_exc', but with no block or track context.  This is for
-- derivations that don't belong to any particular track, e.g. asking for the
-- 'Derive.get_score_duration' of a call.  Since there's no context there's
-- also no cached Dynamic to reuse, so this always starts from
-- 'PlayUtil.initial_dynamic'.  If you want the GLOBAL transform you have to
-- apply 'Derive.C.Prelude.Block.global_transform' yourself.
deriveNoBlock :: Cmd.M m => Derive.Deriver a
    -> m (Either Derive.Error a, [Log.Msg])
deriveNoBlock deriver = do
    uiState <- Ui.get
    (constant, aliases) <- PlayUtil.getConstant uiState mempty mempty
    let (val, _, logs) = PlayUtil.runWithConstant constant
            (PlayUtil.initialDynamic aliases) deriver
    pure (val, logs)

-- | Like 'derive_at', but write logs and throw on a Left.
deriveAtThrow :: Cmd.M m => BlockId -> TrackId -> Derive.Deriver a -> m a
deriveAtThrow blockId trackId deriver = do
    (result, logs) <- deriveAt blockId trackId deriver
    mapM_ Log.write logs
    let prefix = "derive_at " <> pretty blockId <> " " <> pretty trackId
    Cmd.requireRight ((prefix <> ": ") <>) result

-- | Return the NoteDeriver of a particular event on a particular track,
-- or Nothing if the track isn't a NoteTrack.
lookupNoteDeriver :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m (Maybe Derive.NoteDeriver)
lookupNoteDeriver blockId trackId event = do
    Tree.Node track subs <- findTrack blockId trackId
    pure $ case ParseTitle.trackType (TrackTree.trackTitle track) of
        ParseTitle.NoteTrack ->
            Just $ deriveEvent (Derive.Note.trackInfo track subs) event
        _ -> Nothing

deriveEvent :: Derive.CallableExpr d => EvalTrack.TrackInfo d -> Event.Event
    -> Derive.Deriver (Stream.Stream d)
deriveEvent tinfo event = EvalTrack.deriveEvent ctx event
    where ctx = EvalTrack.context tinfo Nothing [] event []
    -- TODO get prev and next events
    -- Also, BlockUtil does 'Derive.with_val EnvKey.block_end' and possibly
    -- does Tempo.with_tempo.  If those things turn out to be important,
    -- I should be able to factor that out of BlockUtil.

findTrack :: Cmd.M m => BlockId -> TrackId -> m TrackTree.EventsNode
findTrack blockId trackId = do
    tree <- TrackTree.blockEventsTree blockId
    Cmd.require ("find_track: "
            <> pretty trackId <> " not in " <> pretty blockId) $
        Trees.find ((== Just trackId) . TrackTree.trackId) tree

-- | Get the environment established by 'Ui.config_global_transform'.
globalEnviron :: Cmd.M m => m Env.Environ
globalEnviron = do
    (result, _, logs) <- PlayUtil.run mempty mempty smuggleEnviron
    mapM_ Log.write logs
    events <- Stream.writeLogs =<< Cmd.requireRight pretty result
    event <- Cmd.require "Perf.global_environ: expected a single Event" $
        Lists.head events
    pure $ Score.eventEnviron event

-- | Smuggle the environ out in an event.  It's annoying to require such
-- shennanigans, rationale in NOTE [transform-without-derive-callable].
smuggleEnviron :: Derive.NoteDeriver
smuggleEnviron = Prelude.Block.globalTransform $ do
    env <- Internal.getDynamic Derive.stateEnviron
    pure $! Stream.fromEvent $!
        Score.emptyEvent { Score.eventEnviron = env }

{- NOTE [transform-without-derive-callable]

    It seems like there should be a way to run a transform without the
    Derive.CallableExpr restriction, which in turn constrains the return type.

    First I need to specify which namespace of transformers to look in, but the
    underlying problem is that e.g. Derive.Transformer Score.Event works with
    Score.Events.  There is a class of them that are agnostic since they only
    change the environ, but some of them do postproc.  I don't have a separate
    namespace for those, so it's impossible to evaluate only them.
-}

-- | A cheap quick derivation that sets up the correct initial state, but
-- runs without the cache and throws away any logs.
derive :: Cmd.M m => Derive.Deriver a -> m (Either Text a)
derive deriver = do
    (val, _, _) <- PlayUtil.run mempty mempty deriver
    pure $ case val of
        Left err -> Left $ pretty err
        Right val -> Right val

-- * perform

perform :: Cmd.M m => [Score.Event] -> m ([Midi.WriteMessage], [Log.Msg])
perform = fmap LEvent.partition . PlayUtil.performRaw
    -- TODO this is midi only, discards sc

-- * environ

-- | Functions that look in the saved Dynamic use this as a key.
--
-- If the TrackId is Nothing, take any old Dynamic found on the given block.
type Track = (BlockId, Maybe TrackId)

{- | Get the scale in scope in a certain track on a certain block.

    This tries really really hard to find a ScaleId.  The reason is that pitch
    track entry uses it to insert pitch names, and if it gets the scale wrong
    it inserts bogus pitches.  I can't rely on 'lookup_val' because a new
    track has never been derived and thus has no Dynamic.
-}
getScaleId :: Cmd.M m => Track -> m Pitch.ScaleId
getScaleId track@(blockId, maybeTrackId) =
    -- Did you know there are so many places to find a ScaleId?  Why are
    -- there so many places?
    (fromMaybe (Pitch.ScaleId Config.defaultScaleId) <$>) $
    firstJust
        (maybe (pure Nothing) (scaleFromTitles blockId) maybeTrackId) $
    firstJust (findScaleId track) $ pure Nothing

findScaleId :: Cmd.M m => Track -> m (Maybe Pitch.ScaleId)
findScaleId (blockId, maybeTrackId) = (toScaleId <$>) $
    firstJust (lookup maybeTrackId) $
    firstJust lookupParents $
    firstJust (lookup Nothing) $
    firstJust (lookupEnvironVal EnvKey.scale =<< globalEnviron) $
    pure Nothing
    where
    toScaleId = fmap Expr.strToScaleId
    lookup maybeTrackId = lookupVal (blockId, maybeTrackId) EnvKey.scale
    lookupParents = case maybeTrackId of
        Nothing -> pure Nothing
        Just trackId -> do
            parents <- map Ui.trackId . maybe [] fst <$>
                TrackTree.parentsChildrenOf blockId trackId
            firstJusts $ map (lookup . Just) parents

getScale :: Cmd.M m => Track -> m Scale.Scale
getScale track = do
    scaleId <- getScaleId track
    Cmd.require ("get_scale: can't find " <> pretty scaleId)
        =<< lookupScale track scaleId

lookupScale :: Cmd.M m => Track -> Pitch.ScaleId -> m (Maybe Scale.Scale)
lookupScale track scaleId = do
    env <- getEnviron track
    lookupScaleEnv env scaleId

lookupScaleEnv :: Cmd.M m => Env.Environ -> Pitch.ScaleId
    -> m (Maybe Scale.Scale)
lookupScaleEnv env scaleId = case lookup env scaleId of
    Nothing -> pure Nothing
    Just (Left err) -> Cmd.throw $ "lookup " <> pretty scaleId <> ": "
        <> pretty err
    Just (Right scale) -> pure $ Just scale
    where
    Derive.LookupScale lookup = Cmd.lookupScale

-- | Try to get a scale from the titles of the parents of the given track.
scaleFromTitles :: Ui.M m => BlockId -> TrackId -> m (Maybe Pitch.ScaleId)
scaleFromTitles blockId trackId = do
    tracks <- maybe [] (uncurry (++)) <$>
        TrackTree.parentsChildrenOf blockId trackId
    pure $ msum $ map scaleOf tracks
    where
    scaleOf track = case ParseTitle.titleToScale (Ui.trackTitle track) of
        Just scaleId | scaleId /= Pitch.emptyScale -> Just scaleId
        _ -> Nothing

-- | Find the instrument in scope.
lookupInstrument :: Cmd.M m => Track -> m (Maybe ScoreT.Instrument)
lookupInstrument track = lookupVal track EnvKey.instrument

-- | Lookup value from the deriver's EnvKey at the given block and (possibly)
-- track.  See 'Derive.TrackDynamic' for details on the limitations here.
--
-- The value is taken first from the root performance, and then the given
-- block's performance if not present in the root performance.  This is so
-- that blocks which are not called from the root at all will still have
-- environ values.
lookupVal :: (Cmd.M m, Typecheck.Typecheck a) => Track -> Env.Key
    -> m (Maybe a)
lookupVal track name = justm (lookupEnviron track) $ lookupEnvironVal name

lookupEnvironVal :: (Ui.M m, Typecheck.Typecheck a) =>
    Env.Key -> Env.Environ -> m (Maybe a)
lookupEnvironVal name env =
    either (Ui.throw . ("Perf.lookup_environ_val: "<>)) pure
        (Env.checkedVal name env)

lookupEnviron :: Cmd.M m => Track -> m (Maybe Env.Environ)
lookupEnviron track = fmap Derive.stateEnviron <$> findDynamic track

getEnviron :: Cmd.M m => Track -> m Env.Environ
getEnviron = fmap (fromMaybe mempty) . lookupEnviron

-- | Try to find the Dynamic for the given block and track, first looking in
-- the root performance, and then in the block's performance.
findDynamic :: Cmd.M m => Track -> m (Maybe Derive.Dynamic)
findDynamic track = lookupRootDynamic track >>= \case
    Just dyn -> pure $ Just dyn
    Nothing -> lookupDynamic (fst track) track

lookupRootDynamic :: Cmd.M m => Track -> m (Maybe Derive.Dynamic)
lookupRootDynamic track =
    justm Ui.lookupRootId $ \rootId -> lookupDynamic rootId track

lookupDynamic :: Cmd.M m => BlockId -> Track -> m (Maybe Derive.Dynamic)
lookupDynamic perfBlockId (blockId, maybeTrackId) =
    maybe Nothing (lookup . Cmd.perfTrackDynamic) <$>
        Cmd.lookupPerformance perfBlockId
    where
    lookup trackDyns = case maybeTrackId of
        Nothing -> do
            (_, dyn) <- List.find ((==blockId) . fst . fst)
                (Map.toAscList trackDyns)
            pure dyn
        Just trackId -> Map.lookup (blockId, trackId) trackDyns

-- * muted instruments

mutedImInstruments :: Cmd.M m => BlockId -> m (Set ScoreT.Instrument)
mutedImInstruments blockId = do
    allocs <- Ui.gets $ UiConfig.configAllocations . Ui.stateConfig
    -- Only im does per-instrument muting, so don't bother if I don't have any
    -- of those.
    muted <- if UiConfig.hasIm allocs
        then getMutedInstrumentTracks blockId
        else pure mempty
    pure $ PlayUtil.mutedInstruments allocs <> muted

getMutedInstrumentTracks :: Cmd.M m => BlockId -> m (Set ScoreT.Instrument)
getMutedInstrumentTracks blockId = do
    muted <- Set.toList <$> PlayUtil.getMutedTracks
    trackInstruments <- Cmd.perfTrackInstruments <$> getCurrent blockId
    pure $ Set.unions $
        map (fromMaybe mempty . (`Map.lookup` trackInstruments)) muted

-- | This is like 'Cmd.perf_track_instruments', except it uses the track titles
-- instead of the performance.  This means it's less accurate, but can work
-- even before there's a performance and might be faster.
inferInstrument :: Cmd.M m => TrackId -> m (Maybe ScoreT.Instrument)
inferInstrument trackId =
    justm (fmap fst . Lists.head <$> Ui.blocksWithTrackId trackId) $
        \blockId -> lookupInstrument (blockId, Just trackId)

-- * default

-- | Get global defaults.
lookupDefaultEnviron :: (Typecheck.Typecheck a, Cmd.M m) =>
    Env.Key -> m (Maybe a)
lookupDefaultEnviron name = do
    -- Eval.eval_transform_expr only applies to things in Derived, so I have to
    -- do this gross hack where I stash the result in a Score.Event.
    -- Ultimately it's because I use the return type to infer the lookup
    -- function, and I want to use Score.Event transformers.  It might be
    -- a better design to figure out the lookup function separately, but
    -- meanwhile this hack should be safe.
    result <- derive smuggleEnviron
    environ <- case result of
        Left err -> Cmd.throw $ caller <> ": " <> err
        Right val -> case Stream.eventsOf val of
            [] -> Cmd.throw $ caller <> " didn't get the fake event it wanted"
            event : _ -> pure $ Score.eventEnviron event
    Cmd.requireRight id $ Env.checkedVal name environ
    where
    caller = "Perf.lookup_default_environ"

getDefaultEnviron :: (Typecheck.Typecheck a, Cmd.M m) => Env.Key -> m a
getDefaultEnviron name =
    Cmd.require ("no default val for " <> pretty name)
        =<< lookupDefaultEnviron name

-- | The default scale established by 'Ui.config_global_transform', or
-- 'Config.default_scale_id' if there is none.
defaultScaleId :: Cmd.M m => m Pitch.ScaleId
defaultScaleId =
    maybe (Pitch.ScaleId Config.defaultScaleId) Expr.strToScaleId <$>
        lookupDefaultEnviron EnvKey.scale


-- * play

-- | Given a block, track, and time, find the realtime at that position.  If
-- the track is Nothing, use the first track that has tempo information.  This
-- is necessary because if a track is muted it will have no tempo, but it's
-- confusing if playing from a muted track fails.
getRealtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m RealTime
getRealtime perf blockId maybeTrackId pos =
    maybe (Cmd.throw $ showt blockId <> " " <> pretty maybeTrackId
            <> " has no tempo information, so it probably failed to derive.")
        pure =<< lookupRealtime perf blockId maybeTrackId pos

lookupRealtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m (Maybe RealTime)
lookupRealtime perf blockId maybeTrackId pos = do
    trackIds <- maybe (Ui.trackIdsOf blockId) (pure . (:[]))
        maybeTrackId
    pure $ msum (map tempo trackIds)
    where tempo tid = Lists.head $ Cmd.perfTempo perf blockId tid pos

-- | Like 'get_realtime', but do multiple at once.
getRealtimes :: Cmd.Performance -> BlockId -> TrackId -> [ScoreTime]
    -> [(ScoreTime, RealTime)]
getRealtimes perf blockId trackId ps =
    [(p, t) | (p, t : _)
        <- zip ps (map (Cmd.perfTempo perf blockId trackId) ps)]

getInverseTempo :: Cmd.M m => BlockId -> m Transport.InverseTempoFunction
getInverseTempo blockId =
    TrackWarp.inverseTempoFunc . Cmd.perfWarps <$> get blockId

-- | Take a RealTime to all the ScoreTimes it corresponds to, if any.
findPlayPos :: Ui.M m => Transport.InverseTempoFunction
    -> RealTime -> m [(ViewId, [(TrackNum, ScoreTime)])]
findPlayPos invTempo = blockPosToPlayPos . invTempo Transport.StopAtEnd

-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
--
-- This function has to be careful to not throw on non-existent IDs, because
-- it's called from the monitor_loop.
blockPosToPlayPos :: Ui.M m => [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
blockPosToPlayPos = concatMapM convert

convert :: Ui.M m => (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
convert (blockId, trackPos) = do
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    Ui.lookupBlock blockId >>= \case
        Nothing -> pure []
        Just block -> pure [(viewId, tracknumPos) | viewId <- viewIds]
            where tracknumPos = concatMap (tracknumsOf block) trackPos

tracknumsOf :: Block.Block -> (TrackId, ScoreTime) -> [(TrackNum, ScoreTime)]
tracknumsOf block (trackId, pos) =
    [ (tracknum, pos)
    | (tracknum, Block.TId tid _)
        <- zip [0..] (Block.blockTracklikeIds block)
    , tid == trackId
    ]

-- ** find score times

-- | Give a block and score time, return the play position on sub-blocks.
-- The block itself is filtered out of the result.
subPos :: Cmd.M m => BlockId -> TrackId -> ScoreTime
    -> m [(BlockId, [(TrackId, ScoreTime)])]
subPos blockId trackId pos = fmap (fromMaybe []) $
    justm (Cmd.lookupPerformance blockId) $ \perf ->
    justm (lookupRealtime perf blockId (Just trackId) pos) $ \real ->
    pure $ Just $ filter ((/=blockId) . fst) $
        Cmd.perfInvTempo perf Transport.StopAtEnd real
    -- lookup_realtime gives Nothing if there's no tempo available which is
    -- likely for a newly added track.  Return [] in that case.

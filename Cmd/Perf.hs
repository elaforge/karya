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
import qualified Data.Tree as Tree
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Tree

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.TrackTree as TrackTree
import qualified Ui.Ui as Ui

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Prelude.Block as Prelude.Block
import qualified Derive.Derive as Derive
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
import qualified Derive.Stream as Stream
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Global
import Types


-- * derive

-- | Specialized version of 'derive_expr' for note calls with no arguments.
derive_note_call :: Cmd.M m => BlockId -> TrackId -> TrackTime
    -> Expr.Symbol -> m (Either Text [Score.Event], [Log.Msg])
derive_note_call block_id track_id pos call =
    derive_expr block_id track_id pos (Expr.generator0 call)

-- | Derive an expression.
derive_expr :: (Cmd.M m, Derive.Callable d) => BlockId -> TrackId -> TrackTime
    -> BaseTypes.Expr -> m (Either Text [d], [Log.Msg])
derive_expr block_id track_id pos expr = do
    (result, logs) <- derive_at block_id track_id
        (Eval.eval_one_at False pos 1 expr)
    return $ case result of
        Left err -> (Left err, logs)
        Right stream -> (Right events, derive_logs ++ logs)
            where (events, derive_logs) = Stream.partition stream

-- | Run an ad-hoc derivation in the context of the given track.
derive_at :: Cmd.M m => BlockId -> TrackId
    -> Derive.Deriver a -> m (Either Text a, [Log.Msg])
derive_at block_id track_id deriver = do
    dynamic <- fromMaybe PlayUtil.initial_dynamic <$>
        find_dynamic (block_id, Just track_id)
    (val, _, logs) <- PlayUtil.run_with_dynamic dynamic deriver
    return (first pretty val, logs)

-- | Like 'derive_at', but write logs and throw on a Left.
get_derive_at :: Cmd.M m => BlockId -> TrackId -> Derive.Deriver a -> m a
get_derive_at block_id track_id deriver = do
    (result, logs) <- derive_at block_id track_id deriver
    mapM_ Log.write logs
    Cmd.require_right (("derive_at " <> pretty (block_id, track_id)) <>) result

-- | Return the NoteDeriver of a particular event on a particular track,
-- or Nothing if the track isn't a NoteTrack.
lookup_note_deriver :: Cmd.M m => BlockId -> TrackId -> Event.Event
    -> m (Maybe Derive.NoteDeriver)
lookup_note_deriver block_id track_id event = do
    Tree.Node track subs <- find_track block_id track_id
    return $ case ParseTitle.track_type (TrackTree.track_title track) of
        ParseTitle.NoteTrack ->
            Just $ derive_event (Derive.Note.track_info track subs) event
        _ -> Nothing

derive_event :: Derive.Callable d => EvalTrack.TrackInfo d -> Event.Event
    -> Derive.Deriver (Stream.Stream d)
derive_event tinfo event = EvalTrack.derive_event ctx event
    where ctx = EvalTrack.context tinfo Nothing [] event []
    -- TODO get prev and next events
    -- Also, BlockUtil does 'Derive.with_val EnvKey.block_end' and possibly
    -- does Tempo.with_tempo.  If those things turn out to be important,
    -- I should be able to factor that out of BlockUtil.

find_track :: Cmd.M m => BlockId -> TrackId -> m TrackTree.EventsNode
find_track block_id track_id = do
    tree <- TrackTree.block_events_tree block_id
    Cmd.require ("find_track: "
            <> pretty track_id <> " not in " <> pretty block_id) $
        Util.Tree.find ((== Just track_id) . TrackTree.track_id) tree

-- | Get the environment established by 'Ui.config_global_transform'.
global_environ :: Cmd.M m => m Env.Environ
global_environ = do
    (result, _, logs) <- PlayUtil.run mempty mempty smuggle_environ
    mapM_ Log.write logs
    events <- Stream.write_logs =<< Cmd.require_right pretty result
    event <- Cmd.require "Perf.global_environ: expected a single Event" $
        Seq.head events
    return $ Score.event_environ event

-- | Smuggle the environ out in an event.  It's annoying to require such
-- shennanigans, rationale in NOTE [transform-without-derive-callable].
smuggle_environ :: Derive.NoteDeriver
smuggle_environ = Prelude.Block.global_transform $ do
    env <- Internal.get_dynamic Derive.state_environ
    return $! Stream.from_event $!
        Score.empty_event { Score.event_environ = env }

{- NOTE [transform-without-derive-callable]

    It seems like there should be a way to run a transform without the
    Derive.Callable restriction, which in turn constrains the return type.

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
    return $ case val of
        Left err -> Left $ pretty err
        Right val -> Right val

-- * perform

perform :: Cmd.M m => [Score.Event] -> m ([Midi.WriteMessage], [Log.Msg])
perform = (LEvent.partition <$>) . PlayUtil.perform_events . Vector.fromList

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
get_scale_id :: Cmd.M m => Track -> m Pitch.ScaleId
get_scale_id track@(block_id, maybe_track_id) =
    -- Did you know there are so many places to find a ScaleId?  Why are
    -- there so many places?
    (fromMaybe (Pitch.ScaleId Config.default_scale_id) <$>) $
    firstJust
        (maybe (return Nothing) (scale_from_titles block_id) maybe_track_id) $
    firstJust (find_scale_id track) $ return Nothing

find_scale_id :: Cmd.M m => Track -> m (Maybe Pitch.ScaleId)
find_scale_id (block_id, maybe_track_id) = (to_scale_id <$>) $
    firstJust (lookup maybe_track_id) $
    firstJust lookup_parents $
    firstJust (lookup Nothing) $
    firstJust (lookup_environ_val EnvKey.scale =<< global_environ) $
    return Nothing
    where
    to_scale_id = fmap Expr.str_to_scale_id
    lookup maybe_track_id = lookup_val (block_id, maybe_track_id) EnvKey.scale
    lookup_parents = case maybe_track_id of
        Nothing -> return Nothing
        Just track_id -> do
            parents <- map Ui.track_id . maybe [] fst <$>
                TrackTree.parents_children_of block_id track_id
            firstJusts $ map (lookup . Just) parents

get_scale :: Cmd.M m => Track -> m Scale.Scale
get_scale track = do
    scale_id <- get_scale_id track
    Cmd.require ("get_scale: can't find " <> pretty scale_id)
        =<< lookup_scale track scale_id

lookup_scale :: Cmd.M m => Track -> Pitch.ScaleId -> m (Maybe Scale.Scale)
lookup_scale track scale_id = do
    env <- get_environ track
    lookup_scale_env env scale_id

lookup_scale_env :: Cmd.M m => Env.Environ -> Pitch.ScaleId
    -> m (Maybe Scale.Scale)
lookup_scale_env env scale_id = do
    Derive.LookupScale lookup <- Cmd.gets $
        Cmd.config_lookup_scale . Cmd.state_config
    case lookup env (Derive.LookupScale lookup) scale_id of
        Nothing -> return Nothing
        Just (Left err) -> Cmd.throw $ "lookup " <> pretty scale_id <> ": "
            <> pretty err
        Just (Right scale) -> return $ Just scale

-- | Try to get a scale from the titles of the parents of the given track.
scale_from_titles :: Ui.M m => BlockId -> TrackId -> m (Maybe Pitch.ScaleId)
scale_from_titles block_id track_id = do
    tracks <- maybe [] (uncurry (++)) <$>
        TrackTree.parents_children_of block_id track_id
    return $ msum $ map scale_of tracks
    where
    scale_of track = case ParseTitle.title_to_scale (Ui.track_title track) of
        Just scale_id | scale_id /= Pitch.empty_scale -> Just scale_id
        _ -> Nothing

-- | Find the instrument in scope.
lookup_instrument :: Cmd.M m => Track -> m (Maybe Score.Instrument)
lookup_instrument track = lookup_val track EnvKey.instrument

-- | Lookup value from the deriver's EnvKey at the given block and (possibly)
-- track.  See 'Derive.TrackDynamic' for details on the limitations here.
--
-- The value is taken first from the root performance, and then the given
-- block's performance if not present in the root performance.  This is so
-- that blocks which are not called from the root at all will still have
-- environ values.
lookup_val :: (Cmd.M m, Typecheck.Typecheck a) => Track -> Env.Key
    -> m (Maybe a)
lookup_val track name = justm (lookup_environ track) $ lookup_environ_val name

lookup_environ_val :: (Ui.M m, Typecheck.Typecheck a) =>
    Env.Key -> Env.Environ -> m (Maybe a)
lookup_environ_val name env =
    either (Ui.throw . ("Perf.lookup_environ_val: "<>)) return
        (Env.checked_val name env)

lookup_environ :: Cmd.M m => Track -> m (Maybe Env.Environ)
lookup_environ track = fmap Derive.state_environ <$> find_dynamic track

get_environ :: Cmd.M m => Track -> m Env.Environ
get_environ = fmap (fromMaybe mempty) . lookup_environ

-- | Try to find the Dynamic for the given block and track, first looking in
-- the root performance, and then in the block's performance.
find_dynamic :: Cmd.M m => Track -> m (Maybe Derive.Dynamic)
find_dynamic track = do
    maybe_dyn <- lookup_root_dynamic track
    case maybe_dyn of
        Just dyn -> return $ Just dyn
        Nothing -> lookup_dynamic (fst track) track

lookup_root_dynamic :: Cmd.M m => Track -> m (Maybe Derive.Dynamic)
lookup_root_dynamic track =
    justm Ui.lookup_root_id $ \root_id -> lookup_dynamic root_id track

lookup_dynamic :: Cmd.M m => BlockId -> Track -> m (Maybe Derive.Dynamic)
lookup_dynamic perf_block_id (block_id, maybe_track_id) =
    maybe Nothing (lookup . Cmd.perf_track_dynamic) <$>
        Cmd.lookup_performance perf_block_id
    where
    lookup track_dyns = case maybe_track_id of
        Nothing -> do
            (_, dyn) <- List.find ((==block_id) . fst . fst)
                (Map.toAscList track_dyns)
            return dyn
        Just track_id -> Map.lookup (block_id, track_id) track_dyns

-- * default

-- | Get global defaults.
lookup_default_environ :: (Typecheck.Typecheck a, Cmd.M m) =>
    Env.Key -> m (Maybe a)
lookup_default_environ name = do
    -- Eval.eval_transform_expr only applies to things in Derived, so I have to
    -- do this gross hack where I stash the result in a Score.Event.
    -- Ultimately it's because I use the return type to infer the lookup
    -- function, and I want to use Score.Event transformers.  It might be
    -- a better design to figure out the lookup function separately, but
    -- meanwhile this hack should be safe.
    result <- derive smuggle_environ
    environ <- case result of
        Left err -> Cmd.throw $ caller <> ": " <> err
        Right val -> case Stream.events_of val of
            [] -> Cmd.throw $ caller <> " didn't get the fake event it wanted"
            event : _ -> return $ Score.event_environ event
    Cmd.require_right id $ Env.checked_val name environ
    where
    caller = "Perf.lookup_default_environ"

get_default_environ :: (Typecheck.Typecheck a, Cmd.M m) => Env.Key -> m a
get_default_environ name =
    Cmd.require ("no default val for " <> pretty name)
        =<< lookup_default_environ name

-- | The default scale established by 'Ui.config_global_transform', or
-- 'Config.default_scale_id' if there is none.
default_scale_id :: Cmd.M m => m Pitch.ScaleId
default_scale_id =
    maybe (Pitch.ScaleId Config.default_scale_id) Expr.str_to_scale_id <$>
        lookup_default_environ EnvKey.scale


-- * play

-- | Given a block, track, and time, find the realtime at that position.  If
-- the track is Nothing, use the first track that has tempo information.  This
-- is necessary because if a track is muted it will have no tempo, but it's
-- confusing if playing from a muted track fails.
get_realtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m RealTime
get_realtime perf block_id maybe_track_id pos =
    maybe (Cmd.throw $ showt block_id <> " " <> pretty maybe_track_id
            <> " has no tempo information, so it probably failed to derive.")
        return =<< lookup_realtime perf block_id maybe_track_id pos

lookup_realtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m (Maybe RealTime)
lookup_realtime perf block_id maybe_track_id pos = do
    track_ids <- maybe (Ui.track_ids_of block_id) (return . (:[]))
        maybe_track_id
    return $ msum (map tempo track_ids)
    where tempo tid = Seq.head $ Cmd.perf_tempo perf block_id tid pos

-- | Like 'get_realtime', but do multiple at once.
get_realtimes :: Cmd.Performance -> BlockId -> TrackId -> [ScoreTime]
    -> [(ScoreTime, RealTime)]
get_realtimes perf block_id track_id ps =
    [(p, t) | (p, t : _)
        <- zip ps (map (Cmd.perf_tempo perf block_id track_id) ps)]

-- | Take a RealTime to all the ScoreTimes it corresponds to, if any.
find_play_pos :: Ui.M m => Transport.InverseTempoFunction
    -> RealTime -> m [(ViewId, [(TrackNum, ScoreTime)])]
find_play_pos inv_tempo = block_pos_to_play_pos . inv_tempo

-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
--
-- This function has to be careful to not throw on non-existent IDs, because
-- it's called from the monitor_loop.
block_pos_to_play_pos :: Ui.M m => [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
block_pos_to_play_pos = concatMapM convert

convert :: Ui.M m => (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
convert (block_id, track_pos) = do
    view_ids <- Map.keys <$> Ui.views_of block_id
    Ui.lookup_block block_id >>= \x -> return $ case x of
        Nothing -> []
        Just block ->
            let tracknum_pos = concatMap (tracknums_of block) track_pos
            in [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (TrackId, ScoreTime) -> [(TrackNum, ScoreTime)]
tracknums_of block (track_id, pos) =
    [ (tracknum, pos)
    | (tracknum, Block.TId tid _)
        <- zip [0..] (Block.block_tracklike_ids block)
    , tid == track_id
    ]

-- ** find score times

-- | Give a block and score time, return the play position on sub-blocks.
-- The block itself is filtered out of the result.
sub_pos :: Cmd.M m => BlockId -> TrackId -> ScoreTime
    -> m [(BlockId, [(TrackId, ScoreTime)])]
sub_pos block_id track_id pos = fmap (fromMaybe []) $
    justm (Cmd.lookup_performance block_id) $ \perf ->
    justm (lookup_realtime perf block_id (Just track_id) pos) $ \real ->
    return $ Just $ filter ((/=block_id) . fst) (Cmd.perf_inv_tempo perf real)
    -- lookup_realtime gives Nothing if there's no tempo available which is
    -- likely for a newly added track.  Return [] in that case.

-- * util

lookup_root :: Cmd.M m => m (Maybe Cmd.Performance)
lookup_root = justm Ui.lookup_root_id Cmd.lookup_performance

get_root :: Cmd.M m => m Cmd.Performance
get_root = Cmd.require "no root performance" =<< lookup_root

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
import qualified Data.Vector as Vector

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Global
import Types


-- * derive

-- | Specialized version of 'derive_expr' for note calls with no arguments.
derive_note_call :: Cmd.M m => BlockId -> TrackId -> TrackTime
    -> TrackLang.CallId -> m (Either String [Score.Event], [Log.Msg])
derive_note_call block_id track_id pos call =
    derive_expr block_id track_id pos (TrackLang.Call call [] :| [])

-- | Derive an expression.
derive_expr :: (Cmd.M m, Derive.Callable d) => BlockId -> TrackId -> TrackTime
    -> TrackLang.Expr -> m (Either String [d], [Log.Msg])
derive_expr block_id track_id pos expr = do
    (result, logs) <- derive_at block_id track_id
        (Eval.eval_one_at False pos 1 expr)
    return $ case result of
        Left err -> (Left err, logs)
        Right levents -> (Right events, derive_logs ++ logs)
            where (events, derive_logs) = LEvent.partition levents

-- | Run an ad-hoc derivation in the context of the given track.
derive_at :: Cmd.M m => BlockId -> TrackId
    -> Derive.Deriver a -> m (Either String a, [Log.Msg])
derive_at block_id track_id deriver = do
    dynamic <- fromMaybe empty_dynamic <$> find_dynamic block_id (Just track_id)
    (val, _, logs) <- PlayUtil.run_with_dynamic dynamic deriver
    return (first pretty val, logs)
    where empty_dynamic = Derive.initial_dynamic mempty

-- | Get the environment established by 'State.config_global_transform'.
global_environ :: Cmd.M m => m TrackLang.Environ
global_environ = do
    global_transform <- State.config#State.global_transform <#> State.get
    (result, _, logs) <- PlayUtil.run mempty mempty $
        Eval.eval_transform_expr "global transform" global_transform $ do
            env <- Internal.get_dynamic Derive.state_environ
            -- Smuggle the environ out in an event.  It's annoying to require
            -- such shennanigans, rationale in
            -- NOTE [transform-withoutderive-callable]
            return [LEvent.Event $
                Score.empty_event { Score.event_environ = env } ]
    mapM_ Log.write logs
    events <- LEvent.write_logs =<< Cmd.require_right pretty result
    event <- Cmd.require "Perf.global_transform: expected a single Event" $
        Seq.head events
    return $ Score.event_environ event

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
derive :: Cmd.M m => Derive.Deriver a -> m (Either String a)
derive deriver = do
    (val, _, _) <- PlayUtil.run mempty mempty deriver
    return $ case val of
        Left err -> Left $ pretty err
        Right val -> Right val

-- * perform

perform :: Cmd.M m => [Score.Event] -> m ([Midi.WriteMessage], [Log.Msg])
perform = (LEvent.partition <$>) . PlayUtil.perform_events . Vector.fromList

-- * environ

{- | Get the scale in scope in a certain track on a certain block.

    This tries really really hard to find a ScaleId.  The reason is that pitch
    track entry uses it to insert pitch names, and if it gets the scale wrong
    it inserts bogus pitches.  I can't rely on 'lookup_val' because a new
    track has never been derived and thus has no Dynamic.
-}
get_scale_id :: Cmd.M m => BlockId -> Maybe TrackId -> m Pitch.ScaleId
get_scale_id block_id maybe_track_id =
    -- Did you know there are so many places to find a ScaleId?  Why are
    -- there so many places?
    (fromMaybe (Pitch.ScaleId Config.default_scale_id) <$>) $
    firstJust
        (maybe (return Nothing) (scale_from_titles block_id) maybe_track_id) $
    firstJust (find_scale_id block_id maybe_track_id) $ return Nothing

find_scale_id :: Cmd.M m => BlockId -> Maybe TrackId -> m (Maybe Pitch.ScaleId)
find_scale_id block_id maybe_track_id = (to_scale_id <$>) $
    firstJust (lookup maybe_track_id) $
    firstJust lookup_parents $
    firstJust (lookup Nothing) $
    firstJust (lookup_environ_val Environ.scale =<< global_environ) $
    return Nothing
    where
    to_scale_id = fmap TrackLang.sym_to_scale_id
    lookup track_id = lookup_val block_id track_id Environ.scale
    lookup_parents = case maybe_track_id of
        Nothing -> return Nothing
        Just track_id -> do
            parents <- map State.track_id . maybe [] fst <$>
                TrackTree.parents_children_of block_id track_id
            firstJusts $ map (lookup . Just) parents

-- | Try to get a scale from the titles of the parents of the given track.
scale_from_titles :: State.M m => BlockId -> TrackId
    -> m (Maybe Pitch.ScaleId)
scale_from_titles block_id track_id = do
    tracks <- maybe [] (uncurry (++)) <$>
        TrackTree.parents_children_of block_id track_id
    return $ msum $ map scale_of tracks
    where
    scale_of track = case ParseTitle.title_to_scale (State.track_title track) of
        Just scale_id | scale_id /= Pitch.empty_scale -> Just scale_id
        _ -> Nothing

lookup_instrument :: Cmd.M m => BlockId -> Maybe TrackId
    -> m (Maybe Score.Instrument)
lookup_instrument block_id maybe_track_id =
    lookup_val block_id maybe_track_id Environ.instrument

-- | Lookup value from the deriver's Environ at the given block and (possibly)
-- track.  See 'Derive.TrackDynamic' for details on the limitations here.
--
-- The value is taken first from the root performance, and then the given
-- block's performance if not present in the root performance.  This is so
-- that blocks which are not called from the root at all will still have
-- environ values.
lookup_val :: (Cmd.M m, TrackLang.Typecheck a) => BlockId
    -> Maybe TrackId
    -- ^ If Nothing, take the env from the first track.  This is for when you
    -- expect the env val to be constant for the entire block.
    -> TrackLang.ValName -> m (Maybe a)
lookup_val block_id maybe_track_id name =
    justm (lookup_environ block_id maybe_track_id) $ lookup_environ_val name

lookup_environ_val :: (State.M m, TrackLang.Typecheck a) =>
    TrackLang.ValName -> TrackLang.Environ -> m (Maybe a)
lookup_environ_val name env =
    either (Cmd.throw . untxt . ("Perf.lookup_environ_val: "<>)) return
        (TrackLang.checked_val name env)

lookup_environ :: Cmd.M m => BlockId -> Maybe TrackId
    -> m (Maybe TrackLang.Environ)
lookup_environ block_id maybe_track_id =
    fmap Derive.state_environ <$> find_dynamic block_id maybe_track_id

get_environ :: Cmd.M m => BlockId -> Maybe TrackId -> m TrackLang.Environ
get_environ block_id = fmap (fromMaybe mempty) . lookup_environ block_id

-- | Try to find the Dynamic for the given block and track, first looking in
-- the root performance, and then in the block's performance.
find_dynamic :: Cmd.M m => BlockId -> Maybe TrackId -> m (Maybe Derive.Dynamic)
find_dynamic block_id maybe_track_id = do
    maybe_dyn <- lookup_root_dynamic block_id maybe_track_id
    case maybe_dyn of
        Just dyn -> return $ Just dyn
        Nothing -> lookup_dynamic block_id block_id maybe_track_id

lookup_root_dynamic :: Cmd.M m => BlockId -> Maybe TrackId
    -> m (Maybe Derive.Dynamic)
lookup_root_dynamic block_id maybe_track_id =
    justm State.lookup_root_id $ \root_id ->
        lookup_dynamic root_id block_id maybe_track_id

lookup_dynamic :: Cmd.M m => BlockId -> BlockId -> Maybe TrackId
    -> m (Maybe Derive.Dynamic)
lookup_dynamic perf_block_id block_id maybe_track_id =
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

-- | Get defaults set by 'State.config_global_transform'.
lookup_default_environ :: (TrackLang.Typecheck a, Cmd.M m) =>
    TrackLang.ValName -> m (Maybe a)
lookup_default_environ name = do
    global <- State.config#State.global_transform <#> State.get
    let apply = Eval.eval_transform_expr caller global
    -- Eval.eval_transform_expr only applies to things in Derived, so I have to
    -- do this gross hack where I stash the result in a Score.Event.
    -- Ultimately it's because I use the return type to infer the lookup
    -- function, and I want to use Score.Event transformers.  It might be
    -- a better design to figure out the lookup function separately, but
    -- meanwhile this hack should be safe.
    result <- derive $ apply $ do
        environ <- Internal.get_environ
        return [LEvent.Event $
            Score.empty_event { Score.event_environ = environ }]
    environ <- case result of
        Left err -> Cmd.throw $ untxt caller <> ": " <> err
        Right val -> case LEvent.events_of val of
            [] -> Cmd.throw $ untxt caller
                <> " didn't get the fake event it wanted"
            event : _ -> return $ Score.event_environ event
    either (Cmd.throw . untxt) return (TrackLang.checked_val name environ)
    where
    caller = "Perf.lookup_default_environ"

get_default_environ :: (TrackLang.Typecheck a, Cmd.M m) =>
    TrackLang.ValName -> m a
get_default_environ name =
    Cmd.require ("no default val for " <> pretty name)
        =<< lookup_default_environ name

-- | The default scale established by 'State.config_global_transform', or
-- 'Config.default_scale_id' if there is none.
default_scale_id :: Cmd.M m => m Pitch.ScaleId
default_scale_id =
    maybe (Pitch.ScaleId Config.default_scale_id) TrackLang.sym_to_scale_id <$>
        lookup_default_environ Environ.scale


-- * play

-- | Given a block, track, and time, find the realtime at that position.  If
-- the track is Nothing, use the first track that has tempo information.  This
-- is necessary because if a track is muted it will have no tempo, but it's
-- confusing if playing from a muted track fails.
get_realtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m RealTime
get_realtime perf block_id maybe_track_id pos =
    maybe (Cmd.throw $ show block_id ++ " " ++ pretty maybe_track_id
            ++ " has no tempo information, so it probably failed to derive.")
        return =<< lookup_realtime perf block_id maybe_track_id pos

lookup_realtime :: Cmd.M m => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m (Maybe RealTime)
lookup_realtime perf block_id maybe_track_id pos = do
    track_ids <- maybe (State.track_ids_of block_id) (return . (:[]))
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
find_play_pos :: State.M m => Transport.InverseTempoFunction
    -> RealTime -> m [(ViewId, [(TrackNum, ScoreTime)])]
find_play_pos inv_tempo = block_pos_to_play_pos . inv_tempo

-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
--
-- This function has to be careful to not throw on non-existent IDs, because
-- it's called from the monitor_loop.
block_pos_to_play_pos :: State.M m => [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
block_pos_to_play_pos = concatMapM convert

convert :: State.M m => (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
convert (block_id, track_pos) = do
    view_ids <- Map.keys <$> State.views_of block_id
    State.lookup_block block_id >>= \x -> return $ case x of
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
lookup_root = justm State.lookup_root_id Cmd.lookup_performance

get_root :: Cmd.M m => m Cmd.Performance
get_root = Cmd.require "no root performance" =<< lookup_root

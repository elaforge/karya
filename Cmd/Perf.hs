{- | This module is for Cmds that want to peek into the derived score for
    whatever reason.  Normally the flow of information goes from Cmd -> Derive
    -> Perform, but this module is for those cases when Cmd wants to know
    about the results of later stages.
-}
module Cmd.Perf where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import Types


-- * notes

-- | Convert a note to a degree by running a mini derivation just for it.
-- This means that if the note does anything magic based on its environment,
-- it will get the base environment for a derivation from scratch.
--
-- TODO I could take BlockId and TrackId and derive it in the proper environ.
note_to_pitch :: (Cmd.M m) => Pitch.ScaleId -> Pitch.Note
    -> m (Either String PitchSignal.Pitch)
note_to_pitch scale_id note = do
    scale <- Cmd.get_scale "Perf.note_to_pitch" scale_id
    derive (Derive.with_scale scale (Call.eval_note (TrackLang.Note note [])))

-- | A cheap quick derivation that sets up the correct initial state, but
-- runs without the cache and throws away any logs.
derive :: (Cmd.M m) => Derive.Deriver a -> m (Either String a)
derive deriver = do
    (val, _, _) <- PlayUtil.run mempty mempty deriver
    return $ case val of
        Left err -> Left $ Pretty.pretty err
        Right val -> Right val


-- * signal

-- | Look up the signal of a track from the last derivation.  It's looked up
-- in the root block's performance.
lookup_signal :: (Cmd.M m) => TrackId -> m (Maybe Track.TrackSignal)
lookup_signal track_id = do
    maybe_perf <- lookup_root
    return $ do
        perf <- maybe_perf
        result <- Map.lookup track_id (Cmd.perf_track_signals perf)
        either (const Nothing) Just result

control_at :: Track.TrackSignal -> [ScoreTime] -> [Signal.Y]
control_at (Track.TrackSignal sig shift stretch _) ps =
    map (flip Signal.at sig . warp) ps
    where warp p = RealTime.score (p * stretch + shift)

nn_at :: Track.TrackSignal -> [ScoreTime] -> [Pitch.NoteNumber]
nn_at tsig ps = map Pitch.NoteNumber (control_at tsig ps)

-- | Get the control values at the given points, or fail if there is no
-- signal there.
get_control_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [Signal.Y]
get_control_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    return $ control_at sig ps

-- | Get the pitch values at the given points, or fail if there is no
-- signal there.  TODO can't tell the difference between a pitch and control
-- track.
get_nn_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [Pitch.NoteNumber]
get_nn_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    return $ nn_at sig ps


-- * environ

-- | Get the scale in scope in a certain track on a certain block, falling
-- back on the default scale if there is none.
get_scale_id :: (Cmd.M m) => BlockId -> Maybe TrackId -> m Pitch.ScaleId
get_scale_id block_id maybe_track_id =
    maybe (State.get_default State.default_scale) return
        =<< lookup_val block_id maybe_track_id TrackLang.v_scale

-- | As with 'get_scale_id' but for the Key.
get_key :: (Cmd.M m) => BlockId -> Maybe TrackId -> m (Maybe Pitch.Key)
get_key block_id maybe_track_id =
    maybe (State.get_default State.default_key) (return . Just . Pitch.Key)
        =<< lookup_val block_id maybe_track_id TrackLang.v_key

lookup_instrument :: (Cmd.M m) => BlockId -> Maybe TrackId
    -> m (Maybe Score.Instrument)
lookup_instrument block_id maybe_track_id =
    maybe (State.get_default State.default_instrument) return
        =<< lookup_val block_id maybe_track_id TrackLang.v_instrument

-- | Resolve the default instrument into whatever it probably resolves to
-- during derivation.
resolve_instrument :: (Cmd.M m) => BlockId -> TrackNum -> Score.Instrument
    -> m Score.Instrument
resolve_instrument block_id tracknum inst
    | inst /= Score.default_inst = return inst
    | otherwise = do
        track_id <- State.get_event_track_at "get_track_status"
            block_id tracknum
        fromMaybe inst <$> lookup_instrument block_id (Just track_id)

-- | Lookup value from the deriver's Environ at the given block and (possibly)
-- track.  See 'Derive.TrackEnviron' for details on the limitations here.
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
    justm (get_environ block_id maybe_track_id) $ \env ->
        either (Cmd.throw . ("Perf.lookup_val: "++)) return
            (TrackLang.checked_val name env)

get_environ :: (Cmd.M m) => BlockId -> Maybe TrackId
    -> m (Maybe TrackLang.Environ)
get_environ block_id maybe_track_id = do
    maybe_val <- maybe Nothing (lookup . Cmd.perf_track_environ) <$> lookup_root
    case maybe_val of
        Just val -> return (Just val)
        Nothing ->
            maybe Nothing (lookup . Cmd.perf_track_environ) <$>
                Cmd.lookup_performance block_id
    where
    lookup env = case maybe_track_id of
        Nothing -> do
            (_, env) <- List.find ((==block_id) . fst . fst) (Map.toAscList env)
            return env
        Just track_id -> Map.lookup (block_id, track_id) env


-- * play

-- | Given a block, track, and time, find the realtime at that position.  If
-- the track is Nothing, use the first track that has tempo information.  This
-- is necessary because if a track is muted it will have no tempo, but it's
-- confusing if playing from a muted track fails.
get_realtime :: (Cmd.M m) => Cmd.Performance -> BlockId -> Maybe TrackId
    -> ScoreTime -> m RealTime
get_realtime perf block_id maybe_track_id pos =
    maybe (Cmd.throw $ show block_id ++ " " ++ Pretty.pretty maybe_track_id
            ++ " has no tempo information, so it probably failed to derive.")
        return =<< lookup_realtime perf block_id maybe_track_id pos

lookup_realtime :: (Cmd.M m) => Cmd.Performance -> BlockId -> Maybe TrackId
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
    [(p, t) | (p, (t:_))
        <- zip ps (map (Cmd.perf_tempo perf block_id track_id) ps)]

-- | Take a RealTime to all the ScoreTimes it corresponds to, if any.
find_play_pos :: (State.M m) => Transport.InverseTempoFunction
    -> RealTime -> m [(ViewId, [(TrackNum, ScoreTime)])]
find_play_pos inv_tempo = block_pos_to_play_pos . inv_tempo

-- | Do all the annoying shuffling around to convert the deriver-oriented
-- blocks and tracks to the view-oriented views and tracknums.
block_pos_to_play_pos :: (State.M m) => [(BlockId, [(TrackId, ScoreTime)])]
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
block_pos_to_play_pos block_pos = fmap concat (mapM convert block_pos)

convert :: (State.M m) => (BlockId, [(TrackId, ScoreTime)])
    -> m [(ViewId, [(TrackNum, ScoreTime)])]
convert (block_id, track_pos) = do
    view_ids <- fmap Map.keys (State.get_views_of block_id)
    block <- State.get_block block_id
    let tracknum_pos = concatMap (tracknums_of block) track_pos
    return [(view_id, tracknum_pos) | view_id <- view_ids]

tracknums_of :: Block.Block -> (TrackId, ScoreTime) -> [(TrackNum, ScoreTime)]
tracknums_of block (track_id, pos) =
    [ (tracknum, pos)
    | (tracknum, Block.TId tid _)
        <- zip [0..] (Block.block_tracklike_ids block)
    , tid == track_id ]

-- ** find score times

-- | Give a block and score time, return the play position on sub-blocks.
-- The block itself is filtered out of the result.
sub_pos :: (Cmd.M m) => BlockId -> TrackId -> ScoreTime
    -> m [(BlockId, [(TrackId, ScoreTime)])]
sub_pos block_id track_id pos = fmap (fromMaybe []) $
    justm (Cmd.lookup_performance block_id) $ \perf ->
    justm (lookup_realtime perf block_id (Just track_id) pos) $ \real ->
    return $ Just $ filter ((/=block_id) . fst) (Cmd.perf_inv_tempo perf real)
    -- lookup_realtime gives Nothing if there's no tempo available which is
    -- likely for a newly added track.  Return [] in that case.

-- * util

lookup_root :: (Cmd.M m) => m (Maybe Cmd.Performance)
lookup_root = justm State.lookup_root_id Cmd.lookup_performance

get_root :: (Cmd.M m) => m Cmd.Performance
get_root = Cmd.require_msg "no root performance" =<< lookup_root

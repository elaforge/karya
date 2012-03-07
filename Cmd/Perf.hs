{- | This module is for Cmds that want to peek into the derived score for
    whatever reason.  Normally the flow of information goes from Cmd -> Derive
    -> Perform, but this module is for those cases when Cmd wants to know
    about the results of later stages.
-}
module Cmd.Perf where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

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
get_scale_id :: (Cmd.M m) => BlockId -> TrackId -> m Pitch.ScaleId
get_scale_id block_id track_id = do
    scale <- lookup_env block_id track_id TrackLang.v_scale
    case scale of
        Just (TrackLang.VScaleId scale_id) -> return scale_id
        _ -> State.get_default State.default_scale

-- | As with 'get_scale_id' but for the Key.
get_key :: (Cmd.M m) => BlockId -> TrackId -> m (Maybe Pitch.Key)
get_key block_id track_id = do
    key <- lookup_env block_id track_id TrackLang.v_key
    case key of
        Just (TrackLang.VString key) -> return $ Just (Pitch.Key key)
        _ -> State.get_default State.default_key

lookup_instrument :: (Cmd.M m) => BlockId -> TrackId
    -> m (Maybe Score.Instrument)
lookup_instrument block_id track_id = do
    inst_val <- lookup_env block_id track_id TrackLang.v_instrument
    case inst_val of
        Just (TrackLang.VInstrument inst) -> return $ Just inst
        _ -> State.get_default State.default_instrument

-- | Lookup value from the deriver's Environ at the given block and track.
-- See 'Derive.TrackEnviron' for details on the limitations here.
--
-- The lookup is done relative to the root block, which means that instruments
-- and scales always default relative to the root.  I suppose I could think of
-- some case where it would be better to look it up relative to some other
-- block, but that seems way too complicated.  This means that the
-- TrackEnvirons from other block derivations are never used.  That leads to
-- a certain amount of void allocation, so maybe I should include a flag to
-- turn off TrackEnviron recording?
lookup_env :: (Cmd.M m) => BlockId -> TrackId -> TrackLang.ValName
    -> m (Maybe TrackLang.Val)
lookup_env block_id track_id name =
    justm lookup_root $ \perf -> do
    let track_env = Cmd.perf_track_environ perf
    return $ Map.lookup name =<< Map.lookup (block_id, track_id) track_env


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
sub_pos block_id track_id pos = fmap (Maybe.fromMaybe []) $
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

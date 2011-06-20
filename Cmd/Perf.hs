{- | This module is for Cmds that want to peek into the derived score for
    whatever reason.  Normally the flow of information goes from Cmd -> Derive
    -> Perform, but this module is for those cases when Cmd wants to know
    about the results of later stages.
-}
module Cmd.Perf where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Pretty as Pretty
import Ui
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


-- * notes

-- | Convert a note to a degree by running a mini derivation just for it.
-- This means that if the note does anything magic based on its environment,
-- it will get the base environment for a derivation from scratch.
note_to_degree :: (Cmd.M m) => Pitch.ScaleId -> Pitch.Note
    -> m (Either String Pitch.Degree)
note_to_degree scale_id note = do
    scale <- Cmd.get_scale "Perf.note_to_degree" scale_id
    derive (Derive.with_scale scale (Call.eval_note note))

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
    maybe_perf <- lookup_root_performance
    return $ do
        perf <- maybe_perf
        result <- Map.lookup track_id (Cmd.perf_track_signals perf)
        either (const Nothing) Just result

control_at :: Track.TrackSignal -> [ScoreTime] -> Maybe [Signal.Y]
control_at (Track.TrackSignal tsig shift stretch) ps = case tsig of
    Track.Control sig -> Just $ map (flip Signal.at sig . warp) ps
    _ -> Nothing
    where warp p = RealTime.score (p * stretch + shift)

pitch_at :: Track.TrackSignal -> [ScoreTime] -> Maybe [PitchSignal.Y]
pitch_at (Track.TrackSignal tsig shift stretch) ps = case tsig of
    Track.Pitch sig _ -> Just $ map (flip PitchSignal.at sig . warp) ps
    _ -> Nothing
    where warp p = RealTime.score (p * stretch + shift)

-- | Get the control values at the given points, or fail if there is no
-- control signal there.
get_control_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [Signal.Y]
get_control_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    Cmd.require_msg "signal is not a control signal" $ control_at sig ps

-- | Get the pitch values at the given points, or fail if there is no
-- pitch signal there.
get_pitch_at :: (Cmd.M m) => TrackId -> [ScoreTime] -> m [PitchSignal.Y]
get_pitch_at track_id ps = do
    sig <- Cmd.require_msg ("no signal for " ++ show track_id)
        =<< lookup_signal track_id
    Cmd.require_msg "signal is not a pitch signal" $ pitch_at sig ps


-- * environ

get_scale_id :: (Cmd.M m) => BlockId -> TrackId -> m Pitch.ScaleId
get_scale_id block_id track_id = do
    scale <- lookup_env block_id track_id TrackLang.v_scale
    case scale of
        Just (TrackLang.VScaleId scale_id) -> return scale_id
        _ -> State.get_default State.default_scale

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
    justm lookup_root_performance $ \perf -> do
    let track_env = Cmd.perf_track_environ perf
    return $ Map.lookup name =<< Map.lookup (block_id, track_id) track_env


-- * util

lookup_root_performance :: (Cmd.M m) => m (Maybe Cmd.Performance)
lookup_root_performance = justm State.lookup_root_id Cmd.lookup_performance

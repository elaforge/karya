{-# LANGUAGE PatternGuards #-}
{- | Derivers for control tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Control where
import Prelude
import Control.Monad

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Track as Track

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang


-- | Top level deriver for control tracks.
d_control_track :: BlockId -> TrackId -> Derive.Transformer
d_control_track block_id track_id deriver = do
    track <- Derive.get_track track_id
    (expr, vals) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackLang.parse_control_track (Track.track_title track))
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    eval_track block_id track_id expr vals deriver

eval_track :: BlockId -> TrackId -> TrackLang.Expr -> [TrackLang.Val]
    -> Derive.Transformer
eval_track block_id track_id expr vals deriver = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    case TrackInfo.parse_control_vals vals of
        Right TrackInfo.Tempo -> do
            let control_deriver = derive_control expr events
            tempo_call block_id track_id
                (Signal.coerce <$> control_deriver) deriver
        Right (TrackInfo.Control maybe_op control) -> do
            let control_deriver = derive_control expr events
            control_call track_id control maybe_op control_deriver deriver
        Right (TrackInfo.Pitch ptype maybe_name) ->
            pitch_call track_id maybe_name ptype expr events deriver
        Left msg -> Derive.throw $
            "failed to parse " ++ Pretty.pretty vals ++ ": " ++ msg

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental
-- warp twice.
tempo_call :: BlockId -> TrackId -> Derive.Deriver Signal.Tempo
    -> Derive.EventDeriver -> Derive.EventDeriver
tempo_call block_id track_id sig_deriver deriver = do
    sig <- Derive.setup_without_warp sig_deriver
    Derive.d_tempo block_id (Just track_id) sig deriver

control_call :: TrackId -> Score.Control -> Maybe TrackLang.CallId
    -> Derive.ControlDeriver -> Derive.Transformer
control_call track_id control maybe_op control_deriver =
    with_control $ Derive.track_setup track_id control_deriver
    where
    with_control control_deriver deriver = do
        signal <- control_deriver
        case maybe_op of
            Nothing -> Derive.with_control control signal deriver
            Just op -> Derive.with_control_operator control op signal deriver

pitch_call :: TrackId -> Maybe Score.Control -> TrackInfo.PitchType
    -> TrackLang.Expr -> [Track.PosEvent] -> Derive.Transformer
pitch_call track_id maybe_name ptype track_expr events deriver =
    Derive.track_setup track_id $ do
        with_scale <- case ptype of
            TrackInfo.PitchRelative _ -> do
                scale <- Derive.lookup_val TrackLang.v_scale
                let relative_scale = maybe Relative.scale Relative.adjust scale
                return $ Derive.with_val TrackLang.v_scale relative_scale
            TrackInfo.PitchAbsolute (Just scale_id) -> do
                scale <- Derive.get_scale "pitch_call" scale_id
                return $ Derive.with_val TrackLang.v_scale scale
            TrackInfo.PitchAbsolute Nothing -> return id
        with_scale $ case ptype of
            TrackInfo.PitchRelative op -> do
                signal <- derive_relative_pitch events
                Derive.with_pitch_operator maybe_name op signal deriver
            _ -> do
                signal <- derive_pitch track_expr events
                Derive.with_pitch maybe_name signal deriver

derive_control :: TrackLang.Expr -> [Track.PosEvent] -> Derive.ControlDeriver
derive_control track_expr events = Derive.with_msg "control" $
    Call.apply_transformer (dinfo, Derive.dummy_call_info) track_expr deriver
    where
    deriver = Signal.merge <$> Call.derive_track dinfo preprocess_control
        last_sample events
    dinfo = Call.DeriveInfo Derive.no_control Call.lookup_control_call
    last_sample prev chunk = Signal.last chunk `mplus` prev

preprocess_control :: Call.PreProcess
preprocess_control expr = case Seq.break_last expr of
    (calls, Just (TrackLang.Call (TrackLang.Symbol sym) []))
        | Right num@(TrackLang.VNum _) <- TrackLang.parse_val sym ->
            calls ++ [TrackLang.Call (TrackLang.Symbol "set")
                [TrackLang.Literal num]]
    _ -> expr


derive_pitch :: TrackLang.Expr -> [Track.PosEvent] -> Derive.PitchDeriver
derive_pitch track_expr events = Derive.with_msg "pitch" $
    Call.apply_transformer (dinfo, Derive.dummy_call_info) track_expr deriver
    where
    deriver = PitchSignal.merge <$>
        Call.derive_track dinfo id last_sample events
    dinfo = Call.DeriveInfo Derive.no_pitch Call.lookup_pitch_call
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev

derive_relative_pitch :: [Track.PosEvent] -> Derive.PitchDeriver
derive_relative_pitch events = Derive.with_msg "relative pitch" $
    PitchSignal.merge <$>
        Call.derive_track dinfo id last_sample events
    where
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev
    dinfo = Call.DeriveInfo Derive.no_pitch Call.lookup_pitch_call

-- TODO this can go away when pitches are calls
-- preprocess_pitch :: Call.PreProcess
-- preprocess_pitch expr = case Seq.break_last expr of
--     (calls, Just (TrackLang.Call (TrackLang.Symbol sym) [])) ->
--         calls ++ [TrackLang.Call (TrackLang.Symbol "set")
--                 [TrackLang.Literal (TrackLang.VNote (Pitch.Note sym))]]
--     _ -> expr

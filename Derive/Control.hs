{-# LANGUAGE PatternGuards #-}
{- | Derivers for control tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Control where
import qualified Data.Map as Map
import Control.Monad

import Util.Control
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


-- | Top level deriver for control tracks.
d_control_track :: BlockId -> TrackId
    -> Derive.EventDeriver -> Derive.EventDeriver
d_control_track block_id track_id deriver = Derive.catch_warn deriver $ do
    track <- Derive.get_track track_id
    if null (Track.track_title track) then deriver else do
    (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackInfo.parse_control_expr (Track.track_title track))
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    eval_track block_id track_id expr ctype deriver

eval_track :: BlockId -> TrackId -> TrackLang.Expr -> TrackInfo.ControlType
    -> Derive.Transformer
eval_track block_id track_id expr ctype deriver = do
    track <- Derive.get_track track_id
    let events = Track.event_list (Track.track_events track)
    block_end <- Derive.get_block_dur block_id
    case ctype of
        TrackInfo.Tempo -> do
            let control_deriver = derive_control block_end expr events
            tempo_call block_id track_id
                (first Signal.coerce <$> control_deriver) deriver
        TrackInfo.Control maybe_op control -> do
            let control_deriver = derive_control block_end expr events
            control_call track_id control maybe_op control_deriver deriver
        TrackInfo.Pitch ptype maybe_name ->
            pitch_call block_end track_id maybe_name ptype expr events deriver

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental
-- warp twice.
tempo_call :: BlockId -> TrackId
    -> Derive.Deriver (Signal.Tempo, Derive.EventDamage)
    -> Derive.EventDeriver -> Derive.EventDeriver
tempo_call block_id track_id sig_deriver deriver = do
    (signal, damage) <- Derive.setup_without_warp sig_deriver
    rendered <- track_is_rendered track_id
    when rendered $
        put_track_signal track_id $
            Track.TrackSignal (Track.Control (Signal.coerce signal)) 0 1
    -- TODO tempo damage should turn into score damage on all events after
    -- it.  It might be more regular to give all generators a dep on tempo,
    -- but this way is probably more efficient and just as clear.
    Derive.with_control_damage (extend damage) $
        Derive.d_tempo block_id (Just track_id) signal deriver
    where
    extend (Derive.EventDamage ranges) = Derive.EventDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> Ranges.range s Types.max_real_time

control_call :: TrackId -> Score.Control -> Maybe TrackLang.CallId
    -> Derive.Deriver (Derive.Control, Derive.EventDamage)
    -> Derive.Transformer
control_call track_id control maybe_op control_deriver deriver = do
    (signal, damage) <- Derive.track_setup track_id control_deriver
    stash_signal track_id (Right (signal, fst <$> control_deriver))
    Derive.with_control_damage damage $ with_control signal deriver
    where
    with_control signal deriver = do
        case maybe_op of
            Nothing -> Derive.with_control control signal deriver
            Just op -> Derive.with_control_operator control op signal deriver

pitch_call :: ScoreTime -> TrackId -> Maybe Score.Control
    -> TrackInfo.PitchType -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Transformer
pitch_call block_end track_id maybe_name ptype track_expr events deriver =
    Derive.track_setup track_id $ do
        (with_scale, scale) <- case ptype of
            TrackInfo.PitchRelative _ -> do
                -- TODO previously I mangled the scale to set the octave, but
                -- I can't do that now unless I put it in the ScaleId
                return (Call.with_scale Relative.scale, Relative.scale)
            TrackInfo.PitchAbsolute (Just scale_id) -> do
                scale <- Derive.get_scale scale_id
                return (Call.with_scale scale, scale)
            TrackInfo.PitchAbsolute Nothing -> do
                scale <- Call.get_scale
                return (id, scale)
        let scale_map = Scale.scale_map scale
            derive = derive_pitch block_end track_expr events
        with_scale $ case ptype of
            TrackInfo.PitchRelative op -> do
                (signal, damage) <- derive
                stash_signal track_id (Left (signal, fst <$> derive, scale_map))
                Derive.with_control_damage damage $
                    Derive.with_pitch_operator maybe_name op signal deriver
            _ -> do
                (signal, damage) <- derive
                stash_signal track_id (Left (signal, fst <$> derive, scale_map))
                Derive.with_control_damage damage $
                    Derive.with_pitch maybe_name signal deriver

derive_control :: ScoreTime -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver (Derive.Control, Derive.EventDamage)
derive_control block_end track_expr events = do
    result <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "control track") track_expr deriver
    damage <- Derive.take_local_damage
    return (result, extend_control_damage result damage)
    where
    deriver = Signal.merge <$>
        Call.derive_track block_end dinfo preprocess_control last_sample events
    dinfo = Call.DeriveInfo Call.lookup_control_call "control"
    last_sample prev chunk = Signal.last chunk `mplus` prev

preprocess_control :: Call.PreProcess
preprocess_control expr = case Seq.break_last expr of
    (calls, Just (TrackLang.Call (TrackLang.Symbol sym) []))
        | Right num@(TrackLang.VNum _) <- TrackLang.parse_val sym ->
            calls ++ [TrackLang.Call (TrackLang.Symbol "set")
                [TrackLang.Literal num]]
    _ -> expr


derive_pitch :: ScoreTime -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver (Derive.Pitch, Derive.EventDamage)
derive_pitch block_end track_expr events = do
    result <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "pitch track") track_expr deriver
    damage <- Derive.take_local_damage
    return (result, extend_pitch_damage result damage)
    where
    deriver = PitchSignal.merge <$>
        Call.derive_track block_end dinfo id last_sample events
    dinfo = Call.DeriveInfo Call.lookup_pitch_call "pitch"
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev

-- | Event damage for a control track only extends to the last sample.
-- However, the actual changed region extends to the /next/ sample.
-- Hacky hack hack.
extend_control_damage :: Derive.Control -> Derive.EventDamage
    -> Derive.EventDamage
extend_control_damage = _extend_damage Signal.sample

extend_pitch_damage :: Derive.Pitch -> Derive.EventDamage -> Derive.EventDamage
extend_pitch_damage = _extend_damage PitchSignal.sample

_extend_damage :: (RealTime -> sig -> [(RealTime, y)]) -> sig
    -> Derive.EventDamage -> Derive.EventDamage
_extend_damage sample sig (Derive.EventDamage ranges) = Derive.EventDamage $
    case Ranges.extract ranges of
        Nothing -> Ranges.everything
        Just pairs -> Ranges.sorted_ranges (map extend pairs)
    where
    extend (s, e) = (s, end)
        where
        end = case sample e sig of
            _ : (x, _) : _ -> x
            _ -> Types.max_real_time

-- TODO this can go away when pitches are calls
-- preprocess_pitch :: Call.PreProcess
-- preprocess_pitch expr = case Seq.break_last expr of
--     (calls, Just (TrackLang.Call (TrackLang.Symbol sym) [])) ->
--         calls ++ [TrackLang.Call (TrackLang.Symbol "set")
--                 [TrackLang.Literal (TrackLang.VNote (Pitch.Note sym))]]
--     _ -> expr


-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal away in
-- the Derive state as a 'Track.TrackSignal'.  I may or may not need to
-- re-derive the signal, for reasons explained in the TrackSignal doc.
--
-- TODO if TrackId appears in more than one place I may wind up running this
-- redundantly.  However, I think the proper way to solve this is to cache
-- the signals and avoid recalculating the control track at all.  Perhaps just
-- add a warped signal to TrackSignal?
stash_signal :: TrackId
    -> Either (PitchSignal.PitchSignal, Derive.PitchDeriver, Track.ScaleMap)
        (Signal.Signal y, Derive.Deriver (Signal.Signal y))
    -> Derive.Deriver ()
stash_signal track_id sig = do
    rendered <- track_is_rendered track_id
    if not rendered then return () else do
    maybe_linear <- linear_tempo
    case maybe_linear of
        Just (shift, stretch) -> do
            let tsig = case sig of
                    Left (psig, _, smap) -> Track.Pitch psig smap
                    Right (csig, _) -> Track.Control (Signal.coerce csig)
            put_track_signal track_id (Track.TrackSignal tsig shift stretch)
        Nothing -> do
            signal <- case sig of
                Left (_, deriver, smap) -> do
                    sig <- Derive.setup_without_warp deriver
                    return $ Track.Pitch sig smap
                Right (_, deriver) -> Track.Control . Signal.coerce <$>
                    Derive.setup_without_warp deriver
            put_track_signal track_id (Track.TrackSignal signal 0 1)

track_is_rendered :: TrackId -> Derive.Deriver Bool
track_is_rendered track_id = do
    track <- Derive.get_track track_id
    return $ case Track.render_style (Track.track_render track) of
        Track.NoRender -> False
        _ -> True

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Derive.d_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
linear_tempo :: Derive.Deriver (Maybe (ScoreTime, ScoreTime))
linear_tempo = do
    warp <- Derive.gets Derive.state_warp
    return $ if Score.warp_signal warp == Score.id_warp_signal
        then Just (Score.warp_shift warp, Score.warp_stretch warp)
        else Nothing

put_track_signal :: TrackId -> Track.TrackSignal -> Derive.Deriver ()
put_track_signal track_id tsig = Derive.modify_collect $ \st ->
    st { Derive.collect_track_signals =
        Map.insert track_id tsig (Derive.collect_track_signals st) }

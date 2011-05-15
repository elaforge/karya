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
import qualified Data.Tree as Tree
import Control.Monad

import Util.Control
import qualified Util.Ranges as Ranges

import Ui
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as Parse
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


type Pitch = PitchSignal.PitchSignal
type Control = Signal.Control

-- | Top level deriver for control tracks.
d_control_track :: State.EventsNode
    -> Derive.EventDeriver -> Derive.EventDeriver
d_control_track (Tree.Node track subs) deriver = do
    let title = State.tevents_title track
    if null title then deriver else do
    (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackInfo.parse_control_expr title)
    -- TODO event calls are evaluated in normalized time, but track calls
    -- aren't.  Should they be?
    eval_track track expr ctype deriver

eval_track :: State.TrackEvents -> TrackLang.Expr
    -> TrackInfo.ControlType -> Derive.EventDeriver -> Derive.EventDeriver
eval_track track expr ctype deriver = do
    let events = Track.event_list (State.tevents_events track)
        block_end = State.tevents_end track
        maybe_track_id = State.tevents_track_id track
    case ctype of
        TrackInfo.Tempo -> do
            let control_deriver = derive_control block_end expr events
            tempo_call block_end maybe_track_id control_deriver deriver
        TrackInfo.Control maybe_op control -> do
            let control_deriver = derive_control block_end expr events
            control_call maybe_track_id control maybe_op control_deriver deriver
        TrackInfo.Pitch ptype maybe_name ->
            pitch_call block_end maybe_track_id maybe_name ptype expr events
                deriver

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: ScoreTime -> Maybe TrackId
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
tempo_call block_end maybe_track_id sig_deriver deriver = do
    (signal, logs, damage) <- Derive.setup_without_warp sig_deriver
    when_just maybe_track_id $ \track_id -> do
        rendered <- track_is_rendered track_id
        when rendered $
            put_track_signal track_id $
                Track.TrackSignal (Track.Control (Signal.coerce signal)) 0 1
    -- TODO tempo damage should turn into score damage on all events after
    -- it.  It might be more regular to give all generators a dep on tempo,
    -- but this way is probably more efficient and just as clear.
    merge_logs logs $ Derive.with_control_damage (extend damage) $
        Derive.d_tempo block_end maybe_track_id (Signal.coerce signal) deriver
    where
    extend (Derive.EventDamage ranges) = Derive.EventDamage $
        case Ranges.extract ranges of
            Nothing -> Ranges.everything
            Just [] -> Ranges.nothing
            Just ((s, _) : _) -> Ranges.range s RealTime.max

control_call :: Maybe TrackId -> Score.Control -> Maybe TrackLang.CallId
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
control_call maybe_track_id control maybe_op control_deriver deriver = do
    (signal, logs, damage) <- track_setup maybe_track_id control_deriver
    stash_signal maybe_track_id (Right (signal, to_display <$> control_deriver))
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'control_deriver'.  To make these parallelize, I need
    -- to run control_deriver as a sub-derive, then mappend the Collect.
    merge_logs logs $ Derive.with_control_damage damage $
        with_control signal deriver
    where
    with_control signal deriver = do
        case maybe_op of
            Nothing -> Derive.with_control control signal deriver
            Just op -> Derive.with_control_operator control op signal deriver

to_display :: TrackResults Signal.Control -> Signal.Display
to_display (sig, _, _) = Signal.coerce sig
    -- I discard the logs since I think if there is anything interesting it
    -- will be logged in the "real" derivation.

merge_logs :: Derive.Events -> Derive.EventDeriver
    -> Derive.EventDeriver
merge_logs logs deriver = do
    events <- deriver
    return $ Derive.merge_events logs events

pitch_call :: ScoreTime -> Maybe TrackId -> Maybe Score.Control
    -> TrackInfo.PitchType -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.EventDeriver -> Derive.EventDeriver
pitch_call block_end maybe_track_id maybe_name ptype track_expr events deriver =
    track_setup maybe_track_id $ do
        (with_scale, scale) <- case ptype of
            TrackInfo.PitchRelative _ -> do
                -- TODO previously I mangled the scale to set the octave, but
                -- I can't do that now unless I put it in the ScaleId
                return (Derive.with_scale Relative.scale, Relative.scale)
            TrackInfo.PitchAbsolute (Just scale_id) -> do
                scale <- Derive.get_scale scale_id
                return (Derive.with_scale scale, scale)
            TrackInfo.PitchAbsolute Nothing -> do
                scale <- Call.get_scale
                return (id, scale)
        let scale_map = Scale.scale_map scale
            derive = derive_pitch block_end track_expr events
        with_scale $ case ptype of
            TrackInfo.PitchRelative op -> do
                (signal, logs, damage) <- derive
                stash_signal maybe_track_id
                    (Left (signal, to_psig <$> derive, scale_map))
                merge_logs logs $ Derive.with_control_damage damage $
                    Derive.with_pitch_operator maybe_name op signal deriver
            _ -> do
                (signal, logs, damage) <- derive
                stash_signal maybe_track_id
                    (Left (signal, to_psig <$> derive, scale_map))
                merge_logs logs $ Derive.with_control_damage damage $
                    Derive.with_pitch maybe_name signal deriver
    where to_psig (sig, _, _) = sig

track_setup :: Maybe TrackId -> Derive.Deriver d -> Derive.Deriver d
track_setup = maybe id Derive.track_setup


-- | Split the signal chunks and log msgs of the 'LEvent.LEvents' stream.
-- Return signal chunks merged into a signal, the logs cast to Score.Event
-- logs, and damage incurred deriving the track.
type TrackResults sig = (sig, Derive.Events, Derive.EventDamage)

-- | Create a deriver for a track with control events in it.  The deriver will
-- be run once in an unwarped context to generate signal for rendering (if its
-- track is being rendered), and once in the normal context for the signal to
-- place in the environment.
derive_control :: ScoreTime -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver (TrackResults Signal.Control)
derive_control block_end track_expr events = do
    stream <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "control track") track_expr deriver
    damage <- Derive.take_local_damage
    let (signal_chunks, logs) = LEvent.extract_events stream
        signal = Signal.merge signal_chunks
    return (signal, logs, extend_control_damage signal damage)
    where
    deriver :: Derive.ControlDeriver
    deriver = do
        state <- Derive.get
        let (stream, collect, cache) = Call.derive_track
                state block_end dinfo Parse.parse_num_expr last_sample [] events
        Derive.modify $ \st -> st {
            Derive.state_collect = collect, Derive.state_cache_state = cache }
        -- I can use concat instead of merge_asc_events because the signals
        -- will be merged with Signal.merge and I don't care if the logs
        -- are a little out of order.
        return (concat stream)
    dinfo = Call.DeriveInfo Call.lookup_control_call "control"
    last_sample prev chunk = Signal.last chunk `mplus` prev

derive_pitch :: ScoreTime -> TrackLang.Expr -> [Track.PosEvent]
    -> Derive.Deriver (TrackResults Pitch)
derive_pitch block_end track_expr events = do
    stream <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "pitch track") track_expr deriver
    damage <- Derive.take_local_damage
    let (signal_chunks, logs) = LEvent.extract_events stream
        signal = PitchSignal.merge signal_chunks
    return (signal, logs, extend_pitch_damage signal damage)
    where
    deriver = do
        state <- Derive.get
        let (stream, collect, cache) = Call.derive_track
                state block_end dinfo Parse.parse_expr last_sample [] events
        Derive.modify $ \st -> st {
            Derive.state_collect = collect, Derive.state_cache_state = cache }
        return (concat stream)
    dinfo = Call.DeriveInfo Call.lookup_pitch_call "pitch"
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev

-- | Event damage for a control track only extends to the last sample.
-- However, the actual changed region extends to the /next/ sample.
-- Hacky hack hack.
extend_control_damage :: Signal.Control -> Derive.EventDamage
    -> Derive.EventDamage
extend_control_damage = _extend_damage Signal.sample

extend_pitch_damage :: Pitch -> Derive.EventDamage -> Derive.EventDamage
extend_pitch_damage = _extend_damage PitchSignal.sample

-- TODO this will force the signal but only when the event damage itself is
-- forced... but when is that?  Test and find out.
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
            _ -> RealTime.max


-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal away in
-- the Derive state as a 'Track.TrackSignal'.  I may or may not need to
-- re-derive the signal, for reasons explained in the TrackSignal doc.
--
-- TODO if TrackId appears in more than one place I may wind up running this
-- redundantly.  However, I think the proper way to solve this is to cache
-- the signals and avoid recalculating the control track at all.  Perhaps just
-- add a warped signal to TrackSignal?
stash_signal :: Maybe TrackId
    -> Either (Pitch, Derive.Deriver Pitch, Track.ScaleMap)
        (Signal.Signal y, Derive.Deriver Signal.Display)
    -- ^ Either a PitchSignal or a control signal.  Both a signal and a deriver
    -- to produce the signal are provided.  If the block has no warp the
    -- already derived signal can be reused, otherwise it must be rederived.
    -> Derive.Deriver ()
stash_signal Nothing _ = return ()
stash_signal (Just track_id) sig = do
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
                    sig <- Derive.in_real_time deriver
                    return $ Track.Pitch sig smap
                Right (_, deriver) -> Track.Control . Signal.coerce <$>
                    Derive.in_real_time deriver
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

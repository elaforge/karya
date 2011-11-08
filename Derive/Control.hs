{- | Derivers for control tracks.

    Interpolation methods:

    - s - Set value at the point.  This is the default if there is no method.

    - i - Approach with linear interpolation.

    - #e - Approach with exponential interpolation with #.  # defaults to 2.

    - method;val - Approach val with method, then jump to val.
-}
module Derive.Control where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Log as Log
import Ui
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as Parse
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Relative as Relative
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


type Pitch = PitchSignal.PitchSignal
type Control = Signal.Control

-- | As returned by 'State.tevents_range', happpens to be used a lot here.
type TrackRange = (ScoreTime, ScoreTime)

-- | Top level deriver for control tracks.
d_control_track :: State.EventsNode
    -> Derive.EventDeriver -> Derive.EventDeriver
d_control_track (Tree.Node track _) deriver = do
    let title = State.tevents_title track
    if null title then deriver else do
    (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackInfo.parse_control_expr title)
    eval_track track expr ctype deriver

eval_track :: State.TrackEvents -> TrackLang.Expr
    -> TrackInfo.ControlType -> Derive.EventDeriver -> Derive.EventDeriver
eval_track track expr ctype deriver = case ctype of
    TrackInfo.Tempo ->
        tempo_call track (derive_control tempo_track expr) deriver
    TrackInfo.Control maybe_op control ->
        control_call track control maybe_op (derive_control track expr) deriver
    TrackInfo.Pitch ptype maybe_name ->
        pitch_call track maybe_name ptype expr deriver
    where
    tempo_track = track { State.tevents_events = tempo_events }
    -- This is a hack due to the way the tempo track works.  Further notes
    -- are on the 'Perform.Signal.integrate' doc.
    tempo_events
        | Maybe.isNothing (Events.at (snd track_range) evts) =
            Events.insert_events
                [(snd track_range, Event.event "set-prev" 0)] evts
        | otherwise = evts
        where
        track_range = State.tevents_range track
        evts = State.tevents_events track

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: State.TrackEvents -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
tempo_call track sig_deriver deriver = do
    (signal, logs) <- Internal.setup_without_warp sig_deriver
    when_just maybe_track_id $ \track_id ->
        unless (State.tevents_sliced track) $
            put_track_signal track_id $ Right $
                Track.TrackSignal (Track.Control (Signal.coerce signal)) 0 1
    -- 'with_damage' must be applied *inside* 'd_tempo'.  If it were outside,
    -- it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    merge_logs logs $ Internal.d_tempo (snd track_range) maybe_track_id
        (Signal.coerce signal) (with_damage deriver)
    where
    maybe_track_id = State.tevents_track_id track
    with_damage = maybe id get_damage maybe_track_id
    get_damage track_id deriver = do
        damage <- Cache.get_tempo_damage track_id track_range
        Internal.with_control_damage damage deriver
    track_range = State.tevents_range track

control_call :: State.TrackEvents -> Score.Control
    -> Maybe TrackLang.CallId -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
control_call track control maybe_op control_deriver deriver = do
    (signal, logs) <- Internal.track_setup track control_deriver
    stash_signal track (Right (signal, to_display <$> control_deriver))
    -- I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'control_deriver'.  To make these parallelize, I need
    -- to run control_deriver as a sub-derive, then mappend the Collect.
    merge_logs logs $ with_damage $ with_control control signal deriver
    where
    maybe_track_id = State.tevents_track_id track
    with_damage = with_control_damage maybe_track_id
        (State.tevents_range track)
    with_control control signal deriver = case maybe_op of
        Nothing -> Derive.with_control control signal deriver
        Just op -> Derive.with_control_operator control op signal deriver

to_display :: TrackResults Signal.Control -> Signal.Display
to_display (sig, _) = Signal.coerce sig
    -- I discard the logs since I think if there is anything interesting it
    -- will be logged in the "real" derivation.

merge_logs :: [Log.Msg] -> Derive.EventDeriver -> Derive.EventDeriver
merge_logs logs deriver = do
    events <- deriver
    return $ Derive.merge_events (map LEvent.Log logs) events

pitch_call :: State.TrackEvents -> Maybe Score.Control -> TrackInfo.PitchType
    -> TrackLang.Expr -> Derive.EventDeriver -> Derive.EventDeriver
pitch_call track maybe_name ptype expr deriver =
    Internal.track_setup track $ do
        (scale, new_scale) <- get_scale ptype
        let scale_map = Scale.scale_map scale
            derive = derive_pitch track expr
        (if new_scale then Derive.with_scale scale else id) $ case ptype of
            TrackInfo.PitchRelative op -> do
                (signal, logs) <- derive
                stash_signal track
                    (Left (signal, to_psig <$> derive, scale_map))
                merge_logs logs $ with_damage $
                    Derive.with_pitch_operator maybe_name op signal deriver
            _ -> do
                (signal, logs) <- derive
                stash_signal track
                    (Left (signal, to_psig <$> derive, scale_map))
                merge_logs logs $ with_damage $
                    Derive.with_pitch maybe_name signal deriver
    where
    maybe_track_id = State.tevents_track_id track
    with_damage = with_control_damage maybe_track_id
        (State.tevents_range track)
    to_psig (sig, _) = sig

get_scale :: TrackInfo.PitchType -> Derive.Deriver (Scale.Scale, Bool)
get_scale ptype = case ptype of
    TrackInfo.PitchRelative _ -> do
        -- TODO previously I mangled the scale to set the octave, but
        -- I can't do that now unless I put it in the ScaleId
        return (Relative.scale, True)
    TrackInfo.PitchAbsolute (Just scale_id) -> do
        scale <- Derive.get_scale scale_id
        return (scale, True)
    TrackInfo.PitchAbsolute Nothing -> do
        scale <- Util.get_scale
        return (scale, False)

with_control_damage :: Maybe TrackId -> TrackRange
    -> Derive.Deriver d -> Derive.Deriver d
with_control_damage maybe_track_id track_range =
    maybe id get_damage maybe_track_id
    where
    get_damage track_id deriver = do
        damage <- Cache.get_control_damage track_id track_range
        Internal.with_control_damage damage deriver


-- | Split the signal chunks and log msgs of the 'LEvent.LEvents' stream.
-- Return signal chunks merged into a signal, the logs cast to Score.Event
-- logs.
type TrackResults sig = (sig, [Log.Msg])

-- | Derive the signal of a control track.
derive_control :: State.TrackEvents -> TrackLang.Expr
    -> Derive.Deriver (TrackResults Signal.Control)
derive_control track expr = do
    stream <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "control track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        signal = Signal.merge signal_chunks
    return (signal, logs)
    where
    deriver :: Derive.ControlDeriver
    deriver = do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                Parse.parse_num_expr last_sample (tevents track)
        Internal.merge_collect collect
        -- I can use concat instead of merge_asc_events because the signals
        -- will be merged with Signal.merge and I don't care if the logs
        -- are a little out of order.
        return (concat stream)
    dinfo = Call.DeriveInfo Call.lookup_control_call "control"
    tinfo = Call.TrackInfo (State.tevents_end track)
        (State.tevents_range track) [] dinfo
    last_sample prev chunk = Signal.last chunk `mplus` prev

derive_pitch :: State.TrackEvents -> TrackLang.Expr
    -> Derive.Deriver (TrackResults Pitch)
derive_pitch track expr = do
    stream <- Call.apply_transformer
        (dinfo, Derive.dummy_call_info "pitch track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        signal = PitchSignal.merge signal_chunks
    return (signal, logs)
    where
    deriver = do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                Parse.parse_expr last_sample (tevents track)
        Internal.merge_collect collect
        return (concat stream)
    dinfo = Call.DeriveInfo Call.lookup_pitch_call "pitch"
    tinfo = Call.TrackInfo (State.tevents_end track)
        (State.tevents_range track) [] dinfo
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev

tevents :: State.TrackEvents -> [Events.PosEvent]
tevents = Events.ascending . State.tevents_events


-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal away in
-- the Derive state as a 'Track.TrackSignal'.  I may or may not need to
-- re-derive the signal, for reasons explained in the TrackSignal doc.
--
-- TODO if TrackId appears in more than one place I may wind up running this
-- redundantly.  However, I think the proper way to solve this is to cache
-- the signals and avoid recalculating the control track at all.  Perhaps just
-- add a warped signal to TrackSignal?
stash_signal :: State.TrackEvents
    -> Either (Pitch, Derive.Deriver Pitch, Track.ScaleMap)
        (Signal.Signal y, Derive.Deriver Signal.Display)
    -- ^ Either a PitchSignal or a control signal.  Both a signal and
    -- a deriver to produce the signal are provided.  If the block has no warp
    -- the already derived signal can be reused, otherwise it must be
    -- rederived.
    -> Derive.Deriver ()
stash_signal track sig =
    case (State.tevents_track_id track, State.tevents_sliced track) of
        (Just track_id, False) -> stash track_id
        _ -> return ()
    where
    stash track_id = do
        maybe_linear <- linear_tempo
        case maybe_linear of
            Just (shift, stretch) -> do
                let tsig = case sig of
                        Left (psig, _, smap) -> Track.Pitch psig smap
                        Right (csig, _) -> Track.Control (Signal.coerce csig)
                put_track_signal track_id $ Right $
                    Track.TrackSignal tsig shift stretch
            Nothing -> do
                signal <- run_sub $ case sig of
                    Left (_, deriver, smap) -> do
                        sig <- Derive.in_real_time deriver
                        return $ Track.Pitch sig smap
                    Right (_, deriver) -> Track.Control . Signal.coerce <$>
                        Derive.in_real_time deriver
                put_track_signal track_id
                    (fmap (\s -> Track.TrackSignal s 0 1) signal)

-- | Ensure the computation runs lazily by detaching it from the state.  This
-- is important because the track signal will not necessarily be demanded.
-- Details in 'Ui.Track.TrackSignals'.
run_sub :: Derive.Deriver a -> Derive.Deriver (Either [Log.Msg] a)
run_sub d = do
    state <- Derive.get
    let (result, _, logs) = Derive.run state d
    return $ case result of
        Right val -> Right val
        Left err -> Left $ Derive.error_to_warn err : logs

-- | Return (shift, stretch) if the tempo is linear.  This relies on an
-- optimization in 'Derive.d_tempo' to notice when the tempo is constant and
-- give it 'Score.id_warp_signal'.
linear_tempo :: Derive.Deriver (Maybe (ScoreTime, ScoreTime))
linear_tempo = do
    warp <- Internal.get_dynamic Derive.state_warp
    return $ if Score.warp_signal warp == Score.id_warp_signal
        then Just (Score.warp_shift warp, Score.warp_stretch warp)
        else Nothing

put_track_signal :: TrackId -> Either [Log.Msg] Track.TrackSignal
    -> Derive.Deriver ()
put_track_signal track_id tsig = put_track_signals [(track_id, tsig)]

put_track_signals :: [(TrackId, Either [Log.Msg] Track.TrackSignal)]
    -> Derive.Deriver ()
put_track_signals [] = return ()
put_track_signals tracks = Internal.merge_collect $ mempty
    { Derive.collect_track_signals = Map.fromList tracks }

-- * track_signal

-- | Derive just the rendered track signal from a track, if this track is to
-- be rendered.  This is like 'eval_track' but specialized to derive only the
-- signal.  The track signal is normally stashed as a side-effect of control
-- track evaluation, but tracks below a note track are not evaluated normally.
track_signal :: State.TrackEvents
    -> Derive.Deriver (Either [Log.Msg] Track.TrackSignal)
track_signal track
    | null title = return $ Left []
    | otherwise = do
        -- Note tracks don't have signals.
        if TrackInfo.is_note_track title then return (Left []) else do
        (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
            return (TrackInfo.parse_control_expr title)
        run_sub $ eval_signal track expr ctype
    where title = State.tevents_title track

eval_signal :: State.TrackEvents -> TrackLang.Expr
    -> TrackInfo.ControlType -> Derive.Deriver Track.TrackSignal
eval_signal track expr ctype = case ctype of
    TrackInfo.Tempo -> do
        (sig, logs) <- derive_control track expr
        mapM_ Log.write logs
        return $ control_sig sig
    TrackInfo.Control _ _ -> do
        (sig, logs) <- derive_control track expr
        mapM_ Log.write logs
        return $ control_sig sig
    TrackInfo.Pitch ptype _ -> do
        (sig, logs) <- derive_pitch track expr
        mapM_ Log.write logs
        (scale, _) <- get_scale ptype
        return $ pitch_sig sig (Scale.scale_map scale)
    where
    control_sig sig = Track.TrackSignal (Track.Control (Signal.coerce sig)) 0 1
    pitch_sig sig smap = Track.TrackSignal (Track.Pitch sig smap) 0 1

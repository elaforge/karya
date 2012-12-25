{- | Derivers for control tracks.  That means tempo, control, and pitch.

    Control tracks (specifically control tracks, not tempo or pitch) can have
    a combining operator.  If no operator is given, they are combined with
    @mul@.  @set@ will replace the signal.  So two tracks named @c@ will
    multiply, same as if the second were @mul c@.  If you want to override @c@
    then @set c@ will do that.

    A control with a combining operator but nothing to combine with should still
    do something sensible because operators come with an identity value, e.g.
    @1@ for @mul@ and @0@ for @add@.

    Tempo tracks don't support operators because they are converted into
    a warp, which is then combined via composition.  Pitch tracks always
    replace each other because adding together absolute pitches is undefined.
    Relative pitches can be added or multiplied, and this is expressed via
    normal controls using transposition signals like 'Score.c_chromatic'.
-}
module Derive.Control where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Track as Track
import qualified Ui.TrackTree as TrackTree

import qualified Derive.Cache as Cache
import qualified Derive.Call as Call
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Types


type Pitch = PitchSignal.Signal
type Control = Signal.Control

-- | As returned by 'TrackTree.tevents_range', happpens to be used a lot here.
type TrackRange = (ScoreTime, ScoreTime)

-- | Top level deriver for control tracks.
d_control_track :: TrackTree.EventsNode
    -> Derive.EventDeriver -> Derive.EventDeriver
d_control_track (Tree.Node track _) deriver = do
    let title = TrackTree.tevents_title track
    if null title then deriver else do
    (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
        return (TrackInfo.parse_control_expr title)
    eval_track track expr ctype deriver

eval_track :: TrackTree.TrackEvents -> [TrackLang.Call]
    -> TrackInfo.ControlType -> Derive.EventDeriver -> Derive.EventDeriver
eval_track track expr ctype deriver = case ctype of
    TrackInfo.Tempo -> ifM Derive.is_lilypond_derive deriver $
        tempo_call track (derive_control True tempo_track expr) deriver
    TrackInfo.Control maybe_op control -> do
        op <- lookup_op maybe_op
        control_call track control op (derive_control False track expr) deriver
    TrackInfo.Pitch scale_id maybe_name ->
        pitch_call track maybe_name scale_id expr deriver
    where
    tempo_track = track { TrackTree.tevents_events = tempo_events }
    -- This is a hack due to the way the tempo track works.  Further notes
    -- are on the 'Perform.Signal.integrate' doc.
    tempo_events
        | Maybe.isNothing (Events.at (snd track_range) evts) =
            Events.insert [Event.event (snd track_range) 0 "set-prev"] evts
        | otherwise = evts
        where
        track_range = TrackTree.tevents_range track
        evts = TrackTree.tevents_events track

lookup_op :: Maybe TrackLang.CallId -> Derive.Deriver (Maybe Derive.ControlOp)
lookup_op op = case op of
    Nothing -> return $ Just Derive.op_mul
    Just sym
        | sym == TrackLang.Symbol "set" -> return Nothing
        | otherwise -> Just <$> Derive.get_control_op sym

-- | A tempo track is derived like other signals, but in absolute time.
-- Otherwise it would wind up being composed with the environmental warp twice.
tempo_call :: TrackTree.TrackEvents
    -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
tempo_call track sig_deriver deriver = do
    (signal, logs) <- Internal.setup_without_warp sig_deriver
    when_just maybe_track_id $ \track_id ->
        unless (TrackTree.tevents_sliced track) $
            put_track_signal track_id $ Right $
                Track.TrackSignal (Signal.coerce signal) 0 1 Nothing
    -- 'with_damage' must be applied *inside* 'd_tempo'.  If it were outside,
    -- it would get the wrong RealTimes when it tried to create the
    -- ControlDamage.
    merge_logs logs $ Internal.d_tempo (snd track_range) maybe_track_id
        (Signal.coerce signal) (with_damage deriver)
    where
    maybe_track_id = TrackTree.tevents_track_id track
    with_damage = maybe id get_damage maybe_track_id
    get_damage track_id deriver = do
        damage <- Cache.get_tempo_damage track_id track_range
        Internal.with_control_damage damage deriver
    track_range = TrackTree.tevents_range track

control_call :: TrackTree.TrackEvents -> Score.Typed Score.Control
    -> Maybe Derive.ControlOp -> Derive.Deriver (TrackResults Signal.Control)
    -> Derive.EventDeriver -> Derive.EventDeriver
control_call track control maybe_op control_deriver deriver = do
    (signal, logs) <- Internal.track_setup track control_deriver
    stash_signal track signal (to_display <$> control_deriver) Nothing
    -- Apply and strip any control modifications made during the above derive.
    Derive.apply_control_modifications $ merge_logs logs $ with_damage $
        with_control control signal deriver
    -- ^ I think this forces sequentialness because 'deriver' runs in the state
    -- from the end of 'control_deriver'.  To make these parallelize, I need to
    -- run control_deriver as a sub-derive, then mappend the Collect.
    where
    maybe_track_id = TrackTree.tevents_track_id track
    with_damage = with_control_damage maybe_track_id
        (TrackTree.tevents_range track)
    with_control (Score.Typed typ control) signal deriver = case maybe_op of
        Nothing -> Derive.with_control control sig deriver
        Just op -> Derive.with_relative_control control op sig deriver
        where sig = Score.Typed typ signal

to_display :: TrackResults Signal.Control -> Signal.Display
to_display (sig, _) = Signal.coerce sig
    -- I discard the logs since I think if there is anything interesting it
    -- will be logged in the "real" derivation.

merge_logs :: [Log.Msg] -> Derive.EventDeriver -> Derive.EventDeriver
merge_logs logs deriver = do
    events <- deriver
    return $ Derive.merge_events (map LEvent.Log logs) events

pitch_call :: TrackTree.TrackEvents -> Maybe Score.Control -> Pitch.ScaleId
    -> [TrackLang.Call] -> Derive.EventDeriver -> Derive.EventDeriver
pitch_call track maybe_name scale_id expr deriver =
    Internal.track_setup track $ do
        scale <- get_scale scale_id
        Derive.with_scale scale $ do
            let scale_map = Scale.scale_map scale
                derive = derive_pitch track expr
            (signal, logs) <- derive
            -- Ignore errors, they should be logged on conversion.
            (nn_sig, _) <- pitch_signal_to_nn signal
            stash_signal track (Signal.coerce nn_sig) (to_psig derive)
                (Just scale_map)
            -- Apply and strip any control modifications made during the above
            -- derive.
            Derive.apply_control_modifications $ merge_logs logs $ with_damage $
                Derive.with_pitch maybe_name signal deriver
    where
    maybe_track_id = TrackTree.tevents_track_id track
    with_damage = with_control_damage maybe_track_id
        (TrackTree.tevents_range track)
    to_psig derive = do
        (sig, _) <- derive
        Signal.coerce . fst <$> pitch_signal_to_nn sig

get_scale :: Pitch.ScaleId -> Derive.Deriver Scale.Scale
get_scale scale_id
    | scale_id == Pitch.empty_scale = Util.get_scale
    | otherwise = Derive.get_scale scale_id

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
derive_control :: Bool -> TrackTree.TrackEvents -> [TrackLang.Call]
    -> Derive.Deriver (TrackResults Signal.Control)
derive_control is_tempo track expr = do
    stream <- Call.apply_transformer
        (Derive.dummy_call_info 0 1 "control track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        signal = Signal.merge signal_chunks
    return (signal, logs)
    where
    deriver :: Derive.ControlDeriver
    deriver = do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                last_sample (tevents track)
        Internal.merge_collect collect
        -- I can use concat instead of merge_asc_events because the signals
        -- will be merged with Signal.merge and I don't care if the logs
        -- are a little out of order.
        return (concat stream)
    tinfo = Call.TrackInfo
        { Call.tinfo_events_end = TrackTree.tevents_end track
        , Call.tinfo_track_range = TrackTree.tevents_range track
        , Call.tinfo_shifted = TrackTree.tevents_shifted track
        , Call.tinfo_sub_tracks = []
        -- TODO provide events around for control tracks?
        , Call.tinfo_events_around = ([], [])
        , Call.tinfo_type =
            if is_tempo then TrackInfo.TempoTrack else TrackInfo.ControlTrack
        }
    last_sample prev chunk = Signal.last chunk `mplus` prev

derive_pitch :: TrackTree.TrackEvents -> [TrackLang.Call]
    -> Derive.Deriver (TrackResults Pitch)
derive_pitch track expr = do
    stream <- Call.apply_transformer
        (Derive.dummy_call_info 0 1 "pitch track") expr deriver
    let (signal_chunks, logs) = LEvent.partition stream
        signal = mconcat signal_chunks
    return (signal, logs)
    where
    deriver = do
        state <- Derive.get
        let (stream, collect) = Call.derive_track state tinfo
                last_sample (tevents track)
        Internal.merge_collect collect
        return (concat stream)
    tinfo = Call.TrackInfo
        { Call.tinfo_events_end = TrackTree.tevents_end track
        , Call.tinfo_track_range = TrackTree.tevents_range track
        , Call.tinfo_shifted = TrackTree.tevents_shifted track
        , Call.tinfo_sub_tracks = []
        -- TODO provide events around for control tracks?
        , Call.tinfo_events_around = ([], [])
        , Call.tinfo_type = TrackInfo.PitchTrack
        }
    last_sample prev chunk = PitchSignal.last chunk `mplus` prev

tevents :: TrackTree.TrackEvents -> [Event.Event]
tevents = Events.ascending . TrackTree.tevents_events


-- * TrackSignal

-- | If this track is to be rendered by the UI, stash the given signal away in
-- the Derive state as a 'Track.TrackSignal'.  I may or may not need to
-- re-derive the signal, for reasons explained in the TrackSignal doc.
--
-- TODO if TrackId appears in more than one place I may wind up running this
-- redundantly.  However, I think the proper way to solve this is to cache
-- the signals and avoid recalculating the control track at all.  Perhaps just
-- add a warped signal to TrackSignal?
stash_signal :: TrackTree.TrackEvents
    -> Signal.Signal y -> Derive.Deriver Signal.Display
    -- ^ Both a signal and a deriver to produce the signal are provided.  If
    -- the block has no warp the already derived signal can be reused,
    -- otherwise it must be rederived.
    -> Maybe Track.ScaleMap
    -- ^ A pitch track is given a ScaleMap so it can be displayed properly.
    -> Derive.Deriver ()
stash_signal track sig derive_sig scale_map =
    case TrackTree.tevents_track_id track of
        Just track_id | not (TrackTree.tevents_sliced track) ->
            stash track_id =<< linear_tempo
        _ -> return ()
    where
    stash track_id (Just (shift, stretch)) =
        put_track_signal track_id $ Right $
            Track.TrackSignal (Signal.coerce sig) shift stretch scale_map
    stash track_id Nothing = do
        logs_or_sig <- run_sub $ Derive.in_real_time derive_sig
        put_track_signal track_id $ case logs_or_sig of
            Left logs -> Left logs
            Right sig -> Right $ Track.TrackSignal sig 0 1 scale_map

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
track_signal :: TrackTree.TrackEvents
    -> Derive.Deriver (Either [Log.Msg] Track.TrackSignal)
track_signal track
    | null title = return $ Left []
    | otherwise = do
        -- Note tracks don't have signals.
        if TrackInfo.is_note_track title then return (Left []) else do
        (ctype, expr) <- either (\err -> Derive.throw $ "track title: " ++ err)
            return (TrackInfo.parse_control_expr title)
        -- Since these signals are being rendered solely for display, they
        -- should ignore any tempo warping, just like 'stash_signal' does
        -- above.
        run_sub $ Derive.in_real_time $ eval_signal track expr ctype
    where title = TrackTree.tevents_title track

eval_signal :: TrackTree.TrackEvents -> [TrackLang.Call]
    -> TrackInfo.ControlType -> Derive.Deriver Track.TrackSignal
eval_signal track expr ctype = case ctype of
    TrackInfo.Tempo -> do
        (sig, logs) <- derive_control True track expr
        mapM_ Log.write logs
        return $ track_sig sig Nothing
    TrackInfo.Control _ _ -> do
        (sig, logs) <- derive_control False track expr
        mapM_ Log.write logs
        return $ track_sig sig Nothing
    TrackInfo.Pitch scale_id _ -> do
        (sig, logs) <- derive_pitch track expr
        mapM_ Log.write logs
        scale <- get_scale scale_id
        -- TODO I log derivation errors... why not log pitch errors?
        (nn_sig, _) <- pitch_signal_to_nn sig
        -- TODO this is incorrect, it should return scale degrees, not NNs.
        -- But that would mean that PitchSignal.Pitches need to be able to emit
        -- degrees in addition to NoteNumbers.
        return $ track_sig nn_sig (Just (Scale.scale_map scale))
    where
    track_sig sig scale_map =
        Track.TrackSignal (Signal.coerce sig) 0 1 scale_map


-- * util

-- | Reduce a 'PitchSignal.Signal' to raw note numbers, taking the current
-- transposition environment into account.
pitch_signal_to_nn :: PitchSignal.Signal
    -> Derive.Deriver (Signal.NoteNumber, [PitchSignal.PitchError])
pitch_signal_to_nn sig = do
    controls <- Internal.get_dynamic Derive.state_controls
    return $ PitchSignal.to_nn $ PitchSignal.apply_controls controls sig

-- | Basic calls for note tracks.
module Derive.Call.Note where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Util.Control

import Ui
import qualified Ui.Event as Event
import qualified Ui.State as State

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Midi.Instrument as Instrument


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    , ("=", Call.c_equal)
    ]

-- * note call

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.NoteCall
c_note = Derive.Call "note"
    (Just $ Derive.GeneratorCall generate Derive.NonCachingGenerator)
    (Just $ Derive.TransformerCall transform Derive.NonIncremental)
    where
    generate args = case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) ->
            Right $ generate_note inst rel_attrs
                (Derive.passed_event args) (next_start args)
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid
    transform args deriver = case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) -> Right $ transform_note inst rel_attrs deriver
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid
    process = process_note_args Nothing []
    next_start args = case Derive.passed_next_events args of
        [] -> Derive.info_block_end (Derive.passed_info args)
        (pos, _) : _ -> pos

-- ** generate

generate_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Event.Event -> ScoreTime -> Derive.EventDeriver
generate_note n_inst rel_attrs event next_start = do
    let (from, to) = if Event.event_duration event < 0 then (1, 0) else (0, 1)
    start <- Derive.score_to_real from
    end <- Derive.score_to_real to
    -- Note that due to negative durations, the end could be before the start.
    -- What this really means is that the sounding duration of the note depends
    -- on the next one, which should be sorted out later by post processing.
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    multiplexed <- inst_is_multiplexed inst
    st <- Derive.get
    real_next <- Derive.score_to_real next_start
    let controls = if multiplexed
            then trimmed_controls start real_next (Derive.state_controls st)
            else Derive.state_controls st
        -- Perform.Midi.Convert flattens the entire pitch signal, so it's best
        -- to always trim the pitch to avoid extra work.
        pitch_sig = trimmed_pitch start real_next (Derive.state_pitch st)
    return [Score.Event start (end - start) (Event.event_text event)
        controls pitch_sig (Derive.state_stack st) inst (apply rel_attrs attrs)]
    where
    apply rel_attrs attrs =
        List.foldl' (.) id (map TrackLang.set_attr rel_attrs) attrs

inst_is_multiplexed :: Maybe Score.Instrument -> Derive.Deriver Bool
inst_is_multiplexed Nothing = return False
inst_is_multiplexed (Just inst) = do
    config <- State.state_midi_config <$> Derive.gets Derive.state_ui
    return $ case Map.lookup inst (Instrument.config_alloc config) of
        Just (_:_) -> True
        _ -> False

-- | In a note track, the pitch signal for each note is constant as soon as the
-- next note begins.  Otherwise, it looks like each note changes pitch during
-- its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch start end =
    PitchSignal.truncate end . PitchSignal.drop_before start

-- Trims will almost all be increasing in time.  Can I save indices or
-- something to make them faster?  That would only work with linear search
-- though.
--
-- It would be nice to strip out deriver-only controls, but without specific
-- annotation I can't know which ones those are.  Presumably laziness will
-- do its thing?
trimmed_controls :: RealTime -> RealTime -> Score.ControlMap -> Score.ControlMap
trimmed_controls start end controls = Map.map trim controls
    where trim = Signal.truncate end . Signal.drop_before start

-- ** transform

transform_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Derive.EventDeriver -> Derive.EventDeriver
transform_note n_inst rel_attrs deriver = with_inst (with_attrs deriver)
    where
    with_inst = maybe id (Derive.with_val TrackLang.v_instrument) n_inst
    with_attrs =
        foldl (.) id (map (Derive.with_val TrackLang.v_attributes) rel_attrs)

process_note_args :: Maybe Score.Instrument
    -> [TrackLang.RelativeAttr] -> [TrackLang.Val]
    -> (Maybe Score.Instrument, [TrackLang.RelativeAttr], [TrackLang.Val])
process_note_args inst attrs args = (inst', attrs', reverse invalid)
    where
    (inst', attrs', invalid) = List.foldl' go (inst, attrs, []) args
    go (inst, attrs, invalid) arg = case arg of
        TrackLang.VInstrument new_inst
            | TrackLang.is_null_instrument new_inst -> (inst, attrs, invalid)
            | otherwise -> (Just new_inst, attrs, invalid)
        TrackLang.VRelativeAttr rel_attr ->
            (inst, attrs ++ [rel_attr], invalid)
        _ -> (inst, attrs, arg : invalid)

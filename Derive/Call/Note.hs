-- | Basic calls for note tracks.
module Derive.Call.Note where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Util.Control

import Ui
import qualified Ui.Event as Event
import qualified Ui.Track as Track

import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    -- | Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    , ("=", Call.c_equal Derive.no_events)
    ]

-- * note call

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.NoteCall
c_note = Derive.Call "note"
    (Just $ \args -> case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) ->
            Right $ one_note $ generate_note inst rel_attrs
                (Derive.passed_event args) (Derive.passed_next_events args)
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid)
    (Just $ \args deriver -> case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) -> Right $ transform_note inst rel_attrs deriver
        (_, _, invalid) -> Left $
            TrackLang.ArgError $ "expected inst or attr: " ++ show invalid)
    where
    process = process_note_args Nothing []

generate_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Event.Event -> [Track.PosEvent] -> Derive.EventDeriver
generate_note n_inst rel_attrs event next = do
    let (from, to) = if Event.event_duration event < 0 then (1, 0) else (0, 1)
    start <- Derive.score_to_real from
    end <- Derive.score_to_real to
    -- TODO due to negative durations, end could be before start.  I need to
    -- add a post-proc step to calculate the proper durations
    next_start <- case next of
        [] -> return Nothing
        (npos, _) : _ -> fmap Just (Derive.score_to_real npos)
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    (controls, pitch_sig) <- Derive.unwarped_controls
    st <- Derive.get
    return [Score.Event start (end - start)
        (Event.event_text event) controls (trimmed_pitch next_start pitch_sig)
        -- state_stack is kept in reverse order
        (reverse (Derive.state_stack st)) inst (apply rel_attrs attrs)]
    where
    apply rel_attrs attrs =
        List.foldl' (.) id (map TrackLang.set_attr rel_attrs) attrs

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

-- | In a note track, the pitch signal for each note is constant as soon as the
-- next note begins.  Otherwise, it looks like each note changes pitch during
-- its decay.
-- TODO when signals are lazy I should drop the heads of the control
-- signals so they can be gced.
trimmed_pitch :: Maybe RealTime -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch (Just next) sig = PitchSignal.truncate next sig
trimmed_pitch Nothing sig = sig

-- * misc

one_note :: Derive.EventDeriver -> (Derive.EventDeriver, Int)
one_note d = (d, 1)

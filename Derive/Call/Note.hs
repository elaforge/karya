-- | Basic calls for note tracks.
module Derive.Call.Note where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    , ("=", Util.c_equal)
    ]

-- * note

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.NoteCall
c_note = Derive.Call "note"
    (Just $ Derive.GeneratorCall (inverting_call generate) (const Nothing))
    (Just $ Derive.TransformerCall transform)
    where
    generate args = case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) ->
            generate_note inst rel_attrs (Derive.passed_event args)
                (Derive.passed_next args)
        (_, _, invalid) -> Derive.throw_arg_error $
            "expected inst or attr: " ++ show invalid
    transform args deriver = case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) -> transform_note inst rel_attrs deriver
        (_, _, invalid) ->
            Derive.throw_arg_error $ "expected inst or attr: " ++ show invalid
    process = process_note_args Nothing []

-- ** generate

generate_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Events.PosEvent -> ScoreTime -> Derive.EventDeriver
generate_note n_inst rel_attrs (pos, event) next_start = do
    start <- Derive.score_to_real pos
    end <- Derive.score_to_real (pos + Event.event_duration event)
    -- Note that due to negative durations, the end could be before the start.
    -- What this really means is that the sounding duration of the note depends
    -- on the next one, which should be sorted out later by post processing.
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    st <- Derive.gets Derive.state_dynamic
    real_next <- Derive.score_to_real next_start
    let controls = trimmed_controls start real_next (Derive.state_controls st)
        -- Perform.Midi.Convert flattens the entire pitch signal, so it's best
        -- to always trim the pitch to avoid extra work.
        pitch_sig = trimmed_pitch start real_next (Derive.state_pitch st)
    return $! LEvent.one $! LEvent.Event $!
        Score.Event start (end - start) (Event.event_bs event) controls
            pitch_sig (Derive.state_stack st) inst (apply rel_attrs attrs)
    where
    apply rel_attrs attrs =
        List.foldl' (.) id (map TrackLang.set_attr rel_attrs) attrs

-- | In a note track, the pitch signal for each note is constant as soon as the
-- next note begins.  Otherwise, it looks like each note changes pitch during
-- its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.PitchSignal
    -> PitchSignal.PitchSignal
trimmed_pitch start end =
    PitchSignal.truncate end . PitchSignal.drop_before start

-- | Trim control signals to the given range.
--
-- Trims will almost all be increasing in time.  Can I save indices or
-- something to make them faster?  That would only work with linear search
-- though.
--
-- It would be nice to strip out deriver-only controls, but without specific
-- annotation I can't know which ones those are.  Presumably laziness will
-- do its thing?
trimmed_controls :: RealTime -> RealTime -> Score.ControlMap
    -> Score.ControlMap
trimmed_controls start end controls = Map.map trim controls
    where trim = Signal.truncate end . Signal.drop_before start

-- ** transform

transform_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Derive.EventDeriver -> Derive.EventDeriver
transform_note n_inst rel_attrs deriver = with_inst (with_attrs deriver)
    where
    with_inst = maybe id Derive.with_instrument n_inst
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


-- * util

-- Utilities for note calls.

-- ** invert

-- | Convert a call into an inverting call.  Documented in doc/inverting_call.
--
-- This requires a bit of hackery in a couple of places:
--
-- The first is that the TrackLang.Val Pretty instance is expected to emit
-- parseable code.  This is because Derive.Call.Note.inverting_call wants the
-- text of the generator it was called for.  Unfortunately this is tricky to
-- get directly because the parser takes a complete string to a complete Expr.
-- So instead I keep the parsed expr by putting it in CallInfo, and use the
-- Pretty instance to turn it back into a string, so it can be parsed again
-- when it is evaluated for real.  It's rather convoluted, but trying to come
-- up with a derive_tracks where the events may already be parsed also seems
-- convoluted.
--
-- The second is that CallInfo has to keep the expression in
-- 'Derive.Derive.info_info_expr'.
--
-- TODO don't invert the call if there are subtracks but after slicing they
-- are empty
inverting_call :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
inverting_call call args = case Derive.info_sub_tracks info of
        [] -> call args
        subs -> invert subs pos (pos + Event.event_duration event)
            (Derive.passed_next args) expr
    where
    (pos, event) = Derive.info_event info
    -- See comment in Derive.TrackLang.Val Pretty instance.
    expr = maybe "" Pretty.pretty $
            Seq.last (Derive.info_expr (Derive.passed_info args))
    info = Derive.passed_info args

invert :: State.EventsTree -> ScoreTime -> ScoreTime -> ScoreTime -> String
    -> Derive.EventDeriver
invert subs start end next_start text = do
    -- Log.warn $ "invert " ++ show (start, end, next_start)
    let sliced = Slice.slice False start next_start (Just (text, end-start))
            subs
    when_just (non_bottom_note_track sliced) $ \track ->
        Derive.throw $ "inverting below note track: "
            ++ Pretty.pretty (State.tevents_track_id track)
    Schema.derive_tracks sliced

-- | An inverting call above another note track will lead to an infinite loop
-- if there are overlapping sub-events that also invert, or confusing results
-- if there are non-overlapping or non-inverting sub-events.  Either way, I
-- don't think I want it.
non_bottom_note_track :: State.EventsTree -> Maybe State.TrackEvents
non_bottom_note_track tree = Seq.head (concatMap go tree)
    where
    go (Tree.Node track subs)
        | TrackInfo.is_note_track (State.tevents_title track)
            && not (null subs) = [track]
        | otherwise = concatMap go subs

-- ** note slice

type Event = (ScoreTime, ScoreTime, Derive.EventDeriver)

-- | Get the Events of subtracks, if any.  This is the top-level utility for
-- note calls that take other note calls as arguments.
sub_events :: Derive.PassedArgs d -> [Event]
sub_events args
    | null subs = []
    | otherwise = [(shift, stretch, Schema.derive_tracks sliced)
        | (shift, stretch, sliced) <- Slice.slice_notes start end subs]
    where
    (start, end) = Derive.passed_range args
    subs = Derive.info_sub_tracks (Derive.passed_info args)

-- | Place and merge a list of Events.
--
-- Since event calls are not normalized 0--1, this start and duration are
-- added to and multiplied with the event's start and duration.  So you can't
-- just do @place . sub_events@ to derive sub events as-is.
--
-- TODO This is error-prone, would it be better to have 'sub_events' normalize
-- the derivers it returns?
place :: [Event] -> Derive.EventDeriver
place = Derive.d_merge . map (\(off, dur, d) -> Derive.d_place off dur d)

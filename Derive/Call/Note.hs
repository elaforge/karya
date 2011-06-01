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
import qualified Ui.State as State
import qualified Ui.Track as Track

import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score

import qualified Derive.Schema as Schema

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

-- * note call

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.NoteCall
c_note = Derive.Call "note"
    (Just $ Derive.GeneratorCall (inverting_call generate) (const Nothing))
    (Just $ Derive.TransformerCall transform Derive.NonIncremental)
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
    -> Track.PosEvent -> ScoreTime -> Derive.EventDeriver
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
    st <- Derive.get
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
inverting_call :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
inverting_call call args = case Derive.info_sub_tracks info of
        [] -> call args
        subs -> do
            invert subs pos (pos + Event.event_duration event)
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
    -- let i = Derive.passed_info args
    -- let st = Derive.info_stretch i
    -- Log.warn $ "args: " ++ show (Derive.info_warp i)

    let sliced = slice start next_start text (end-start) subs
    -- Log.warn $ "range: " ++ show (start, end, next_start)
    -- Log.warn $ "sliced: " ++ PPrint.pshow sliced
    Schema.derive_tracks sliced

-- | Slice the tracks below me to lie within start and end, and put
-- a note track with a single event of given string at the bottom.
--
-- Also strip the TrackIds out of the result.  TrackIds are used to record
-- the tempo map and signal for rendering.  Sliced segments are evaluated
-- piecemeal, overlap with each other if there is a previous sample, and
-- may be evaluated in a different warp than the track.

-- TODO the problem is that I'm slicing the event tracks, but also slicing out
-- the note track event, so it can no longer see prev and next events, and
-- can't see their pitches.  I can get the former by supplying prev and next,
-- but the latter has a problem if the pitches haven't been evaluated yet.
--
-- Ornaments like tick need to either predict the pitch of the next note,
-- pre-evaluate it, or be under the pitch track.  Prediction is going to lead
-- to inconsistent results, and pre-evaluation could lead to recursion and
-- confusingness.  So for the moment I just insist that the order of evaluation
-- be correct.  Hopefully inversion will only be a special case.
slice :: ScoreTime -> ScoreTime -> String
    -> ScoreTime -- ^ duration of inserted event, may be shorter than end-start
    -> State.EventsTree -> State.EventsTree
slice start end text dur = map go
    where
    go (Tree.Node track subs) = Tree.Node (slice_t track)
        (if null subs then [Tree.Node insert []] else map go subs)
    insert = State.TrackEvents ">" (Track.make_track_events [event]) end
        Nothing (Just (start, start + dur))
    event = (start, Event.event text dur)
    slice_t track = track
        { State.tevents_events =
            Track.track_events_around start end (State.tevents_events track)
        , State.tevents_end = end
        , State.tevents_range = Just (start, start + dur)
        -- , State.tevents_track_id = Nothing
        -- TODO if I strip it, then the stack misses track ids, but how do
        -- I omit warp and signal?
        }

-- TODO slice for sub note tracks
--
-- Don't slice events around for sub note tracks, only for controls.
--
-- Tracks that are empty as a result are omitted from the result.  The reason
-- is that otherwise empty note tracks will suppress all further evaluation
-- since they have no events

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

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
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
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
    , ("t", c_tuplet)
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


-- * tuplet

c_tuplet :: Derive.NoteCall
c_tuplet = Derive.stream_generator "tuplet" $ \args -> place (stretched args)
    where
    stretched args = map stretch $ Seq.sort_on (\(s, _, _) -> s) events
        where
        events = sub_events args
        (start, end) = Derive.passed_range args
        event_end = Seq.maximum (map (\(off, dur, _) -> off + dur) events)
        factor = (end - start) / maybe 1 id event_end
        stretch (off, _, d) = (off*factor + start, factor, d)

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
    -- let i = Derive.passed_info args
    -- let st = Derive.info_stretch i
    -- Log.warn $ "args: " ++ show (Derive.info_warp i)

    let sliced = slice start next_start (Just (text, end-start)) subs
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
slice :: ScoreTime -> ScoreTime
    -> Maybe (String, ScoreTime)
    -- ^ if given, text and duration of inserted event, which may be shorter
    -- than end-start
    -> State.EventsTree -> State.EventsTree
slice start end insert_event = map go
    where
    go (Tree.Node track subs) = Tree.Node (slice_t track)
        (if null subs then insert else map go subs)
    insert = case insert_event of
        Nothing -> []
        Just (text, dur) -> [Tree.Node (make text dur) []]
    make text dur =
        State.TrackEvents ">"
            (Track.make_track_events [(start, Event.event text dur)])
            end Nothing (Just (start, end))
    slice_t track = track
        { State.tevents_events = events track
        , State.tevents_end = end
        , State.tevents_range = Just (start, end)
        -- , State.tevents_track_id = Nothing
        -- TODO if I strip it, then the stack misses track ids, but how do
        -- I omit warp and signal?
        }
    -- Note tracks don't include pre and post events like control tracks.
    events track
        | TrackInfo.is_note_track (State.tevents_title track) =
            -- TODO ugly, Track needs a more logical design
            Track.from_sorted_events (Track.events_in_range start end es)
        | otherwise = Track.track_events_around start end es
        where es = State.tevents_events track

-- ** note slice

type Event = (ScoreTime, ScoreTime, Derive.EventDeriver)

-- | Get the Events of subtracks, if any.  This is the top-level utility for
-- note calls that take other note calls as arguments.
sub_events :: Derive.PassedArgs d -> [Event]
sub_events args
    | null subs = []
    | otherwise = [(shift, stretch, Schema.derive_tracks sliced)
        | (shift, stretch, sliced) <- slice_notes start end subs]
    where
    (start, end) = Derive.passed_range args
    subs = Derive.info_sub_tracks (Derive.passed_info args)

place :: [Event] -> Derive.EventDeriver
place = Derive.d_merge . map (\(off, dur, d) -> Derive.d_place off dur d)

-- | Expect a note track somewhere in the tree.  Slice the tracks above and
-- below it to each of its events.
--
-- The shift of each Event will be subtracted from the track events, so they
-- start at 0.  Note that there will be negative control events if they lie
-- before the note.
--
-- If there are no note tracks, return [].
--
-- Technically the children of the note track don't need to be sliced, since
-- if it is inverting it will do that anyway.  But slicing lets me shift fewer
-- events, so it's probably a good idea anyway.
slice_notes :: ScoreTime -> ScoreTime -> State.EventsTree
    -> [(ScoreTime, ScoreTime, State.EventsTree)]
    -- ^ @(shift, stretch, tree)@, in unsorted order
slice_notes start end =
    map shift . concatMap slice_track . concatMap note_tracks
    where
    note_tracks (Tree.Node track subs)
        | TrackInfo.is_note_track (State.tevents_title track) =
            [([], track, subs)]
        | otherwise = [(track : parents, ntrack, nsubs)
            | (parents, ntrack, nsubs) <- concatMap note_tracks subs]
    slice_track (parents, track, subs) =
        map (slice_event (make_tree parents)) events
        where
        events = Track.events_in_range start end (State.tevents_events track)
        make_tree [] = [Tree.Node track subs]
        make_tree (p:ps) = [Tree.Node p (make_tree ps)]
    slice_event tree event = (s, e - s, slice s e Nothing tree)
        where (s, e) = (Track.event_start event, Track.event_end event)
    shift (shift, stretch, tree) =
        (shift, stretch, map (fmap (shift_tree shift)) tree)
    shift_tree shift track = track
        { State.tevents_events = Track.map_sorted_events
            (\(p, e) -> (p - shift, e)) (State.tevents_events track)
        , State.tevents_end = State.tevents_end track - shift
        , State.tevents_range = fmap (\(s, e) -> (s-shift, e-shift))
            (State.tevents_range track)
        }


-- | Basic calls for note tracks.
module Derive.Call.Note where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Derive.Args as Args
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Slice as Slice
import qualified Derive.TrackInfo as TrackInfo
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls
    [ ("", c_note)
    -- Since you can never call "" with arguments, I need a non-null form
    -- to handle the args version.
    , ("n", c_note)
    , ("=", Util.c_equal)
    , ("note-track", c_note_track)
    ]

-- * note

-- | The note call is the default deriver for a track.  As a convenience, it
-- will interpret @>inst@ and @+attr@ args as the relevant assignments,
-- which means you can assign these to a note generator or a call with an
-- abbreviated syntax: @+attr@ to generate a note with that attr, or
-- @>i | call@ to run call with that instrument.
c_note :: Derive.NoteCall
c_note = Derive.Call "note"
    (Just $ Derive.GeneratorCall (inverting generate) (const Nothing))
    (Just note_transform)
    where
    generate args = case process (Derive.passed_vals args) of
        (inst, rel_attrs, []) ->
            generate_note inst rel_attrs (Args.event args) (Args.end args)
        (_, _, invalid) -> Derive.throw_arg_error $
            "expected inst or attr: " ++ show invalid
    process = process_note_args Nothing []

-- | This is implicitly the call for note track titles---the \">...\" will be
-- the first argument.
c_note_track :: Derive.NoteCall
c_note_track = Derive.Call "note-track" Nothing (Just note_transform)

note_transform :: Derive.TransformerCall Score.Event
note_transform = Derive.TransformerCall $ \args deriver ->
    case process_note_args Nothing [] (Derive.passed_vals args) of
        (inst, rel_attrs, []) -> transform_note inst rel_attrs deriver
        (_, _, invalid) ->
            Derive.throw_arg_error $ "expected inst or attr: " ++ show invalid

-- ** generate

generate_note :: Maybe Score.Instrument -> [TrackLang.RelativeAttr]
    -> Events.PosEvent -> ScoreTime -> Derive.EventDeriver
generate_note n_inst rel_attrs (pos, event) next_start = do
    start <- Derive.real pos
    end <- Derive.real (pos + Event.event_duration event)
    real_next <- Derive.real next_start
    -- Note that due to negative durations, the end could be before the start.
    -- What this really means is that the sounding duration of the note depends
    -- on the next one, which should be sorted out later by post processing.
    inst <- case n_inst of
        Just inst -> return (Just inst)
        Nothing -> Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    st <- Derive.gets Derive.state_dynamic
    let controls = trimmed_controls start real_next (Derive.state_controls st)
        pitch_sig = trimmed_pitch start real_next (Derive.state_pitch st)
    let sustain = maybe 1
            (RealTime.seconds . Signal.at start . Score.typed_val)
            (Map.lookup Score.c_sustain controls)
    (start, end) <- randomized controls start ((end - start) * sustain + start)
    return $! LEvent.one $! LEvent.Event $!
        Score.Event start (end - start) (Event.event_bs event)
            controls pitch_sig (Derive.state_stack st) inst
            (apply rel_attrs attrs)
    where
    apply rel_attrs attrs =
        List.foldl' (.) id (map TrackLang.set_attr rel_attrs) attrs

-- | Interpret the c_start_rnd and c_dur_rnd controls.
--
-- This only ever makes notes shorter.  Otherwise, it's very easy for
-- previously non-overlapping notes to become overlapping and MIDI doesn't
-- like that.
randomized :: Score.ControlMap -> RealTime -> RealTime
    -> Derive.Deriver (RealTime, RealTime)
randomized controls start end = do
    let start_r = Score.typed_val $
            Score.control controls Score.c_start_rnd start
        dur_r = Score.typed_val $ Score.control controls Score.c_dur_rnd start
    if start_r == 0 && dur_r == 0 then return (start, end) else do
    r1 : r2 : _ <- Util.randoms
    return (start + RealTime.seconds (Num.restrict 0 start_r r1),
        end + RealTime.seconds (Num.restrict (-dur_r) 0 r2))

-- | In a note track, the pitch signal for each note is constant as soon as
-- the next note begins.  Otherwise, it looks like each note changes pitch
-- during its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.Signal
    -> PitchSignal.Signal
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
trimmed_controls start end =
    Map.map (fmap (Signal.truncate end . Signal.drop_before start))

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
-- parseable code.  This is because 'inverting' wants the
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
-- If there are no sub-tracks then the call is performed as-is.  Otherwise
-- the expression must be broken up and re-evaluated.
inverting :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
inverting call args =
    maybe (call args) BlockUtil.derive_tracks =<< invert_call args

invert_call :: Derive.PassedArgs d -> Derive.Deriver (Maybe State.EventsTree)
invert_call args = case Derive.info_sub_tracks info of
    [] -> return Nothing
    subs -> Just <$> invert (Derive.info_track_range info) subs
        pos (pos + Event.event_duration event) (Args.end args) expr
    where
    (pos, event) = Derive.info_event info
    -- It may seem surprising that only the final call is retained, and any
    -- transformers are discarded.  But 'inverting' only applies to generators
    -- so those transformers should have already done their thing.
    -- See comment above and in Derive.TrackLang.Val Pretty instance.
    expr = maybe "" Pretty.pretty $ Seq.last (Derive.info_expr info)
    info = Derive.passed_info args

invert :: (ScoreTime, ScoreTime) -> State.EventsTree -> ScoreTime
    -> ScoreTime -> ScoreTime -> String -> Derive.Deriver State.EventsTree
invert (track_start, _) subs start end next_start text = do
    when_just (non_bottom_note_track sliced) $ \track ->
        Derive.throw $
            "inverting below note track will lead to an endless loop: "
            ++ Pretty.pretty (State.tevents_track_id track)
    return sliced
    where
    sliced = Slice.slice False start next_start
        (Just (text, end - start, (track_start, next_start))) subs
        -- Use 'next_start' instead of track_end because in the absence of
        -- a next note, the track end becomes next note and clips controls.

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

-- | Start, duration, deriver.
data Event = Event {
    event_start :: !ScoreTime
    , event_duration :: !ScoreTime
    , event_deriver :: !Derive.EventDeriver
    }

event_end :: Event -> ScoreTime
event_end event = event_start event + event_duration event

instance Show Event where
    show (Event start dur _) =
        "Event " ++ show start ++ " " ++ show dur ++ " ((deriver))"

-- | Get the Events of subtracks, if any, returning one list of events per sub
-- note track.  This is the top-level utility for note calls that take other
-- note calls as arguments.
sub_events :: Derive.PassedArgs d -> [[Event]]
sub_events args = map (map mkevent) (Slice.slice_notes start end subs)
    where
    (start, end) = Args.range args
    subs = Derive.info_sub_tracks (Derive.passed_info args)
    -- The events have been shifted back to 0 by 'Slice.slice_notes', but
    -- are still their original lengths.  Stretch them back to 1 so Events
    -- are normalized.
    mkevent (shift, stretch, tree) = Event shift stretch $
        Derive.d_stretch (recip stretch) (BlockUtil.derive_tracks tree)

-- | Place and merge a list of Events.
place :: [Event] -> Derive.EventDeriver
place = Derive.d_merge
    . map (\(Event start dur d) -> Derive.d_place start dur d)

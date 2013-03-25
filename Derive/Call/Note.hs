{-# LANGUAGE CPP #-}
-- | Basic calls for note tracks.
module Derive.Call.Note (
    note_calls
    , c_note, transformed_note, note_call
    , Config(..), use_attributes, no_duration_attributes
    , GenerateNote, default_note
    -- * inversion
    , when_under_inversion
    , inverting, inverting_args, inverting_around
    -- ** events
    , Event(..), event_end, event_overlaps, map_event, map_events
    , stretch
    , sub_events
    , place, place_at
#ifdef TESTING
    , invert_call, trimmed_controls
#endif
) where
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.TrackTree as TrackTree
import qualified Derive.Args as Args
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Tags as Tags
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Slice as Slice
import qualified Derive.Stack as Stack
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
    , ("=", c_equal)
    , ("note-track", c_note_track)
    ]

-- * note

c_note :: Derive.NoteCall
c_note = note_call "" "" (default_note use_attributes)

transformed_note :: String -> (Derive.EventDeriver -> Derive.EventDeriver)
    -> Derive.NoteCall
transformed_note prepend_doc transform =
    note_call "" prepend_doc (transform . default_note use_attributes)

-- | Create a note call, configuring it with the actual note generating
-- function.  The generator is called with the usual note arguments, and
-- receives the usual instrument and attribute transform.
note_call :: String
    -- ^ Append to the name, if non-null.  The documentation for all calls that
    -- differ only in name can be grouped together, so it's easier to read
    -- if small modifications are reflected in the name only.  But since
    -- the name is then no longer a valid identifier, it can't be used to set
    -- default arguments.  That's not really a big deal for the note call,
    -- though.
    -> String -> GenerateNote -> Derive.NoteCall
note_call append_name prepend_doc generate = Derive.Call
    { Derive.call_name = "note"
        ++ (if null append_name then "" else ' ' : append_name)
    , Derive.call_generator = Just $ Derive.generator_call mempty prepended
        (Sig.call parser (note_generate generate))
    , Derive.call_transformer = Just $ Derive.transformer_call Tags.subs
        transformer_doc (Sig.callt parser note_transform)
    }
    where
    parser = Sig.many "attribute" "Change the instrument or attributes."
    prepended
        | null prepend_doc = generator_doc
        | otherwise = "Modified note call: " ++ prepend_doc ++ "\n"
            ++ generator_doc
    generator_doc =
        "The note call is the main note generator, and will emit a single"
        <> " score event. It interprets `>inst` and `+attr` args by"
        <> " setting those fields of the event.  This is bound to the"
        <> " null call, \"\", but any potential arguments would wind up"
        <> " looking like a different call, so it's bound to `n` as well."
    transformer_doc =
        "This takes the same arguments as the generator and instead sets"
        <> " those values in the transformed score, similar to the `=`"
        <> " call."
    note_generate generate_note vals = inverting generate
        where generate args = transform_note vals $ generate_note args

-- | This is implicitly the call for note track titles---the \">...\" will be
-- the first argument.
c_note_track :: Derive.NoteCall
c_note_track =
    Derive.transformer "note-track" Tags.internal
        ("This is used internally as the implicit"
        <> " call for note track titles. Similar to the note transformer, it"
        <> " takes `>inst` and `+attr` args and sets them in the environment.")
    (Sig.callt parser note_transform)
    where parser = Sig.many "attribute" "Change the instrument or attributes."

note_transform :: [Either Score.Instrument TrackLang.RelativeAttrs]
    -> Derive.PassedArgs d -> Derive.EventDeriver -> Derive.EventDeriver
note_transform vals _ deriver = transform_note vals deriver

-- ** generate

-- | Generate a single note.  This is intended to be used as the lowest level
-- null call for some instrument.
type GenerateNote = Derive.PassedArgs Score.Event -> Derive.EventDeriver

data Config = Config {
    config_legato :: Bool
    , config_staccato :: Bool
    } deriving (Show)

use_attributes :: Config
use_attributes = Config True True

-- | Don't observe any of the duration affecting attributes.
no_duration_attributes :: Config
no_duration_attributes = Config False False

default_note :: Config -> GenerateNote
default_note config args = do
    start <- Args.real_start args
    end <- Args.real_end args
    real_next <- Derive.real (Args.next args)
    -- Note that due to negative durations, the end could be before the start.
    -- What this really means is that the sounding duration of the note depends
    -- on the next one, which should be sorted out later by post processing.
    inst <- fromMaybe Score.empty_inst <$>
        Derive.lookup_val TrackLang.v_instrument
    environ <- Internal.get_dynamic Derive.state_environ
    let attrs = either (const Score.no_attrs) id $
            TrackLang.get_val TrackLang.v_attributes environ
    st <- Derive.gets Derive.state_dynamic
    let controls = trimmed_controls start real_next (Derive.state_controls st)
        pitch_sig = trimmed_pitch start real_next (Derive.state_pitch st)
    (start, end) <- randomized controls start $
        duration_attributes config controls attrs start end real_next
    return $! LEvent.one $! LEvent.Event $! Score.Event
        { Score.event_start = start
        , Score.event_duration = end - start
        , Score.event_bs = Event.event_bytestring (Args.event args)
        , Score.event_controls = controls
        , Score.event_pitch = pitch_sig
        , Score.event_stack = Derive.state_stack st
        , Score.event_instrument = inst
        , Score.event_environ = environ
        }

-- | Interpret attributes and controls that effect the note's duration.
--
-- This is actually somewhat complicated.  Instead of all the
-- duration-affecting controls all applying together, notes fit into distinct
-- categories:
--
-- - Zero-duration notes ignore all this.
--
-- - Legato notes last until the next note plus 'Score.c_legato_overlap'.
--
-- - Staccato notes divide their duration by 2.
--
-- - Normal notes multiply 'Score.c_sustain' and add 'Score.c_duration_abs',
-- which could be negative.  They clip at a minimum duration to keep from going
-- negative.
duration_attributes :: Config -> Score.ControlMap -> Score.Attributes
    -> RealTime -> RealTime -> RealTime -> RealTime -- ^ new end time
duration_attributes config controls attrs start end next
    | start >= end = end -- don't mess with 0 dur or negative notes
    | config_legato config && has Attrs.legato =
        next + lookup_time 0.1 Score.c_legato_overlap
    | otherwise = start + max min_duration (dur * sustain + sustain_abs)
    where
    has = Score.attrs_contain attrs
    dur = end - start
    staccato = config_staccato config && has Attrs.staccato
    sustain = if staccato then sustain_ * 0.5 else sustain_
    sustain_abs = if staccato then 0 else lookup_time 0 Score.c_sustain_abs
    sustain_ = lookup_time 1 Score.c_sustain
    lookup_time deflt control = maybe deflt
        (RealTime.seconds . Signal.at start . Score.typed_val)
        (Map.lookup control controls)
    -- This keeps a negative sustain_abs from making note duration negative.
    min_duration = 0.01

-- | Interpret the c_start_rnd and c_dur_rnd controls.
--
-- This only ever makes notes shorter.  Otherwise, it's very easy for
-- previously non-overlapping notes to become overlapping and MIDI doesn't
-- like that.
randomized :: Score.ControlMap -> RealTime -> RealTime
    -> Derive.Deriver (RealTime, RealTime)
randomized controls start end
    | start_r == 0 && dur_r == 0 = return (start, end)
    | start == end = do
        r1 : _ <- Util.randoms
        let offset = RealTime.seconds (Num.restrict (-start_r/2) (start_r/2) r1)
        return (start + offset, end + offset)
    | otherwise = do
        r1 : r2 : _ <- Util.randoms
        let start2 = start + RealTime.seconds (Num.restrict 0 start_r r1)
        return (start2, max start2 $
            end + RealTime.seconds (Num.restrict (-dur_r) 0 r2))
    where
    start_r = Score.typed_val $
        Score.control controls Score.c_start_rnd start
    dur_r = Score.typed_val $
        Score.control controls Score.c_dur_rnd start

-- | In a note track, the pitch signal for each note is constant as soon as
-- the next note begins.  Otherwise, it looks like each note changes pitch
-- during its decay.
trimmed_pitch :: RealTime -> RealTime -> PitchSignal.Signal
    -> PitchSignal.Signal
trimmed_pitch start end
    | start == end = PitchSignal.take 1 . PitchSignal.drop_before start
    | otherwise = PitchSignal.drop_after end . PitchSignal.drop_before start

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
trimmed_controls start end = Map.map (fmap trim)
    where
    trim = if start == end
        -- Otherwise 0 dur events tend to get no controls.
        -- I'm not sure exactly when this happens because 'generate_note' trims
        -- until the start of the next note, but I think it did because
        -- I added this fix for it...
        then Signal.take 1 . Signal.drop_before start
        else Signal.drop_after end . Signal.drop_before start

-- ** transform

transform_note :: [Either Score.Instrument TrackLang.RelativeAttrs]
    -> Derive.EventDeriver -> Derive.EventDeriver
transform_note vals deriver = with_inst (with_attrs deriver)
    where
    (insts, rel_attrs) = Seq.partition_either vals
    with_inst = maybe id Derive.with_instrument $
        Seq.last $ filter (/=Score.empty_inst) insts
    with_attrs
        | null rel_attrs = id
        | otherwise = Util.with_attrs (TrackLang.apply_attrs rel_attrs)


-- * c_equal

c_equal :: Derive.NoteCall
c_equal = Derive.Call
    { Derive.call_name = "equal"
    , Derive.call_generator = Just $ Derive.generator_call Tags.internal
        ("Similar to the transformer, this will evaluate the notes below in"
            <> " a transformed environ.")
        (Sig.parsed_manually Util.equal_arg_doc generate)
    , Derive.call_transformer = Just $ Derive.transformer_call
        (Tags.internal <> Tags.subs) Util.equal_doc
        (Sig.parsed_manually Util.equal_arg_doc Util.equal_transformer)
    }
    where
    generate args = place . map (map_event (Util.equal_transformer args))
        . concat =<< sub_events args

-- * inversion

-- | Apply the function (likely a transformer) only when at the bottom of
-- the inversion.  This is useful for transforming an inverted call because
-- otherwise the transformation happens twice: once before inversion and once
-- after.  Track calls avoid this because they only invert the last part of
-- the pipeline, but transformers applied directly in haskell can't do that.
when_under_inversion :: Derive.PassedArgs d -> (a -> a) -> a -> a
when_under_inversion args transform deriver
    | under_inversion args = transform deriver
    | otherwise = deriver

-- | True if the call will not invert.  This means it has no children, which
-- possibly means it already inverted and now we're at the \"real\" call.
under_inversion :: Derive.PassedArgs d -> Bool
under_inversion = null . Derive.info_sub_tracks . Derive.passed_info

-- | Convert a call into an inverting call.  Documented in doc/inverting_calls.
--
-- This requires a bit of hackery:
--
-- The first is that the ShowVal TrackLang.Val instance is expected to emit
-- parseable code.  This is because 'inverting' wants the
-- text of the generator it was called for.  Unfortunately this is tricky to
-- get directly because the parser takes a complete string to a complete Expr.
-- So instead I keep the parsed expr by putting it in CallInfo's
-- 'Derive.info_expr', and use the ShowVal instance to turn it back into
-- a string, so it can be parsed again when it is evaluated for real.  It's
-- rather convoluted, but trying to come up with a derive_tracks where the
-- events may already be parsed also seems convoluted.
--
-- If there are no sub-tracks then the call is performed as-is.  Otherwise
-- the expression must be broken up and re-evaluated.
inverting :: (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
inverting = inverting_around (1, 1)

inverting_args :: Derive.PassedArgs d -> Derive.EventDeriver
    -> Derive.EventDeriver
inverting_args args f = inverting (const f) args

inverting_around :: (Int, Int) -- ^ Capture this many control points at+before
    -- and after the slice boundary.  Also documented in 'Slice.slice'.
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
    -> (Derive.PassedArgs d -> Derive.EventDeriver)
inverting_around around call args =
    -- If I can invert, the call isn't actually called.  Instead I make a track
    -- with event text that will result in this being called again, and at
    -- that point it actually will be called.
    maybe (call args) BlockUtil.derive_tracks =<< invert_call around args

invert_call :: (Int, Int)
    -> Derive.PassedArgs d -> Derive.Deriver (Maybe TrackTree.EventsTree)
invert_call around args = case Derive.info_sub_tracks info of
    [] -> return Nothing
    subs -> Just <$> invert around (Derive.info_track_range info) subs
        (Event.start event) (Event.end event) (Args.next args) expr
        (Derive.info_prev_events info, Derive.info_next_events info)
    where
    event = Derive.info_event info
    -- It may seem surprising that only the final call is retained, and any
    -- transformers are discarded.  But 'inverting' only applies to generators
    -- so those transformers should have already done their thing.
    -- See comment above and on ShowVal typeclass.
    expr = TrackLang.show_val $ NonEmpty.last (Derive.info_expr info)
    info = Derive.passed_info args

invert :: (Int, Int) -> (ScoreTime, ScoreTime) -> TrackTree.EventsTree
    -> ScoreTime -> ScoreTime -> ScoreTime -> String
    -> ([Event.Event], [Event.Event])
    -> Derive.Deriver TrackTree.EventsTree
invert around (track_start, _) subs start end next_start text events_around = do
    -- Pick the current track out of the stack, and give that to the inverted
    -- track.
    -- TODO I'm not 100% comfortable with this, I don't like putting implicit
    -- dependencies on the stack like this.  Too many of these and someday
    -- I change how the stack works and all sorts of things break.  It would be
    -- more explicit to put TrackId into CallInfo.
    track_id <- stack_track_id
    let sliced = slice track_id
    when_just (non_bottom_note_track sliced) $ \track ->
        Derive.throw $
            "inverting below a note track will lead to an endless loop: "
            ++ Pretty.pretty (TrackTree.tevents_track_id track)
    return sliced
    where
    slice track_id = concatMap Slice.strip_empty_tracks $
        Slice.slice False around start next_start (Just (insert track_id)) subs
    -- Use 'next_start' instead of track_end because in the absence of a next
    -- note, the track end becomes next note and clips controls.
    insert track_id = Slice.InsertEvent
        { Slice.ins_text = text
        , Slice.ins_duration = end - start
        , Slice.ins_range = (track_start, next_start)
        , Slice.ins_around = events_around
        , Slice.ins_track_id = track_id
        }

stack_track_id :: Derive.Deriver (Maybe TrackId)
stack_track_id = Seq.head . mapMaybe Stack.track_of . Stack.innermost
    <$> Internal.get_stack

-- | An inverting call above another note track will lead to an infinite loop
-- if there are overlapping sub-events that also invert, or confusing results
-- if there are non-overlapping or non-inverting sub-events.  Either way, I
-- don't think I want it.
non_bottom_note_track :: TrackTree.EventsTree -> Maybe TrackTree.TrackEvents
non_bottom_note_track tree = Seq.head (concatMap go tree)
    where
    go (Tree.Node track subs)
        | TrackInfo.is_note_track (TrackTree.tevents_title track)
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

event_overlaps :: ScoreTime -> Event -> Bool
event_overlaps pos (Event start dur _)
    | dur == 0 = pos == start
    | otherwise = start <= pos && pos < start+dur

map_event :: (Derive.EventDeriver -> Derive.EventDeriver) -> Event -> Event
map_event f event = event { event_deriver = f (event_deriver event) }

map_events :: (Derive.EventDeriver -> Derive.EventDeriver) -> [Event] -> [Event]
map_events f = map (map_event f)

stretch :: ScoreTime -> ScoreTime -> Event -> Event
stretch offset factor (Event start dur deriver) =
    Event ((start - offset) * factor + offset) (dur * factor) deriver

instance Show Event where
    show (Event start dur _) = "Event " ++ show start ++ " " ++ show dur
instance Pretty.Pretty Event where pretty = show


-- | Get the Events of subtracks, if any, returning one list of events per sub
-- note track.  This is the top-level utility for note calls that take other
-- note calls as arguments.
sub_events :: Derive.PassedArgs d -> Derive.Deriver [[Event]]
sub_events args =
    map (map mkevent) <$> Slice.checked_slice_notes start end subs
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

-- | Fit the given events into a time range.  Any leading space (time between
-- the start of the range and the first Event) and trailing space is
-- eliminated.
place_at :: (ScoreTime, ScoreTime) -> [Event] -> Derive.EventDeriver
place_at (start, end) notes = Derive.d_place start factor $
    place [note { event_start = event_start note - note_start } | note <- notes]
    where
    factor = (end - start) / (note_end - note_start)
    note_end = fromMaybe 1 $ Seq.maximum (map event_end notes)
    note_start = fromMaybe 1 $ Seq.minimum (map event_start notes)

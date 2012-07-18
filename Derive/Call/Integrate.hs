module Derive.Call.Integrate (
    note_calls, Track(..), integrate
    , unwarp
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State

import qualified Derive.Call.BlockUtil as BlockUtil
import qualified Derive.Call.Util as Util
import qualified Derive.CallSig as CallSig
import qualified Derive.Derive as Derive
import Derive.Derive (Track(..))
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.ParseBs as ParseBs
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackInfo as TrackInfo

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified App.Config as Config
import Types


-- * call

note_calls :: Derive.NoteCallMap
note_calls = Derive.make_calls [("<", c_integrate)]

-- | Integrate.
c_integrate :: Derive.NoteCall
c_integrate = Derive.transformer "integrate" $ \args deriver ->
    CallSig.call0 args $ do
        events <- deriver
        integrate_events (LEvent.events_of events)
        return events

integrate_events :: [Score.Event] -> Derive.Deriver ()
integrate_events events = do
    stack <- Internal.get_stack
    case Maybe.mapMaybe Stack.block_of (Stack.innermost stack) of
        -- Only collect an integration if this is the top level block.
        -- Otherwise I can get integrating blocks called from many places and
        -- who knows which one is supposed to be integrated.
        [block_id] -> do
            events <- Derive.eval_ui "c_integrate" $ unwarp block_id events
            key <- Util.lookup_key
            lookup_scale <- Derive.gets
                (Derive.state_lookup_scale . Derive.state_constant)
            lookup_inst <- Derive.gets
                (Derive.state_lookup_instrument . Derive.state_constant)
            let lookup_attrs = maybe mempty Derive.inst_attribute_map
                    . lookup_inst
            let (tracks, errs) = integrate lookup_scale lookup_attrs key events
            if null errs
                then Internal.merge_collect $ mempty
                    { Derive.collect_integrated = Just tracks }
                else Log.warn $ "errors integrating: " ++ Seq.join "; " errs
        _ -> return ()

-- * create block

-- | If the block uses a default tempo, it will get applied once during
-- integration, and again when it's played.  I should avoid applying the
-- default tempo at all for integration, but that's too much bother.  Instead,
-- unwarp the events if the default tempo was applied.
--
-- TODO Getting rid of the default tempo entirely is also an option.
unwarp :: (State.M m) => BlockId -> [Score.Event] -> m [Score.Event]
unwarp block_id events = ifM (uses_default_tempo block_id)
    (do tempo <- State.get_default State.default_tempo
        return $ move (RealTime.seconds tempo) events)
    (return events)
    where move tempo = map $ Score.move (*tempo) . Score.duration (*tempo)

uses_default_tempo :: (State.M m) => BlockId -> m Bool
uses_default_tempo block_id =
    BlockUtil.has_nontempo_track <$> State.events_tree_of block_id


-- * integrate

type LookupAttrs = Score.Instrument -> Derive.AttributeMap

-- | Convert derived score events back into UI events.
--
-- Split into tracks by track id, instrument, and scale.
--
-- TODO and overlapping events should be split, deal with that later
-- TODO optionally quantize the ui events
integrate :: Derive.LookupScale -> LookupAttrs -> Maybe Pitch.Key
    -> [Score.Event] -> ([Track], [String]) -- ^ (tracks, errs)
integrate lookup_scale lookup_attrs key = convert . Seq.partition_either
    . map (integrate_track lookup_scale lookup_attrs key)
    . Seq.keyed_group_on group_key
    where
    convert (errs, tracks) = (concat tracks, errs)
    group_key event = (track_of event, Score.event_instrument event,
        PitchSignal.sig_scale_id (Score.event_pitch event))

track_of :: Score.Event -> Maybe TrackId
track_of = Seq.head . Maybe.mapMaybe Stack.track_of . Stack.innermost
    . Score.event_stack

integrate_track :: Derive.LookupScale -> LookupAttrs -> Maybe Pitch.Key
    -> ((Maybe TrackId, Score.Instrument, Pitch.ScaleId), [Score.Event])
    -> Either String [Track]
integrate_track lookup_scale lookup_attrs key ((_, inst, scale_id), events) = do
    pitch_track <- if no_pitch_signals events then return [] else do
        scale <- maybe (Left $ "scale not found: " ++ Pretty.pretty scale_id)
            return (lookup_scale scale_id)
        case pitch_events scale scale_id key event_stacks of
            (track, []) -> return [track]
            (_, errs) -> Left $ Seq.join "; " errs
    return $ note_events inst (lookup_attrs inst) event_stacks
        : pitch_track ++ control_events event_stacks
    where
    event_stacks = zip events (stack_serials (map Score.event_stack events))

-- | Make deriver stacks unique by appending a serial number.
stack_serials :: [Stack.Stack] -> [Stack]
stack_serials = snd . List.mapAccumL go Map.empty
    where
    go seen stack = (seen2, (stack, Maybe.fromMaybe 0 count))
        where
        (count, seen2) = Map.insertLookupWithKey' (const (+)) stack 1 seen

type Stack = (Stack.Stack, Int)

-- ** note

note_events :: Score.Instrument -> Derive.AttributeMap
    -> [(Score.Event, Stack)] -> Track
note_events inst attr_map event_stacks =
    make_track note_title (map (note_event attr_map) event_stacks)
    where note_title = TrackInfo.instrument_to_title inst

note_event :: Derive.AttributeMap -> (Score.Event, Stack) -> Events.PosEvent
note_event attr_map (event, stack) =
    (RealTime.to_score (Score.event_start event), ui_event)
    where
    ui_event = Event.Event
        { Event.event_bs = Event.to_text $
            Map.findWithDefault "" (Score.event_attributes event) attr_map
        , Event.event_duration = RealTime.to_score (Score.event_duration event)
        , Event.event_style = Config.default_style
        , Event.event_stack = Just (make_stack ">" stack)
        }

-- ** pitch

pitch_events :: Scale.Scale -> Pitch.ScaleId -> Maybe Pitch.Key
    -> [(Score.Event, Stack)] -> (Track, [String])
pitch_events scale scale_id key event_stacks =
    (make_track pitch_title (tidy_events ui_events), concat errs)
    where
    pitch_title = TrackInfo.scale_to_title scale_id
    (ui_events, errs) = unzip $
        map (pitch_signal_events scale key) event_stacks

no_pitch_signals :: [Score.Event] -> Bool
no_pitch_signals = all (PitchSignal.null . Score.event_pitch)

pitch_signal_events :: Scale.Scale -> Maybe Pitch.Key
    -> (Score.Event, Stack) -> ([Events.PosEvent], [String])
pitch_signal_events scale key (event, stack) =
    (ui_events, map Pretty.pretty pitch_errs ++ note_errs)
    where
    sig = Score.event_pitch event
    ui_events = [ui_event (make_stack "*" stack) (RealTime.to_score x)
            (Pitch.note_text note) 0
        | (x, _, Just note) <- notes]
    notes = [(x, nn, Scale.nn_to_note scale key nn)
        | (x, nn) <- map (second Pitch.NoteNumber) (Signal.unsignal nns)]
    note_errs = [Pretty.pretty x ++ ": nn out of range: " ++ Pretty.pretty nn
        | (x, nn, Nothing) <- notes]
    (nns, pitch_errs) = PitchSignal.to_nn sig

-- ** control

control_events :: [(Score.Event, Stack)] -> [Track]
control_events event_stacks =
    filter (not . empty_track) $ map (control_track event_stacks) controls
    where
    controls = List.sort $ Seq.unique $ concatMap
        (map typed_control . Map.toList . Score.event_controls . fst)
        event_stacks
    typed_control (control, sig) = Score.Typed (Score.type_of sig) control

control_track :: [(Score.Event, Stack)] -> Score.Typed Score.Control -> Track
control_track event_stacks control =
    make_track (TrackInfo.unparse_typed control) ui_events
    where
    ui_events = drop_dyn $ tidy_events $
        map (signal_events (Score.typed_val control)) event_stacks
    -- Don't emit a dyn track if it's just the default.
    -- TODO generalize this to everything in in Derive.initial_controls
    drop_dyn [(pos, event)]
        | Score.typed_val control == Score.c_dynamic && pos == 0
            && Event.event_string event == default_dyn = []
    drop_dyn events = events
    default_dyn = ParseBs.show_hex_val Derive.default_dynamic

signal_events :: Score.Control -> (Score.Event, Stack) -> [Events.PosEvent]
signal_events control (event, stack) = case Map.lookup control controls of
    Nothing -> []
    Just sig ->
        [ui_event (make_stack (Score.control_name control) stack)
            (RealTime.to_score x) (ParseBs.show_hex_val y) 0
            | (x, y) <- Signal.unsignal (Score.typed_val sig)]
    where controls = Score.event_controls event

-- * util

make_stack :: String -> Stack -> Event.Stack
make_stack tag (stack, serial) = Event.Stack stack tag serial

ui_event :: Event.Stack -> ScoreTime -> String -> ScoreTime -> Events.PosEvent
ui_event stack pos text dur = (pos, (Event.event text dur)
    { Event.event_stack = Just stack })

tidy_events :: [[Events.PosEvent]] -> [Events.PosEvent]
tidy_events = clip_to_zero . drop_dups . clip_concat

-- | Concatenate the events, dropping ones that are out of order.  The
-- durations are not modified, so they still might overlap in duration, but the
-- start times will be increasing.
clip_concat :: [[Events.PosEvent]] -> [Events.PosEvent]
clip_concat = Seq.drop_with out_of_order . concat
    where out_of_order e1 e2 = fst e2 <= fst e1

-- | Drop subsequent events with the same text, since those are redundant for
-- controls.
drop_dups :: [Events.PosEvent] -> [Events.PosEvent]
drop_dups = Seq.drop_dups (Event.event_string . snd)

-- | Drop events before 0, keeping at least one at 0.  Controls can wind up
-- with samples before 0 (e.g. after using 'Derive.Score.move'), but events
-- can't start before 0.
clip_to_zero :: [Events.PosEvent] -> [Events.PosEvent]
clip_to_zero ((p1, e1) : rest@((p2, _) : _))
    | p1 <= 0 && p2 <= 0 = clip_to_zero rest
    | otherwise = (max 0 p1, e1) : rest
clip_to_zero [(p, e)] = [(max 0 p, e)]
clip_to_zero [] = []

make_track :: String -> [Events.PosEvent] -> Track
make_track title events = Track title (Seq.sort_on fst events)

empty_track :: Track -> Bool
empty_track (Track _ []) = True
empty_track _ = False

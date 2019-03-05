-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{- | This has the basic data structures for the deriver level.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score (
    module Derive.BaseTypes
    -- * Event
    , Event(..)
    , short_event, short_events
    , empty_event, event_end, event_min, event_max
    , events_overlap
    , event_scale_id
    , copy, normalize
    -- ** flags
    , has_flags, add_flags, remove_flags
    -- ** logs
    , add_log, add_log_msg
    -- ** environ
    , modify_environ, modify_environ_key
    -- ** attributes
    , event_attributes, has_attribute, intersecting_attributes
    , modify_attributes, add_attributes, remove_attributes
    -- ** delayed args
    , put_arg, take_arg

    -- ** modify events
    , move, place, move_start, duration, set_duration, set_instrument
    -- *** control
    , Control, control_name
    , ControlValMap, TypedControlValMap
    , control_at, event_control, initial_dynamic, modify_dynamic, set_dynamic
    , modify_control
    , set_control, event_controls_at
    -- *** pitch
    , PControl, pcontrol_name
    , default_pitch, set_pitch, set_named_pitch, event_named_pitch
    , transposed_at, pitch_at, apply_controls
    , initial_pitch, nn_at, initial_nn, note_at, initial_note
    , nn_signal

    -- * Type
    , Type(..), Typed(..)
    , untyped, merge_typed, type_to_code, code_to_type

    -- * instrument
    , Instrument(..)
    , instrument_name, empty_instrument

    -- * util
    , control, unchecked_control
    , pcontrol, unchecked_pcontrol
    , c_dynamic
    , parse_generic_control
) where
import qualified Control.DeepSeq as DeepSeq
import           Control.DeepSeq (rnf)
import qualified Data.Dynamic as Dynamic
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import           Derive.BaseTypes
       (ControlFunctionMap, ControlMap, PitchMap, ControlFunction(..))
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import           Derive.ScoreT
       (code_to_type, control_name, empty_instrument, instrument_name,
        merge_typed, pcontrol_name, type_to_code, untyped, Control,
        ControlValMap, PControl, TypedControlValMap, Instrument(..), Type(..),
        Typed(..))
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Ui.Color as Color
import qualified Ui.Id as Id

import           Global
import           Types


-- * Event

data Event = Event {
    event_start :: !RealTime
    , event_duration :: !RealTime
    -- | This is the text of the call that created the event.  It's basically
    -- just for debugging.
    , event_text :: !Text
    , event_controls :: !ControlMap
    , event_pitch :: !PSignal.PSignal
    -- | Named pitch signals.
    , event_pitches :: !PitchMap
    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: !Stack.Stack
    , event_highlight :: !Color.Highlight
    , event_instrument :: !Instrument
    , event_environ :: !BaseTypes.Environ
    -- | Flags have their own field rather than being in 'event_environ', this
    -- emphasizes that they're meant to be used by calls and not from the
    -- score.
    , event_flags :: !Flags.Flags
    -- | This has arguments passed from a call that applies an attribute to one
    -- which is meant to later realize the attribute.  This happens when a call
    -- needs to be configured at the track level, but also needs some
    -- information only available later, such as the real start time or pitch
    -- of the next note.  They are indexed by attribute because there may be
    -- multiple delayed calls on a single note, and the realize postproc may
    -- want to ignore some, e.g. if they are overidden by another attribute.
    --
    -- I couldn't think of a type safe way to do this, but Dynamic should be
    -- safe enough if you use a shared type declaration in both writer and
    -- reader.
    , event_delayed_args :: !(Map Text Dynamic.Dynamic)
    -- | Keep track of interesting things that have happened to this event.
    -- Postproc transforms that alter it should prefix a note.
    , event_logs :: ![Log.Msg]
    } deriving (Show, Typeable.Typeable)

-- | Format an event in a way suitable for including inline in log messages.
-- It's short, but hopefully enough information to identify the event in
-- question.
--
-- This is the derive equivalent to 'Cmd.Cmd.log_event'.
short_event :: Event -> Text
short_event e = pretty $ foldr1 (Pretty.<+>) $ concat $ filter (not . null)
    [ [Pretty.format (event_start e, event_duration e)]
    , [Pretty.format (event_instrument e)]
    , [Pretty.format n | Just n <- [initial_note e]]
    , [Pretty.format (event_text e) | not (Text.null (event_text e))]
    , [Pretty.text stack
        | Just stack <- [Stack.pretty_ui_inner (event_stack e)]]
    ]

short_events :: [Event] -> Text
short_events =
    pretty . Pretty.formattedList '[' ']' . map (Pretty.text . short_event)

empty_event :: Event
empty_event = Event
    { event_start = 0
    , event_duration = 0
    , event_text = mempty
    , event_controls = mempty
    , event_pitch = mempty
    , event_pitches = mempty
    , event_stack = Stack.empty
    , event_highlight = Color.NoHighlight
    , event_instrument = empty_instrument
    , event_environ = mempty
    , event_flags = mempty
    , event_delayed_args = mempty
    , event_logs = []
    }

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

-- | Get minimum and maximum edges of the event.  'event_start' isn't
-- necessarily the minimum because of negative durations.
event_min, event_max :: Event -> RealTime
event_min event = min (event_start event) (event_end event)
event_max event = max (event_start event) (event_end event)

events_overlap :: Event -> Event -> Bool
events_overlap e1 e2 =
    not $ event_end e1 <= event_end e2 || event_start e1 >= event_end e2

event_scale_id :: Event -> Pitch.ScaleId
event_scale_id = PSignal.sig_scale_id . event_pitch

-- | If you use an event to create another event, call this to clear out
-- data that shouldn't go with the copy.
copy :: Event -> Event
copy event = event { event_flags = mempty, event_logs = [] }

-- | Apply environ and controls to pitches.
--
-- Normally this is done by Convert, but if you want to see an event for
-- debugging it can be nicer to see the normalized version.
--
-- Unlike "Perform.Midi.Convert", this doesn't trim the controls, so it applies
-- out-of-range transpositions.
normalize :: Event -> Event
normalize event = event
    { event_pitch = apply $ event_pitch event
    , event_pitches = apply <$> event_pitches event
    }
    where
    apply = PSignal.apply_controls controls
        . PSignal.apply_environ (event_environ event)
    controls = event_controls event

-- ** flags

has_flags :: Flags.Flags -> Event -> Bool
has_flags flags = (`Flags.has` flags) . event_flags

add_flags :: Flags.Flags -> Event -> Event
add_flags flags event = event { event_flags = flags <> event_flags event }

remove_flags :: Flags.Flags -> Event -> Event
remove_flags flags event =
    event { event_flags = event_flags event Set.\\ flags }

-- ** logs

add_log :: CallStack.Stack => Text -> Event -> Event
add_log msg = add_log_msg (Log.msg Log.Debug Nothing msg)

add_log_msg :: Log.Msg -> Event -> Event
add_log_msg msg event = event { event_logs = msg : event_logs event }

-- ** environ

modify_environ :: (BaseTypes.Environ -> BaseTypes.Environ) -> Event -> Event
modify_environ f event = event { event_environ = f (event_environ event) }

-- | Modify the value at the given key.
modify_environ_key :: EnvKey.Key
    -> (Maybe BaseTypes.Val -> BaseTypes.Val) -> Event -> Event
modify_environ_key name modify = modify_environ $ \(BaseTypes.Environ env) ->
    BaseTypes.Environ $ Map.alter (Just . modify) name env

-- ** attributes

event_attributes :: Event -> Attrs.Attributes
event_attributes = BaseTypes.environ_attributes . event_environ

has_attribute :: Attrs.Attributes -> Event -> Bool
has_attribute attr = (`Attrs.contain` attr) . event_attributes

intersecting_attributes :: Attrs.Attributes -> Event -> Bool
intersecting_attributes attrs event =
    Attrs.intersection attrs (event_attributes event) /= mempty

modify_attributes :: (Attrs.Attributes -> Attrs.Attributes) -> Event -> Event
modify_attributes modify = modify_environ $ \env ->
    BaseTypes.insert EnvKey.attributes
        (BaseTypes.VAttributes (modify (BaseTypes.environ_attributes env))) env

add_attributes :: Attrs.Attributes -> Event -> Event
add_attributes attrs
    | attrs == mempty = id
    | otherwise = modify_attributes (<>attrs)

remove_attributes :: Attrs.Attributes -> Event -> Event
remove_attributes attrs event
    | attrs == mempty || not (has_attribute attrs event) = event
    | otherwise = modify_attributes (Attrs.remove attrs) event

instance DeepSeq.NFData Event where
    rnf (Event start dur text controls pitch pitches _ _ _ _
            flags _delayed_args logs) =
        -- I can't force Dynamic, so leave off _delayed_args.
        rnf (start, dur, text, controls, pitch, pitches, flags, logs)

instance Pretty Event where
    format (Event start dur text controls pitch pitches
            stack highlight inst env flags delayed_args logs) =
        Pretty.record ("Event"
                Pretty.<+> Pretty.format (start, dur)
                Pretty.<+> Pretty.format text)
            [ ("instrument", Pretty.format inst)
            , ("pitch", Pretty.format pitch)
            , ("pitches", Pretty.format pitches)
            , ("controls", Pretty.format controls)
            , ("stack", Pretty.format stack)
            , ("highlight", Pretty.text $ showt highlight)
            , ("environ", Pretty.format env)
            , ("flags", Pretty.format flags)
            , ("delayed_args", Pretty.format delayed_args)
            , ("logs", Pretty.format logs)
            ]

-- ** delayed args

put_arg :: Typeable.Typeable a => Text -> a -> Event -> Event
put_arg key arg event = event
    { event_delayed_args = Map.insert key (Dynamic.toDyn arg)
        (event_delayed_args event)
    }

-- | Find an arg in 'event_delayed_args', and remove it from the event if it
-- existed.  Throw an error if it existed but had an unexpected type.
take_arg :: Typeable.Typeable a => Text -> Event
    -> Either Text (Event, Maybe a)
take_arg key event = case Map.lookup key (event_delayed_args event) of
    Nothing -> Right (event, Nothing)
    Just arg -> case Dynamic.fromDynamic arg of
        Nothing -> Left $ "incorrect delayed arg type for " <> showt key
            <> ": " <> pretty arg
        Just a -> Right (delete_arg key event, Just a)

delete_arg :: Text -> Event -> Event
delete_arg key event =
    event { event_delayed_args = Map.delete key (event_delayed_args event) }

-- ** modify events

-- These operate directly on events, so we are in RealTime at this point.

-- | Change the start time of an event and move its controls along with it.
move :: (RealTime -> RealTime) -> Event -> Event
move modify event
    | pos == event_start event = event
    | otherwise = event
        { event_start = pos
        , event_controls = fmap (Signal.shift delta) <$> event_controls event
        , event_pitch = PSignal.shift delta $ event_pitch event
        , event_pitches = PSignal.shift delta <$> event_pitches event
        }
    where
    pos = modify (event_start event)
    delta = pos - event_start event

place :: RealTime -> RealTime -> Event -> Event
place start dur event = (move (const start) event) { event_duration = dur }

move_start :: RealTime -> RealTime -> Event -> Event
move_start min_duration offset
    | offset == 0 = id
    | otherwise = duration (max min_duration . subtract offset) . move (+offset)

duration :: (RealTime -> RealTime) -> Event -> Event
duration modify event
    | dur == event_duration event = event
    | otherwise = event { event_duration = dur }
    where dur = modify (event_duration event)

set_duration :: RealTime -> Event -> Event
set_duration = duration . const

-- | Set the instrument on an event, and also update its environ from the
-- instrument.  You should really rederive with the new instrument, but this
-- way can be more convenient, if somewhat sketchy.
set_instrument :: Instrument -> BaseTypes.Environ -> Event -> Event
set_instrument score_inst inst_environ event = event
    { event_instrument = score_inst
    , event_environ = inst_environ <> event_environ event
    }

-- *** control

-- | Get a control value from the event, or Nothing if that control isn't
-- present.
control_at :: RealTime -> Control -> Event -> Maybe (Typed Signal.Y)
control_at pos control event =
    fmap (Signal.at pos) <$> Map.lookup control (event_controls event)

event_control :: Control -> Event -> Maybe (Typed Signal.Control)
event_control control = Map.lookup control . event_controls

initial_dynamic :: Event -> Signal.Y
initial_dynamic event = maybe 0 typed_val $
     -- Derive.initial_controls should mean this is never Nothing.
    control_at (event_start event) c_dynamic event

-- | Use this instead of 'modify_control_vals' because it also sets
-- 'EnvKey.dynamic_val'.  This is only valid for linear functions like (+) or
-- (*).
modify_dynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_dynamic modify =
    modify_environ_key EnvKey.dynamic_val
            (BaseTypes.VNum . untyped . modify . num_of)
        . modify_control_vals c_dynamic modify
    where
    num_of (Just (BaseTypes.VNum n)) = typed_val n
    num_of _ = 0

-- | Use this instead of 'set_control' because it also sets
-- 'EnvKey.dynamic_val'.
set_dynamic :: Signal.Y -> Event -> Event
set_dynamic dyn =
    modify_environ_key EnvKey.dynamic_val (const $ BaseTypes.VNum $ untyped dyn)
        . set_control c_dynamic (untyped (Signal.constant dyn))

modify_control_vals :: Control -> (Signal.Y -> Signal.Y) -> Event -> Event
modify_control_vals control modify event = event
    { event_controls = Map.adjust (fmap (Signal.map_y_linear modify)) control
        (event_controls event)
    }

-- | Modify a control.  If there is no existing control, the modify function
-- gets an empty signal.
modify_control :: Control -> (Signal.Control -> Signal.Control) -> Event
    -> Event
modify_control control modify event = event
    { event_controls =
        Map.alter (Just . alter) control (event_controls event)
    }
    where alter old = modify <$> fromMaybe mempty old

set_control :: Control -> Typed Signal.Control -> Event -> Event
set_control control signal event = event
    { event_controls = Map.insert control signal (event_controls event) }

event_controls_at :: RealTime -> Event -> ControlValMap
event_controls_at t event =
    typed_val . fmap (Signal.at t) <$> event_controls event

-- *** pitch

default_pitch :: PControl
default_pitch = ""

set_pitch :: PSignal.PSignal -> Event -> Event
set_pitch = set_named_pitch default_pitch

set_named_pitch :: PControl -> PSignal.PSignal -> Event -> Event
set_named_pitch pcontrol signal event
    | pcontrol == default_pitch = event { event_pitch = signal }
    | otherwise = event
        { event_pitches = Map.insert pcontrol signal (event_pitches event) }

event_named_pitch :: PControl -> Event -> Maybe PSignal.PSignal
event_named_pitch pcontrol
    | pcontrol == default_pitch = Just . event_pitch
    | otherwise = Map.lookup pcontrol . event_pitches

-- | Unlike 'Derive.Derive.pitch_at', the transposition has already been
-- applied.  This is because callers expect to get the actual pitch, not the
-- pitch plus some homework to do on the pitch.  If you use this pitch to emit
-- another pitch you proabbly need the raw pitch, but so far everyone doing
-- that is at the Derive level, not postproc, so they use Derive.pitch_at.
transposed_at :: RealTime -> Event -> Maybe PSignal.Transposed
transposed_at pos event = PSignal.apply_config config <$> pitch_at pos event
    where
    config = PSignal.PitchConfig (event_environ event)
        (event_controls_at pos event)

pitch_at :: RealTime -> Event -> Maybe PSignal.Pitch
pitch_at pos event = PSignal.at pos $ event_pitch event

apply_controls :: Event -> RealTime -> PSignal.Pitch -> PSignal.Transposed
apply_controls event pos = PSignal.apply (event_controls_at pos event)

initial_pitch :: Event -> Maybe PSignal.Transposed
initial_pitch event = transposed_at (event_start event) event

nn_at :: RealTime -> Event -> Maybe Pitch.NoteNumber
nn_at pos event = either (const Nothing) Just . PSignal.pitch_nn
    =<< transposed_at pos event

initial_nn :: Event -> Maybe Pitch.NoteNumber
initial_nn event = nn_at (event_start event) event

note_at :: RealTime -> Event -> Maybe Pitch.Note
note_at pos event = either (const Nothing) Just . PSignal.pitch_note
    =<< transposed_at pos event

initial_note :: Event -> Maybe Pitch.Note
initial_note event = note_at (event_start event) event

nn_signal :: Event -> (Signal.NoteNumber, [(RealTime, PSignal.PitchError)])
nn_signal event =
    PSignal.to_nn $ PSignal.apply_controls (event_controls event) $
        PSignal.apply_environ (event_environ event) $
        event_pitch event


-- * util

-- | Use this constructor when making a Control from user input.  Literals
-- can use the IsString instance.
control :: Text -> Either Text Control
control name
    | Text.null name = Left "empty control name"
    | Id.valid_symbol name = Right $ ScoreT.Control name
    | otherwise = Left $ "invalid characters in control: " <> showt name

unchecked_control :: Text -> Control
unchecked_control = ScoreT.Control

-- | Use this constructor when making a PControl from user input.  Literals
-- can use the IsString instance.
pcontrol :: Text -> Either Text PControl
pcontrol name
    | Text.null name || Id.valid_symbol name = Right $ ScoreT.PControl name
    | otherwise = Left $ "invalid characters in pitch control: " <> showt name

unchecked_pcontrol :: Text -> PControl
unchecked_pcontrol = ScoreT.PControl

-- | Converted into velocity or breath depending on the instrument.
c_dynamic :: Control
c_dynamic = "dyn"

-- | Parse either a Control or PControl.
parse_generic_control :: Text -> Either Text (Either Control PControl)
parse_generic_control name = case Text.uncons name of
    Just ('#', rest) -> Right <$> pcontrol rest
    _ -> Left <$> control name

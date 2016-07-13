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
    module Derive.ScoreTypes, module Derive.BaseTypes
    -- * Event
    , Event(..)
    , log_event, log_events
    , empty_event, event_end, event_min, event_max, event_scale_id
    , event_transformed_controls, event_transformed_pitch
    , event_transformed_pitches
    , copy, normalize
    -- ** flags
    , has_flags, add_flags, remove_flags
    -- ** environ
    , modify_environ, modify_environ_key
    -- ** attributes
    , event_attributes, has_attribute, intersecting_attributes
    , environ_attributes
    , modify_attributes, add_attributes, remove_attributes
    -- ** delayed args
    , put_arg, take_arg

    -- ** modify events
    , move, place, move_start, duration, set_duration
    -- *** control
    , control_at, event_control, initial_dynamic, modify_dynamic, set_dynamic
    , modify_control, merge_control
    , set_control, event_controls_at
    -- *** pitch
    , default_pitch, set_pitch, set_named_pitch, event_pitch
    , transposed_at, pitch_at, pitch_sample_at, apply_controls
    , initial_pitch, nn_at, initial_nn, note_at, initial_note

    -- ** warp
    , warp, is_id_warp
    , warp_pos, unwarp_pos, compose_warps
    , warp_to_signal

    -- * instrument
    , instrument_valid_chars
    , instrument_name, empty_instrument, instrument, split_instrument

    -- * util
    , control, unchecked_control
    , pcontrol, unchecked_pcontrol
    , c_dynamic
    , parse_generic_control
) where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import qualified Data.Dynamic as Dynamic
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

import qualified Util.Pretty as Pretty
import qualified Ui.Color as Color
import qualified Ui.Id as Id
import qualified Derive.Attrs as Attrs
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes
       (ControlMap, ControlFunction(..), ControlFunctionMap, PitchMap)
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Flags as Flags
import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreTypes as ScoreTypes
import Derive.ScoreTypes
       (Instrument(..), Control, control_name, PControl, pcontrol_name,
        Warp(..), id_warp, id_warp_signal, Type(..), Typed(..), ControlValMap,
        TypedControlValMap, untyped, merge_typed, type_to_code, code_to_type,
        TypedControl, TypedVal)
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


-- * Event

data Event = Event {
    -- | 'event_start', 'event_duration', and 'event_text' are the core
    -- attributes that define an event.
    event_start :: !RealTime
    , event_duration :: !RealTime
    , event_text :: !Text
    -- | See NOTE [event_control_offset] for what the untransformed is about.
    , event_untransformed_controls :: !ControlMap
    , event_untransformed_pitch :: !PSignal.PSignal
    -- | Named pitch signals.
    , event_untransformed_pitches :: !PitchMap
    -- | This is added to the untransformed controls on acces, so you can move
    -- an event without having to move all the samples in all the controls.
    , event_control_offset :: !RealTime
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
    , event_delayed_args :: !(Map.Map Text Dynamic.Dynamic)
    } deriving (Show, Typeable.Typeable)

-- | Format an event in a way suitable for including inline in log messages.
-- It's short, but hopefully enough information to identify the event in
-- question.
log_event :: Event -> Text
log_event e = pretty $ foldr1 (Pretty.<+>) $ concat $ filter (not . null)
    [ [Pretty.format (event_start e, event_duration e)]
    , [Pretty.format (event_instrument e)]
    , [Pretty.format n | Just n <- [initial_note e]]
    , [Pretty.format (event_text e) | not (Text.null (event_text e))]
    , [Pretty.text stack
        | Just stack <- [Stack.pretty_ui_inner (event_stack e)]]
    ]

log_events :: [Event] -> Text
log_events =
    pretty . Pretty.formattedList '[' ']' . map (Pretty.text . log_event)

-- NOTE [event_control_offset]
-- event_control_offset is a hack to make moving Events cheap.  Unfortunately
-- it could be error prone because you don't want to look at the untransformed
-- signals by accident.  I try to reduce the error-prone-ness by giving the
-- fields big ugly names to encourage the access functions which apply the
-- offset.
--
-- It might be better to provide a wrapper around a TimeVector with an offset
-- built-in, then the untransformed signal could never leak out.  But I don't
-- quite want to duplicate every signal function four times, and it seems silly
-- to give every single signal an offset when only the ones in Event use it,
-- and it's the same for all of them.  Also it turns out to be a bit of
-- a hassle to mess with the signals on events since every time you have to
-- decide if you need transformed on untransformed.
--
-- TODO Actually I kind of hate this, because of the ugly long names and the
-- constant need to decide if I need to deal with transformed or untransformed
-- controls.

empty_event :: Event
empty_event = Event
    { event_start = 0
    , event_duration = 0
    , event_text = mempty
    , event_untransformed_controls = mempty
    , event_untransformed_pitch = mempty
    , event_untransformed_pitches = mempty
    , event_control_offset = 0
    , event_stack = Stack.empty
    , event_highlight = Color.NoHighlight
    , event_instrument = empty_instrument
    , event_environ = mempty
    , event_flags = mempty
    , event_delayed_args = mempty
    }

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

-- | Get minimum and maximum edges of the event.  'event_start' isn't
-- necessarily the minimum because of negative durations.
event_min, event_max :: Event -> RealTime
event_min event = min (event_start event) (event_end event)
event_max event = max (event_start event) (event_end event)

event_scale_id :: Event -> Pitch.ScaleId
event_scale_id = PSignal.sig_scale_id . event_untransformed_pitch

event_transformed_controls :: Event -> ControlMap
event_transformed_controls event =
    Map.map (fmap (Signal.shift (event_control_offset event)))
        (event_untransformed_controls event)

event_transformed_pitch :: Event -> PSignal.PSignal
event_transformed_pitch event =
    PSignal.shift (event_control_offset event)
        (event_untransformed_pitch event)

event_transformed_pitches :: Event -> PitchMap
event_transformed_pitches event =
    Map.map (PSignal.shift (event_control_offset event))
        (event_untransformed_pitches event)

-- | If you use an event to create another event, call this to clear out
-- data that shouldn't go with the copy.
copy :: Event -> Event
copy event = event { event_flags = mempty }

-- | Apply 'event_control_offset' and apply environ and controls to pitches.
-- Normally this is done by Convert, but if you want to see an event for
-- debugging it can be nicer to see the normalized version.
--
-- Unlike "Perform.Midi.Convert", this doesn't trim the controls, so it applies
-- out-of-range transpositions.
normalize :: (Instrument -> BaseTypes.Environ) -> Event -> Event
normalize lookup_environ event = event
    { event_untransformed_pitch = apply $ event_transformed_pitch event
    , event_untransformed_pitches = apply <$> event_transformed_pitches event
    , event_untransformed_controls = controls
    , event_control_offset = 0
    }
    where
    apply = PSignal.apply_controls controls . PSignal.apply_environ env
    env = lookup_environ (event_instrument event) <> event_environ event
    controls = event_transformed_controls event

-- ** flags

has_flags :: Flags.Flags -> Event -> Bool
has_flags flags = (`Flags.has` flags) . event_flags

add_flags :: Flags.Flags -> Event -> Event
add_flags flags event = event { event_flags = flags <> event_flags event }

remove_flags :: Flags.Flags -> Event -> Event
remove_flags flags event =
    event { event_flags = event_flags event Set.\\ flags }

-- ** environ

modify_environ :: (BaseTypes.Environ -> BaseTypes.Environ) -> Event -> Event
modify_environ f event = event { event_environ = f (event_environ event) }

modify_environ_key :: BaseTypes.Key
    -> (Maybe BaseTypes.Val -> BaseTypes.Val) -> Event -> Event
modify_environ_key name modify = modify_environ $ \(BaseTypes.Environ env) ->
    BaseTypes.Environ $ Map.alter (Just . modify) name env

-- ** attributes

event_attributes :: Event -> Attrs.Attributes
event_attributes = environ_attributes . event_environ

has_attribute :: Attrs.Attributes -> Event -> Bool
has_attribute attr = (`Attrs.contain` attr) . event_attributes

intersecting_attributes :: Attrs.Attributes -> Event -> Bool
intersecting_attributes attrs event =
    Attrs.intersection attrs (event_attributes event) /= mempty

environ_attributes :: BaseTypes.Environ -> Attrs.Attributes
environ_attributes environ =
    case BaseTypes.lookup EnvKey.attributes environ of
        Just (BaseTypes.VAttributes attrs) -> attrs
        _ -> mempty

modify_attributes :: (Attrs.Attributes -> Attrs.Attributes) -> Event -> Event
modify_attributes modify = modify_environ $ \env ->
    BaseTypes.insert EnvKey.attributes
        (BaseTypes.VAttributes (modify (environ_attributes env))) env

add_attributes :: Attrs.Attributes -> Event -> Event
add_attributes attrs
    | attrs == mempty = id
    | otherwise = modify_attributes (<>attrs)

remove_attributes :: Attrs.Attributes -> Event -> Event
remove_attributes attrs event
    | attrs == mempty || not (has_attribute attrs event) = event
    | otherwise = modify_attributes (Attrs.remove attrs) event

instance DeepSeq.NFData Event where
    rnf (Event start dur text controls pitch pitches _ _ _ _ _ flags
            _delayed_args) =
        rnf start `seq` rnf dur `seq` rnf text `seq` rnf controls
            `seq` rnf pitch `seq` rnf pitches `seq` rnf flags
            -- I can't force Dynamic, so leave off _delayed_args.

instance Pretty.Pretty Event where
    format (Event start dur text controls pitch pitches coffset stack highlight
            inst env flags delayed_args) =
        Pretty.record ("Event"
                Pretty.<+> Pretty.format (start, dur)
                Pretty.<+> Pretty.format text)
            [ ("instrument", Pretty.format inst)
            , ("pitch", Pretty.format pitch)
            , ("pitches", Pretty.format pitches)
            , ("controls", Pretty.format controls)
            , ("control_offset", Pretty.format coffset)
            , ("stack", Pretty.format stack)
            , ("highlight", Pretty.text $ showt highlight)
            , ("environ", Pretty.format env)
            , ("flags", Pretty.format flags)
            , ("delayed_args", Pretty.format delayed_args)
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
        , event_control_offset = pos - event_start event
        }
    where pos = modify (event_start event)

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

-- set_instrument is in "Derive.Call.Post".

-- *** control

control_val_at :: Event -> RealTime -> TypedControl -> TypedVal
control_val_at event t = fmap (Signal.at (t - event_control_offset event))

-- | Get a control value from the event, or Nothing if that control isn't
-- present.
control_at :: RealTime -> Control -> Event -> Maybe TypedVal
control_at pos control event =
    control_val_at event pos <$>
        Map.lookup control (event_untransformed_controls event)

event_control :: Control -> Event -> Maybe (Typed Signal.Control)
event_control control = Map.lookup control . event_transformed_controls

initial_dynamic :: Event -> Signal.Y
initial_dynamic event = maybe 0 typed_val $
     -- Derive.initial_controls should mean this is never Nothing.
    control_at (event_start event) c_dynamic event

-- | Use this instead of 'modify_control' because it also sets
-- 'EnvKey.dynamic_val'.
modify_dynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_dynamic modify =
    modify_environ_key EnvKey.dynamic_val
            (BaseTypes.VNum . untyped . modify . num_of)
        . modify_control c_dynamic modify
    where
    num_of (Just (BaseTypes.VNum n)) = typed_val n
    num_of _ = 0

-- | Use this instead of 'set_control' because it also sets
-- 'EnvKey.dynamic_val'.
set_dynamic :: Signal.Y -> Event -> Event
set_dynamic dyn =
    modify_environ_key EnvKey.dynamic_val (const $ BaseTypes.VNum $ untyped dyn)
        . set_control c_dynamic (untyped (Signal.constant dyn))

modify_control :: Control -> (Signal.Y -> Signal.Y) -> Event -> Event
modify_control control modify event = event
    { event_untransformed_controls =
        Map.adjust (fmap (Signal.map_y modify)) control
            (event_untransformed_controls event)
    }

-- | Merge a signal with an existing one.  If there is no existing control,
-- the merge function gets an empty signal.
merge_control :: Control -> (Signal.Control -> Signal.Control)
    -> Event -> Event
merge_control control merge event = event
    { event_untransformed_controls =
        Map.alter (Just . alter) control (event_untransformed_controls event)
    }
    where alter old = merge <$> fromMaybe mempty old

set_control :: Control -> Typed Signal.Control -> Event -> Event
set_control control signal event = event
    { event_untransformed_controls = Map.insert control
        (Signal.shift (- event_control_offset event) <$> signal)
        (event_untransformed_controls event)
    }

event_controls_at :: RealTime -> Event -> ControlValMap
event_controls_at t event = Map.map (typed_val . control_val_at event t)
    (event_untransformed_controls event)

-- *** pitch

default_pitch :: PControl
default_pitch = ""

set_pitch :: PSignal.PSignal -> Event -> Event
set_pitch = set_named_pitch default_pitch

set_named_pitch :: PControl -> PSignal.PSignal -> Event -> Event
set_named_pitch pcontrol signal event
    | pcontrol == default_pitch = event
        { event_untransformed_pitch =
            PSignal.shift (- event_control_offset event) signal
        }
    | otherwise = event
        { event_untransformed_pitches = Map.insert pcontrol
            (PSignal.shift (- event_control_offset event) signal)
            (event_untransformed_pitches event)
        }

event_pitch :: PControl -> Event -> Maybe PSignal.PSignal
event_pitch pcontrol
    | pcontrol == default_pitch = Just . event_transformed_pitch
    | otherwise = Map.lookup pcontrol . event_transformed_pitches

-- | Unlike 'Derive.Derive.pitch_at', the transposition has already been
-- applied.  This is because callers expect to get the actual pitch, not the
-- pitch plus some homework to do on the pitch.  If you use this pitch to emit
-- another pitch you proabbly need the raw pitch, but so far everyone doing
-- that is at the Derive level, not postproc, so they use Derive.pitch_at.
transposed_at :: RealTime -> Event -> Maybe PSignal.Transposed
transposed_at pos event = apply_controls event pos <$> pitch_at pos event

pitch_at :: RealTime -> Event -> Maybe PSignal.Pitch
pitch_at pos event = snd <$> pitch_sample_at pos event

pitch_sample_at :: RealTime -> Event -> Maybe (RealTime, PSignal.Pitch)
pitch_sample_at pos event = PSignal.sample_at (pos - event_control_offset event)
    (event_untransformed_pitch event)

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


-- ** warp

-- | Convert a Signal to a Warp.
warp :: Signal.Warp -> Warp
warp sig = Warp sig 0 1

is_id_warp :: Warp -> Bool
is_id_warp = (== id_warp)

-- | Warp a ScoreTime to a RealTime.
--
-- The warp signal is extended linearly in either direction infinitely, with
-- its last slope, or 1:1 if there are no samples at all.  Previously, the warp
-- signal was flat before zero and after its end, which effectively made the
-- tempo infinitely fast at those points.  But that made the optimization for
-- 'id_warp_signal' produce inconsistent results with a warp that was linear
-- but not not equal to id_warp_signal, which in turn led to inconsistent
-- results from ornaments that placed notes before ScoreTime 0.
warp_pos :: Warp -> ScoreTime -> RealTime
warp_pos (Warp sig shift stretch) pos
    | sig == id_warp_signal = warp pos
    | otherwise = Signal.y_to_x $ Signal.at_linear_extend (warp pos) sig
    where warp p = to_real p * stretch + shift

-- | The inverse of 'warp_pos'.  I originally would fail when the RealTime
-- doesn't occur in the Warp, but now I extend it in the same way as
-- 'warp_pos'.  Failing caused awkwardness with events at the end of the score.
unwarp_pos :: Warp -> RealTime -> ScoreTime
unwarp_pos (Warp sig shift stretch) pos
    | sig == id_warp_signal = unwarp pos
    | otherwise = unwarp $ Signal.inverse_at_extend (Signal.x_to_y pos) sig
    where unwarp p = to_score $ (p - shift) / stretch

-- | Compose two warps.  Warps with id signals are optimized.
-- This is standard right to left composition
compose_warps :: Warp -> Warp -> Warp
compose_warps
        warp1@(Warp sig1 shift1 stretch1) warp2@(Warp sig2 shift2 stretch2)
    | is_id_warp warp1 = warp2
    | is_id_warp warp2 = warp1
    | sig2 == id_warp_signal =
        Warp sig1 (shift1 + shift2 * stretch1) (stretch1 * stretch2)
    -- A sig==id_warp_signal optimization isn't possible because shift and
    -- stretch are applied before indexing the signal, not after.
    | otherwise = compose warp1 warp2
    where
    -- Shift and stretch are applied before the signal, so map the shift and
    -- stretch of the first signal across the output of the second signal
    -- before composing them.
    --
    -- f(g(t*sg + og)*sf + of)
    -- f((warp_to_signal g sf of)*sf + of)
    -- compose f (warp_to_signal g sf of)
    compose warp1 (Warp sig2 shift2 stretch2) = Warp fg shift2 stretch2
        where fg = Signal.compose (warp_to_signal warp1) sig2

-- | Flatten a 'Warp' to a 'Signal.Warp'.
warp_to_signal :: Warp -> Signal.Warp
warp_to_signal (Warp sig shift stretch)
    | stretch == 1 && shift == 0 = sig
    | stretch == 1 = Signal.map_x (subtract shift) sig
    | otherwise = Signal.map_x ((`RealTime.div` factor) . subtract shift) sig
    where factor = RealTime.to_seconds stretch

-- * instrument

-- | Set of characters allowed in an instrument name.
instrument_valid_chars :: [Char]
instrument_valid_chars = '-' : ['0'..'9'] ++ ['a'..'z']

instrument_name :: Instrument -> Text
instrument_name (Instrument s) = s

empty_instrument :: Instrument
empty_instrument = Instrument ""

instrument :: Text -> Instrument
instrument = Instrument

split_instrument :: Instrument -> (Text, Text)
split_instrument (Instrument inst) = (synth, Text.drop 1 name)
    where (synth, name) = Text.break (=='/') inst

-- * util

-- | Use this constructor when making a Control from user input.  Literals
-- can use the IsString instance.
control :: Text -> Either Text Control
control name
    | Text.null name = Left "empty control name"
    | Id.valid name = Right $ ScoreTypes.Control name
    | otherwise = Left $ "invalid characters in control: " <> showt name

unchecked_control :: Text -> Control
unchecked_control = ScoreTypes.Control

-- | Use this constructor when making a PControl from user input.  Literals
-- can use the IsString instance.
pcontrol :: Text -> Either Text PControl
pcontrol name
    | Text.null name || Id.valid name = Right $ ScoreTypes.PControl name
    | otherwise = Left $ "invalid characters in pitch control: " <> showt name

unchecked_pcontrol :: Text -> PControl
unchecked_pcontrol = ScoreTypes.PControl

-- | Converted into velocity or breath depending on the instrument.
c_dynamic :: Control
c_dynamic = "dyn"

to_real :: ScoreTime -> RealTime
to_real = RealTime.score

to_score :: RealTime -> ScoreTime
to_score = RealTime.to_score

-- | Parse either a Control or PControl.
parse_generic_control :: Text -> Either Text (Either Control PControl)
parse_generic_control name = case Text.uncons name of
    Just ('#', rest) -> Right <$> pcontrol rest
    _ -> Left <$> control name

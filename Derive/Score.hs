-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | This has the basic data structures for the deriver level.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score (
    module Derive.Score, module Derive.BaseTypes
) where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.Color as Color
import qualified Derive.BaseTypes as BaseTypes
import Derive.BaseTypes
       (Instrument(..), Control, PControl(..), Warp(..), Type(..), Typed(..),
        ControlValMap, ControlMap, ControlFunction(..), ControlFunctionMap,
        PitchMap, untyped, merge_typed, type_to_code, code_to_type,
        TypedControl, TypedVal, Attributes, Attribute, attr, attrs,
        set_to_attrs, attrs_diff, attrs_contain, attrs_remove, attrs_set,
        attrs_list, no_attrs)
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


-- * Event

data Event = Event {
    -- | These are the core attributes that define an event.
    event_start :: !RealTime
    , event_duration :: !RealTime
    , event_text :: !Text
    , event_controls :: !ControlMap
    , event_pitch :: !PitchSignal.Signal
    -- | Named pitch signals.
    , event_pitches :: !PitchMap
    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: !Stack.Stack
    , event_highlight :: !Color.Highlight
    , event_instrument :: !Instrument
    , event_environ :: !BaseTypes.Environ
    } deriving (Show)

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
    , event_instrument = empty_inst
    , event_environ = mempty
    }

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

-- | Get minimum and maximum edges of the event.  'event_start' isn't
-- necessarily the minimum because of negative durations.
event_min, event_max :: Event -> RealTime
event_min event = min (event_start event) (event_end event)
event_max event = max (event_start event) (event_end event)

-- ** environ

modify_environ :: (BaseTypes.Environ -> BaseTypes.Environ) -> Event -> Event
modify_environ f event = event { event_environ = f (event_environ event) }

-- ** attributes

event_attributes :: Event -> Attributes
event_attributes = environ_attributes . event_environ

has_attribute :: Attributes -> Event -> Bool
has_attribute attr = (`attrs_contain` attr) . event_attributes

environ_attributes :: BaseTypes.Environ -> Attributes
environ_attributes environ =
    case BaseTypes.lookup_val Environ.attributes environ of
        Just (BaseTypes.VAttributes attrs) -> attrs
        _ -> mempty

modify_attributes :: (Attributes -> Attributes) -> Event -> Event
modify_attributes modify = modify_environ $ \env ->
    BaseTypes.insert_val Environ.attributes
        (BaseTypes.VAttributes (modify (environ_attributes env))) env

add_attributes :: Attributes -> Event -> Event
add_attributes attrs
    | attrs == mempty = id
    | otherwise = modify_attributes (<>attrs)

remove_attributes :: Attributes -> Event -> Event
remove_attributes attrs
    | attrs == mempty = id
    | otherwise = modify_attributes (attrs_remove attrs)

instance DeepSeq.NFData Event where
    rnf (Event start dur text controls pitch pitches _ _ _ _) =
        rnf start `seq` rnf dur `seq` rnf text `seq` rnf controls
            `seq` rnf pitch `seq` rnf pitches

instance Pretty.Pretty Event where
    format (Event start dur txt controls pitch pitches stack highl inst env) =
        Pretty.record (Pretty.text "Event" Pretty.<+> Pretty.format (start, dur)
                Pretty.<+> Pretty.format txt)
            [ ("instrument", Pretty.format inst)
            , ("pitch", Pretty.format pitch)
            , ("pitches", Pretty.format pitches)
            , ("controls", Pretty.format controls)
            , ("stack", Pretty.format stack)
            , ("highlight", Pretty.text $ show highl)
            , ("environ", Pretty.format env)
            ]

event_scale_id :: Event -> Pitch.ScaleId
event_scale_id = PitchSignal.sig_scale_id . event_pitch

-- ** modify events

-- These operate directly on events, so we are in RealTime at this point.

-- | Change the start time of an event and move its controls along with it.
move :: (RealTime -> RealTime) -> Event -> Event
move f event =
    move_controls (pos - event_start event) $ event { event_start = pos }
    where pos = f (event_start event)

place :: RealTime -> RealTime -> Event -> Event
place start dur event = (move (const start) event) { event_duration = dur }

move_start :: RealTime -> Event -> Event
move_start offset =
    duration (if offset >= 0 then subtract offset else (+offset))
        . move (+offset)

duration :: (RealTime -> RealTime) -> Event -> Event
duration f event = event { event_duration = f (event_duration event) }

set_duration :: RealTime -> Event -> Event
set_duration = duration . const

-- *** control

control_val_at :: RealTime -> TypedControl -> TypedVal
control_val_at t = fmap (Signal.at t)

-- | Get control value at the given time.
control_at :: ControlMap -> Control -> RealTime -> TypedVal
control_at controls c t =
    maybe (untyped 0) (control_val_at t) (Map.lookup c controls)

move_controls :: RealTime -> Event -> Event
move_controls shift event
    | shift == 0 = event
    | otherwise = event
        { event_controls =
            Map.map (fmap (Signal.shift shift)) (event_controls event)
        , event_pitch = PitchSignal.shift shift (event_pitch event)
        }

event_control_at :: RealTime -> Control -> Maybe TypedVal -> Event
    -> Maybe TypedVal
event_control_at pos cont deflt event =
    maybe deflt (Just . control_val_at pos) $
        Map.lookup cont (event_controls event)

initial_dynamic :: Event -> Signal.Y
initial_dynamic event = maybe 0 typed_val $
     -- Derive.initial_controls should mean Nothing never happens.
    event_control_at (event_start event) c_dynamic (Just (untyped 0)) event

modify_dynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_dynamic modify =
    modify_control c_dynamic_function modify . modify_control c_dynamic modify

modify_control :: Control -> (Signal.Y -> Signal.Y) -> Event -> Event
modify_control control modify event = event
    { event_controls = Map.adjust (fmap (Signal.map_y modify)) control
        (event_controls event)
    }

event_controls_at :: RealTime -> Event -> ControlValMap
event_controls_at t = controls_at t . event_controls

controls_at :: RealTime -> ControlMap -> ControlValMap
controls_at p = Map.map (typed_val . control_val_at p)

-- *** pitch

-- | Unlike 'Derive.Derive.pitch_at', the transposition has already been
-- applied.  This is because callers expect to get the actual pitch, not the
-- pitch plus some homework to do on the pitch.  If you use this pitch to emit
-- another pitch you proabbly need the raw pitch, but so far everyone doing
-- that is at the Derive level, not postproc, so they use Derive.pitch_at.
transposed_at :: RealTime -> Event -> Maybe PitchSignal.Transposed
transposed_at pos event = apply_controls event pos <$> pitch_at pos event

pitch_at :: RealTime -> Event -> Maybe PitchSignal.Pitch
pitch_at pos event = PitchSignal.at pos (event_pitch event)

apply_controls :: Event -> RealTime -> PitchSignal.Pitch
    -> PitchSignal.Transposed
apply_controls event pos =
    PitchSignal.apply (event_environ event) (event_controls_at pos event)

initial_pitch :: Event -> Maybe PitchSignal.Transposed
initial_pitch event = transposed_at (event_start event) event

nn_at :: RealTime -> Event -> Maybe Pitch.NoteNumber
nn_at pos event = either (const Nothing) Just . PitchSignal.pitch_nn
    =<< transposed_at pos event

initial_nn :: Event -> Maybe Pitch.NoteNumber
initial_nn event = nn_at (event_start event) event

note_at :: RealTime -> Event -> Maybe Pitch.Note
note_at pos event = either (const Nothing) Just . PitchSignal.pitch_note
    =<< transposed_at pos event

initial_note :: Event -> Maybe Pitch.Note
initial_note event = note_at (event_start event) event


-- ** warp

-- | Convert a Signal to a Warp.
signal_to_warp :: Signal.Warp -> Warp
signal_to_warp sig = Warp sig 0 1

id_warp :: Warp
id_warp = signal_to_warp id_warp_signal

id_warp_signal :: Signal.Warp
-- TODO I should be able to make this Signal.empty, but I'd need to make
-- 'unwarp_pos' extend the signal in the same way that 'warp_pos' does.
id_warp_signal = Signal.signal [(0, 0),
    (RealTime.large, RealTime.to_seconds RealTime.large)]

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
warp_pos :: ScoreTime -> Warp -> RealTime
warp_pos pos (Warp sig shift stretch)
    | sig == id_warp_signal = pos1
    | otherwise = Signal.y_to_real $ Signal.at_linear_extend pos1 sig
    where pos1 = to_real pos * stretch + shift

-- | Unlike 'warp_pos', 'unwarp_pos' can fail.  This asymmetry is because
-- warp_pos will project a signal on forever, but 'Signal.inverse_at' won't.
unwarp_pos :: RealTime -> Warp -> Maybe ScoreTime
unwarp_pos pos (Warp sig shift stretch) =
    case Signal.inverse_at (Signal.x_to_y pos) sig of
        Nothing -> Nothing
        Just p -> Just $ to_score $ (p - shift) / stretch

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
inst_valid_chars :: [Char]
inst_valid_chars = '-' : ['0'..'9'] ++ ['a'..'z']

inst_name :: Instrument -> Text
inst_name (Instrument s) = s

empty_inst :: Instrument
empty_inst = Instrument ""

instrument :: Text -> Text -> Instrument
instrument synth inst = Instrument $ synth <> "/" <> inst

split_inst :: Instrument -> (Text, Text)
split_inst (Instrument inst) = (synth, Text.drop 1 inst_name)
    where (synth, inst_name) = Text.break (=='/') inst

-- * util

-- | Use this control constructor instead of directly calling "Control", though
-- that's not enforced yet.
control :: Text -> Control
control = BaseTypes.Control

control_name :: Control -> Text
control_name (BaseTypes.Control text) = text

-- | Converted into velocity or breath depending on the instrument.
c_dynamic :: Control
c_dynamic = "dyn"

c_dynamic_function :: Control
c_dynamic_function = "_dynamic-function"
    -- The leading underscore should make it unparseable in tracklang.

to_real :: ScoreTime -> RealTime
to_real = RealTime.score

to_score :: RealTime -> ScoreTime
to_score = RealTime.to_score

{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for Monoid and NFData instance
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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Ui.ScoreTime as ScoreTime
import Derive.BaseTypes
       (Instrument(..), Control(..), Type(..), Typed(..), untyped,
        merge_typed, type_to_code, code_to_type, TypedControl, TypedVal,
        Attributes(..), Attribute, attrs_set, attrs_list, no_attrs)
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Stack as Stack

import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


-- * Event

data Event = Event {
    -- | These are the core attributes that define an event.  UI display
    -- attributes like font style are not preserved here.
    event_start :: !RealTime
    , event_duration :: !RealTime
    -- | Event text, carried over from 'Ui.Event.event_bs' for debugging.
    -- UTF8 encoded.
    , event_bs :: !B.ByteString
    , event_controls :: !ControlMap
    , event_pitch :: !PitchSignal.Signal
    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: !Stack.Stack

    -- | These are optional parameters that may or may not be required by the
    -- performer.
    , event_instrument :: !(Maybe Instrument)
    , event_attributes :: !Attributes
    }
    deriving (Read, Show)

empty_event :: Event
empty_event = Event 0 0 mempty mempty mempty Stack.empty Nothing mempty

instance DeepSeq.NFData Event where
    rnf (Event start dur text controls pitch _ _ _) =
        rnf start `seq`  rnf dur `seq` rnf text `seq` rnf controls
            `seq` rnf pitch

instance Pretty.Pretty Event where
    format (Event start dur bytes controls pitch stack inst attrs) =
        Pretty.record (Pretty.text "Event" Pretty.<+> Pretty.format (start, dur)
                Pretty.<+> Pretty.format bytes)
            [ ("controls", Pretty.format controls)
            , ("pitch", Pretty.format pitch)
            , ("stack", Pretty.format stack)
            , ("instrument", Pretty.format inst)
            , ("attributes", Pretty.format attrs)
            ]

-- | This is not a great place, maybe I can make a special module for NFData
-- orphans.
instance DeepSeq.NFData B.ByteString where
    rnf b = b `seq` () -- bytestrings are already strict

type ControlMap = Map.Map Control TypedControl
type PitchMap = Map.Map Control PitchSignal.Signal

event_string :: Event -> String
event_string = UTF8.toString . event_bs

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

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
    duration (if offset >= 0 then (subtract offset) else (+offset))
        . move (+offset)

duration :: (RealTime -> RealTime) -> Event -> Event
duration f event = event { event_duration = f (event_duration event) }

-- *** control

control_at :: RealTime -> TypedControl -> TypedVal
control_at t = fmap (Signal.at t)

-- | Get control value at the given time.
control :: ControlMap -> Control -> RealTime -> TypedVal
control controls c t =
    maybe (untyped 0) (control_at t) (Map.lookup c controls)

move_controls :: RealTime -> Event -> Event
move_controls shift event = event
    { event_controls =
        Map.map (fmap (Signal.shift shift)) (event_controls event)
    , event_pitch = PitchSignal.shift shift (event_pitch event)
    }

event_control_at :: RealTime -> Control -> Maybe TypedVal -> Event
    -> Maybe TypedVal
event_control_at pos cont deflt event = maybe deflt (Just . control_at pos) $
    Map.lookup cont (event_controls event)

initial_dynamic :: Event -> Signal.Y
initial_dynamic event = maybe 0 typed_val $
     -- Derive.initial_controls should mean Nothing never happens.
    event_control_at (event_start event) c_dynamic (Just (untyped 0)) event

modify_dynamic :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_dynamic = modify_event_signal c_dynamic

modify_event_signal :: Control -> (Signal.Y -> Signal.Y) -> Event -> Event
modify_event_signal control f = modify_event_control control (Signal.map_y f)

modify_event_control :: Control -> (Signal.Control -> Signal.Control)
    -> Event -> Event
modify_event_control control f event = case Map.lookup control controls of
        Nothing -> event
        Just typed -> event
            { event_controls = Map.insert control (fmap f typed) controls }
    where controls = event_controls event

event_controls_at :: RealTime -> Event -> PitchSignal.Controls
event_controls_at t = controls_at t .  event_controls

controls_at :: RealTime -> ControlMap -> PitchSignal.Controls
controls_at = Map.map . control_at

-- *** pitch

-- | The pitch at the given time.  The transposition controls have not been
-- applied for the reasons given in 'Derive.Derive.pitch_at'.
pitch_at :: RealTime -> Event -> Maybe PitchSignal.Pitch
pitch_at pos = PitchSignal.at pos . event_pitch

initial_pitch :: Event -> Maybe PitchSignal.Pitch
initial_pitch event = pitch_at (event_start event) event

-- | Unlike 'pitch_at', the transposition has already been applied, because you
-- can't transpose any further once you have a NoteNumber.
nn_at :: RealTime -> Event -> Maybe Pitch.NoteNumber
nn_at pos event = do
    pitch <- pitch_at pos event
    either (const Nothing) Just $ PitchSignal.pitch_nn $
        PitchSignal.apply (event_controls_at pos event) pitch

initial_nn :: Event -> Maybe Pitch.NoteNumber
initial_nn event = nn_at (event_start event) event


-- ** warp

-- | A tempo warp signal.  The shift and stretch are an optimization hack
-- stolen from nyquist.  The idea is to make composed shifts and stretches more
-- efficient if only the shift and stretch are changed.  The necessary magic
-- is in 'compose_warps'.
--
-- The order of operation is: stretch -> shift -> signal.  That is, if the
-- signal is \"f\": f(t*stretch + shift).
data Warp = Warp {
    warp_signal :: !Signal.Warp
    , warp_shift :: !ScoreTime
    , warp_stretch :: !ScoreTime
    } deriving (Eq, Show)

instance Pretty.Pretty Warp where
    format (Warp sig shift stretch) =
        Pretty.record (Pretty.text "Warp"
                Pretty.<+> Pretty.format (shift, stretch))
            [("signal", Pretty.format sig)]

-- | Convert a Signal to a Warp.
signal_to_warp :: Signal.Warp -> Warp
signal_to_warp sig = Warp sig 0 1

id_warp :: Warp
id_warp = signal_to_warp id_warp_signal

id_warp_signal :: Signal.Warp
    -- TODO this is ugly, but would disappear if I decide to make warps
    -- implicitly end with 1/1.  The numbers have to be big enough to not be
    -- exceeded in normal work, but not so big they overflow.
id_warp_signal = Signal.signal [(0, 0),
    (RealTime.large, RealTime.to_seconds RealTime.large)]

is_id_warp :: Warp -> Bool
is_id_warp = (== id_warp)

warp_pos :: ScoreTime -> Warp -> RealTime
warp_pos pos warp@(Warp sig shift stretch)
    | is_id_warp warp = to_real pos
    | otherwise = Signal.y_to_real $
        Signal.at_linear (to_real (pos*stretch + shift)) sig

-- | Unlike 'warp_pos', 'unwarp_pos' can fail.  This asymmetry is because
-- at_linear will project a signal on forever, but inverse_at won't.
unwarp_pos :: RealTime -> Warp -> Maybe ScoreTime
unwarp_pos pos (Warp sig shift stretch) = case Signal.inverse_at pos sig of
    Nothing -> Nothing
    Just p -> Just $ (to_score p - shift) / stretch

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
    compose warp1 (Warp sig2 shift2 stretch2) =
        Warp fg shift2 stretch2
        where fg = Signal.compose (warp_to_signal warp1) sig2

warp_to_signal :: Warp -> Signal.Warp
warp_to_signal (Warp sig shift stretch)
    | stretch == 1 && shift == 0 = sig
    | stretch == 1 = Signal.map_x (subtract (to_real shift)) sig
    | otherwise = Signal.map_x
        ((`RealTime.div` factor) . subtract (to_real shift)) sig
    where factor = ScoreTime.to_double stretch

-- ** warp util

-- | Modify a warp to shift and stretch it.  The argument order is tricky:
-- the composed warp goes *before* the modified warp, but in haskell tradition,
-- the modified argument is last.
place_warp :: ScoreTime -> ScoreTime -> Warp -> Warp
place_warp shift stretch warp = compose_warps warp
    (id_warp { warp_stretch = stretch, warp_shift = shift })

-- TODO this should be merged with warp_control, I need to use SignalBase.map_x
-- warp_pitch :: PitchSignal.PitchSignal -> Warp -> PitchSignal.PitchSignal
-- warp_pitch psig warp@(Warp sig shift stretch)
--     | is_id_warp warp = psig
--         -- optimization
--     | sig == id_warp_signal =
--         PitchSignal.map_x (\p -> (p + to_real shift) * to_real stretch) psig
--     | otherwise = PitchSignal.map_x (\x -> warp_pos (to_score x) warp) psig

-- * instrument

inst_name :: Instrument -> String
inst_name (Instrument s) = s

default_inst :: Instrument
default_inst = Instrument ""

-- * attributes

attrs_diff :: Attributes -> Attributes -> Attributes
attrs_diff (Attributes x) (Attributes y) = Attributes (Set.difference x y)

attrs_union :: Attributes -> Attributes -> Attributes
attrs_union (Attributes x) (Attributes y) = Attributes (Set.union x y)

attributes :: [String] -> Attributes
attributes = Attributes . Set.fromList

attr :: String -> Attributes
attr = Attributes . Set.singleton

-- ** controls

-- | Used as the default control by control block calls.  This is because
-- a ControlCall produces a Signal, but for it to be derived in a block it
-- needs a temporary name.
c_null :: Control
c_null = Control ""

-- | Converted into velocity or breath depending on the instrument.
c_dynamic :: Control
c_dynamic = Control "dyn"

-- | Scale note duration.
c_sustain :: Control
c_sustain = Control "sustain"

c_start_rnd, c_dur_rnd, c_vel_rnd :: Control
c_start_rnd = Control "start-rnd"
c_dur_rnd = Control "dur-rnd"
c_vel_rnd = Control "vel-rnd"

-- | MIDI controls.
c_velocity, c_breath :: Control
c_velocity = Control "vel"
c_breath = Control "breath"

-- *** transposition

c_chromatic :: Control
c_chromatic = Control "t-chromatic"

c_diatonic :: Control
c_diatonic = Control "t-diatonic"

c_hz :: Control
c_hz = Control "t-hz"

-- * util

to_real = RealTime.score
to_score = RealTime.to_score

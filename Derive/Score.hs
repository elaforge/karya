{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- for Monoid and NFData instance
{- | This has the basic data structures for the deriver level.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score where
import qualified Control.DeepSeq as DeepSeq
import Control.DeepSeq (rnf)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set

import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Types as Types

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Derive.Stack as Stack


-- * Event

data Event = Event {
    -- | These are the core attributes that define an event.  UI display
    -- attributes like font style are not preserved here.
    event_start :: !RealTime
    , event_duration :: !RealTime
    , event_bs :: !B.ByteString
    , event_controls :: !ControlMap
    , event_pitch :: !PitchSignal.PitchSignal

    -- | Keep track of where this event originally came from.  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: !Stack.Stack

    -- | These are optional parameters that may or may not be required by the
    -- performer.
    , event_instrument :: !(Maybe Instrument)
    , event_attributes :: !Attributes
    }
    deriving (Eq, Show)

instance DeepSeq.NFData Event where
    rnf (Event start dur text controls pitch _ _ _) =
        rnf start `seq`  rnf dur `seq` rnf text `seq` rnf controls
            `seq` rnf pitch

-- | This is not a great place, maybe I can make a special module for NFData
-- orphans.
instance DeepSeq.NFData B.ByteString where
    rnf b = b `seq` () -- bytestrings are already strict

type ControlMap = Map.Map Control Signal.Control
type PitchMap = Map.Map Control PitchSignal.PitchSignal

event_string :: Event -> String
event_string = B.unpack . event_bs

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

-- | Get control value at the given time.
control :: ControlMap -> Control -> RealTime -> Signal.Y
control controls c pos = maybe 0 (Signal.at pos) (Map.lookup c controls)

move_controls :: RealTime -> Event -> Event
move_controls shift event = event
    { event_controls = Map.map (Signal.shift shift) (event_controls event)
    , event_pitch = PitchSignal.shift shift (event_pitch event)
    }

event_control_at :: RealTime -> Control -> Maybe Signal.Y -> Event
    -> Maybe Signal.Y
event_control_at pos cont deflt event =
    maybe deflt (Just . Signal.at pos) (Map.lookup cont (event_controls event))

initial_velocity :: Event -> Signal.Y
initial_velocity event = maybe 0 id $
     -- Derive.initial_controls should mean Nothing never happens.
    event_control_at (event_start event) c_velocity (Just 0) event

modify_velocity :: (Signal.Y -> Signal.Y) -> Event -> Event
modify_velocity = modify_event_signal c_velocity

modify_event_signal :: Control -> (Signal.Y -> Signal.Y) -> Event -> Event
modify_event_signal control f = modify_event_control control (Signal.map_y f)

modify_event_control :: Control -> (Signal.Control -> Signal.Control)
    -> Event -> Event
modify_event_control control f event = case Map.lookup control controls of
        Nothing -> event
        Just sig ->
            event { event_controls = Map.insert control (f sig) controls }
    where controls = event_controls event

-- *** pitch

pitch_at :: RealTime -> Event -> PitchSignal.Y
pitch_at pos = PitchSignal.at pos . event_pitch

degree_at :: RealTime -> Event -> Pitch.Degree
degree_at pos = PitchSignal.y_to_degree . pitch_at pos

initial_pitch :: Event -> Pitch.Degree
initial_pitch event = degree_at (event_start event) event

modify_pitch :: (Pitch.Degree -> Pitch.Degree) -> Event -> Event
modify_pitch f event =
    event { event_pitch = PitchSignal.map_degree f (event_pitch event) }

transpose :: Pitch.Degree -> Event -> Event
transpose n = modify_pitch (+n)


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

pretty_warp :: Warp -> ([RealTime], ScoreTime, ScoreTime)
pretty_warp (Warp sig shift stretch) =
    ([Signal.y_to_real (Signal.at_linear n sig) | n <- [0, 1, 2, 3]],
        shift, stretch)

-- | Convert a Signal to a Warp.
signal_to_warp :: Signal.Warp -> Warp
signal_to_warp sig = Warp sig 0 1

id_warp :: Warp
id_warp = signal_to_warp id_warp_signal

id_warp_signal :: Signal.Warp
    -- TODO this is ugly, but would disappear if I decide to make warps
    -- implicitly end with 1/1
id_warp_signal = Signal.signal [(0, 0), (RealTime.seconds (2^32), 2^32)]

is_id_warp :: Warp -> Bool
is_id_warp = (== id_warp)

warp_pos :: ScoreTime -> Warp -> RealTime
warp_pos pos warp@(Warp sig shift stretch)
    | is_id_warp warp = to_real pos
    | otherwise = Signal.y_to_real $
        Signal.at_linear (to_real (pos*stretch + shift)) sig

-- | Unlike 'warp_pos', 'unwarp_pos' can fail.  This asymmetry is because
-- at_linear will project a signal on forever, but inverse_at won't.
-- TODO this behaviour is important for playback, but maybe I could have
-- another projecting version to get rid of the Maybe here.  It would be
-- projecting at 1:1 instead of 0:1 which might be a little odd, but
-- convenient.
unwarp_pos :: RealTime -> Warp -> Maybe ScoreTime
unwarp_pos pos (Warp sig shift stretch) = case Signal.inverse_at pos sig of
    Nothing -> Nothing
    Just p -> Just $ (to_score p - shift) / stretch

-- | Compose two warps.  Warps with id signals are optimized.
compose_warps :: Warp -> Warp -> Warp
compose_warps
        warp1@(Warp sig1 shift1 stretch1) warp2@(Warp sig2 shift2 stretch2)
    | is_id_warp warp1 = warp2
    | is_id_warp warp2 = warp1
    | sig2 == id_warp_signal =
        Warp sig1 (shift1 + shift2 * stretch1) (stretch1 * stretch2)
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
    | otherwise = Signal.map_x
        ((`RealTime.div` factor) . subtract (to_real shift)) sig
    where factor = Types.score_to_double stretch

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

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as the
-- backend itself, but things at the Derive layer and above don't care about
-- all that.
newtype Instrument = Instrument String
    deriving (DeepSeq.NFData, Eq, Ord, Show, Read)
inst_name (Instrument s) = s

instance Pretty.Pretty Instrument where
    pretty inst = '>' : inst_name inst

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = String
newtype Attributes = Attributes (Set.Set Attribute)
    deriving (Monoid.Monoid, Eq, Ord, Show)

instance Pretty.Pretty Attributes where
    pretty attrs = "{" ++ Seq.join ", " (attrs_list attrs) ++ "}"

attrs_set :: Attributes -> Set.Set Attribute
attrs_set (Attributes attrs) = attrs

attrs_list :: Attributes -> [Attribute]
attrs_list = Set.toList . attrs_set

attrs_diff :: Attributes -> Attributes -> Attributes
attrs_diff (Attributes x) (Attributes y) = Attributes (Set.difference x y)

attrs_union :: Attributes -> Attributes -> Attributes
attrs_union (Attributes x) (Attributes y) = Attributes (Set.union x y)

attributes :: [String] -> Attributes
attributes = Attributes . Set.fromList

attr :: String -> Attributes
attr = Attributes . Set.singleton

no_attrs :: Attributes
no_attrs = Attributes Set.empty

newtype Control = Control String deriving (Eq, Ord, Show, DeepSeq.NFData)

-- ** controls

-- | Converted into velocity or breath depending on the instrument.
c_pressure :: Control
c_pressure = Control "p"

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

-- * util

to_real = RealTime.score
to_score = RealTime.to_score

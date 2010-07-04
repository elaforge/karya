{- | This has the basic data structures for the deriver level.

    The events here are generated from UI Events, and will eventually be
    transformed into Perform Events, which are specific to the performance
    backend.
-}
module Derive.Score where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Ui
import qualified Ui.Types as Types

import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal
import qualified Perform.Warning as Warning


-- * Event

data Event = Event {
    -- | These are the core attributes that define an event.  UI display
    -- attributes like font style are not preserved here.
    event_start :: RealTime
    , event_duration :: RealTime
    -- | The UI level keeps it in UTF8 for easy communication with fltk, but
    -- haskell will always need to decode it, so I might as well do it here.
    , event_text :: Text.Text
    , event_controls :: ControlMap
    , event_pitch :: PitchSignal.PitchSignal

    -- | Keep track of this event's display in various tracks (it may appear
    -- in more than one if it appears in a merged track).  That way, if an
    -- error or warning is emitted concerning this event, its position on the
    -- UI can be highlighted.
    , event_stack :: Warning.Stack

    -- | These are optional parameters that may or may not be required by the
    -- performer.
    , event_instrument :: Maybe Instrument
    , event_attributes :: Attributes
    } deriving (Eq, Show)

type ControlMap = Map.Map Control Signal.Control

event_string :: Event -> String
event_string = Text.unpack . event_text

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

-- ** modify events

-- These operate directly on events, so we are in RealTime at this point.

move :: (RealTime -> RealTime) -> Event -> Event
move f event =
    move_controls (pos - event_start event) $ event { event_start = pos }
    where pos = f (event_start event)

place :: RealTime -> RealTime -> Event -> Event
place start dur event = (move (const start) event) { event_duration = dur }

-- *** control

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
-- efficient since only the shift and stretch are changed.  They have to be
-- flattened out when the warp is composed though (in 'd_warp').
data Warp = Warp {
    warp_signal :: Signal.Warp
    , warp_shift :: ScoreTime
    , warp_stretch :: ScoreTime
    } deriving (Eq, Show)

id_warp :: Warp
id_warp = signal_to_warp id_warp_signal

is_id_warp :: Warp -> Bool
is_id_warp = (== id_warp)

id_warp_signal :: Signal.Warp
id_warp_signal = Signal.signal [(0, 0), (Signal.max_x, Signal.max_y)]

stretch_warp :: ScoreTime -> Warp -> Warp
stretch_warp factor warp = warp { warp_stretch = warp_stretch warp * factor }

shift_warp :: ScoreTime -> Warp -> Warp
shift_warp shift warp =
    warp { warp_shift = warp_shift warp + warp_stretch warp * shift }

warp_pos :: ScoreTime -> Warp -> RealTime
warp_pos pos warp@(Warp sig shift stretch)
    | is_id_warp warp = to_real pos
    | otherwise = Signal.y_to_real $
        Signal.at_linear (to_real (pos * stretch + shift)) sig

-- | Unlike 'warp_pos', 'unwarp_pos' can fail.  This asymmetry is because
-- at_linear will project a signal on forever, but inverse_at won't.
-- TODO this behaviour is important for playback, but maybe I could have
-- another projecting version to get rid of the Maybe here.  It would be
-- projecting at 1:1 instead of 0:1 which might be a little odd, but
-- convenient.
unwarp_pos :: RealTime -> Warp -> Maybe ScoreTime
unwarp_pos pos (Warp sig shift stretch) = case Signal.inverse_at pos sig of
    Nothing -> Nothing
    Just p -> Just $ (Types.real_to_score p - shift) / stretch

-- | Warp a Warp with a warp signal.
compose_warp :: Warp -> Signal.Warp -> Warp
compose_warp warp sig
    | is_id_warp warp = signal_to_warp sig
    | otherwise = compose warp sig
    where
    -- From the nyquist warp function:
    -- > f(stretch * g(t) + shift)
    -- > f(scale(stretch, g) + offset)
    -- > (shift f -offset)(scale(stretch, g))
    -- > (compose (shift-time f (- offset)) (scale stretch g))
    compose (Warp f shift stretch) g = signal_to_warp $
        Signal.compose (Signal.shift (- (to_real shift)) f)
            (Signal.scale (Signal.x_to_y (to_real stretch)) g)

-- | Convert a Signal to a Warp.
signal_to_warp :: Signal.Warp -> Warp
signal_to_warp sig = Warp sig (ScoreTime 0) (ScoreTime 1)

-- TODO unused?  remove this?
warp_to_signal :: Warp -> Signal.Warp
warp_to_signal (Warp sig shift stretch) =
    Signal.map_x (subtract (to_real shift) . (* to_real stretch)) sig

-- | Warp a signal.
-- TODO this does the same thing as compose_warp, but Signal.compose doesn't
-- work correctly with unmatched sampling rates.  I get around it composing
-- warps with warps because they go through integrate, which enforces
-- a constant sampling rate, but this is a brittle hack and should go away.
warp_control :: Signal.Control -> Warp -> Signal.Control
warp_control control warp@(Warp sig shift stretch)
    | is_id_warp warp = control
        -- optimization
    | sig == id_warp_signal =
        Signal.map_x (\p -> (p + to_real shift) * to_real stretch) control
    | otherwise = Signal.map_x (\x -> warp_pos (to_score x) warp) control

-- TODO this should be merged with warp_control, I need to use SignalBase.map_x
warp_pitch :: PitchSignal.PitchSignal -> Warp -> PitchSignal.PitchSignal
warp_pitch psig warp@(Warp sig shift stretch)
    | is_id_warp warp = psig
        -- optimization
    | sig == id_warp_signal =
        PitchSignal.map_x (\p -> (p + to_real shift) * to_real stretch) psig
    | otherwise = PitchSignal.map_x (\x -> warp_pos (to_score x) warp) psig

-- * instrument

-- | An Instrument is identified by a plain string.  This will be looked up in
-- the instrument db to get the backend specific Instrument type as well as the
-- backend itself, but things at the Derive layer and above don't care about
-- all that.
newtype Instrument = Instrument String
    deriving (Eq, Ord, Show, Read)
inst_name (Instrument s) = s

-- | Instruments can have a set of attributes along with them.  These are
-- propagated dynamically down the derivation stack.  They function like
-- arguments to an instrument, and will typically select an articulation, or
-- a drum from a drumset, or something like that.
type Attribute = String
newtype Attributes = Attributes (Set.Set Attribute) deriving (Eq, Show)
attrs_set (Attributes attrs) = attrs
attrs_list = Set.toList . attrs_set

attributes :: [String] -> Attributes
attributes = Attributes . Set.fromList

no_attrs :: Attributes
no_attrs = Attributes Set.empty

newtype Control = Control String deriving (Eq, Ord, Show)

-- ** controls

-- Some standard controls.  The MIDI deriver should understand them.

-- TODO this could be converted into velocity or breath depending on the
-- instrument.
c_pressure = Control "pres"

c_velocity = Control "vel"
c_breath = Control "breath"

-- * attributes

-- ** articulations

pizz = "pizz"
trem = "trem"

-- ** dynamics

cresc = "cresc"
dim = "dim"
sfz = "sfz"

-- * util

to_real = Types.score_to_real
to_score = Types.real_to_score

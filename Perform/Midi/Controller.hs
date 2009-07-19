{- | Support for MIDI controllers.
-}
module Perform.Midi.Controller where
import Control.Monad
import qualified Data.Map as Map

import qualified Midi.Midi as Midi

import qualified Perform.Signal as Signal


-- TODO This means I can't map to aftertouch or something
-- so I have 3 levels: symbolic name, general midi controller, specific cc
type ControllerMap = Map.Map Controller Midi.Controller

controller_map :: [(Integer, String)] -> ControllerMap
controller_map cmap =
    Map.fromList [(Controller c, fromIntegral n) | (n, c) <- cmap]

empty_map = controller_map []

controller_map_names :: ControllerMap -> [String]
controller_map_names cmap = [name | Controller name <- Map.keys cmap]

-- | A controller is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controllers or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- This is the MIDI performer's version of Controller.  The more general
-- Score.Controller is converted along with Score.Events in
-- Perform.Midi.Convert.
newtype Controller = Controller String
    deriving (Eq, Ord, Show, Read)

-- | Pitchbend range in tempered semitones below and above unity.  The second
-- integer should be negative.
type PbRange = (Integer, Integer)

-- | Convert from a controller to a function that creates its MIDI message.
controller_constructor :: ControllerMap -> Controller
    -> (Maybe (Signal.Val -> Midi.ChannelMessage))
controller_constructor cmap cont = msum
    [ Map.lookup cont special_controllers
    , fmap make_midi_cc (Map.lookup cont cmap)
    ]

make_midi_cc :: Midi.Controller -> Signal.Val -> Midi.ChannelMessage
make_midi_cc cnum val = Midi.ControlChange cnum (val_to_cc val)

special_controllers = Map.fromList
    [ (c_channel_pressure, \val -> Midi.ChannelPressure (val_to_cc val))
    -- Don't include pitch becase it's handled separately.
    -- doing c_aftertouch is a bit trickier because it needs a note number
    ]

-- | True for controllers that must have a channel to themselves.
-- Midi controllers are a subset of what I consider controllers, since
-- I include all variable note parameters.
is_channel_controller :: Controller -> Bool
    -- Don't include c_pitch because that is checked for sharing separately.
is_channel_controller cont = cont `notElem` [c_velocity, c_aftertouch, c_pitch]

controller_range :: (Signal.Val, Signal.Val)
controller_range = (0, 1)

-- | Given a pitch val, return the midi note number and pitch bend amount to
-- sound at the pitch.
pitch_to_midi :: PbRange -> Signal.Val -> (Midi.Key, Midi.PitchBendValue)
pitch_to_midi pb_range val = (nn, pb_from_nn pb_range nn val)
    where nn = floor (max 1 (min 127 val))

pb_from_nn :: PbRange -> Midi.Key -> Signal.Val -> Midi.PitchBendValue
pb_from_nn pb_range nn val =
    realToFrac $ if bend >= 0 then bend / high else bend / (-low)
    where
    (low, high) = (fromIntegral (fst pb_range), fromIntegral (snd pb_range))
    bend = max low (min high (val - fromIntegral nn))

-- * built in controllers

-- ** special

c_velocity = Controller "velocity"
c_channel_pressure = Controller "channel_pressure"
c_aftertouch = Controller "aftertouch"

-- | The pitch controller is even more special, because it doesn't directly
-- correspond to any midi controller.  Instead it's decomposed into
-- (midi_nn, pb_val) by 'pitch_to_midi'.
c_pitch :: Controller
c_pitch = Controller "pitch"

-- ** ccs

default_controllers :: ControllerMap
default_controllers = controller_map
    [ (1, c_mod), (2, "breath"), (5, "port"), (7, "volume") ]

-- Pull one out as a symbol for tests.
c_mod = "mod"

-- * util

val_to_pb :: Signal.Val -> Int
val_to_pb val = floor ((val + 1) * 0x2000 - 0x2000)

val_to_cc :: Signal.Val -> Midi.ControlValue
val_to_cc val = floor (val * 0x7f)

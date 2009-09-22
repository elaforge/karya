{- | Support for MIDI controllers.
-}
module Perform.Midi.Controller where
import Control.Monad
import qualified Data.Map as Map
import qualified Util.Num as Num

import qualified Midi.Midi as Midi

import qualified Perform.Signal as Signal


-- TODO This means I can't map to aftertouch or something
-- so I have 3 levels: symbolic name, general midi controller, specific cc
type ControllerMap = Map.Map Controller Midi.Controller

controller_map :: [(Midi.Controller, String)] -> ControllerMap
controller_map cmap =
    Map.fromList [(Controller c, fromIntegral n) | (n, c) <- cmap]

empty_map :: ControllerMap
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
newtype Controller = Controller String deriving (Eq, Ord, Show, Read)

-- | Pitchbend range in tempered semitones below and above unity.  The first
-- integer should probably be negative.
type PbRange = (Integer, Integer)

-- | Convert from a controller to a function that creates its MIDI message.
controller_constructor :: ControllerMap -> Controller
    -> (Maybe (Signal.Val -> Midi.ChannelMessage))
controller_constructor cmap cont = msum
    [ Map.lookup cont special_controllers
    , fmap make_midi_cc (Map.lookup cont cmap)
    , fmap make_midi_cc (Map.lookup cont universal_controller_map)
    ]

make_midi_cc :: Midi.Controller -> Signal.Val -> Midi.ChannelMessage
make_midi_cc cnum val = Midi.ControlChange cnum (val_to_cc val)

special_controllers = Map.fromList
    [ (c_aftertouch, \val -> Midi.ChannelPressure (val_to_cc val))
    -- Don't include pitch becase it's handled separately.
    -- doing c_poly_aftertouch is a bit trickier because it needs a note number
    ]

-- | True for controllers that must have a channel to themselves.
-- Midi controllers are a subset of what I consider controllers, since
-- I include all variable note parameters.
is_channel_controller :: Controller -> Bool
    -- Don't include c_pitch because that is checked for sharing separately.
is_channel_controller cont =
    cont `notElem` [c_velocity, c_poly_aftertouch, c_pitch]

controller_range :: (Signal.Val, Signal.Val)
controller_range = (0, 1)

-- | Given a pitch val, return the midi note number and pitch bend amount to
-- sound at the pitch.
pitch_to_midi :: PbRange -> Signal.Val -> (Midi.Key, Midi.PitchBendValue)
pitch_to_midi pb_range val = (key, pb_from_nn pb_range key val)
    where key = floor (Num.clamp 0 127 val)

pb_from_nn :: PbRange -> Midi.Key -> Signal.Val -> Midi.PitchBendValue
pb_from_nn pb_range key val =
    realToFrac $ if bend >= 0 then bend / high else bend / (-low)
    where
    (low, high) = (fromIntegral (fst pb_range), fromIntegral (snd pb_range))
    bend = Num.clamp low high (val - fromIntegral key)

-- * built in controllers

-- ** non-cc controllers

-- | The pitch controller is even more special, because it doesn't directly
-- correspond to any midi controller.  Instead it's decomposed into
-- (midi_nn, pb_val) by 'pitch_to_midi'.
c_pitch, c_velocity :: Controller
c_pitch = Controller "pitch"
c_velocity = Controller "velocity"

-- | I call channel pressure \"aftertouch\" because true aftertouch is so rare.
-- All controllers here are per-note anyway.  Mainly I want to be able to reuse
-- "pressure" for breath / bow pressure.
c_poly_aftertouch, c_aftertouch :: Controller
c_poly_aftertouch = Controller "poly_aftertouch"
c_aftertouch = Controller "aftertouch"

-- ** cc controllers

-- | This map is used by both input and outpt.  On input, InputNote maps
-- a midi cc to a symbolic controller name, and on output, maps it back again.
-- Of course it may very well be mapped to and from a higher level controller
-- name but the defaults are always available.
--
-- On output, the \"standard\" set of symbolic names are understood, but the
-- low level cc## names work uniformly.
-- TODO an array would be a little faster
cc_map :: [(Midi.Controller, String)]
cc_map = [(n, "cc" ++ show n) | n <- [0..127]] ++
    [ (1, "modulation")
    , (2, "breath")
    , (4, "foot")
    , (7, "volume")
    , (8, "balance")
    , (10, "pan")
    , (64, "damper pedal")
    , (65, "portamento pedal")
    , (66, "sustenuto pedal")
    , (67, "soft pedal")
    ]

-- | This will also be checked by 'controller_constructor', so these are
-- controllers that every instrument will respond to.  Of course it may
-- override some of these names if it wishes.
universal_controller_map :: ControllerMap
universal_controller_map = controller_map cc_map

-- Pull one out as a symbol for tests.
c_mod = "modulation"

-- * util

val_to_pb :: Signal.Val -> Int
val_to_pb val = floor ((val + 1) * 0x2000 - 0x2000)

val_to_cc :: Signal.Val -> Midi.ControlValue
val_to_cc val = floor (val * 0x7f)

{-# OPTIONS_GHC -XDeriveDataTypeable #-}
{- | Support for MIDI controllers.
-}
module Perform.Midi.Controller where
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import qualified Util.Seq as Seq

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
    deriving (Eq, Ord, Show, Read, Generics.Data, Generics.Typeable)

-- | Pitchbend range in tempered semitones below and above unity.
type PbRange = (Integer, Integer)

-- | Convert from a controller to a function that creates its MIDI message.
controller_constructor :: ControllerMap -> Controller
    -> (Maybe (Signal.Val -> Midi.ChannelMessage))
controller_constructor cmap cont = Seq.first_just
    [ Map.lookup cont special_controllers
    , fmap make_midi_cc (Map.lookup cont cmap)
    ]

make_midi_cc :: Midi.Controller -> Signal.Val -> Midi.ChannelMessage
make_midi_cc cnum val = Midi.ControlChange cnum (val_to_cc val)

special_controllers = Map.fromList
    [ (c_pitch, \val -> Midi.PitchBend (val_to_pb val))
    , (c_channel_pressure, \val -> Midi.ChannelPressure (val_to_cc val))
    -- doing c_aftertouch is a bit trickier because it needs a note number
    ]

-- | True for controllers that must have a channel to themselves.
-- Midi controllers are a substet of what I consider controllers, since
-- I include all variable note parameters.
is_channel_controller :: Controller -> Bool
is_channel_controller cont = not (cont `elem` [c_velocity, c_aftertouch])

controller_range :: Controller -> (Signal.Val, Signal.Val)
controller_range cont
    | cont == c_pitch = (-1, 1)
    | otherwise = (0, 1)

-- TODO
cents_to_pb_val :: PbRange -> Int -> Int
cents_to_pb_val (low, high) cents = cents

-- * built in controllers

-- ** special

c_velocity = Controller "velocity"
c_pitch = Controller "pitch"
c_channel_pressure = Controller "channel_pressure"
c_aftertouch = Controller "aftertouch"

-- ** ccs

default_controllers :: ControllerMap
default_controllers = controller_map
    [ (1, "mod"), (2, "breath"), (5, "port time"), (7, "volume") ]


-- * util

val_to_pb :: Signal.Val -> Int
val_to_pb val = floor ((val + 1) * 0x2000 - 0x2000)

val_to_cc :: Signal.Val -> Midi.ControlValue
val_to_cc val = floor (val * 0x7f)

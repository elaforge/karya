{- | Support for MIDI controllers.
-}
module Perform.Midi.Controller where

import qualified Midi.Midi as Midi

import qualified Perform.Signal as Signal

import qualified Perform.Midi.Instrument as Instrument


-- | A controller is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controllers or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- This is the MIDI performer's version of Controller.  The more general
-- Score.Controller is converted along with Score.Events in
-- Perform.Midi.Convert.
data Controller = Controller String deriving (Show, Eq, Ord)
controller name
    | Controller name `elem` valid_controllers = Just (Controller name)
    | otherwise = Nothing

-- | Convert from a controller to a function that creates its MIDI message.
controller_constructor :: Controller
    -> (Maybe (Signal.Val -> Midi.ChannelMessage))
controller_constructor cont = lookup cont controller_constructors
controller_constructors =
    [ (c_pitch, \val -> Midi.PitchBend (val_to_pb val))
    , (c_channel_pressure, \val -> Midi.ChannelPressure (val_to_cc val))
    ] ++ [(cont, cc num) | (num, cont) <- channel_controllers]

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
cents_to_pb_val :: Instrument.PbRange -> Int -> Int
cents_to_pb_val (low, high) cents = cents

-- * hard coded controllers

valid_controllers =
    [ c_velocity, c_pitch, c_channel_pressure, c_aftertouch
    , c_breath, c_volume
    ] -- TODO some way to include generic cc with number

-- ** special
c_velocity = Controller "velocity"
c_pitch = Controller "pitch"
c_channel_pressure = Controller "channel_pressure"
c_aftertouch = Controller "aftertouch"

-- ** ccs
channel_controllers =
    [ (2, c_breath)
    , (7, c_volume)
    ]

c_breath = Controller "breath"
c_volume = Controller "volume"


-- * util

val_to_pb :: Signal.Val -> Int
val_to_pb val = floor ((val + 1) * 0x2000 - 0x2000)

val_to_cc :: Signal.Val -> Midi.ControlValue
val_to_cc val = floor (val * 0x7f)

cc :: Midi.Controller -> Signal.Val -> Midi.ChannelMessage
cc cnum val = Midi.ControlChange cnum (val_to_cc val)

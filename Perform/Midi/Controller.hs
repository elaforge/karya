{- | Support for MIDI controllers.

Eventually, this will need to be merged in some way with the more general
notion of control signals, which can affect things other than midi rendering.
-}
module Perform.Midi.Controller where

import qualified Midi.Midi as Midi

import qualified Perform.Signal as Signal

import qualified Perform.Midi.Instrument as Instrument


-- | A controller is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controllers or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
data Controller = Controller String deriving (Show, Eq, Ord)

controller_constructor :: Controller
    -> (Maybe (Signal.Val -> Midi.ChannelMessage))
controller_constructor cont = lookup cont controller_constructors
controller_constructors =
    [ (c_pitch, \val -> Midi.PitchBend (pb_normalize val))
    , (c_channel_pressure, \val -> Midi.ChannelPressure (cc_normalize val))
    ] ++ [(cont, cc num) | (num, cont) <- channel_controllers]

-- | True for controllers that must have a channel to themselves.
-- Midi controllers are a substet of what I consider controllers, since
-- I include all variable note parameters.
is_channel_controller :: Controller -> Bool
is_channel_controller cont = not (cont `elem` [c_velocity, c_aftertouch])

cents_to_pb_val :: Instrument.PbRange -> Int -> Int
cents_to_pb_val (low, high) cents = cents

-- * hard coded controllers

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

pb_normalize :: Signal.Val -> Int
pb_normalize val = floor ((val + 1) * 0x2000 - 0x2000)

cc_normalize :: Signal.Val -> Midi.ControlValue
cc_normalize val = floor (val * 0x7f)

cc :: Midi.Controller -> Signal.Val -> Midi.ChannelMessage
cc cnum val = Midi.ControlChange cnum (cc_normalize val)

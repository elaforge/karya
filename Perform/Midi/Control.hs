{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- NFData instance
{- | Support for MIDI controls.
-}
module Perform.Midi.Control where
import Control.DeepSeq
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Signal as Signal


-- TODO This means I can't map to aftertouch or something
-- so I have 3 levels: symbolic name, general midi control, specific cc
type ControlMap = Map.Map Control Midi.Control

control_map :: [(Midi.Control, String)] -> ControlMap
control_map cmap = Map.fromList [(Control c, fromIntegral n) | (n, c) <- cmap]

empty_map :: ControlMap
empty_map = control_map []

control_map_names :: ControlMap -> [String]
control_map_names cmap = [name | Control name <- Map.keys cmap]

-- | A control is an abstract parameter that influences derivation.  Some of
-- them affect performance and will be rendered as MIDI controls or note
-- parameters or whatever, while others may affect derivation (e.g. tempo) and
-- won't be seen by the backend at all.
--
-- This is the MIDI performer's version of Control.  The more general
-- Score.Control is converted along with Score.Events in
-- Perform.Midi.Convert.
newtype Control = Control String deriving (Eq, Ord, Show, Read, NFData)

instance Pretty.Pretty Control where
    pretty (Control s) = '%' : s

-- | Pitchbend range in tempered semitones below and above unity.  The first
-- integer should probably be negative.
type PbRange = (Int, Int)

-- | Convert from a control to a function that creates its MIDI message.
control_constructor :: ControlMap -> Control
    -> Maybe (Signal.Y -> Midi.ChannelMessage)
control_constructor cmap cont = msum
    [ Map.lookup cont special_controls
    , fmap make_midi_cc (Map.lookup cont cmap)
    , fmap make_midi_cc (Map.lookup cont universal_control_map)
    ]

-- | True if the given control will be used by the MIDI performer.
-- Takes a Score.Control because being a MIDI control is a precondition for
-- conversion into 'Control'.
is_midi_control :: ControlMap -> Score.Control -> Bool
is_midi_control cmap control = cont == c_velocity
    || Map.member cont special_controls
    || Map.member cont universal_control_map
    || Map.member cont cmap
    where cont = convert_control control

make_midi_cc :: Midi.Control -> Signal.Y -> Midi.ChannelMessage
make_midi_cc cnum val = Midi.ControlChange cnum (val_to_cc val)

special_controls :: Map.Map Control (Signal.Y -> Midi.ChannelMessage)
special_controls = Map.fromList
    [ (c_aftertouch, Midi.ChannelPressure . val_to_cc)
    -- Don't include pitch becase it's handled separately.
    -- doing c_poly_aftertouch is a bit trickier because it needs a note number
    ]

-- | True for controls that must have a channel to themselves.
-- Midi controls are a subset of what I consider controls, since
-- I include all variable note parameters.
is_channel_control :: Control -> Bool
    -- Don't include c_pitch because that is checked for sharing separately.
is_channel_control cont = cont `notElem` [c_velocity, c_poly_aftertouch]

control_range :: (Signal.Y, Signal.Y)
control_range = (0, 1)

-- | Given a pitch val, return the midi note number and pitch bend amount to
-- sound at the pitch.
pitch_to_midi :: PbRange -> Signal.Y -> Maybe (Midi.Key, Midi.PitchBendValue)
pitch_to_midi pb_range val
    -- A NoteOn at 0 is actually a NoteOff.  In addition, signals default to
    -- 0 so 0 here probably means the pitch signal was empty.
    | val <= 0 || val > 127 = Nothing
    | otherwise = Just (key, pb_from_nn pb_range key val)
    where key = Midi.to_key (floor val)

pb_from_nn :: PbRange -> Midi.Key -> Signal.Y -> Midi.PitchBendValue
pb_from_nn pb_range key val
    | bend == 0 = 0
    | bend > 0 = Num.d2f $ bend / high
    | otherwise = Num.d2f $ bend / (-low)
    where
    (low, high) = (fromIntegral (fst pb_range), fromIntegral (snd pb_range))
    bend = Num.clamp low high (val - Midi.from_key key)

-- * built in controls

convert_control :: Score.Control -> Control
convert_control (Score.Control c) = Control c

-- ** non-cc controls

c_velocity :: Control
c_velocity = convert_control Score.c_velocity

-- | I call channel pressure \"aftertouch\" because true aftertouch is so rare.
-- All controls here are per-note anyway.  Mainly I want to be able to reuse
-- "pressure" for breath / bow pressure.
c_poly_aftertouch, c_aftertouch :: Control
c_poly_aftertouch = Control "poly_aftertouch"
c_aftertouch = Control "aftertouch"

-- ** cc controls

-- | This map is used by both input and outpt.  On input, InputNote maps
-- a midi cc to a symbolic control name, and on output, maps it back again.
-- Of course it may very well be mapped to and from a higher level control
-- name but the defaults are always available.
--
-- On output, the \"standard\" set of symbolic names are understood, but the
-- low level cc## names work uniformly.
-- TODO an array would be a little faster
cc_map :: [(Midi.Control, String)]
cc_map = [(n, "cc" ++ show n) | n <- [0..127]] ++
    [ (1, "mod")
    , (2, "breath")
    , (4, "foot")
    , (7, "volume")
    , (8, "balance")
    , (10, "pan")
    , (64, "damper-pedal")
    , (65, "portamento-pedal")
    , (66, "sostenuto-pedal")
    , (67, "soft-pedal")
    ]

-- | This will also be checked by 'control_constructor', so these are
-- controls that every instrument will respond to.  Of course it may
-- override some of these names if it wishes.
universal_control_map :: ControlMap
universal_control_map = control_map cc_map

c_breath :: Control
c_breath = Control "breath"

-- Pull one out as a symbol for tests.
c_mod :: String
c_mod = "mod"

-- * util

val_to_pb :: Signal.Y -> Int
val_to_pb val = round $ (Num.clamp (-1) 1 val + 1) * 0x2000 - 0x2000

val_to_cc :: Signal.Y -> Midi.ControlValue
val_to_cc val = round $ Num.clamp 0 1 val * 0x7f

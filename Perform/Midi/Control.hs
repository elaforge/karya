-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Support for MIDI controls.
module Perform.Midi.Control where
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

import qualified Util.Num as Num
import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import Global


type ControlMap = Map Score.Control Midi.Control

control_map :: [(Midi.Control, Score.Control)] -> ControlMap
control_map = Map.fromList . map Tuple.swap

empty_map :: ControlMap
empty_map = control_map []

-- | Pitchbend range in tempered semitones below and above unity.  The first
-- integer should probably be negative.
type PbRange = (Int, Int)

-- | Convert from a control to a function that creates its MIDI message.
control_constructor :: ControlMap -> Score.Control -> Midi.Key
    -> Maybe (Signal.Y -> Midi.ChannelMessage)
control_constructor cmap cont key
    | Just cc <- Map.lookup cont cmap =
        Just $ Midi.ControlChange cc . val_to_cc
    | Just cc <- Map.lookup cont universal_control_map =
        Just $ Midi.ControlChange cc . val_to_cc
    | cont == Controls.pressure = Just $ Midi.ChannelPressure . val_to_cc
    | cont == Controls.aftertouch = Just $ Midi.Aftertouch key . val_to_cc
    | otherwise = Nothing

-- | True if the given control will be used by the MIDI performer.
-- Takes a Score.Control because being a MIDI control is a precondition for
-- conversion into 'Control'.
is_midi_control :: ControlMap -> Score.Control -> Bool
is_midi_control cmap control =
    Map.member control universal_control_map
    || Map.member control cmap
    || control == Controls.pressure || control == Controls.aftertouch

-- | True for controls that must have a channel to themselves.  Midi controls
-- are a subset of what I consider controls, since I include all variable note
-- parameters.
is_channel_control :: Score.Control -> Bool
is_channel_control = (/= Controls.aftertouch)

-- | Given a NoteNumber, return the midi note number and pitch bend amount to
-- sound at the pitch.
pitch_to_midi :: PbRange -> Pitch.NoteNumber
    -> Maybe (Midi.Key, Midi.PitchBendValue)
pitch_to_midi pb_range nn
    -- Signals default to 0 so 0 probably means the pitch signal was empty.
    | nn <= 0 || nn > 127 = Nothing
    -- Due to floating point imprecision, I can end up with nns that are
    -- supposed be integral, but are sightly off.  So if the difference is
    -- below the level of perception, just round to 0.
    | abs (fromIntegral (round nn) - nn) < 0.005 =
        Just (Midi.to_key (round nn), 0)
    | otherwise = Just (key, pb_from_nn pb_range key nn)
    where key = Midi.to_key (floor nn)

pb_from_nn :: PbRange -> Midi.Key -> Pitch.NoteNumber -> Midi.PitchBendValue
pb_from_nn pb_range key (Pitch.NoteNumber nn)
    | bend > 0 = if high > 0 then Num.d2f $ bend / high else 0
    | otherwise = if low < 0 then Num.d2f $ bend / (-low) else 0
    where
    (low, high) = bimap fromIntegral fromIntegral pb_range
    bend = Num.clamp low high (nn - Midi.from_key key)

-- ** cc controls

-- | This map is used by both input and outpt.  On input, InputNote maps
-- a midi cc to a symbolic control name, and on output, maps it back again.
-- Of course it may very well be mapped to and from a higher level control
-- name but the defaults are always available.
--
-- On output, the \"standard\" set of symbolic names are understood, but the
-- low level cc## names work uniformly.
--
-- This will also be checked by 'control_constructor', so these are controls
-- that every instrument will respond to.  Of course it may override some of
-- these names if it wishes.
universal_control_map :: ControlMap
universal_control_map = control_map $
    [(n, Score.unchecked_control $ "cc" <> showt n) | n <- [0..127]] ++
    [ (1, "mod")
    , (2, "breath")
    , (4, "foot")
    , (7, "vol")
    , (8, "balance")
    , (10, "pan")
    , (64, "pedal")
    , (65, "porta-pedal")
    , (66, "sost-pedal")
    , (67, "soft-pedal")
    ]

-- * util

val_to_pb :: Signal.Y -> Int
val_to_pb val = round $ (Num.clamp (-1) 1 val + 1) * 0x2000 - 0x2000

val_to_cc :: Signal.Y -> Midi.ControlValue
val_to_cc val = round $ Num.clamp 0 1 val * 0x7f

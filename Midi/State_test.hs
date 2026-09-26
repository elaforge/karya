-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.State_test where
import qualified Data.Map as Map
import Util.Test
import qualified Midi.Midi as Midi
import qualified Midi.State as State


test_play :: Test
test_play = do
    let f msgs = State.play msgs State.empty
        f2 = f . map chanMsg
    equal (f2 [Midi.NoteOn 10 20]) (mkstate [(10, 20)] 0 [])
    equal (f2 [Midi.PitchBend 0.25]) (mkstate [] 0.25 [])
    equal (f2 [Midi.NoteOn 10 20, Midi.PitchBend 0.25])
        (mkstate [(10, 20)] 0.25 [])
    equal (f2 [Midi.ControlChange 1 2])
        (mkstate [] 0 [(State.CC 1, 2)])
    equal (f2 [Midi.ChannelPressure 42])
        (mkstate [] 0 [(State.Pressure, 42)])

test_diff :: Test
test_diff = do
    let st1 = State.play
            [chanMsg (Midi.NoteOn 10 20), chanMsg (Midi.ControlChange 42 32),
                chanMsg (Midi.PitchBend 0.25)]
            State.empty
    let f msgs = State.diff st1 (State.play msgs st1)
    equal (f (map chanMsg [Midi.PitchBend 0]))
        (map chanMsg [Midi.PitchBend 0])
    equal (f (map chanMsg [Midi.PitchBend 0.25]))
        []
    equal (f (map chanMsg [Midi.NoteOn 12 34]))
        (map chanMsg [Midi.NoteOn 12 34])
    equal (f (map chanMsg [Midi.NoteOn 10 20]))
        []
    equal (f (map chanMsg [Midi.NoteOn 10 0]))
        (map chanMsg [Midi.NoteOff 10 0])
    equal (f [(dev, Midi.ChannelMessage 1 (Midi.NoteOn 1 2))])
        [(dev, Midi.ChannelMessage 1 (Midi.NoteOn 1 2))]

dev :: Midi.WriteDevice
dev = Midi.writeDevice "dev"

chanMsg msg = (dev, Midi.ChannelMessage 0 msg)

mkstate notes pb controls = State.State $ Map.fromList
    [((dev, 0), State.Channel (Map.fromList notes) pb (Map.fromList controls))]

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.InputNote_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Derive.Controls as Controls
import qualified Perform.Pitch as Pitch


test_from_midi :: Test
test_from_midi = do
    let state = InputNote.ReadDeviceState $ Map.fromList
            [(rdev, InputNote.empty_control_state (-1, 2))]
        rdev = Midi.read_device "rdev"
        chan = Midi.ChannelMessage 0
        nid = InputNote.NoteId
        input oct pc = Pitch.Input Pitch.PianoKbd (CmdTest.pitch oct pc 0) 0
    let f st = InputNote.from_midi st rdev
        thread st (msg:msgs) = case f st msg of
            Just (input, state) -> Just input : thread state msgs
            Nothing -> Nothing : thread state msgs
        thread _ [] = []

    equal (f state (Midi.RealtimeMessage Midi.Start)) Nothing
    let msgs =
            [ Midi.NoteOn Key.c4 127
            , Midi.PitchBend 1, Midi.PitchBend (-1)
            , Midi.NoteOn Key.ds4 127
            , Midi.NoteOff Key.c4 0
            ]
    equal (thread state (map chan msgs))
        [ Just $ InputNote.NoteOn (nid 60) (input 4 0) 1
        -- picks up previous note id, and range works
        , Just $ InputNote.PitchChange (nid 60) (input 4 1)
        , Just $ InputNote.PitchChange (nid 60) (input 3 6)
        -- remembers the previous pb, so ds4 becomes d4
        , Just $ InputNote.NoteOn (nid 63) (input 4 1) 1
        , Just $ InputNote.NoteOff (nid 60) 0
        ]

    -- Just controls, with no note.
    let msgs = [Midi.PitchBend 1, Midi.Aftertouch 10 127,
            Midi.ChannelPressure 127, Midi.ControlChange 1 127]
    equal (thread state (map chan msgs))
        [ Nothing
        , Just (InputNote.Control (nid 10) Controls.aftertouch 1)
        -- note id not remembered after aftertouch
        , Nothing, Nothing
        ]

    -- Give those controls a note.
    equal (thread state (map chan (Midi.NoteOn Key.c4 127 : msgs)))
        [ Just (InputNote.NoteOn (nid 60) (input 4 0) 1)
        , Just (InputNote.PitchChange (nid 60) (input 4 1))
        , Just (InputNote.Control (nid 10) Controls.aftertouch 1)
        , Just (InputNote.Control (nid 60) Controls.pressure 1)
        , Just (InputNote.Control (nid 60) Controls.mod 1)
        ]

test_to_midi :: Test
test_to_midi = do
    let id_to_key ns = Map.fromList
            [(InputNote.NoteId nid, key) | (nid, key) <- ns]
    let f = InputNote.to_midi (-1, 1) Map.empty
        note_on note_id nn = InputNote.NoteOn (InputNote.NoteId note_id) nn 1
        pb = Midi.PitchBend

    let n = Midi.NoteOn 64 127
    equal (f (note_on 60 64)) ([pb 0, n], id_to_key [(60, 64)])
    equal (f (note_on 64 64.5))
        ([Midi.PitchBend 0.5, n], id_to_key [(64, 64)])
    equal (f (note_on 64 64.5)) ([pb 0.5, n], id_to_key [(64, 64)])
    -- NoteOffs remove from the state.
    equal (InputNote.to_midi (-1, 1) (id_to_key [(60, 70)])
            (CmdTest.note_off 60))
        ([Midi.NoteOff 70 127], Map.empty)
    equal (f (CmdTest.control 64 "cc1" 1))
        ([Midi.ControlChange 1 127], Map.empty)
    -- Unrecognized cc.
    equal (f (CmdTest.control 64 "blahblah" 1)) ([], Map.empty)

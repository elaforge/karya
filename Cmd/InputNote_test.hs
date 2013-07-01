-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.InputNote_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Midi as Midi
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Derive.Controls as Controls
import qualified Perform.Pitch as Pitch


test_from_midi = do
    let state = InputNote.empty_state (-2, 2)
        rdev = Midi.read_device "rdev"
        chan = Midi.ChannelMessage 0
        nid = InputNote.NoteId
        input = Pitch.InputKey
    let f st = InputNote.from_midi st rdev
        thread st (msg:msgs) = case f st msg of
            Just (input, state) -> Just input : thread state msgs
            Nothing -> Nothing : thread state msgs
        thread _ [] = []

    equal (f state (Midi.RealtimeMessage Midi.Start)) Nothing
    let msgs = [Midi.NoteOn 10 127, Midi.PitchBend 1, Midi.PitchBend (-1),
            Midi.NoteOn 20 127, Midi.NoteOff 10 0]
    equal (thread state (map chan msgs))
        [ Just (InputNote.NoteOn (nid 10) (input 10) 1)
        -- picks up previous note id, and range works
        , Just (InputNote.PitchChange (nid 10) (input 12))
        , Just (InputNote.PitchChange (nid 10) (input 8))
        -- picks up previous pb
        , Just (InputNote.NoteOn (nid 20) (input 18) 1)
        , Just (InputNote.NoteOff (nid 10) 0)
        ]

    let msgs = [Midi.PitchBend 1, Midi.Aftertouch 10 127,
            Midi.ChannelPressure 127, Midi.ControlChange 1 127]
    equal (thread state (map chan msgs))
        [ Nothing
        , Just (InputNote.Control (nid 10) Controls.aftertouch 1)
        -- note id not remembered after aftertouch
        , Nothing, Nothing
        ]

    equal (thread state (map chan (Midi.NoteOn 20 127 : msgs)))
        [ Just (InputNote.NoteOn (nid 20) (input 20) 1)
        , Just (InputNote.PitchChange (nid 20) (input 22))
        , Just (InputNote.Control (nid 10) Controls.aftertouch 1)
        , Just (InputNote.Control (nid 20) Controls.pressure 1)
        , Just (InputNote.Control (nid 20) Controls.mod 1)
        ]

test_to_midi = do
    let id_to_key ns = Map.fromList
            [(InputNote.NoteId nid, key) | (nid, key) <- ns]
    let f pb = InputNote.to_midi (-1, 1) pb Map.empty

    let n = Midi.NoteOn 64 127
    equal (f 0 (CmdTest.note_on 60 64 127)) ([n], id_to_key [(60, 64)])
    equal (f 0 (CmdTest.note_on 64 64.5 127))
        ([Midi.PitchBend 0.5, n], id_to_key [(64, 64)])
    equal (f 0.5 (CmdTest.note_on 64 64.5 127)) ([n], id_to_key [(64, 64)])
    equal (f 0 (CmdTest.pitch 64 64)) ([], Map.empty)
    equal (InputNote.to_midi (-1, 1) 0 (id_to_key [(60, 70)])
            (CmdTest.note_off 60 127))
        ([Midi.NoteOff 70 127], Map.empty)
    equal (f 0 (CmdTest.control 64 "cc1" 127))
        ([Midi.ControlChange 1 127], Map.empty)
    equal (f 0 (CmdTest.control 64 "blahblah" 127)) ([], Map.empty)

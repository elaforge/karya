-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.CUtil_test where
import Util.Control
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))

import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Msg as Msg

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified App.MidiInst as MidiInst
import Types


test_insert_call = do
    let note_on key = Midi.ChannelMessage 0 (Midi.NoteOn key 127)
        note_off key = Midi.ChannelMessage 0 (Midi.NoteOff key 127)
        empty_tracks = [(">synth/1", [])]
    -- NoEdit means midi but no note.
    io_equal (insert_call empty_tracks 1 False (CmdTest.key_down 'a'))
        (Right empty_tracks, [note_on Key.c2])
    io_equal (insert_call empty_tracks 1 True (CmdTest.key_down 'a'))
        (Right [(">synth/1", [(0, 0, "anote")])], [note_on Key.c2])
    io_equal (insert_call empty_tracks 1 True (CmdTest.key_up 'a'))
        (Right [(">synth/1", [])], [note_off Key.c2])

insert_call :: [UiTest.TrackSpec] -> TrackNum -> Bool -> Msg.Msg
    -> IO (Either String [UiTest.TrackSpec], [Midi.Message])
insert_call tracks tracknum val_edit msg =
    fmap extract $ CmdTest.run_perf ustate cstate $ do
        Cmd.modify_edit_state (set_edit_mode val_edit)
        CmdTest.set_sel tracknum 0 tracknum 0
        CUtil.insert_call char_to_call msg
    where
    (ustate, cstate) = CmdTest.set_synths (make_synth note_keys)
        ["synth/1"] (CmdTest.make_tracks tracks)
        CmdTest.default_cmd_state
    char_to_call = CUtil.notes_to_calls (map fst note_keys)
    note_keys =
        [ (Drums.Note "anote" (Score.attr "a") 'a' 1, Key.c2)
        , (Drums.Note "bnote" (Score.attr "b") 'b' 1, Key.d2)
        ]
    extract r = (CmdTest.trace_logs (CmdTest.e_tracks r), CmdTest.e_midi r)

set_edit_mode :: Bool -> Cmd.EditState -> Cmd.EditState
set_edit_mode val_edit state = state
    { Cmd.state_edit_mode = if val_edit then Cmd.ValEdit else Cmd.NoEdit
    , Cmd.state_kbd_entry = True
    }

test_drum_instrument = do
    let run = DeriveTest.derive_tracks_with (DeriveTest.with_inst_db drum_synth)
        extract = DeriveTest.extract $ \e -> DeriveTest.e_attributes e
    let result = run [(">synth/x", [(0, 0, "bd"), (1, 0, "sn")])]
    equal (extract result) (["+bd", "+snare"], [])

    let (_, midi, logs) = DeriveTest.perform_inst drum_synth [("synth/x", [0])]
            (Derive.r_events result)
    equal logs []
    let e_midi = mapMaybeSnd Midi.channel_message . filter (Midi.is_note . snd)
            . DeriveTest.extract_midi
    equal (e_midi midi)
        [ (0, NoteOn Key.c2 127), (10, NoteOff Key.c2 127)
        , (1000, NoteOn Key.d2 127), (1010, NoteOff Key.d2 127)
        ]

-- I'll bet lenses could do this.
mapMaybeSnd :: (b -> Maybe c) -> [(a, b)] -> [(a, c)]
mapMaybeSnd f xs = [(a, b) | (a, Just b) <- map (second f) xs]

synth :: MidiInst.Softsynth
synth = MidiInst.softsynth "synth" "Synth" (-24, 24) []

drum_synth :: [MidiInst.SynthDesc]
drum_synth = make_synth [(Drums.c_bd, Key.c2), (Drums.c_sn, Key.d2)]

make_synth :: [(Drums.Note, Midi.Key)] -> [MidiInst.SynthDesc]
make_synth note_keys = MidiInst.make $ synth
    { MidiInst.modify_wildcard = CUtil.drum_patch note_keys
    , MidiInst.code =
        MidiInst.note_generators (CUtil.drum_calls (map fst note_keys))
        <> MidiInst.cmd (CUtil.drum_cmd (map fst note_keys))
    }

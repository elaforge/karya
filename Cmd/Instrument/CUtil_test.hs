-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.CUtil_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))

import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Msg as Msg

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import Global
import Types


test_insert_call = do
    let note_on key = Midi.ChannelMessage 0 (Midi.NoteOn key 127)
        note_off key = Midi.ChannelMessage 0 (Midi.NoteOff key 127)
        empty_tracks = [(">i1", [])]
    -- NoEdit means midi but no note.
    io_equal (insert_call empty_tracks 1 False (CmdTest.key_down 'a'))
        (Right empty_tracks, [note_on Key.c2])
    io_equal (insert_call empty_tracks 1 True (CmdTest.key_down 'a'))
        (Right [(">i1", [(0, 0, "anote")])], [note_on Key.c2])
    io_equal (insert_call empty_tracks 1 True (CmdTest.key_up 'a'))
        (Right [(">i1", [])], [note_off Key.c2])

insert_call :: [UiTest.TrackSpec] -> TrackNum -> Bool -> Msg.Msg
    -> IO (Either String [UiTest.TrackSpec], [Midi.Message])
insert_call tracks tracknum val_edit msg =
    fmap extract $ CmdTest.run_perf ustate cstate $ do
        Cmd.modify_edit_state (set_edit_mode val_edit)
        CmdTest.set_sel tracknum 0 tracknum 0
        CUtil.insert_call char_to_call msg
    where
    (ustate, cstate) = CmdTest.set_synths [make_synth note_keys]
        (UiTest.allocations [("i1", "synth/1")]) (CmdTest.make_tracks tracks)
        CmdTest.default_cmd_state
    char_to_call = CUtil.notes_to_calls (map fst note_keys)
    note_keys =
        [ (Drums.note 'a' "anote" (Attrs.attr "a"), Key.c2)
        , (Drums.note 'b' "bnote" (Attrs.attr "b"), Key.d2)
        ]
    extract r = (CmdTest.trace_logs (CmdTest.e_tracks r), CmdTest.e_midi r)

set_edit_mode :: Bool -> Cmd.EditState -> Cmd.EditState
set_edit_mode val_edit state = state
    { Cmd.state_edit_mode = if val_edit then Cmd.ValEdit else Cmd.NoEdit
    , Cmd.state_kbd_entry = True
    }

test_drum_instrument = do
    let run = DeriveTest.derive_tracks_setup
            (DeriveTest.with_synths allocs [drum_synth]) ""
        allocs = UiTest.allocations [("x", "synth/1")]
        extract = DeriveTest.extract DeriveTest.e_attributes
    let perform = DeriveTest.perform_synths allocs [drum_synth]
            . Derive.r_events
    let result = run [(">x", [(0, 0, "bd"), (1, 0, "sn")])]
    equal (extract result) (["+bd", "+snare"], [])

    let (_, midi, logs) = perform result
    equal logs []
    let e_midi = Seq.map_maybe_snd Midi.channel_message
            . filter (Midi.is_note . snd) . DeriveTest.extract_midi
    equal (e_midi midi)
        [ (0, NoteOn Key.c2 127), (20, NoteOff Key.c2 127)
        , (1000, NoteOn Key.d2 127), (1020, NoteOff Key.d2 127)
        ]

    -- Ensure the generated calls invert.
    equal (DeriveTest.extract Score.initial_dynamic $
            run [(">x", [(0, 0, "bd")]), ("dyn", [(0, 0, ".5")])])
        ([0.5], [])

test_make_cc_keymap = do
    let f = CUtil.make_cc_keymap
        cw = Patch.ControlSwitch
    equal (f 0 12 Key.c4
        [[Attrs.left, Attrs.right], [Attrs.open], [Attrs.low, Attrs.high]]) $
        Map.fromList
            [ (Attrs.left, ([cw 102 0], Key.c_1, Key.b_1, Key.c4))
            , (Attrs.right, ([cw 102 1], Key.c_1, Key.b_1, Key.c4))
            , (Attrs.open, ([], Key.c0, Key.b0, Key.c4))
            , (Attrs.low, ([cw 103 0], Key.c1, Key.b1, Key.c4))
            , (Attrs.high, ([cw 103 1], Key.c1, Key.b1, Key.c4))
            ]

drum_synth :: MidiInst.Synth
drum_synth = make_synth [(Drums.c_bd, Key.c2), (Drums.c_sn, Key.d2)]

make_synth :: [(Drums.Note, Midi.Key)] -> MidiInst.Synth
make_synth note_keys = DeriveTest.make_synth "synth" [patch]
    where
    patch = MidiInst.code #= code $
        CUtil.drum_patch note_keys $ MidiInst.named_patch (-24, 24) "1" []
    code =
        MidiInst.note_generators (CUtil.drum_calls Nothing (map fst note_keys))
        <> MidiInst.cmd (CUtil.drum_cmd (map fst note_keys))

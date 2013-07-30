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

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified App.MidiInst as MidiInst


test_keymaps = do
    -- 'CUtil.keymaps' sets the inst to Nothing, which means to use the current
    -- one.  But since in the test there's no valid performance, that winds up
    -- being nothing.
    let f = CUtil.inst_keymaps [('a', "anote", 1, inst), ('b', "bnote", 2, inst)]
        inst = Just (Score.Instrument "s/1")
        empty = [(">", [])]
        run = run_tracks empty
        msg = Midi.ChannelMessage 0
    -- NoEdit means midi but no note.
    equal (run False (f (CmdTest.key_down 'a')))
        (Right empty, [msg (Midi.NoteOn 1 64)])
    equal (run True (f (CmdTest.key_down 'a')))
        (Right [(">", [(0, 1, "anote")])], [msg (Midi.NoteOn 1 64)])
    equal (run True (f (CmdTest.key_up 'a')))
        (Right empty, [msg (Midi.NoteOff 1 64)])

run_tracks :: [UiTest.TrackSpec] -> Bool -> Cmd.CmdId a
    -> (Either String [UiTest.TrackSpec], [Midi.Message])
run_tracks tracks val_edit cmd = extract $ CmdTest.run_sel 0 tracks $ do
    Cmd.modify_edit_state $ \st -> st
        { Cmd.state_edit_mode = if val_edit then Cmd.ValEdit else Cmd.NoEdit
        , Cmd.state_kbd_entry = True
        }
    cmd
    where
    extract res =
        (CmdTest.trace_logs (CmdTest.e_tracks res), CmdTest.e_midi res)

test_drum_instrument = do
    let run = DeriveTest.derive_tracks_with
            (DeriveTest.with_inst_db drum_synth)
        extract = DeriveTest.extract $ \e -> DeriveTest.e_attributes e
    let result = run [(">synth/x", [(0, 0, "bd"), (1, 0, "sn")])]
    equal (extract result) (["+bd", "+snare"], [])

    let (_, midi, logs) = DeriveTest.perform_inst drum_synth [("synth/x", [0])]
            (Derive.r_events result)
    equal logs []
    equal (mapMaybeSnd Midi.channel_message (filter (Midi.is_note . snd) midi))
        [ (0, NoteOn Key.c2 127), (10, NoteOff Key.c2 127)
        , (1000, NoteOn Key.d2 127), (1010, NoteOff Key.d2 127)
        ]

-- I'll bet lenses could do this.
mapMaybeSnd :: (b -> Maybe c) -> [(a, b)] -> [(a, c)]
mapMaybeSnd f xs = [(a, b) | (a, Just b) <- map (second f) xs]

drum_synth :: [MidiInst.SynthDesc]
drum_synth = MidiInst.make $
    (MidiInst.softsynth "synth" "Synth" (-24, 24) [])
    { MidiInst.modify_wildcard = CUtil.drum_instrument notes
    , MidiInst.code =
        MidiInst.note_calls (CUtil.drum_calls (map fst notes))
        <> MidiInst.cmd (CUtil.drum_cmd notes)
    }
    where notes = [(Drums.c_bd, Key.c2), (Drums.c_sn, Key.d2)]

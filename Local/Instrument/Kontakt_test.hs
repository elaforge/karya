-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt_test where
import Util.Test
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Msg as Msg

import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Pitch as Pitch
import qualified Local.Instrument.Kontakt.KontaktTest as KontaktTest
import qualified Local.Instrument.Kontakt.ScGamelan as ScGamelan


test_pasang_thru = do
    -- This is also a test for 'Bali.pasang_thru'.
    let note_on = CmdTest.note_on 1
    result <- run_pasang (note_on (Pitch.pitch 4 0))
    equal (CmdTest.e_logs result) []
    equal (CmdTest.e_midi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn 60 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn 60 127)
        ]
    result <- run_pasang (note_on (Pitch.pitch 4 1))
    equal (CmdTest.e_logs result) []
    equal (CmdTest.e_midi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn 62 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn 62 127)
        ]

run_pasang :: InputNote.Input -> IO (CmdTest.Result Cmd.Status)
run_pasang input =
    CmdTest.run_perf ui_state cmd_state $ do
        CmdTest.set_point_sel 1 0
        Bali.pasang_thru (Msg.InputNote input)
    where
    setup = KontaktTest.with_synth $ ScGamelan.kebyar_allocations "loop1"
    cmd_state = DeriveTest.setup_cmd setup CmdTest.default_cmd_state
    ui_state = DeriveTest.setup_ui setup $
        snd $ UiTest.run_mkview [(">pemade | scale=legong", [])]

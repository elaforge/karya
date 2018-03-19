-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt_test where
import Util.Test
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.Bali as Bali

import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.Pitch as Pitch
import qualified User.Elaforge.Instrument.Kontakt as Kontakt
import qualified User.Elaforge.Instrument.Kontakt.ScGamelan as ScGamelan


test_pasang_thru = do
    -- This is also a test for 'Bali.pasang_thru'.
    let note_on = CmdTest.note_on 1
    result <- run_pasang (note_on (Pitch.pitch 4 0))
    equal (CmdTest.e_logs result) []
    equal (CmdTest.e_midi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn open_ks 64)
        , Midi.ChannelMessage 6 (Midi.PitchBend 0)
        , Midi.ChannelMessage 6 (Midi.NoteOn 60 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn open_ks 64)
        , Midi.ChannelMessage 7 (Midi.PitchBend 0)
        , Midi.ChannelMessage 7 (Midi.NoteOn 60 127)
        ]
    result <- run_pasang (note_on (Pitch.pitch 4 1))
    equal (CmdTest.e_logs result) []
    equal (CmdTest.e_midi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn open_ks 64)
        , Midi.ChannelMessage 6 (Midi.PitchBend 0)
        , Midi.ChannelMessage 6 (Midi.NoteOn 62 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn open_ks 64)
        , Midi.ChannelMessage 7 (Midi.PitchBend 0)
        , Midi.ChannelMessage 7 (Midi.NoteOn 62 127)
        ]

open_ks :: Midi.Key
open_ks = Key2.c1

run_pasang :: InputNote.Input -> IO (CmdTest.Result Cmd.Status)
run_pasang input =
    CmdTest.run_with_performance ui_state cmd_state $ do
        CmdTest.set_point_sel 1 0
        Cmd.lift_id $ Bali.pasang_thru mempty input
        return Cmd.Continue
    where
    cmd_state = DeriveTest.setup_cmd setup CmdTest.default_cmd_state
    ui_state = DeriveTest.setup_ui setup $
        snd $ UiTest.run_mkview [(">pemade | scale=legong", [])]
    setup = DeriveTest.with_synths allocs [Kontakt.synth]
    allocs = ScGamelan.kebyar_allocations "loop1"

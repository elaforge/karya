-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Kontakt_test where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.Bali as Bali
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.DeriveTest as DeriveTest
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Kontakt as Kontakt
import qualified User.Elaforge.Instrument.Kontakt.ScGamelan as ScGamelan

import Util.Test


test_pasang_thru :: Test
test_pasang_thru = do
    -- This is also a test for 'Bali.pasang_thru'.
    let noteOn = CmdTest.noteOn 1
    result <- runPasang (noteOn (Pitch.pitch 4 0))
    equal (CmdTest.eLogs result) []
    equal (CmdTest.eMidi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn openKs 64)
        , Midi.ChannelMessage 6 (Midi.PitchBend 0)
        , Midi.ChannelMessage 6 (Midi.NoteOn 60 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn openKs 64)
        , Midi.ChannelMessage 7 (Midi.PitchBend 0)
        , Midi.ChannelMessage 7 (Midi.NoteOn 60 127)
        ]
    result <- runPasang (noteOn (Pitch.pitch 4 1))
    equal (CmdTest.eLogs result) []
    equal (CmdTest.eMidi result)
        [ Midi.ChannelMessage 6 (Midi.NoteOn openKs 64)
        , Midi.ChannelMessage 6 (Midi.PitchBend 0)
        , Midi.ChannelMessage 6 (Midi.NoteOn 62 127)
        , Midi.ChannelMessage 7 (Midi.NoteOn openKs 64)
        , Midi.ChannelMessage 7 (Midi.PitchBend 0)
        , Midi.ChannelMessage 7 (Midi.NoteOn 62 127)
        ]

openKs :: Midi.Key
openKs = Key2.c1

runPasang :: InputNote.Input -> IO (CmdTest.Result Cmd.Status)
runPasang input =
    CmdTest.runWithPerformance uiState cmdState $ do
        CmdTest.setPointSel 1 0
        scale <- Perf.getScale =<< Selection.track
        Cmd.liftId $ mapM_ Cmd.writeThru
            =<< Bali.pasangThru scale mempty input
        pure Cmd.Continue
    where
    cmdState = DeriveTest.setupCmd setup CmdTest.defaultCmdState
    uiState = DeriveTest.setupUi setup $
        snd $ UiTest.runMkview [(">pemade | scale=legong", [])]
    setup = DeriveTest.withSynths allocs [Kontakt.synth]
    allocs = ScGamelan.kebyarAllocations "loop1"

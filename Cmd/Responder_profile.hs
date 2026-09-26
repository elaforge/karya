-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Responder_profile where
import qualified Data.Map as Map
import qualified Text.Printf as Printf

import qualified Util.Log as Log
import qualified Util.Thread as Thread
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Create as Create
import qualified Cmd.ResponderTest as ResponderTest
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive_profile as Derive_profile
import qualified Midi.Midi as Midi
import qualified Ui.Key as Key
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import           Global


profileEditsMiddle = do
    -- Test editing a large score in the middle as a real user would.
    Log.configure $ \st -> st { Log.statePriority = Log.Warn }
    let editBlockId = UiTest.bid "b1.5.0"
    let (viewId, uiState) = UiTest.run Ui.empty $ do
            Ui.modifyConfig $
                UiConfig.allocations #= UiTest.defaultAllocations
            Derive_profile.makeNestedControls 15 3 60
            viewId <- Create.unfittedView editBlockId
            UiTest.selectPoint viewId 1 0.0
            pure viewId
    let cmdState = modifyEditState
            (ResponderTest.mkCmdState uiState viewId) $ \st ->
                st { Cmd.stateEditMode = Cmd.ValEdit }
    -- pprint (UiTest.extract_tracks_of edit_block_id ui_state)

    let wait = [(CmdTest.makeKey UiMsg.KeyDown Key.ShiftL, 0.1),
            (CmdTest.makeKey UiMsg.KeyUp Key.ShiftL, 4)]
        alterNote = [(CmdTest.mNoteOn 64, 1), (CmdTest.mNoteOff 64, 1)]
        keys = concat $ replicate 4 alterNote
    ResponderTest.threadDelay True (uiState, cmdState) (wait ++ keys)
    pure ()

profileNullCmd = do
    -- Test a msg that matches no cmds so I can see how much garbage it
    -- produces by itself.
    let states = ResponderTest.mkstates [(">i1", [(0, 0, "")])]
    let key = CmdTest.keypress Key.ShiftL
    let keys = take (10*1024) (cycle key)
    cpu <- timeCpu $ ResponderTest.thread False states keys
    Printf.printf "%.2f sec, %.4f sec per cmd\n"
        (secs cpu) (secs cpu / (10*1024))

profileSelection = do
    Log.configure $ \st -> st { Log.statePriority = Log.Warn }
    let (uiState, cmdState) = ResponderTest.mkstates [(">i1", [(0, 0, "")])]
    let uiState2 = uiState { Ui.stateRulers =
            Map.insert (UiTest.rid "b1.r0") (UiTest.mkruler44 256 1)
                (Ui.stateRulers uiState) }
    let cmdState2 = modifyEditState cmdState $ \st -> st
            { Cmd.stateTimeStep = TimeStep.timeStep
                (TimeStep.AbsoluteMark TimeStep.AllMarklists Meter.Q)
            }
    let states = (uiState2, cmdState2)
    let oneCycle = take (256*2) (cycle (CmdTest.keypress Key.Down))
            ++ take (256*2) (cycle (CmdTest.keypress Key.Up))
    let keys = take (10*1024) (cycle oneCycle)
    cpu <- timeCpu $ ResponderTest.thread False states keys
    Printf.printf "%.2f sec, %.4f sec per cmd\n"
        (secs cpu) (secs cpu / (10*1024))

profileThru = do
    let (uiState, cmdState) = ResponderTest.mkstates [(">i1", [(0, 0, "")])]
    let ncmds = 10 * 1024
    let key = [CmdTest.makeMidi (Midi.NoteOn 60 20),
            CmdTest.makeMidi (Midi.NoteOff 60 20)]
        keys = take ncmds (cycle key)
    cpu <- timeCpu $ ResponderTest.thread False (uiState, cmdState) keys
    Printf.printf "%.2f sec, %.4f sec per cmd\n"
        (secs cpu) (secs cpu / fromIntegral ncmds)

timeCpu :: IO a -> IO Thread.Seconds
timeCpu = fmap (Thread.metricCpu . snd) . Thread.timeAction

secs :: Thread.Seconds -> Double
secs = realToFrac

modifyEditState :: Cmd.State -> (Cmd.EditState -> Cmd.EditState) -> Cmd.State
modifyEditState st f = st { Cmd.stateEdit = f (Cmd.stateEdit st) }

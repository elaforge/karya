-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.PlayUtil_test where
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_events_from :: Test
test_events_from = do
    let i1 = UiTest.i1
        i2 = UiTest.i2
        i3 = UiTest.i3
    let f pos = bimap (map extract) (map extract . Vector.toList)
            . PlayUtil.eventsFrom resumeInsts pos
            . Vector.fromList . map mkevent
        resumeInsts = Set.fromList [i1, i2]
        mkevent (start, dur, inst) =
            DeriveTest.mkevent (start, dur, "4c", [], inst)
        extract e = (Score.eventStart e, Score.eventDuration e,
            Score.eventInstrument e)
    equal (f 0 []) ([], [])
    -- The one at means don't look for the previous one.
    equal (f 2 [(0, 1, i1), (1, 1, i1), (2, 1, i1)]) ([], [(2, 1, i1)])
    equal (f 2 [(0, 1, i1), (1, 2, i1), (3, 1, i1)])
        ([(2, 1, i1)], [(3, 1, i1)])
    -- Not interested in i3.
    equal (f 2 [(0, 1, i3), (1, 2, i3), (3, 1, i3)]) ([], [(3, 1, i3)])

    -- Scan back for events slightly before the start.
    equal (f 1 [(0, 1, i3), (0.98, 1, i3), (2, 1, i3)])
        ([(0.98, 1, i3)], [(2, 1, i3)])
    -- Same with 0 dur.
    equal (f 1 [(0, 0, i3), (0.98, 0, i3), (2, 0, i3)])
        ([(0.98, 0, i3)], [(2, 0, i3)])

test_control_defaults :: Test
test_control_defaults = do
    let make = UiTest.setInstruments allocKy
            . CmdTest.makeTracks . uncurry UiTest.instNoteTrack
        allocKy = ">i1 s/1 [ms] wdev 1 { control_defaults: {cc17: .5} }"
        extract = first $ fmap (map snd . DeriveTest.midiChannel)
    let run state = extract $ performEvents state UiTest.defaultBlockId
    let (midi, logs) = run $ make ("i1", [(0, 1, "4c")])
    equal logs []
    equal midi $
        Right [Midi.ControlChange 17 64, Midi.NoteOn Key.c4 127,
            Midi.NoteOff Key.c4 127]
    -- Default controls won't override an existing one.
    let (midi, logs) = run $ make ("i1 | cc17=0", [(0, 1, "4c")])
    equal logs []
    equal midi $
        Right [Midi.ControlChange 17 0, Midi.NoteOn Key.c4 127,
            Midi.NoteOff Key.c4 127]

performEvents :: Ui.State -> BlockId
    -> (Either Text [Midi.WriteMessage], [Text])
performEvents uiState blockId =
    (midi, mapMaybe DeriveTest.showInterestingLog allLogs)
    where
    (midi, midiLogs) = case CmdTest.resultOk result of
        Right events -> first Right $ LEvent.partition events
        Left err -> (Left err, [])
    allLogs = Cmd.perfLogs perf ++ logs ++ CmdTest.resultLogs result
        ++ midiLogs
    result = CmdTest.run uiState cmdState $
        PlayUtil.performRaw (Vector.toList (Cmd.perfEvents perf))
    (perf, logs) = Performance.derive uiState cmdState blockId
    cmdState = CmdTest.defaultCmdState

-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.PlayUtil_test where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Performance as Performance
import qualified Cmd.PlayUtil as PlayUtil

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import Global
import Types


test_events_from = do
    let i1 = UiTest.i1
        i2 = UiTest.i2
        i3 = UiTest.i3
    let f pos = bimap (map extract) (map extract . Vector.toList)
            . PlayUtil.events_from resume_insts pos
            . Vector.fromList . map mkevent
        resume_insts = Set.fromList [i1, i2]
        mkevent (start, dur, inst) =
            DeriveTest.mkevent (start, dur, "4c", [], inst)
        extract e = (Score.event_start e, Score.event_duration e,
            Score.event_instrument e)
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

test_control_defaults = do
    let make = (Ui.allocation UiTest.i1 #= Just alloc)
            . CmdTest.make_tracks . uncurry UiTest.inst_note_track
        alloc = UiTest.midi_allocation "s/1" $
            Patch.settings#Patch.control_defaults
                #= Just (Map.fromList [("cc17", 0.5)]) $
            DeriveTest.simple_midi_config
        extract = first $ fmap (map snd . DeriveTest.midi_channel)
    let run state = extract $ perform_events state UiTest.default_block_id
    let (midi, logs) = run $ make ("i1", [(0, 1, "4c")])
    equal logs []
    equal midi $
        Right [Midi.ControlChange 17 64, Midi.NoteOn Key.c4 127,
            Midi.NoteOff Key.c4 127]
    -- Default controls won't override an existing one.
    let (midi, logs) = run $ make ("i1 | %cc17=0", [(0, 1, "4c")])
    equal logs []
    equal midi $
        Right [Midi.ControlChange 17 0, Midi.NoteOn Key.c4 127,
            Midi.NoteOff Key.c4 127]

perform_events :: Ui.State -> BlockId
    -> (Either Text [Midi.WriteMessage], [Text])
perform_events ui_state block_id =
    (midi, mapMaybe DeriveTest.show_interesting_log all_logs)
    where
    (midi, midi_logs) = case CmdTest.result_ok result of
        Right events -> first Right $ LEvent.partition events
        Left err -> (Left err, [])
    all_logs = Cmd.perf_logs perf ++ logs ++ CmdTest.result_logs result
        ++ midi_logs
    result = CmdTest.run ui_state cmd_state $
        PlayUtil.perform_events (Cmd.perf_events perf)
    (perf, logs) = Performance.derive ui_state cmd_state block_id
    cmd_state = CmdTest.default_cmd_state

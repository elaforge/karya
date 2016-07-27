-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Selection_test where
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple

import Types
import Global


test_events_around = do
    let f evts start end = extract_selected_around $
            run_sel Selection.events_around evts start end
    equal (f [(0, 1), (2, 1)] 1 1) $ Right (Just [([], [0], [2])], [])
    equal (f [(1, -1), (3, -1)] 2 2) $ Right (Just [([1], [3], [])], [])
    equal (f [(0, 1), (3, -1)] 2 2) $ Right (Just [([0], [3], [])], [])
    -- positive events win when there are both choices
    equal (f [(0, 1), (3, -1)] 1.5 1.5) $ Right (Just [([], [0], [3])], [])
    -- no one wins when there are no choices
    equal (f [(1, -1), (3, 1)] 2 2) $ Right (Just [([1], [], [3])], [])
    equal (f [(0, 1), (1, 1), (2, 1)] 0.5 2.5) $
        Right (Just [([0], [1, 2], [])], [])
    equal (f [(4, -4)] 4 4) $ Right (Just [([], [4], [])], [])

test_events_around_range = do
    let f evts start end = CmdTest.extract (map extract) $
            run_sel Selection.events_around evts start end
        extract (_, range, (_, within, _)) = (range, map Simple.event within)
    equal (f [(2, -2), (4, -2)] 1 1) $
        Right (Just [((0, 2), [(2, -2, "2t")])], [])
    equal (f [(2, -2), (4, -2)] 2 2) $
        Right (Just [((0, 2), [(2, -2, "2t")])], [])
    equal (f [(2, -2), (4, -2)] 3 3) $
        Right (Just [((2, 4), [(4, -2, "4t")])], [])

run_sel :: Cmd.CmdId a -> [(ScoreTime, ScoreTime)] -> ScoreTime -> ScoreTime
    -> CmdTest.Result a
run_sel cmd events start end =
    CmdTest.run_tracks [(">", start_dur_events events)] $ do
        CmdTest.set_sel 1 start 1 end
        cmd

extract_selected_around :: CmdTest.Result Selection.SelectedAround
    -> CmdTest.Extracted (Maybe [([ScoreTime], [ScoreTime], [ScoreTime])])
extract_selected_around = CmdTest.extract (map e_sel)
    where
    e_sel (_track_id, _range, (before, within, after)) =
        (map Event.trigger before, map Event.trigger within,
            map Event.trigger after)

start_dur_events :: [(ScoreTime, ScoreTime)] -> [UiTest.EventSpec]
start_dur_events specs = [(p, d, prettys p) | (p, d) <- specs]

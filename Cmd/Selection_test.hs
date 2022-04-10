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

import Global
import Types


test_events_around :: Test
test_events_around = do
    let f evts start end = CmdTest.extract (extract . head) $
            run_sel Selection.events_around evts start end
        extract (_, (pre, within, post)) =
            (map Event.start pre, map Event.start within, map Event.start post)
    equal (f [(0, 2), (2, 2)] 1 1) $ Right (Just ([], [0], [2]), [])
    equal (f [(0, 2), (2, 2)] 1 5) $ Right (Just ([0], [2], []), [])
    equal (f [(0, 2), (2, 2), (4, 2)] 2 4) $ Right (Just ([0], [2], [4]), [])
    equal (f [(2, -2), (4, -2), (6, -2)] 2 4) $ Right (Just ([2], [4], [6]), [])
    -- Event at the end is treated specially.
    equal (f [(0, 2), (2, 0)] 0 2) $ Right (Just ([], [0, 2], []), [])

    -- Point selection.
    equal (f [(0, 2), (2, 2)] 0 0) $ Right (Just ([], [0], [2]), [])
    -- Negative events.
    equal (f [(2, -2)] 1 1) $ Right (Just ([], [2], []), [])
    -- Overlapping event wins.
    equal (f [(0, 2), (6, -2)] 5 5) $ Right (Just ([0], [6], []), [])
    equal (f [(0, 2), (6, -2)] 1 1) $ Right (Just ([], [0], [6]), [])
    -- Otherwise, use selection orientation.
    equal (f [(0, 2), (6, -2)] 4 4) $ Right (Just ([], [0], [6]), [])

run_sel :: Cmd.CmdId a -> [(ScoreTime, ScoreTime)] -> ScoreTime -> ScoreTime
    -> CmdTest.Result a
run_sel cmd events start end =
    CmdTest.run_tracks_ruler [(">", start_dur_events events)] $ do
        CmdTest.set_sel 1 start 1 end
        cmd

start_dur_events :: [(ScoreTime, ScoreTime)] -> [UiTest.EventSpec]
start_dur_events specs = [(p, d, pretty p) | (p, d) <- specs]

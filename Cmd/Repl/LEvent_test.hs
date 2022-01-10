-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LEvent_test where
import qualified Cmd.Repl.LEvent as LEvent
import qualified Ui.Event as Event
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Types
import           Util.Test


test_quantize_timestep :: Test
test_quantize_timestep = do
    let f mode step events = LEvent.quantize_timestep mode step
            UiTest.default_block_id (UiTest.mk_tid 1)
            (map UiTest.make_event [(s, e, "") | (s, e) <- events])
        run mode marks step events = map extract <$>
            run_ruler marks [(">", [])] (f mode step events)
        extract e = (Event.start e, Event.duration e)

    let marks1 = map (, 0) [0, 1, 2, 3, 4]
    equal (run LEvent.Both marks1 "w" [(0.000000000000199, 1)])
        (Just [(0, 1)])
    equal (run LEvent.Both marks1 "w" [(0, 1.2)]) (Just [(0, 1)])
    equal (run LEvent.Both marks1 "w" [(0, 1.51)]) (Just [(0, 2)])
    -- The end won't quantize to the beginning.
    equal (run LEvent.Both marks1 "w" [(0, 0.1)]) (Just [(0, 1)])
    -- A non-zero dur event won't quantize to 0.
    equal (run LEvent.Both marks1 "w" [(0.6, 0.1)]) (Just [(1, 1)])
    -- But a zero dur one can.
    equal (run LEvent.Both marks1 "w" [(0.6, 0)]) (Just [(1, 0)])
    -- Two events won't quantize into the same time.
    equal (run LEvent.Both marks1 "w" [(0.6, 0.5), (1, 0.5)])
        (Just [(1, 1), (2, 1)])
    equal (run LEvent.Both marks1 "w" [(0.6, 0.5), (1, 0.5), (1.2, 0.5)])
        (Just [(1, 1), (2, 1), (3, 1)])

    let marks2 = [(1, 0), (2, 1), (3, 0), (4, 0)]
    -- Section ignores the rank 1, note with duration won't snap to 0 dur.
    equal (run LEvent.Both marks2 "section" [(2, 0.25)]) (Just [(1, 2)])
    -- but a 0 dur note will
    equal (run LEvent.Both marks2 "section" [(2, 0)]) (Just [(1, 0)])
    -- w snaps to 2
    equal (run LEvent.Both marks2 "w" [(2, 0.25)]) (Just [(2, 1)])
    -- end rounds up
    equal (run LEvent.Both marks2 "w" [(1, 2.6)]) (Just [(1, 3)])
    -- End has no effect for 0 dur
    equal (run LEvent.End marks2 "w" [(0.5, 0)]) (Just [(0.5, 0)])
    -- otherwise it wants to have a duration
    equal (run LEvent.End marks2 "w" [(0.5, 0.1)]) (Just [(0.5, 0.5)])
    equal (run LEvent.End marks2 "w" [(0.5, 0.5)]) (Just [(0.5, 0.5)])

run_ruler :: [(TrackTime, UiTest.RankNum)] -> [UiTest.TrackSpec]
    -> Ui.StateId a -> a
run_ruler marks tracks = UiTest.eval $ UiTest.exec Ui.empty $
    UiTest.mkblock_ruler (UiTest.mkruler_ranks marks) UiTest.default_block_id
        "" tracks

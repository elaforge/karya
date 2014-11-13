-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LEvent_test where
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Repl.LEvent as LEvent
import Global
import Types


test_quantize_timestep = do
    let f mode step events = LEvent.quantize_timestep mode step
            UiTest.default_block_id (UiTest.mk_tid 1)
            (map UiTest.make_event events)
        run mode marks step events = map extract <$>
            run_ruler (make_marklist marks) [(">", [])]
                (f mode step events)
        extract e = (Event.start e, Event.duration e)
    let marks = [(1, 0), (2, 1), (3, 0), (4, 0)]

    -- section ignores the rank 1, note with duration won't snap to 0 dur
    equal (run LEvent.Both marks "section" [(2, 0.25, "")]) (Just [(1, 2)])
    -- but a 0 dur note will
    equal (run LEvent.Both marks "section" [(2, 0, "")]) (Just [(1, 0)])
    -- w snaps to 2
    equal (run LEvent.Both marks "w" [(2, 0.25, "")]) (Just [(2, 1)])
    -- end rounds up
    equal (run LEvent.Both marks "w" [(1, 2.6, "")]) (Just [(1, 3)])
    -- End has no effect for 0 dur
    equal (run LEvent.End marks "w" [(0.5, 0, "")]) (Just [(0.5, 0)])
    -- otherwise it wants to have a duration
    equal (run LEvent.End marks "w" [(0.5, 0.1, "")]) (Just [(0.5, 0.5)])
    equal (run LEvent.End marks "w" [(0.5, 0.5, "")]) (Just [(0.5, 0.5)])

    let marks2 = map (flip (,) 0) [0, 1, 2, 3, 4]
    equal (run LEvent.Both marks2 "w" [(0.000000000000199, 1, "")])
        (Just [(0, 1)])

make_marklist :: [(ScoreTime, Ruler.Rank)] -> Ruler.Marklist
make_marklist = Ruler.marklist . map (second mark)
    where mark rank = Ruler.null_mark { Ruler.mark_rank = rank }

run_ruler :: Ruler.Marklist -> [UiTest.TrackSpec] -> State.StateId a -> a
run_ruler marklist tracks = UiTest.eval $ UiTest.exec State.empty $
    UiTest.mkblock_marklist marklist UiTest.default_block_id "" tracks

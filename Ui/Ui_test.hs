-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ui_test where
import qualified Util.Ranges as Ranges
import Util.Test
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import Global


test_toggle_skeleton_edge = do
    let bid = UiTest.default_block_id
    let run allow edge = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock (UiTest.default_block_name,
                [('t' : show n, []) | n <- [1..3]])
            Ui.set_skeleton bid Skeleton.empty
            Ui.add_edges bid [(1, 2), (2, 3)]
            Ui.toggle_skeleton_edge allow bid edge
            Skeleton.flatten <$> Ui.get_skeleton bid
    equal (run True (1, 3)) (Right [(1, 2), (1, 3), (2, 3)])
    equal (run False (1, 3)) (Right [(1, 2), (1, 3)])
    equal (run True (2, 3)) (Right [(1, 2)])
    equal (run False (2, 3)) (Right [(1, 2)])

test_skeleton_cycles = do
    let bid = UiTest.default_block_id
    let run ntracks m = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock (UiTest.default_block_name,
                [('t' : show n, []) | n <- [0..ntracks]])
            Ui.set_skeleton bid Skeleton.empty
            m
    equal (run 1 (Ui.toggle_skeleton_edge False bid (1, 1))) (Right False)
    left_like (run 1 (Ui.toggle_skeleton_edge False bid (1, 10)))
        "toggle: tracknum out of range"
    left_like (run 1 (Ui.toggle_skeleton_edge False bid (0, 1)))
        "toggle: edge points to non-event track"
    equal (run 1 (Ui.toggle_skeleton_edge False bid (1, 2))) (Right True)
    left_like (run 1 (Ui.add_edges bid [(1, 1)]))
        "would have caused a cycle"
    left_like (run 1 (Ui.add_edges bid [(1, 2), (2, 1)]))
        "would have caused a cycle"
    left_like (run 1 (Ui.add_edges bid [(0, 1), (1, 2)]))
        "edge points to non-event track"
    equal (run 1 (Ui.add_edges bid [(1, 2)])) (Right ())

test_calculate_damage = do
    let f xs ys = Ui.calculate_damage (mkevents xs) (mkevents ys)
    equal (f [(0, 1, "a")] [(0, 1, "a")]) Ranges.nothing
    equal (f [(0, 1, "a")] [(0, 1, "b")]) (Ranges.range 0 1)
    equal (f [(0, 1, "a")] [(2, 1, "a")]) (Ranges.ranges [(0, 1), (2, 3)])
    equal (f [(0, 1, "a"), (3, 1, "c")] [(2, 1, "b")])
        (Ranges.ranges [(0, 1), (2, 4)])
    equal (f [(0, 1, "a")] [(0, 2, "a")]) (Ranges.range 0 2)

mkevents = Events.from_list . map UiTest.make_event

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ui_test where
import qualified Util.Ranges as Ranges
import qualified Cmd.Create as Create
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_implicit_skeleton :: Test
test_implicit_skeleton = do
    let run = run_implicit
    let create tracknum title =
            Create.track UiTest.default_block_id tracknum title mempty
    equal (run ["tempo"] (create 2 ">")) [(1, 2)]
    equal (run ["tempo", ">"] (create 3 "*")) [(1, 2), (2, 3)]
    equal (run ["tempo", ">"] (create 3 ">")) [(1, 2), (1, 3)]

    let remove = Ui.remove_track UiTest.default_block_id
    equal (run ["tempo", ">"] (remove 2)) []
    equal (run ["tempo", ">", ">"] (remove 2)) [(1, 2)]

    let title = Ui.set_track_title . UiTest.mk_tid
    equal (run [">", "*", "c"] (pure ())) [(1, 2), (2, 3)]
    equal (run [">", "*", "c"] (title 3 ">")) [(1, 2)]
    equal (run [">", "*", ">"] (title 3 "c")) [(1, 2), (2, 3)]

run_implicit :: [Text] -> Ui.StateId a -> [(TrackNum, TrackNum)]
run_implicit titles action = UiTest.extract_skeleton $
    UiTest.exec (snd $ UiTest.run_mkblock (map (, []) titles)) $ do
        Ui.set_skeleton_config UiTest.default_block_id Block.Implicit
        action

test_toggle_skeleton_edge :: Test
test_toggle_skeleton_edge = do
    let bid = UiTest.default_block_id
    let run allow edge = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock [("t" <> showt n, []) | n <- [1..3]]
            Ui.set_skeleton bid Skeleton.empty
            Ui.add_edges bid [(1, 2), (2, 3)]
            Ui.toggle_skeleton_edge allow bid edge
            Skeleton.flatten <$> Ui.get_skeleton bid
    equal (run True (1, 3)) (Right [(1, 2), (1, 3), (2, 3)])
    equal (run False (1, 3)) (Right [(1, 2), (1, 3)])
    equal (run True (2, 3)) (Right [(1, 2)])
    equal (run False (2, 3)) (Right [(1, 2)])

test_skeleton_cycles :: Test
test_skeleton_cycles = do
    let bid = UiTest.default_block_id
    let run ntracks m = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock [("t" <> showt n, []) | n <- [0..ntracks]]
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

test_calculate_damage :: Test
test_calculate_damage = do
    let f xs ys = Ui.calculate_damage (mkevents xs) (mkevents ys)
    equal (f [(0, 1, "a")] [(0, 1, "a")]) Ranges.nothing
    equal (f [(0, 1, "a")] [(0, 1, "b")]) (Ranges.range 0 1)
    equal (f [(0, 1, "a")] [(2, 1, "a")]) (Ranges.ranges [(0, 1), (2, 3)])
    equal (f [(0, 1, "a"), (3, 1, "c")] [(2, 1, "b")])
        (Ranges.ranges [(0, 1), (2, 4)])
    equal (f [(0, 1, "a")] [(0, 2, "a")]) (Ranges.range 0 2)

mkevents :: [UiTest.EventSpec] -> Events.Events
mkevents = Events.from_list . map UiTest.make_event

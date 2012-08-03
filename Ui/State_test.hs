module Ui.State_test where
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest


test_skeleton_cycles = do
    let bid = UiTest.default_block_id
    let extract = either (Left . Pretty.pretty) Right
    let run ntracks m = extract $ State.eval State.empty $ do
            UiTest.mkblock (UiTest.default_block_name,
                [('t' : show n, []) | n <- [0..ntracks]])
            State.set_skeleton bid Skeleton.empty
            m
    equal (run 1 (State.toggle_skeleton_edge bid (1, 1))) (Right False)
    left_like (run 1 (State.toggle_skeleton_edge bid (1, 10)))
        "toggle: tracknum out of range"
    left_like (run 1 (State.toggle_skeleton_edge bid (0, 1)))
        "toggle: edge points to non-event track"
    equal (run 1 (State.toggle_skeleton_edge bid (1, 2))) (Right True)
    left_like (run 1 (State.add_edges bid [(1, 1)]))
        "would have caused a cycle"
    left_like (run 1 (State.add_edges bid [(1, 2), (2, 1)]))
        "would have caused a cycle"
    left_like (run 1 (State.add_edges bid [(0, 1), (1, 2)]))
        "edge points to non-event track"
    equal (run 1 (State.add_edges bid [(1, 2)])) (Right ())

test_muted_tracknums = do
    let (_, st) = UiTest.run_mkblock
            [("tempo", []), (">i1", []), ("c1", []), (">i2", [])]
    let with_flag flag ts = UiTest.eval st $ do
            mapM_ (\n -> State.toggle_track_flag UiTest.default_block_id n flag)
                ts
            block <- State.get_block UiTest.default_block_id
            tree <- State.get_track_tree UiTest.default_block_id
            return $ State.muted_tracknums block tree
    -- pprint (UiTest.eval st (State.get_track_tree UiTest.default_block_id))

    let mute = with_flag Block.Mute
    equal (mute []) []
    equal (mute [1]) [1]
    equal (mute [3]) [3]
    equal (mute [4]) [4]

    -- solo keeps both parents and children alive
    let solo = with_flag Block.Solo
    equal (solo [4]) [2, 3]
    equal (solo [3, 4]) []
    equal (solo [2, 4]) []
    equal (solo [1]) []

test_calculate_damage = do
    let f xs ys = State.calculate_damage (mkevents xs) (mkevents ys)
    equal (f [(0, 1, "a")] [(0, 1, "a")]) Ranges.nothing
    equal (f [(0, 1, "a")] [(0, 1, "b")]) (Ranges.range 0 1)
    equal (f [(0, 1, "a")] [(2, 1, "a")]) (Ranges.ranges [(0, 1), (2, 3)])
    equal (f [(0, 1, "a"), (3, 1, "c")] [(2, 1, "b")])
        (Ranges.ranges [(0, 1), (2, 4)])
    equal (f [(0, 1, "a")] [(0, 2, "a")]) (Ranges.range 0 2)

mkevents = Events.from_list . map UiTest.make_event

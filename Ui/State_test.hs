module Ui.State_test where
import qualified Util.Pretty as Pretty
import Util.Test
import qualified Ui.Block as Block
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
    let (_, st) = UiTest.run_mkstate
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

mkid = UiTest.mkid

simple_state = snd $ UiTest.run State.empty $ do
    t0 <- State.create_track (mkid "t0") (UiTest.empty_track "tempo")
    ruler <- State.create_ruler (mkid "r1") (UiTest.ruler [])
    b1 <- State.create_block (mkid "b1") $
        UiTest.make_block "hi b1"
            [(Block.RId ruler, 20), (Block.TId t0 ruler, 40)]
    _v1 <- State.create_view (mkid "v1") $ Block.view b1
        UiTest.default_rect UiTest.default_zoom
    return ()

module Ui.TrackTree_test where
import Util.Test
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.TrackTree as TrackTree
import qualified Ui.UiTest as UiTest


test_muted_tracknums = do
    let (_, st) = UiTest.run_mkblock
            [("tempo", []), (">i1", []), ("c1", []), (">i2", [])]
    let with_flag flag ts = UiTest.eval st $ do
            mapM_ (\n -> State.toggle_track_flag UiTest.default_block_id n flag)
                ts
            block <- State.get_block UiTest.default_block_id
            tree <- TrackTree.get_track_tree UiTest.default_block_id
            return $ TrackTree.muted_tracknums block tree
    -- pprint (UiTest.eval st
    -- (TrackTree.get_track_tree UiTest.default_block_id))

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

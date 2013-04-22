module Cmd.BlockConfig_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.BlockConfig as BlockConfig


test_move_tracks = do
    let f ntracks tracks sources dest = extract $
            UiTest.exec (mkstate ntracks tracks) $
            BlockConfig.move_tracks UiTest.default_block_id sources dest
        mkstate ntracks tracks = UiTest.exec State.empty $
            UiTest.mkblocks_skel [((UiTest.default_block_name,
                [(show n, []) | n <- [1..ntracks]]), tracks)]
        extract st = (concatMap fst $ UiTest.extract_tracks st,
            UiTest.extract_skeleton st)

    -- Move up.
    equal (f 4 [(2, 3)] [1] 3) ("2314", [(1, 2)])
    equal (f 4 [(3, 4)] [1, 2] 3) ("3124", [(1, 4)])
    -- Move down.
    equal (f 4 [(2, 3)] [3] 1) ("3124", [(3, 1)])
    equal (f 4 [(2, 3)] [3, 4] 2) ("1342", [(4, 2)])

module Ui.State_test where
import qualified Data.Map as Map
import qualified Data.Tree as Tree

import qualified Util.Log as Log
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified App.Config as Config


test_verify = do
    let broken = simple_state
            { State.state_views = Map.map f (State.state_views simple_state) }
        f view = view { Block.view_tracks = [] }
        (Right unbroken, logs) = State.verify broken

    -- reports and fixes view problem
    equal (map Log.msg_text logs)
        ["block has 2 tracks while view has 0, fixing"]
    equal (snd (State.verify unbroken)) []

test_get_track_tree_muted = do
    let (_, st) = UiTest.run_mkstate
            [("tempo", []), (">i1", []), ("c1", []), (">i2", [])]
        tree = [Tree.Node "tempo" [Tree.Node ">i2" [],
            Tree.Node "c1" [Tree.Node ">i1" []]]]
    let to_title = map (fmap State.track_title)
        f = fmap to_title
            (State.get_unmuted_track_tree UiTest.default_block_id)
    let with_flag flag ts = UiTest.eval st $
            mapM_ (\n -> State.toggle_track_flag UiTest.default_block_id n flag)
                ts >> f

    let mute = with_flag Block.Mute
    equal (mute []) tree
    equal (mute [1]) []
    equal (mute [3]) [Tree.Node "tempo" [Tree.Node ">i2" []]]
    equal (mute [4]) [Tree.Node "tempo" [Tree.Node "c1" [Tree.Node ">i1" []]]]

    -- solo keeps both parents and children alive
    let solo = with_flag Block.Solo
    equal (solo [4]) [Tree.Node "tempo" [Tree.Node ">i2" []]]
    equal (solo [3, 4]) tree
    equal (solo [2, 4]) tree
    equal (solo [1]) tree

mkid = UiTest.mkid

simple_state = snd $ UiTest.run State.empty $ do
    t0 <- State.create_track (mkid "t0") (UiTest.empty_track "tempo")
    ruler <- State.create_ruler (mkid "r1") (UiTest.ruler [])
    b1 <- State.create_block (mkid "b1") $ UiTest.mkblock "hi b1"
        Config.block_config [(Block.RId ruler, 20), (Block.TId t0 ruler, 40)]
    _v1 <- State.create_view (mkid "v1") $ Block.view b1
        UiTest.default_rect UiTest.default_zoom Config.view_config
    return ()

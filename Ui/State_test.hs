module Ui.State_test where
import qualified Data.Map as Map

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

mkid = UiTest.mkid

simple_state = snd $ UiTest.run State.empty $ do
    t0 <- State.create_track (mkid "t0") (UiTest.empty_track "tempo")
    ruler <- State.create_ruler (mkid "r1") (UiTest.ruler [])
    b1 <- State.create_block (mkid "b1") $ Block.block "hi b1"
        Config.block_config
        [(Block.RId ruler, 20), (Block.TId t0 ruler, 40)]
        Config.schema
    v1 <- State.create_view (mkid "v1") $ Block.view b1
        UiTest.default_rect UiTest.default_zoom Config.view_config
    return ()

module Ui.State_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.TestSetup as TestSetup

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


simple_state = snd $ State.run_state State.empty $ do
    t0 <- State.create_track "t0" (TestSetup.empty_track "tempo")
    ruler <- State.create_ruler "r1" (TestSetup.ruler [])
    b1 <- State.create_block "b1" $ Block.block "hi b1"
        Config.block_config
        [(Block.RId ruler, 20), (Block.TId t0 ruler, 40)]
        Config.schema
    v1 <- State.create_view "v1"
        (Block.view b1 TestSetup.default_rect Config.view_config)
    return ()

module Cmd.ResponderSync_test where

import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update
import qualified Cmd.ResponderSync as ResponderSync


test_dirty_blocks = do
    let bid = UiTest.default_block_id
        ([tid], ustate) = UiTest.run_mkstate [("t1", [])]
    let f = ResponderSync.dirty_blocks

    equal (f ustate ustate []) []
    equal (f ustate (snd $ UiTest.run_mkstate [("t2", [])]) []) [bid]
    equal (f ustate ustate [Update.BlockUpdate bid (Update.BlockTitle "ho")])
        [bid]
    equal (f ustate ustate [Update.TrackUpdate tid Update.TrackAllEvents]) [bid]
    equal (f ustate ustate [Update.TrackUpdate tid Update.TrackBg]) []

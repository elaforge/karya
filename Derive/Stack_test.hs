module Derive.Stack_test where

import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Stack as Stack

block = Stack.Block . UiTest.bid
track = Stack.Track . UiTest.tid
region = Stack.Region

test_to_ui = do
    pprint (Stack.make [block "b1", block "b2"])
    pprint (Stack.make [block "b1", track "t1"])
    pprint (Stack.make [block "b1", track "t1", block "b2"])
    pprint (Stack.make [block "b1", track "t1", block "b2", region 1 2])
    pprint (Stack.make [block "b1", track "t1", block "b2", region 1 2, region 3 4])

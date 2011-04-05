module Derive.Stack_test where

import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Stack as Stack

bid = UiTest.bid
tid = Just . UiTest.tid

block = Stack.Block . UiTest.bid
track = Stack.Track . UiTest.tid
region = Stack.Region

test_to_ui = do
    let mk = Stack.to_ui . Stack.from_outermost
    equal (mk [block "b1", block "b2"])
        [(bid "b1", Nothing, Nothing), (bid "b2", Nothing, Nothing)]
    equal (mk [block "b1", track "t1"])
        [(bid "b1", tid "t1", Nothing)]
    equal (mk [block "b1", track "t1", block "b2"])
        [(bid "b1", tid "t1", Nothing), (bid "b2", Nothing, Nothing)]
    -- region must be preceded by a track
    equal (mk [block "b1", track "t1", block "b2", region 1 2])
        [(bid "b1", tid "t1", Nothing), (bid "b2", Nothing, Nothing)]
    -- I shouldn't have two regions in a row, but if so the last region wins
    equal (mk [block "b1", track "t1", region 1 2, region 3 4])
        [(bid "b1", tid "t1", Just (3, 4))]

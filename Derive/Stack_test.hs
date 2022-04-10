-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Stack_test where
import qualified Data.Aeson as Aeson

import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Stack as Stack

bid = Just . UiTest.bid
tid = Just . UiTest.tid

block = Stack.Block . UiTest.bid
track = Stack.Track . UiTest.tid
region = Stack.Region

test_block_track_of :: Test
test_block_track_of = do
    let f = Stack.block_track_of . Stack.from_outermost
    equal (f [block "b"]) Nothing
    equal (f [track "t1", block "b1"]) Nothing
    equal (f [block "b", track "t1", track "t2"])
        (Just (UiTest.bid "test/b", UiTest.tid "test/t2"))
    equal (f [block "b1", block "b2", track "t1", track "t2"])
        (Just (UiTest.bid "test/b2", UiTest.tid "test/t2"))
    equal (f [block "b1", track "t1", block "b2", track "t2"])
        (Just (UiTest.bid "test/b2", UiTest.tid "test/t2"))

test_to_ui :: Test
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

test_json :: Test
test_json = do
    let convert :: Stack.Frame -> Either String Stack.Frame
        convert = to_either . Aeson.fromJSON . Aeson.toJSON
        to_either (Aeson.Success a) = Right a
        to_either (Aeson.Error a) = Left a
        roundtrip a = (Right a, convert a)
    uncurry equal (roundtrip (block "a"))
    uncurry equal (roundtrip (track "a"))
    uncurry equal (roundtrip (region 1 2))
    uncurry equal (roundtrip (Stack.Call "a"))
    uncurry equal (roundtrip (Stack.Serial 1))

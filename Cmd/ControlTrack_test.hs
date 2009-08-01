module Cmd.ControlTrack_test where

import Util.Test
import qualified Cmd.ControlTrack as ControlTrack

test_parse = do
    let f = ControlTrack.parse
    equal (f "a,b") ("a", "b")
    equal (f "a,") ("a", "")
    equal (f "a") ("", "a")
    equal (f "") ("", "")

test_unparse = do
    let f = ControlTrack.unparse
    equal (f (Nothing, Nothing)) Nothing
    equal (f (Just "", Just "")) Nothing
    equal (f (Just "a", Just "")) (Just "a,")
    equal (f (Just "", Just "b")) (Just "b")
    equal (f (Just "a", Just "b")) (Just "a,b")

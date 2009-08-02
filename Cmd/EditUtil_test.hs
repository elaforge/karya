module Cmd.EditUtil_test where

import Util.Test

import qualified Ui.Key as Key
import qualified Cmd.Msg as Msg
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.Twelve as Twelve
import qualified Perform.Pitch as Pitch

import qualified Cmd.EditUtil as EditUtil


test_get_note = do
    let f = EditUtil.get_note Twelve.scale_id
    equal (f (Msg.KeyNumber (1, 2))) (Just (Right (Just (Pitch.Note "1d-"))))
    equal (f (Msg.KeyNumber (100, 0)))
        (Just (Left "note number 1200.0 is out of range 0--9"))
    equal (f (CmdTest.make_key True Key.Backspace)) (Just (Right Nothing))
    equal (f (CmdTest.make_key True (Key.KeyChar 'c'))) Nothing

test_modify_text = do
    let f k = EditUtil.modify_text (EditUtil.Key k)
    equal (EditUtil.modify_text (EditUtil.Note (Pitch.Note "abc")) "")
        (Just "*abc")
    equal (EditUtil.modify_text (EditUtil.Note (Pitch.Note "abc")) "a")
        (Just "a *abc")
    equal (f (Key.KeyChar 'c') "a") (Just "ac")
    equal (f (Key.KeyChar ' ') "a") (Just "a ")
    equal (f (Key.KeyChar ' ') "") (Just "")
    equal (f Key.Backspace "a") (Just "")
    equal (f Key.Backspace "") Nothing

test_backspace = do
    let f = EditUtil.backspace
    equal (f "") Nothing
    equal (f "a") (Just "")
    equal (f "a *hi") (Just "a")
    equal (f "a *hi *there") (Just "a *hi")

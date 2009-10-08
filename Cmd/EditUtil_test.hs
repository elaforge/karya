module Cmd.EditUtil_test where

import Util.Test

import qualified Ui.Key as Key
import qualified Perform.Pitch as Pitch

import qualified Cmd.EditUtil as EditUtil


test_modify_text_key = do
    let f c = EditUtil.modify_text_key (Key.KeyChar c)
    equal (f 'c' "a") (Just "ac")
    equal (f ' ' "a") (Just "a ")
    equal (f ' ' "") (Just "")

test_modify_text_note = do
    let f n = EditUtil.modify_text_note (Pitch.Note n)
    equal (f "abc" "") (Just "*abc")
    equal (f "abc" "a") (Just "a *abc")

test_backspace = do
    let f = EditUtil.backspace
    equal (f "") Nothing
    equal (f "a") (Just "")
    equal (f "a *hi") (Just "a")
    equal (f "a *hi *there") (Just "a *hi")

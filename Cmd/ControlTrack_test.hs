module Cmd.ControlTrack_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ControlTrack as ControlTrack


test_parse = do
    let f = ControlTrack.parse
    equal (f "a b") ("a", "b")
    equal (f "a") ("", "a")
    equal (f "") ("", "")

test_unparse = do
    let f = ControlTrack.unparse
    equal (f (Nothing, Nothing)) Nothing
    equal (f (Just "", Just "")) Nothing
    equal (f (Just "a", Just "")) (Just "a ")
    equal (f (Just "", Just "b")) (Just "b")
    equal (f (Just "a", Just "b")) (Just "a b")

test_cmd_val_edit = do
    let f pos msgs = fmap extract $ thread pos ControlTrack.cmd_val_edit msgs
        extract = UiTest.extract_tracks . fst
    equal (f 1 [CmdTest.m_note_on 60 60 100]) $ Right
        [("*", []), ("c", [(1, 0, ".787")])]
    equal (f 1 [CmdTest.m_control 60 "c" 100]) $ Right
        [("*", []), ("c", [(1, 0, ".787")])]

thread pos cmd msgs = CmdTest.thread_tracks [("*", []), ("c", [])] id
    (CmdTest.set_point_sel 2 pos : map cmd msgs)



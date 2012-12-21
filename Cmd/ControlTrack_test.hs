module Cmd.ControlTrack_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ControlTrack as ControlTrack


test_parse = do
    let f = ControlTrack.parse
        e = ControlTrack.Event
    equal (f "") $ e "" "" ""
    equal (f "a") $ e "" "a" ""
    equal (f "a ") $ e "a" "" ""
    equal (f "a b") $ e "a" "b" ""
    equal (f "a b c") $ e "a" "b" "c"
    equal (f "a (b ')z') c") $ e "a" "(b ')z')" "c"

test_unparse = do
    let f = ControlTrack.unparse
        e = ControlTrack.Event
    equal (f (e "" "" "")) ""
    equal (f (e "a" "" "")) "a "
    equal (f (e "" "b" "")) "b"
    equal (f (e "a" "b" "")) "a b"
    equal (f (e "a" "b" "c")) "a b c"

test_cmd_val_edit = do
    let f pos msgs = fmap extract $ thread pos ControlTrack.cmd_val_edit msgs
        extract = UiTest.extract_tracks . fst
    equal (f 1 [CmdTest.m_note_on 60 60 100]) $ Right
        [("*", []), ("c", [(1, 0, "`0x`c9")])]
    equal (f 1 [CmdTest.m_control 60 "c" 100]) $ Right
        [("*", []), ("c", [(1, 0, "`0x`c9")])]

thread pos cmd msgs = CmdTest.thread_tracks [("*", []), ("c", [])] id
    (CmdTest.set_point_sel 2 pos : map cmd msgs)

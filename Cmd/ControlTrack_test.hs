-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ControlTrack_test where
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
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
    let f = fmap extract . thread [] ControlTrack.cmd_val_edit
        extract = UiTest.extract_tracks . fst
    equal (f [CmdTest.m_note_on 60 60 100]) $ Right
        [("c", [(0, 0, "`0x`c9")])]
    equal (f [CmdTest.m_control 60 "c" 100]) $ Right
        [("c", [(0, 0, "`0x`c9")])]

test_cmd_tempo_val_edit = do
    let f events = fmap extract . thread events ControlTrack.cmd_tempo_val_edit
        extract = head . snd . head . UiTest.extract_tracks . fst
    equal (f [] (map CmdTest.key_down "1.2")) $
        Right (0, 0, "1.2")
    equal (f [(0, 0, "i ")] (map CmdTest.key_down "1.2")) $
        Right (0, 0, "i 1.2")

thread :: [UiTest.EventSpec] -> (a -> Cmd.CmdId Cmd.Status) -> [a]
    -> Either String (State.State, Cmd.State)
thread events cmd msgs =
    CmdTest.thread_tracks [("c", events)] id
        (CmdTest.set_point_sel tracknum 0 : map cmd msgs)
    where tracknum = 1

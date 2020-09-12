-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.ControlTrack_test where
import Util.Test
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ControlTrack as ControlTrack
import Global


test_parse :: Test
test_parse = do
    let f = ControlTrack.parse
        p = ControlTrack.Partial
    equal (f "") $ p [] "" "" [] ""
    equal (f "a") $ p [] "" "a" [] ""
    equal (f "a ") $ p [] "a" "" [] ""
    equal (f "a b") $ p [] "a" "b" [] ""
    equal (f "a b c") $ p [] "a" "b" ["c"] ""
    equal (f "a (b ')z') c") $ p [] "a" "(b ')z')" ["c"] ""
    equal (f "x=y | a b -- c") $ p [["x", "=", "y "]] "a" "b" [] "-- c"

test_unparse :: Test
test_unparse = do
    let f = ControlTrack.unparse
        p = ControlTrack.Partial
    equal (f (p [] "" "" [] "")) ""
    equal (f (p [] "a" "" [] "")) "a "
    equal (f (p [] "" "b" [] "")) "b"
    equal (f (p [] "a" "b" [] "")) "a b"
    equal (f (p [] "a" "b" ["c"] "")) "a b c"

test_parse_roundtrip :: Test
test_parse_roundtrip = do
    let f t = (t, ControlTrack.unparse $ ControlTrack.parse t)
    uncurry equal (f "a b")
    uncurry equal (f "a | b")
    uncurry equal (f "a | i 0 -- c")
    uncurry equal (f "i 0 1")

test_cmd_val_edit :: Test
test_cmd_val_edit = do
    let f events = fmap extract . thread events ControlTrack.cmd_val_edit
        extract = UiTest.extract_tracks . fst
    equal (f [] [CmdTest.m_note_on 60]) $
        Right [("c", [(0, 0, "`0x`ff")])]
    equal (f [] [CmdTest.m_control 60 "c" 0.5]) $
        Right [("c", [(0, 0, "`0x`80")])]
    equal (f [] (map CmdTest.key_down "a1")) $
        Right [("c", [(0, 0, "`0x`a1")])]
    equal (f [] (map CmdTest.key_down "1-")) $
        Right [("c", [(0, 0, "-`0x`01")])]
    equal (f [] (map CmdTest.key_down "1--")) $
        Right [("c", [(0, 0, "`0x`01")])]

    -- Both `0x` and 0x work.
    equal (f [(0, 0, "`0x`01")] (map CmdTest.key_down "4")) $
        Right [("c", [(0, 0, "`0x`14")])]
    equal (f [(0, 0, "0x01")] (map CmdTest.key_down "4")) $
        Right [("c", [(0, 0, "`0x`14")])]

    -- Duration is preserved.
    equal (f [(0, 1, "`0x`01")] (map CmdTest.key_down "4")) $
        Right [("c", [(0, 1, "`0x`14")])]

test_cmd_tempo_val_edit :: Test
test_cmd_tempo_val_edit = do
    let f events = fmap extract . thread events ControlTrack.cmd_tempo_val_edit
        extract = head . snd . head . UiTest.extract_tracks . fst
    equal (f [] (map CmdTest.key_down "1.2")) $
        Right (0, 0, "1.2")
    equal (f [(0, 0, "i ")] (map CmdTest.key_down "1.2")) $
        Right (0, 0, "i 1.2")

thread :: [UiTest.EventSpec] -> (a -> Cmd.CmdId Cmd.Status) -> [a]
    -> Either Text (Ui.State, Cmd.State)
thread events cmd msgs =
    CmdTest.thread_tracks [("c", events)] id
        (CmdTest.set_point_sel tracknum 0 : map cmd msgs)
    where tracknum = 1

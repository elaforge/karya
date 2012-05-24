module Cmd.PitchTrack_test where
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale.Twelve as Twelve
import qualified Perform.Pitch as Pitch


test_cmd_val_edit = do
    let run track_specs cmd = run_tracks track_specs cmd
        note = CmdTest.m_note_on 60 60 127
        f = PitchTrack.cmd_val_edit

    equal (run [("*", [])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f CmdTest.backspace)) $
        Right [("*", [])]

test_cmd_method_edit = do
    let run track_specs cmd = run_tracks track_specs cmd
        f key = PitchTrack.cmd_method_edit key
    equal (run [("*", [])] (f (CmdTest.key_down 'x'))) $
        Right [("*", [(0, 0, "x ")])]
    equal (run [("*", [(0, 0, "y")])] (f (CmdTest.key_down 'x'))) $
        Right [("*", [(0, 0, "x (y)")])]
    equal (run [("*", [(0, 0, "y")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "y")])]
    equal (run [("*", [(0, 0, "x (y)")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "y")])]
    equal (run [("*", [(0, 0, "x ")])] (f CmdTest.backspace)) $
        Right [("*", [])]

    equal (run [("*", [(0, 0, "x y")])] (f (CmdTest.key_down 'z'))) $
        Right [("*", [(0, 0, "z (x y)")])]
    equal (run [("*", [(0, 0, "z (x y)")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "x y")])]

    -- tab falls through, does not create an event with tab
    equal (run_tracks [] (f (CmdTest.make_key UiMsg.KeyDown Key.Tab))) $
        Left "aborted"

test_parse = do
    let f = PitchTrack.parse
    equal (f "f x") ("", "f x")
    equal (f "f (x) y") ("f", "(x) y")
    equal (f "f x (y)") ("f", "x (y)")
    equal (f "f x y") ("", "f x y")
    equal (f "f (x)") ("f", "(x)")
    equal (f "f ") ("f", "")

test_unparse = do
    let f = PitchTrack.unparse
    equal (f (Just "i", Just "4c")) (Just "i (4c)")
    equal (f (Just "", Just "(4c)")) (Just "4c")
    equal (f (Just "i", Just "4c .2")) (Just "i (4c .2)")
    equal (f (Just "", Just "(4c) .2")) (Just "4c")

test_modify_note = do
    let f = PitchTrack.modify_note
            (\n -> Just $ Pitch.Note $ "*" ++ Pitch.note_text n ++ "*")
    equal (f "x") (Just "*x*")
    equal (f "x (y)") (Just "x (*y*)")
    equal (f "x (y 1 2)") (Just "x (*y* 1 2)")
    equal (f "x y 1 2") (Just "*x* y 1 2")
    equal (PitchTrack.modify_note (const Nothing) "abc") Nothing
    -- Non-pitch calls are unchanged.
    equal (f "x = 'y'") (Just "x = 'y'")
    equal (f "x = (y)") (Just "x = (y)")

test_transpose = do
    let f octs steps event =
            PitchTrack.transpose Twelve.scale Nothing octs
                (Pitch.Chromatic steps) (UiTest.make_event event)
    equal (f 0 1 (0, 1, "4c")) (Just (UiTest.make_event (0, 1, "4c#")))
    equal (f 1 0 (0, 1, "4c")) (Just (UiTest.make_event (0, 1, "5c")))
    equal (f 20 0 (0, 1, "4c")) Nothing
    equal (f 1 0 (0, 1, "i (4c 5)"))
        (Just (UiTest.make_event (0, 1, "i (5c 5)")))


run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdId a
    -> Either String [UiTest.TrackSpec]
run_tracks track_specs cmd = CmdTest.trace_logs $ CmdTest.e_tracks $
    CmdTest.run_tracks track_specs $ CmdTest.set_sel 1 0 1 0 >> cmd

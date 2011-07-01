module Cmd.PitchTrack_test where
import Util.Test
import qualified Util.Log as Log

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale.Twelve as Twelve

import qualified Ui.Key as Key
import qualified Ui.UiTest as UiTest
import qualified Perform.Pitch as Pitch


run_tracks track_specs cmd = CmdTest.trace_logs $ CmdTest.e_tracks $
    CmdTest.run_tracks track_specs $ CmdTest.set_sel 1 0 1 0 >> cmd

extract (Right (Just Cmd.Done, tracks, logs)) = Log.trace_logs logs tracks
extract val = error $ "unexpected: " ++ show val

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
    equal (run_tracks [] (f (CmdTest.make_key True Key.Tab))) $
        Right []

test_parse = do
    let f = PitchTrack.parse
    equal (f "f x") ("", "f x")
    equal (f "f (x) y") ("f", "(x) y")
    equal (f "f x (y)") ("f", "x (y)")
    equal (f "f x y") ("", "f x y")
    equal (f "f (x)") ("f", "(x)")
    equal (f "f ") ("f", "")

test_modify_note = do
    let f = PitchTrack.modify_note
    equal (f Just "x") (Just "x")
    equal (f Just "x (y)") (Just "x (y)")
    let z = Just (Pitch.Note "z")
    equal (f (const z) "x (y)") (Just "x (z)")
    equal (f (const z) "x (y 1 2)") (Just "x (z 1 2)")
    equal (f (const z) "x y 1 2") (Just "z y 1 2")
    equal (f (const Nothing) "abc") Nothing
    equal (f (const (Just (Pitch.Note ""))) "a") (Just "")

test_transpose = do
    let f octs degs event = PitchTrack.transpose Twelve.scale octs degs
            (UiTest.mkevent event)
    equal (f 0 1 (0, 1, "4c")) (Just (UiTest.mkevent (0, 1, "4c#")))
    equal (f 1 0 (0, 1, "4c")) (Just (UiTest.mkevent (0, 1, "5c")))
    equal (f 20 0 (0, 1, "4c")) Nothing
    equal (f 1 0 (0, 1, "i (4c 5)")) (Just (UiTest.mkevent (0, 1, "i (5c 5)")))

module Cmd.PitchTrack_test where
import Util.Test
import qualified Util.Log as Log

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale.Twelve as Twelve

import qualified Ui.Key as Key


run_sel track_specs cmd = CmdTest.e_tracks $
    CmdTest.run_tracks track_specs $ CmdTest.set_sel 1 0 1 0 >> cmd

extract (Right (Just Cmd.Done, tracks, logs)) = Log.trace_logs logs tracks
extract val = error $ "unexpected: " ++ show val

test_cmd_val_edit = do
    let run track_specs cmd = run_sel track_specs cmd
        note = CmdTest.m_note_on 60 60 127
        f = PitchTrack.cmd_val_edit Twelve.scale_id

    equal (run [("*", [])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f CmdTest.backspace)) $
        Right [("*", [])]

test_cmd_method_edit = do
    let run track_specs cmd = run_sel track_specs cmd
        f key = PitchTrack.cmd_method_edit key
    equal (run [("*", [])] (f (CmdTest.key_down 'x'))) $
        Right [("*", [(0, 0, "x,")])]
    equal (run [("*", [(0, 0, "x")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "x")])]
    equal (run [("*", [(0, 0, "x,y")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "y")])]
    equal (run [("*", [(0, 0, "x,")])] (f CmdTest.backspace)) $
        Right [("*", [])]

    equal (run_sel [] (f (CmdTest.make_key True Key.Tab))) $
        Right []

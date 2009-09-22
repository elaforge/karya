module Cmd.NoteTrack_test where
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Key as Key

import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Cmd as Cmd
import qualified Derive.Scale.Twelve as Twelve

import qualified Cmd.NoteTrack as NoteTrack


mkkey = CmdTest.make_key True

run_sel track_specs cmd = CmdTest.run_tracks track_specs
    (CmdTest.with_sel (Block.point_selection 1 0) cmd)

extract (Right (Just Cmd.Done, tracks, [])) = tracks
extract val = error $ "unexpected: " ++ show val

test_cmd_raw_edit = do
    let f = NoteTrack.cmd_raw_edit Twelve.scale_id
        run track_specs cmd = extract $ run_sel track_specs cmd
    -- Created event has dur according to ruler.
    equal (run [(">i", [])] (f (CmdTest.m_note_on 60 60 127)))
        [(">i", [(0, 10, "*4c")])]
    equal (run [(">i", [])] (f (mkkey (Key.KeyChar ' '))))
        [(">i", [(0, 10, "")])]
    equal (run [(">i", [])] (f (mkkey (Key.KeyChar 'x'))))
        [(">i", [(0, 10, "x")])]
    equal (run [(">i", [(0, 5, "")])] (f (mkkey Key.Backspace)))
        [(">i", [])]

    -- Modified event keeps dur.
    equal (run [(">i", [(0, 5, "a")])] (f (mkkey Key.Backspace)))
        [(">i", [(0, 5, "")])]
    equal (run [(">i", [(0, 5, "a *a")])] (f (mkkey Key.Backspace)))
        [(">i", [(0, 5, "a")])]

test_cmd_val_edit = do
    let f = NoteTrack.cmd_val_edit (NoteTrack.PitchTrack True 2) Twelve.scale_id
        run track_specs cmd = extract $ run_sel track_specs cmd
        note = CmdTest.m_note_on 60 60 127
    -- create new track
    equal (run [(">i", [])] (f note))
        [(">i", [(0, 10, "")]), ("*", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note))
        [(">i", [(0, 10, "")]), ("*", [(0, 0, "4c")]), ("mod", [])]

    -- modify existing track
    let f = NoteTrack.cmd_val_edit
            (NoteTrack.PitchTrack False 2) Twelve.scale_id
        note_track = [(">i", [(0, 10, "")]), ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey Key.Backspace)))
        [(">i", []), ("*", [])]
    equal (run note_track (f note))
        [(">i", [(0, 10, "")]), ("*", [(0, 0, "4c")])]

test_cmd_method_edit = do
    let f = NoteTrack.cmd_method_edit (NoteTrack.PitchTrack False 2)
        run track_specs cmd = extract $ run_sel track_specs cmd
        inst = (">i", [(0, 10, "")])
        note_track = [inst, ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey (Key.KeyChar 'x'))))
        [inst, ("*", [(0, 0, "x,4d")])]
    equal (run [inst, ("*", [(0, 0, "x,4d")])] (f (mkkey Key.Backspace)))
        [inst, ("*", [(0, 0, "4d")])]

module Cmd.NoteTrack_test where
import Util.Test
import qualified Util.Log as Log

import Ui
import qualified Ui.Types as Types
import qualified Ui.Key as Key
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple
import qualified Derive.Scale.Twelve as Twelve

import qualified Cmd.NoteTrack as NoteTrack


mkkey = CmdTest.make_key True

run_sel track_specs cmd = CmdTest.run_tracks track_specs
    (CmdTest.with_sel (Types.point_selection 1 0) cmd)

extract (Right (Just Cmd.Done, tracks, logs)) = Log.trace_logs logs tracks
extract val = error $ "unexpected: " ++ show val

thread :: [UiTest.TrackSpec] -> Cmd.Cmd -> [Msg.Msg]
    -> ([(String, [Simple.Event])], (Types.TrackNum, TrackPos))
thread track_specs cmd msgs =
    extract_sel $ CmdTest.thread ustate CmdTest.default_cmd_state cmds
    where
    (_, ustate) = UiTest.run_mkview track_specs
    set_sel = CmdTest.with_sel (Types.point_selection 1 0) (return Cmd.Done)
    cmds = set_sel : map (cmd$) msgs

-- drop 1 is for set_sel above, and should really be in 'thread'.
extract_sel (Right (ustate, cstate)) = (UiTest.extract_tracks ustate,
        CmdTest.eval ustate cstate get_sel)
    where
    get_sel = do
        (_, tracknum, _, pos) <- Selection.get_insert
        return (tracknum, pos)
extract_sel val = error $ "unexpected: " ++ show val

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
    let create_track = NoteTrack.CreateTrack 1 "*new" 2
        run track_specs cmd = extract $ run_sel track_specs cmd
        note = CmdTest.m_note_on 60 60 127
    let f = NoteTrack.cmd_val_edit create_track Twelve.scale_id
    -- creates a new pitch track
    equal (run [(">i", [])] (f note))
        [(">i", [(0, 10, "")]), ("*new", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note))
        [(">i", [(0, 10, "")]), ("*new", [(0, 0, "4c")]), ("mod", [])]

    -- modify existing track
    let f = NoteTrack.cmd_val_edit
            (NoteTrack.ExistingTrack 2) Twelve.scale_id
        note_tracks = [(">i", [(0, 10, "")]), ("*", [(0, 0, "4d")])]
    -- both note and pitch get deleted
    equal (run note_tracks (f (mkkey Key.Backspace)))
        [(">i", []), ("*", [])]
    equal (run note_tracks (f note))
        [(">i", [(0, 10, "")]), ("*", [(0, 0, "4c")])]

    -- selection advances after final keyup
    let empty_tracks = [(">i", []), ("*", [])]
        on nn = CmdTest.m_note_on nn (fromIntegral nn) 127
        off nn = CmdTest.m_note_off nn 127
    equal (thread empty_tracks f [on 60, off 60])
        ([(">i", [(0, 10, "")]), ("*", [(0, 0, "4c")])], (1, 10))
    equal (thread empty_tracks f [on 60, on 61, off 60, off 61])
        ([(">i", [(0, 10, "")]), ("*", [(0, 0, "4c#")])], (1, 10))
    -- TODO later test chord input

test_cmd_method_edit = do
    let f = NoteTrack.cmd_method_edit (NoteTrack.ExistingTrack 2)
        run track_specs cmd = extract $ run_sel track_specs cmd
        inst = (">i", [(0, 10, "")])
        note_track = [inst, ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey (Key.KeyChar 'x'))))
        [inst, ("*", [(0, 0, "x,4d")])]
    equal (run [inst, ("*", [(0, 0, "x,4d")])] (f (mkkey Key.Backspace)))
        [inst, ("*", [(0, 0, "4d")])]

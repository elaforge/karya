module Cmd.NoteTrack_test where
import Util.Test
import Ui
import qualified Ui.Key as Key
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection
import qualified Cmd.Simple as Simple


mkkey = CmdTest.make_key UiMsg.KeyDown
run track_specs cmd = CmdTest.trace_logs $
    CmdTest.e_tracks $ CmdTest.run_sel 0 track_specs cmd

-- | Thread a bunch of msgs through the command and return the final state
-- and the selection position.
thread :: [UiTest.TrackSpec] -> Cmd.Cmd -> [Msg.Msg]
    -> ([(String, [Simple.Event])], (Types.TrackNum, ScoreTime))
thread track_specs cmd msgs =
    extract_sel $ CmdTest.thread ustate CmdTest.default_cmd_state cmds
    where
    (_, ustate) = UiTest.run_mkview track_specs
    cmds = (CmdTest.set_sel 1 0 1 0 >> return Cmd.Done) : map cmd msgs

-- drop 1 is for set_sel above, and should really be in 'thread'.
extract_sel (Right (ustate, cstate)) = (UiTest.extract_tracks ustate,
        CmdTest.eval ustate cstate get_sel)
    where
    get_sel = do
        (_, tracknum, _, pos) <- Selection.get_insert
        return (tracknum, pos)
extract_sel val = error $ "unexpected: " ++ show val

test_cmd_raw_edit = do
    let f = NoteTrack.cmd_raw_edit
    -- Created event has dur according to ruler.
    equal (run [(">i", [])] (f (CmdTest.m_note_on 60 60 127))) $
        Right [(">i", [(0, 1, "(4c)")])]
    -- Space creates a zero-dur note.
    equal (run [(">i", [])] (f (mkkey (Key.Char ' ')))) $
        Right [(">i", [(0, 0, "")])]
    equal (run [(">i", [])] (f (mkkey (Key.Char 'x')))) $
        Right [(">i", [(0, 1, "x")])]
    equal (run [(">i", [(0, 5, "")])] (f (mkkey Key.Backspace))) $
        Right [(">i", [])]

    -- Modified event keeps dur.
    equal (run [(">i", [(0, 5, "a")])] (f (mkkey Key.Backspace))) $
        Right [(">i", [(0, 5, "")])]

test_cmd_val_edit = do
    let create_track = NoteTrack.CreateTrack 1 2
        note = CmdTest.m_note_on 60 60 127
    let f = NoteTrack.cmd_val_edit create_track
    -- creates a new pitch track
    equal (run [(">i", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")]), ("mod", [])]

    -- modify existing track
    let f = NoteTrack.cmd_val_edit
            (NoteTrack.ExistingTrack 2 (UiTest.mk_tid 2))
        note_tracks = [(">i", [(0, 1, "")]), ("*", [(0, 0, "4d")])]
    -- both note and pitch get deleted
    equal (run note_tracks (f (mkkey Key.Backspace))) $
        Right [(">i", []), ("*", [])]
    equal (run note_tracks (f note)) $
        Right [(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")])]

    -- selection advances after final keyup
    let empty_tracks = [(">i", []), ("*", [])]
        on nn = CmdTest.m_note_on nn (fromIntegral nn) 127
        off nn = CmdTest.m_note_off nn 127
    equal (thread empty_tracks f [on 60, off 60])
        ([(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")])], (1, 1))
    equal (thread empty_tracks f [on 60, on 61, off 60, off 61]) $
        ([(">i", [(0, 1, "")]), ("*", [(0, 0, "4c#")])], (1, 1))
    -- TODO later test chord input

test_cmd_method_edit = do
    let f = NoteTrack.cmd_method_edit
            (NoteTrack.ExistingTrack 2 (UiTest.mk_tid 2))
        inst = (">i", [(0, 1, "")])
        note_track = [inst, ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey (Key.Char 'x')))) $
        Right [inst, ("*", [(0, 0, "x (4d)")])]
    equal (run [inst, ("*", [(0, 0, "x (4d)")])] (f (mkkey Key.Backspace))) $
        Right [inst, ("*", [(0, 0, "4d")])]

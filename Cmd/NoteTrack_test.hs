module Cmd.NoteTrack_test where
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import qualified Derive.TrackInfo as TrackInfo
import Types


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

test_cmd_val_edit_create = do
    let f = NoteTrack.cmd_val_edit
        note = CmdTest.m_note_on 60 60 127
    -- creates a new pitch track
    equal (run [(">i", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*twelve", [(0, 0, "4c")]), ("mod", [])]

test_cmd_val_edit_simple = do
    let f = NoteTrack.cmd_val_edit
    -- modify existing track
    let note_tracks = [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4d")])]
    -- both note and pitch get deleted
    equal (run note_tracks (f (mkkey Key.Backspace))) $
        Right [(">i", []), ("*", [])]
    -- pitch is changed, note text remains
    equal (run note_tracks (f (CmdTest.m_note_on 60 60 127))) $
        Right [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4c")])]

test_cmd_val_edit_advance = do
    -- Test advance mode.
    let f advance = extract . val_edit advance False [(">i", []), ("*", [])]
        extract (Right result) = (simplify result, extract_sel result)
        extract (Left err) = error $ "left: " ++ err
    let on nn = CmdTest.m_note_on nn (fromIntegral nn) 127
        off nn = CmdTest.m_note_off nn 127

    -- selection advances after each key
    equal (f True [on 60, off 60])
        ([(">i", [(0, 1, "4c")])], (1, 1))
    equal (f True [on 60, on 61, off 60, off 61]) $
        ([(">i", [(0, 1, "4c"), (1, 1, "4c#")])], (1, 2))

    -- selection doesn't advance at all
    equal (f False [on 60, off 60])
        ([(">i", [(0, 1, "4c")])], (1, 0))
    equal (f False [on 60, on 61, off 60, off 61]) $
        ([(">i", [(0, 1, "4c#")])], (1, 0))

    -- pitch changes retune the entered note
    let pitch nn = CmdTest.m_pitch (floor nn) nn
    equal (f False [on 60, pitch 60.5]) $
        ([(">i", [(0, 1, "4c 50")])], (1, 0))

    -- even if it advanced
    equal (f True [on 60, pitch 60.5]) $
        ([(">i", [(0, 1, "4c 50")])], (1, 1))

test_cmd_val_edit_chord = do
    let f advance tracks = val_edit advance True [(t, []) | t <- tracks]
        e_sel (Right result) = (simplify result, extract_sel result)
        e_sel (Left err) = error $ "left: " ++ err
        e_events = fmap simplify
    let on nn = CmdTest.m_note_on nn (fromIntegral nn) 127
        off nn = CmdTest.m_note_off nn 127

    -- chord mode advances on note off
    equal (e_sel $ f True [">", "*"] [on 60])
        ([(">", [(0, 1, "4c")])], (1, 0))
    equal (e_sel $ f True [">", "*"] [on 60, off 60])
        ([(">", [(0, 1, "4c")])], (1, 1))
    equal (e_sel $ f False [">", "*"] [on 60, off 60])
        ([(">", [(0, 1, "4c")])], (1, 0))

    -- creates the pitch track
    equal (e_events $ f False [">"] [on 60])
        (Right [(">", [(0, 1, "4c")])])
    left_like (e_events $ f False [">"] [on 60, on 61])
        "no next note track"
    -- both pitch tracks are created
    equal (e_events $ f False [">", ">"] [on 60, on 61])
        (Right [(">", [(0, 1, "4c")]), (">", [(0, 1, "4c#")])])
    left_like (e_events $ f False [">", ">"] [on 60, on 61, on 62])
        "no next note track"
    -- existing pitch track is reused
    equal (e_events $ f False [">", ">", "*"] [on 60, on 61])
        (Right [(">", [(0, 1, "4c")]), (">", [(0, 1, "4c#")])])

    -- won't spill onto another instrument's track
    left_like (e_events $ f False [">", ">i2"] [on 60, on 61])
        "no next note track"

    -- make sure things reset on key up
    equal (e_events $ f False [">", ">"] [on 60, on 61, off 60, off 61, on 62])
        (Right [(">", [(0, 1, "4d")]), (">", [(0, 1, "4c#")])])

    -- TODO test the instrument_of stuff.  I'd need testing infrastructure to
    -- set up the perf stuff.

val_edit :: Bool -> Bool -> [UiTest.TrackSpec] -> [Msg.Msg]
     -> Either String States
val_edit advance chord tracks msgs =
    thread tracks (mode advance chord) NoteTrack.cmd_val_edit msgs
    where
    mode advance chord st = st { Cmd.state_edit = (Cmd.state_edit st)
        { Cmd.state_advance = advance, Cmd.state_chord = chord } }

test_cmd_method_edit = do
    let f = NoteTrack.cmd_method_edit
        inst = (">i", [(0, 1, "")])
        note_track = [inst, ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey (Key.Char 'x')))) $
        Right [inst, ("*", [(0, 0, "x (4d)")])]
    equal (run [inst, ("*", [(0, 0, "x (4d)")])] (f (mkkey Key.Backspace))) $
        Right [inst, ("*", [(0, 0, "4d")])]

mkkey :: Key.Key -> Msg.Msg
mkkey = CmdTest.make_key UiMsg.KeyDown

run :: [UiTest.TrackSpec] -> Cmd.CmdId a
    -> Either String [(String, [(Double, Double, String)])]
run track_specs cmd = CmdTest.trace_logs $
    CmdTest.e_tracks $ CmdTest.run_sel 0 track_specs cmd

type States = (State.State, Cmd.State)

-- | Thread a bunch of msgs through the command and return the final state
-- and the selection position.
thread :: [UiTest.TrackSpec] -> (Cmd.State -> Cmd.State) -> Cmd.Cmd
    -> [Msg.Msg] -> Either String (State.State,  Cmd.State)
thread track_specs modify_cmd_state cmd msgs =
    CmdTest.thread ustate (modify_cmd_state CmdTest.default_cmd_state) cmds
    where
    (_, ustate) = UiTest.run_mkview track_specs
    cmds = (CmdTest.set_sel 1 0 1 0 >> return Cmd.Done) : map cmd msgs

simplify :: States -> [UiTest.TrackSpec]
simplify = simplify_tracks . UiTest.extract_tracks . fst

-- | [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
-- -> [(">", [(0, 1, "4c")])]
simplify_tracks :: [UiTest.TrackSpec] -> [UiTest.TrackSpec]
simplify_tracks tracks =
    case Seq.split_with (TrackInfo.is_note_track . fst) tracks of
        [] -> []
        [] : groups -> map simplify groups
        hd : _ ->
            error $ "simplify_tracks: extra tracks in front: " ++ show hd
    where
    simplify [(note, notes), (pitch, pitches)]
        | TrackInfo.is_note_track note && TrackInfo.is_pitch_track pitch =
            (note, combine "" notes pitches)
    simplify tracks = error $ "simplify_tracks: expected a note and a pitch: "
        ++ show tracks
    combine _ [] _ = []
    combine last_p notes [] = [(s, e, last_p) | (s, e, _) <- notes]
    combine last_p all_notes@((s, e, _) : notes)
            all_pitches@((s_p, _, next_p) : pitches)
        | s_p <= s = combine next_p all_notes pitches
        | otherwise = (s, e, last_p) : combine last_p notes all_pitches

extract_sel :: States -> (TrackNum, ScoreTime)
extract_sel (ustate, cstate) = CmdTest.eval ustate cstate get_sel
    where
    get_sel = do
        (_, tracknum, _, pos) <- Selection.get_insert
        return (tracknum, pos)

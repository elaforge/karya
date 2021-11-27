-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.NoteTrack_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import qualified Perform.NN as NN
import Global
import Types


test_cmd_val_edit_create :: Test
test_cmd_val_edit_create = do
    let f = NoteTrack.cmd_val_edit
        note = CmdTest.m_note_on NN.middle_c
    -- creates a new pitch track
    equal (run [(">i", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")]), ("mod", [])]

test_cmd_val_edit_simple :: Test
test_cmd_val_edit_simple = do
    let f = NoteTrack.cmd_val_edit
    let note_on = CmdTest.m_note_on NN.middle_c
    -- create pitch track
    equal (run [(">", [])] (f note_on)) $
        Right [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
    -- put pitch at the end for Negative
    equal (run [(">", [])] (Edit.cmd_toggle_note_orientation
            >> CmdTest.set_point_sel 1 1 >> f note_on)) $
        Right [(">", [(1, -1, "")]), ("*", [(1, -0, "4c")])]
    -- modify existing track
    let note_tracks = [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4d")])]
    -- both note and pitch get deleted
    equal (run note_tracks (f (mkkey Key.Backspace))) $
        Left "aborted" -- general Backspace handler will get it
    -- pitch is changed, note text remains
    equal (run note_tracks (f note_on)) $
        Right [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4c")])]

test_cmd_val_edit_advance :: Test
test_cmd_val_edit_advance = do
    -- Test advance mode.
    let f advance = extract . expect_right . val_edit advance False [">i", "*"]
        extract result = (simplify result, extract_sel result)
    let on = CmdTest.m_note_on
        off = CmdTest.m_note_off

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
    let pitch = CmdTest.m_pitch_change
    equal (f False [on 60, pitch 60 60.5]) $
        ([(">i", [(0, 1, "4c 50")])], (1, 0))

    -- even if it advanced
    equal (f True [on 60, pitch 60 60.5]) $
        ([(">i", [(0, 1, "4c 50")])], (1, 1))

test_cmd_val_edit_chord :: Test
test_cmd_val_edit_chord = do
    let f advance tracks = val_edit advance True tracks
        e_sel = (\r -> (simplify r, extract_sel r)) . expect_right
        e_events = fmap simplify
    let on = CmdTest.m_note_on
        off = CmdTest.m_note_off

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

test_cmd_val_edit_dyn :: Test
test_cmd_val_edit_dyn = do
    let f tracks msgs = fmap extract $ thread tracks set_dyn
            NoteTrack.cmd_val_edit msgs
        set_dyn st = st { Cmd.state_edit = (Cmd.state_edit st)
            { Cmd.state_record_velocity = True } }
        extract = UiTest.extract_tracks . fst
    let on vel = Msg.InputNote $ InputNote.NoteOn (InputNote.NoteId 60)
            (InputNote.nn_to_input 60) vel
    equal (f [">"] [on 1]) $ Right
        [ (">", [(0, 1, "")])
        , ("dyn", [(0, 0, "`0x`ff")])
        , ("*", [(0, 0, "4c")])
        ]
    equal (f [">", "dyn"] [on 0.5]) $ Right
        [ (">", [(0, 1, "")])
        , ("*", [(0, 0, "4c")])
        , ("dyn", [(0, 0, "`0x`80")])
        ]

val_edit :: Bool -> Bool -> [Text] -> [Msg.Msg] -> Either Text States
val_edit advance chord tracks msgs =
    thread tracks (mode advance chord) NoteTrack.cmd_val_edit msgs
    where
    mode advance chord st = st { Cmd.state_edit = (Cmd.state_edit st)
        { Cmd.state_advance = advance, Cmd.state_chord = chord } }

test_cmd_method_edit :: Test
test_cmd_method_edit = do
    let f = NoteTrack.cmd_method_edit
        inst = (">i", [(0, 1, "")])
        note_track = [inst, ("*", [(0, 0, "4d")])]
    equal (run note_track (f (mkkey (Key.Char 'x')))) $
        Right [inst, ("*", [(0, 0, "x (4d)")])]
    equal (run [inst, ("*", [(0, 0, "x (4d)")])] (f (mkkey Key.Backspace))) $
        Right [inst, ("*", [(0, 0, "4d")])]


-- * util

mkkey :: Key.Key -> Msg.Msg
mkkey = CmdTest.make_key UiMsg.KeyDown

run :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Either Text [UiTest.TrackSpec]
run track_specs cmd = CmdTest.trace_logs $
    CmdTest.e_tracks $ CmdTest.run_sel 0 track_specs cmd

type States = (Ui.State, Cmd.State)

-- | Thread a bunch of msgs through the command with the selection set to
-- (1, 0).
thread :: [Text] -> (Cmd.State -> Cmd.State)
    -> (Msg.Msg -> Cmd.CmdId Cmd.Status)
    -> [Msg.Msg] -> Either Text (Ui.State, Cmd.State)
thread tracks modify_cmd_state cmd msgs =
    CmdTest.thread_tracks [(t, []) | t <- tracks] modify_cmd_state
        (CmdTest.set_point_sel 1 0 : map cmd msgs)

simplify :: States -> [UiTest.TrackSpec]
simplify = simplify_tracks . UiTest.extract_tracks . fst

-- | [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
-- -> [(">", [(0, 1, "4c")])]
simplify_tracks :: [UiTest.TrackSpec] -> [UiTest.TrackSpec]
simplify_tracks tracks =
    case Seq.split_before (ParseTitle.is_note_track . fst) tracks of
        [] -> []
        [] : groups -> map simplify groups
        hd : _ -> error $ "extra tracks in front: " ++ show hd
    where
    simplify [(note, notes), (pitch, pitches)]
        | ParseTitle.is_note_track note && ParseTitle.is_pitch_track pitch =
            (note, combine "" notes pitches)
    simplify tracks = error $ "expected a note and a pitch: " ++ show tracks
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

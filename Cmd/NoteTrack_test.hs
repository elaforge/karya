-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.NoteTrack_test where
import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import qualified Perform.NN as NN
import qualified Ui.Key as Key
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_cmd_val_edit_create :: Test
test_cmd_val_edit_create = do
    let f = NoteTrack.cmdValEdit
        note = CmdTest.mNoteOn NN.middleC
    -- creates a new pitch track
    equal (run [(">i", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
    equal (run [(">i", []), ("mod", [])] (f note)) $
        Right [(">i", [(0, 1, "")]), ("*", [(0, 0, "4c")]), ("mod", [])]

test_cmd_val_edit_simple :: Test
test_cmd_val_edit_simple = do
    let f = NoteTrack.cmdValEdit
    let noteOn = CmdTest.mNoteOn NN.middleC
    -- create pitch track
    equal (run [(">", [])] (f noteOn)) $
        Right [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
    -- put pitch at the end for Negative
    equal (run [(">", [])] (Edit.cmdToggleNoteOrientation
            >> CmdTest.setPointSel 1 1 >> f noteOn)) $
        Right [(">", [(1, -1, "")]), ("*", [(1, -0, "4c")])]
    -- modify existing track
    let noteTracks = [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4d")])]
    -- both note and pitch get deleted
    equal (run noteTracks (f (mkkey Key.Backspace))) $
        Left "aborted" -- general Backspace handler will get it
    -- pitch is changed, note text remains
    equal (run noteTracks (f noteOn)) $
        Right [(">i", [(0, 1, "x")]), ("*", [(0, 0, "4c")])]

test_cmd_val_edit_advance :: Test
test_cmd_val_edit_advance = do
    -- Test advance mode.
    let f advance = extract . expectRight . valEdit advance False [">i", "*"]
        extract result = (simplify result, extractSel result)
    let on = CmdTest.mNoteOn
        off = CmdTest.mNoteOff

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

    -- TODO: doesn't work until scale_input_to_note returns an actual Expr,
    -- see ScaleDegre.pitch_expr.
    -- -- pitch changes retune the entered note
    -- let pitch = CmdTest.m_pitch_change
    -- equal (f False [on 60, pitch 60 60.5]) $
    --     ([(">i", [(0, 1, "4c 50")])], (1, 0))
    --
    -- -- even if it advanced
    -- equal (f True [on 60, pitch 60 60.5]) $
    --     ([(">i", [(0, 1, "4c 50")])], (1, 1))

test_cmd_val_edit_chord :: Test
test_cmd_val_edit_chord = do
    let f advance tracks = valEdit advance True tracks
        eSel = (\r -> (simplify r, extractSel r)) . expectRight
        eEvents = fmap simplify
    let on = CmdTest.mNoteOn
        off = CmdTest.mNoteOff

    -- chord mode advances on note off
    equal (eSel $ f True [">", "*"] [on 60])
        ([(">", [(0, 1, "4c")])], (1, 0))
    equal (eSel $ f True [">", "*"] [on 60, off 60])
        ([(">", [(0, 1, "4c")])], (1, 1))
    equal (eSel $ f False [">", "*"] [on 60, off 60])
        ([(">", [(0, 1, "4c")])], (1, 0))

    -- creates the pitch track
    equal (eEvents $ f False [">"] [on 60])
        (Right [(">", [(0, 1, "4c")])])
    leftLike (eEvents $ f False [">"] [on 60, on 61])
        "no next note track"
    -- both pitch tracks are created
    equal (eEvents $ f False [">", ">"] [on 60, on 61])
        (Right [(">", [(0, 1, "4c")]), (">", [(0, 1, "4c#")])])
    leftLike (eEvents $ f False [">", ">"] [on 60, on 61, on 62])
        "no next note track"
    -- existing pitch track is reused
    equal (eEvents $ f False [">", ">", "*"] [on 60, on 61])
        (Right [(">", [(0, 1, "4c")]), (">", [(0, 1, "4c#")])])

    -- won't spill onto another instrument's track
    leftLike (eEvents $ f False [">", ">i2"] [on 60, on 61])
        "no next note track"

    -- make sure things reset on key up
    equal (eEvents $ f False [">", ">"] [on 60, on 61, off 60, off 61, on 62])
        (Right [(">", [(0, 1, "4d")]), (">", [(0, 1, "4c#")])])

    -- TODO test the instrument_of stuff.  I'd need testing infrastructure to
    -- set up the perf stuff.

test_cmd_val_edit_dyn :: Test
test_cmd_val_edit_dyn = do
    let f tracks msgs = fmap extract $ thread tracks setDyn
            NoteTrack.cmdValEdit msgs
        setDyn st = st { Cmd.stateEdit = (Cmd.stateEdit st)
            { Cmd.stateRecordVelocity = True } }
        extract = UiTest.extractTracks . fst
    let on vel = Msg.InputNote $ InputNote.NoteOn (InputNote.NoteId 60)
            (InputNote.nnToInput 60) vel
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

valEdit :: Bool -> Bool -> [Text] -> [Msg.Msg] -> Either Text States
valEdit advance chord tracks msgs =
    thread tracks (mode advance chord) NoteTrack.cmdValEdit msgs
    where
    mode advance chord st = st { Cmd.stateEdit = (Cmd.stateEdit st)
        { Cmd.stateAdvance = advance, Cmd.stateChord = chord } }

test_cmd_method_edit :: Test
test_cmd_method_edit = do
    let f = NoteTrack.cmdMethodEdit
        inst = (">i", [(0, 1, "")])
        noteTrack = [inst, ("*", [(0, 0, "4d")])]
    equal (run noteTrack (f (mkkey (Key.Char 'x')))) $
        Right [inst, ("*", [(0, 0, "x (4d)")])]
    equal (run [inst, ("*", [(0, 0, "x (4d)")])] (f (mkkey Key.Backspace))) $
        Right [inst, ("*", [(0, 0, "4d")])]


-- * util

mkkey :: Key.Key -> Msg.Msg
mkkey = CmdTest.makeKey UiMsg.KeyDown

run :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Either Text [UiTest.TrackSpec]
run trackSpecs cmd = CmdTest.traceLogs $
    CmdTest.eTracks $ CmdTest.runSel 0 trackSpecs cmd

type States = (Ui.State, Cmd.State)

-- | Thread a bunch of msgs through the command with the selection set to
-- (1, 0).
thread :: [Text] -> (Cmd.State -> Cmd.State)
    -> (Msg.Msg -> Cmd.CmdId Cmd.Status)
    -> [Msg.Msg] -> Either Text (Ui.State, Cmd.State)
thread tracks modifyCmdState cmd msgs =
    CmdTest.threadTracks [(t, []) | t <- tracks] modifyCmdState
        (CmdTest.setPointSel 1 0 : map cmd msgs)

simplify :: States -> [UiTest.TrackSpec]
simplify = simplifyTracks . UiTest.extractTracks . fst

-- | [(">", [(0, 1, "")]), ("*", [(0, 0, "4c")])]
-- -> [(">", [(0, 1, "4c")])]
simplifyTracks :: [UiTest.TrackSpec] -> [UiTest.TrackSpec]
simplifyTracks tracks =
    case Lists.splitBefore (ParseTitle.isNoteTrack . fst) tracks of
        [] -> []
        [] : groups -> map simplify groups
        hd : _ -> error $ "extra tracks in front: " ++ show hd
    where
    simplify [(note, notes), (pitch, pitches)]
        | ParseTitle.isNoteTrack note && ParseTitle.isPitchTrack pitch =
            (note, combine "" notes pitches)
    simplify tracks = error $ "expected a note and a pitch: " ++ show tracks
    combine _ [] _ = []
    combine lastP notes [] = [(s, e, lastP) | (s, e, _) <- notes]
    combine lastP allNotes@((s, e, _) : notes)
            allPitches@((sP, _, nextP) : pitches)
        | sP <= s = combine nextP allNotes pitches
        | otherwise = (s, e, lastP) : combine lastP notes allPitches

extractSel :: States -> (TrackNum, ScoreTime)
extractSel (ustate, cstate) = CmdTest.eval ustate cstate getSel
    where
    getSel = do
        (_, tracknum, _, pos) <- Selection.getInsert
        pure (tracknum, pos)

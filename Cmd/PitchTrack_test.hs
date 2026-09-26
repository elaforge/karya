-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.PitchTrack_test where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.ControlTrack as ControlTrack
import qualified Cmd.PitchTrack as PitchTrack

import qualified Derive.Scale as Scale
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_cmd_val_edit :: Test
test_cmd_val_edit = do
    let run trackSpecs cmd = runTracks trackSpecs 0 0 cmd
        note = CmdTest.mNoteOn NN.middleC
        f = PitchTrack.cmdValEdit
    equal (run [("*", [])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f note)) $
        Right [("*", [(0, 0, "4c")])]
    equal (run [("*", [(0, 0, "5e-")])] (f CmdTest.backspace)) $
        Left "aborted" -- general Backspace handler will get it

test_cmd_method_edit :: Test
test_cmd_method_edit = do
    let run trackSpecs cmd = runTracks trackSpecs 0 0cmd
        f key = PitchTrack.cmdMethodEdit key
    equal (run [("*", [])] (f (CmdTest.keyDown 'x'))) $
        Right [("*", [(0, 0, "x ")])]
    equal (run [("*", [(0, 0, "y")])] (f (CmdTest.keyDown 'x'))) $
        Right [("*", [(0, 0, "x (y)")])]
    equal (run [("*", [(0, 0, "y")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "y")])]
    equal (run [("*", [(0, 0, "x (y)")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "y")])]
    equal (run [("*", [(0, 0, "x ")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "")])]

    equal (run [("*", [(0, 0, "x y")])] (f (CmdTest.keyDown 'z'))) $
        Right [("*", [(0, 0, "z (x y)")])]
    equal (run [("*", [(0, 0, "z (x y)")])] (f CmdTest.backspace)) $
        Right [("*", [(0, 0, "x y")])]

    -- tab falls through, does not create an event with tab
    equal (runTracks [] 0 0 (f (CmdTest.makeKey UiMsg.KeyDown Key.Tab))) $
        Left "aborted"

test_parse :: Test
test_parse = do
    let f = PitchTrack.parse
    let p = ControlTrack.Partial
    -- Uses parens to disambiguate between call and val vs. val with args.
    equal (f "4c 0") $ p [] "" "4c" ["0"] ""
    equal (f "i (4c)") $ p [] "i" "(4c)" [] ""
    equal (f "i (4c 0)") $ p [] "i" "(4c 0)" [] ""
    equal (f "i (4c 0) x") $ p [] "i" "(4c 0)" ["x"] ""
    equal (f "(4c)") $ p [] "" "(4c)" [] ""
    equal (f "4c") $ p [] "" "4c" [] ""
    equal (f "tr ") $ p [] "tr" "" [] ""
    equal (f "tr (4c) x -- c") $ p [] "tr" "(4c)" ["x "] "-- c"
    equal (f "tr -- c") $ p [] "tr" "" [] "-- c"

test_unparse :: Test
test_unparse = do
    let f = PitchTrack.unparse
    let p = ControlTrack.Partial
    equal (f $ p [] "tr" "(4c)" [] "") "tr (4c)"
    equal (f $ p [] "" "(4c)" [] "") "4c"
    equal (f $ p [] "" "(4c)" ["0"] "") "4c 0"
    equal (f $ p [] "tr" "" [] "") "tr "
    equal (f $ p [] "tr" "4c" [] "") "tr (4c)"
    equal (f $ p [] "tr" "4c" ["0"] "") "tr (4c 0)"
    equal (f $ p [] "tr" "" [] "-- c") "tr  -- c"
    equal (f $ p [] "tr" "4c" [] "-- c") "tr (4c) -- c"

test_parse_roundtrip :: Test
test_parse_roundtrip = do
    let f t = (t, PitchTrack.unparse $ PitchTrack.parse t)
    uncurry equal (f "a b")
    uncurry equal (f "a | b")
    uncurry equal (f "a | 4c 0")
    uncurry equal (f "tr (4c) 0")

test_modify_note :: Test
test_modify_note = do
    let f = PitchTrack.modifyNote
            (\n -> Right $ Pitch.Note $ "*" <> Pitch.noteText n <> "*")
    equal (f "x") (Right "*x*")
    equal (f "x (y)") (Right "x (*y*)")
    equal (f "x (y 1 2)") (Right "x (*y* 1 2)")
    equal (f "x y 1 2") (Right "*x* y 1 2")
    equal (PitchTrack.modifyNote (const (Left "blah")) "abc") (Left "blah")
    -- Non-pitch calls are unchanged.
    equal (f "x = 'y'") (Right "x = 'y'")
    equal (f "x = (y)") (Right "x = (y)")

test_transpose_selection :: Test
test_transpose_selection = do
    let f = PitchTrack.transposeSelection Scale.Chromatic
        run events = runTracks [("*", events)]
    equal (run [(0, 0, "4c")] 0 0 (f 0 1)) $ Right [("*", [(0, 0, "4c#")])]
    equal (run [(0, 0, "4c"), (1, 0, "5e")] 0 2 (f 1 0)) $ Right
        [("*", [(0, 0, "5c"), (1, 0, "6e")])]
    equal (run [(0, 0, "i (4c 5)")] 0 0 (f 0 1)) $ Right
        [("*", [(0, 0, "i (4c# 5)")])]
    -- Symbolic transposition is not limited, because it may ultimately be
    -- transposed back into a reasonable range.
    equal (run [(0, 0, "4c")] 0 0 (f 20 1))
        (Right [("*", [(0, 0, "24c#")])])


runTracks :: [UiTest.TrackSpec] -> ScoreTime -> ScoreTime -> Cmd.CmdId a
    -> Either Text [UiTest.TrackSpec]
runTracks trackSpecs selStart selEnd cmd =
    CmdTest.traceLogs $ CmdTest.eTracks $ CmdTest.runTracks trackSpecs $
        CmdTest.setSel 1 selStart 1 selEnd >> cmd

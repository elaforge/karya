-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BohlenPierce_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.State as State
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BohlenPierce as BP
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run ps = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("*bp", [(t, 0, p) | (t, p) <- times ps])
            , (">", [(t, 1, "") | (t, _) <- times ps])
            ]
            where times = zip (Seq.range_ 0 1)
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
        c = Pitch.middle_c_hz
    -- Ensure that the octave wraps at A.
    equalf 0.001 (run ["4a"]) ([Just c], [])
    equalf 0.001 (run ["4i"]) ([Just (c * 25/9)], [])
    equalf 0.001 (run ["5a"]) ([Just (c * 3)], [])

test_input_to_note = do
    let f = maybe "" Pitch.note_text
            . Scale.scale_input_to_note BP.absolute_scale Nothing
    let ascii oct = CmdTest.ascii_kbd . CmdTest.oct_pc oct
        piano oct = CmdTest.piano_kbd . CmdTest.oct_pc oct
    equal [f (ascii 4 pc) | pc <- [0..9]]
        ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "4h", "4i", "5a"]
    equal [f (piano 4 pc) | pc <- [0..6]]
        ["2a", "2b", "2c", "2d", "2e", "2f", "2g"]
    equal [f (piano 5 pc) | pc <- [0..6]]
        ["2h", "2i", "", "", "", "", ""]

ascii :: Theory.Octave -> Theory.PitchClass -> Pitch.Input
ascii oct pc = Pitch.Input Pitch.AsciiKbd
    (Theory.Pitch oct (Theory.Note pc 0)) 0

piano :: Theory.Octave -> Theory.PitchClass -> Pitch.Input
piano oct pc = Pitch.Input Pitch.PianoKbd
    (Theory.Pitch oct (Theory.Note pc 0)) 0

test_input_to_nn = do
    let f input = DeriveTest.eval State.empty $
            Scale.scale_input_to_nn BP.absolute_scale 0 input
    let ascii oct = CmdTest.ascii_kbd . CmdTest.oct_pc oct
    equalf 0.001 (f (ascii 4 0)) $ Right (Just NN.middle_c)
    let ratio = 25/21
    equalf 0.001 (f (ascii 4 1)) $
        Right $ Just $ Pitch.modify_hz (*ratio) NN.middle_c

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BohlenPierce_test where
import qualified Util.Lists as Lists
import Util.Test
import qualified Ui.Ui as Ui
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BohlenPierce as BP
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global


test_note_to_call :: Test
test_note_to_call = do
    let run ps = DeriveTest.extract extract $ DeriveTest.deriveTracks ""
            [ ("*bp", [(t, 0, p) | (t, p) <- times ps])
            , (">", [(t, 1, "") | (t, _) <- times ps])
            ]
            where times = zip (Lists.range_ 0 1)
        extract = fmap Pitch.nnToHz . Score.initialNn
        c = Pitch.middleCHz
    -- Ensure that the octave wraps at A.
    equalf 0.001 (run ["4a"]) ([Just c], [])
    equalf 0.001 (run ["4i"]) ([Just (c * 25/9)], [])
    equalf 0.001 (run ["5a"]) ([Just (c * 3)], [])

test_input_to_note :: Test
test_input_to_note = do
    let f = either pretty Pitch.noteText
            . Scale.scaleInputToNote BP.absoluteScale mempty
    let ascii oct = CmdTest.asciiKbd . CmdTest.octPc oct
        piano oct = CmdTest.pianoKbd . CmdTest.octPc oct
    equal [f (ascii 4 pc) | pc <- [0..9]]
        ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "4h", "4i", "5a"]
    equal [f (piano 4 pc) | pc <- [0..6]]
        ["2a", "2b", "2c", "2d", "2e", "2f", "2g"]
    equal [f (piano 5 pc) | pc <- [0..6]] $
        ["2h", "2i"] ++ replicate 5 "invalid input"

test_input_to_nn :: Test
test_input_to_nn = do
    let f input = second (first prettys) $ DeriveTest.eval Ui.empty $
            Scale.scaleInputToNn BP.absoluteScale 0 input
    let ascii oct = CmdTest.asciiKbd . CmdTest.octPc oct
    equalf 0.001 (f (ascii 4 0)) $ Right (Right NN.middleC)
    let ratio = 25/21
    equalf 0.001 (f (ascii 4 1)) $
        Right $ Right $ Pitch.modifyHz (*ratio) NN.middleC

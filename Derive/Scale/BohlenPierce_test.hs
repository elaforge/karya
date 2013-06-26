-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.BohlenPierce_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BohlenPierce as BP
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
    let f key input = Pitch.note_text
            <$> Scale.scale_input_to_note BP.absolute_scale
                (Pitch.Key <$> key) input
    -- equal (map (f Nothing) (Seq.range 0 5 1)) $
    --     map Just ["-1a", "-1a#", "-1b", "-1c", "-1d", "-1d#"]
    -- equal (f Nothing 0) (Just "4a")
    equal (f Nothing middle_a) (Just "4a")
    equal (f Nothing (middle_a + 2)) (Just "4b")
    -- equal (f Nothing (Pitch.middle_c + 1)) (Just "4a")


-- TODO should be middle_c, but I need to rework NoteEntry to get tritaves to
-- line up.
middle_a :: Pitch.InputKey
middle_a = Pitch.middle_c + 5

test_input_to_nn = do
    let f input = DeriveTest.eval State.empty $
            Scale.scale_input_to_nn BP.absolute_scale 0 input
    equalf 0.001 (f middle_a) $ Right (Just NN.middle_c)
    let ratio = 25/21
    equalf 0.001 (f (middle_a + 2)) $
        Right $ Just $ Pitch.modify_hz (*ratio) NN.middle_c

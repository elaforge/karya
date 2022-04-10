-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Harmonic_test where
import Util.Test
import qualified Derive.Scale.Harmonic as Harmonic
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Perform.NN as NN


test_note_to_call :: Test
test_note_to_call = do
    let run = ScaleTest.note_to_call "harmonic" ""
    equalf 0.01 (run ["五1", "四2"]) ([Just NN.c4, Just NN.c4], [])
    -- Support undertones, just for fun.
    equalf 0.01 (run ["五-1", "五-2", "五-4"])
        ([Just NN.c4, Just NN.c3, Just NN.c2], [])

test_input_to_note :: Test
test_input_to_note = do
    let f = ScaleTest.input_to_note scale mempty
        scale = ScaleTest.get_scale Harmonic.scales "harmonic"
    equal (map f [(1, pc, acc) | pc <- [0, 1], acc <- [0, 1]])
        ["一1", "一2", "一3", "一4"]

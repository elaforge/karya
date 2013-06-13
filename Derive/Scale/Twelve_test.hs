-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Twelve_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.NN as NN


test_note_to_nn = do
    let f p = DeriveTest.extract extract $
            DeriveTest.derive_tracks $ UiTest.note_track [(0, 1, p)]
        extract = Score.initial_nn
    equal (f "3b") ([Just 59], [])
    equal (f "4c") ([Just 60], [])
    equal (f "-1c") ([Nothing], [])
    equal (f "-1c#") ([Just 1], [])
    equal (f "-2b") ([Nothing], [])
    equal (f "0c") ([Just 12], [])
    equal (f "9f#") ([Just 126], [])
    equal (f "9g") ([Just 127], [])
    equal (f "9g#") ([Nothing], [])

test_relative = do
    let f key p = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [(">", [(0, 1, "")]), ("*twelve-r" ++ key, [(0, 0, p)])]
        extract = Score.initial_nn
    equal (f "" "4s") ([Just NN.c4], [])
    equal (f "" "4g") ([Just NN.e4], [])
    equal (f " | key = c-min" "4g") ([Just NN.ds4], [])
    equal (f " | key = d-maj" "4s") ([Just NN.d4], [])
    equal (f " | key = d#-maj" "4s") ([Just NN.ds4], [])

    equal (f " | key = c-min" "4p") ([Just NN.g4], [])
    equal (f " | key = c-min" "4p#") ([Just NN.gs4], [])
    equal (f " | key = c-min" "4d") ([Just NN.gs4], [])
    equal (f " | key = c-min" "4db") ([Just NN.g4], [])

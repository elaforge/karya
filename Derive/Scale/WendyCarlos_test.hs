-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.WendyCarlos_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import Global


test_pitches = do
    let run title = DeriveTest.extract DeriveTest.e_nns_old
            . DeriveTest.derive_tracks ("scale=alpha" <> title)
            . UiTest.note_track
    equal (run "" [(0, 1, "1a"), (1, 1, "1b")])
        ([[(0, 12)], [(1, 12.78)]], [])
    equal (run " | %a0-nn=1" [(0, 1, "1a"), (1, 1, "1b")])
        ([[(0, 13)], [(1, 13.78)]], [])

test_transpose_controls = do
    let f p = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks "scale=alpha" $
            UiTest.note_track [(0, 1, p)]
    equal (f "1a") ([Just 12], [])
    equal (f "%t-chrom=1 | -- 1a") ([Just 12.78], [])
    equal (f "%t-dia=1 | -- 1a") ([Just 12.78], [])
    equal (f "%t-oct=1 | -- 1a") ([Just 24], [])

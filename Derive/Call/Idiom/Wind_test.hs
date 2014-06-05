-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Idiom.Wind_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Idiom.Wind as Wind
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


title :: String
title = "import idiom.wind | fundamentals = (list (nn 7) (nn 10)) | wind-idiom"

test_wind = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks title
            . UiTest.note_track . mknotes
        extract e = (Score.event_start e,
            map (second Pitch.nn_to_hz) $ DeriveTest.e_nns e)
        mknotes ns = [(t, 1, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    equalf 0.01 (run ["set (nn 10)"]) ([(0, [(0, 10)])], [])

    -- f1: 7 14 21 28
    -- f2: 10 20 30 40
    -- f1 * h1 to f2 * h1, go to f2 * h1
    equalf 0.01 (run ["set (nn 7)", "set (nn 10)"])
        ([(0, [(0, 7), (1, 10)]), (1, [(1, 10)])], [])

    -- f1 * h2 + 1 to f2 * h3, so go to f2 * h2
    equalf 0.01 (run ["set (nn 15)", "set (nn 30)"])
        ([(0, [(0, 15), (1, 20)]), (1, [(1, 30)])], [])

    -- f1 * h2 to f1 * h3
    equalf 0.01 (run ["set (nn 14)", "set (nn 21)"])
        ([(0, [(0, 14), (1, 14)]), (1, [(1, 21)])], [])

test_find_harmonic = do
    let f = Wind.find_harmonic
    equal (f [7, 10] 7) $ Just (7, 1)
    equal (f [7, 10] 30) $ Just (10, 3)
    -- It wants to find a harmonic above, but accepts a bit of eta.
    let f2 = 10.0000002
    equal (f [7, f2] 30) $ Just (f2, 3)

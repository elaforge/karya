-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Idiom.Wind_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.C.Idiom.Wind as Wind
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


title :: Text
title =
    "import idiom.wind | fundamentals = (list (nn 70) (nn 100)) | wind-idiom"

test_wind = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks title
            . UiTest.note_track . mknotes
        extract e =
            ( Score.event_start e
            , map (second Pitch.nn_to_hz) $ DeriveTest.e_nns_old e
            )
        mknotes ns = [(t, 1, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    equalf 0.01 (run ["set (nn 100)"]) ([(0, [(0, 100)])], [])

    -- harmonics of f1: 70 140 210 280
    -- harmonics of f2: 10 20 30 40
    -- f1 * h1 to f2 * h1, go to f2 * h1
    equalf 0.01 (run ["set (nn 70)", "set (nn 100)"])
        ([(0, [(0, 70), (1, 100)]), (1, [(1, 100)])], [])

    -- f1 * h2 + 1 to f2 * h3, so go to f2 * h2
    equalf 0.01 (run ["set (nn 150)", "set (nn 300)"])
        ([(0, [(0, 150), (1, 200)]), (1, [(1, 300)])], [])

    -- f1 * h2 to f1 * h3
    equalf 0.01 (run ["set (nn 140)", "set (nn 210)"])
        ([(0, [(0, 140), (1, 140)]), (1, [(1, 210)])], [])

test_find_harmonic = do
    let f = Wind.find_harmonic
    equal (f [7, 10] 7) $ Just (7, 1)
    equal (f [7, 10] 30) $ Just (10, 3)
    -- It wants to find a harmonic above, but accepts a bit of eta.
    let f2 = 10.0000002
    equal (f [7, f2] 30) $ Just (f2, 3)

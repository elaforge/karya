-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Speed_test where
import qualified Derive.Call.Speed as Speed
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Signal as Signal
import qualified Ui.Ui as Ui

import           Types
import           Util.Test


test_starts :: Test
test_starts = do
    let f speed range include_end =
            DeriveTest.eval Ui.empty (Speed.starts speed range include_end)
        score_control = DeriveT.ControlSignal . ScoreT.Typed ScoreT.Score
            . Signal.constant
    equal (f (DeriveT.constant_control 1) (1 :: RealTime, 4) True)
        (Right [1, 2, 3, 4])
    equal (f (score_control 1) (1 :: RealTime, 4) True) (Right [1, 2, 3, 4])
    equal (f (score_control 2) (0 :: RealTime, 2) False)
        (Right [0, 0.5, 1, 1.5])
    let s = 4 * 2/3 :: RealTime
    -- Float imprecision doesn't cause the end to be omitted.
    equalf 0.0001 (f (DeriveT.constant_control 12) (s, 4.375 * 2/3) True)
        (Right [s, s + 1/12, s + 2/12, s + 3/12])
    equalf 0.0001 (f (score_control 12) (s, 4.375 * 2/3) True)
        (Right [s, s + 1/12, s + 2/12, s + 3/12])

test_duration_starts :: Test
test_duration_starts = do
    let f = Speed.duration_starts
    let s = 4 * 2/3 :: RealTime
    -- Float imprecision doesn't cause the end to be omitted.
    equal (f (const (1/12)) s (s + 3/12))
        (Right [s, s + 1/12, s + 2/12, s + 3/12])
    equal (f (\t -> if t < 4 then 1 else 2) 1 8) (Right [1, 2, 3, 4, 6, 8])
    left_like (f (\t -> if t < 4 then 1 else 0) 1 8) "duration <= 0"

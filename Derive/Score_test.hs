-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Score_test where
import qualified Data.Map as Map

import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Signal as Signal

import           Util.Test


test_move :: Test
test_move = do
    let event = Score.empty_event
            { Score.event_start = 2
            , Score.event_controls = Map.fromList
                [("c", ScoreT.untyped $ Signal.from_pairs [(2, 2)])]
            , Score.event_pitch = PSignal.from_pairs [(2, PSignal.nn_pitch 42)]
            }
        signal = ScoreT.untyped . Signal.from_pairs
    let moved = Score.move (+2) event
    equal (Score.event_start moved) 4
    equal (Score.control_at 2 "c" moved) (Just (ScoreT.untyped 0))
    equal (Score.control_at 4 "c" moved) (Just (ScoreT.untyped 2))
    equal (fmap Signal.to_pairs <$> Score.event_control "c" moved)
        (Just $ ScoreT.untyped [(4, 2)])
    equal (Score.event_control "d" $
            Score.set_control "d" (signal [(0, 1)]) moved)
        (Just $ signal [(0, 1)])
    equal (Score.nn_at 2 moved) Nothing
    equal (Score.nn_at 4 moved) (Just 42)

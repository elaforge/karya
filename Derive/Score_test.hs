-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Score_test where
import qualified Data.Map as Map

import qualified Derive.Controls as Controls
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT

import qualified Perform.Signal as Signal

import           Util.Test


test_move :: Test
test_move = do
    let event = Score.set_control "c" (signal [(2, 2)]) $ Score.empty_event
            { Score.event_start = 2
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

test_modify_dynamic :: Test
test_modify_dynamic = do
    let event = Score.modify_dynamic (/2) $
            Score.set_dynamic 0.5 Score.empty_event
    equal (map (fmap DeriveT.constant_val) $
            Env.to_list $ Score.event_environ event)
        [ (ScoreT.control_name Controls.dynamic, Just (ScoreT.untyped 0.25))
        , (EnvKey.dynamic_val, Just (ScoreT.untyped 0.25))
        ]
    equal (Score.initial_dynamic event) 0.25

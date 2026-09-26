-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Score_test where
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
    let event = Score.setControl "c" (signal [(2, 2)]) $ Score.emptyEvent
            { Score.eventStart = 2
            , Score.eventPitch = PSignal.fromPairs [(2, PSignal.nnPitch 42)]
            }
        signal = ScoreT.untyped . Signal.fromPairs
    let moved = Score.move (+2) event
    equal (Score.eventStart moved) 4
    equal (Score.controlAt 2 "c" moved) (Just (ScoreT.untyped 0))
    equal (Score.controlAt 4 "c" moved) (Just (ScoreT.untyped 2))
    equal (fmap Signal.toPairs <$> Score.eventControl "c" moved)
        (Just $ ScoreT.untyped [(4, 2)])
    equal (Score.eventControl "d" $
            Score.setControl "d" (signal [(0, 1)]) moved)
        (Just $ signal [(0, 1)])
    equal (Score.nnAt 2 moved) Nothing
    equal (Score.nnAt 4 moved) (Just 42)

test_modify_dynamic :: Test
test_modify_dynamic = do
    let event = Score.modifyDynamic (/2) $
            Score.setDynamic 0.5 Score.emptyEvent
    equal (map (fmap DeriveT.constantVal) $
            Env.toList $ Score.eventEnviron event)
        [ (ScoreT.controlName Controls.dynamic, Just (ScoreT.untyped 0.25))
        , (EnvKey.dynamicVal, Just (ScoreT.untyped 0.25))
        ]
    equal (Score.initialDynamic event) 0.25

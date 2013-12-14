-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Speed_test where
import Util.Test
import qualified Ui.State as State
import qualified Derive.Call.Speed as Speed
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


test_starts = do
    let f speed range =
            DeriveTest.eval State.empty (Speed.starts speed range True)
        score_control = TrackLang.ControlSignal . Score.Typed Score.Score
            . Signal.constant
    let s = 4 * 2/3 :: RealTime
    -- Make sure float imprecision doesn't cause the end to be omitted.
    equalf 0.0001 (f (TrackLang.constant_control 12) (s, 4.375 * 2/3))
        (Right [s, s + 1/12, s + 2/12, s + 3/12])
    equalf 0.0001 (f (score_control 12) (s, 4.375 * 2/3))
        (Right [s, s + 1/12, s + 2/12, s + 3/12])

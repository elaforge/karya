-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Patch.Lib.Util_test where
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import           Util.Test


test_dynEnvelope :: Test
test_dynEnvelope = do
    let f dyn = Signal.to_pairs $ Util.dynEnvelope 0.25 1 $
            Note.withControl Control.dynamic (Signal.from_pairs dyn) $
            Note.testNote 4 4
    equal (f []) [(8, 0), (9, 0)]
    equal (f [(0, 1), (100, 1)]) [(0, 1), (8, 1), (9, 0)]
    equal (f [(4, 0), (8, 1)]) [(4, 0.25), (8, 1), (9, 0)]
    equal (f [(4, 0), (8, 0.5)]) [(4, 0.25), (8, 0.625), (9, 0)]


test_dynamicAutoScale :: Test
test_dynamicAutoScale = do
    let f dynRange (low, high) =
            map (Util.dynamicAutoScale dynRange (low, high))
                [low, (low+high) / 2, high]
    equal (f (0, 1) (0, 1)) [0, 0.5, 1]
    -- sample covers half the range, so it's only modified by dynRange by 0.5.
    equal (f (0, 1) (0, 0.5)) [0.25, 0.5, 0.75]
    equal (f (0, 1) (0.5, 1)) [0.25, 0.5, 0.75]
    equal (f (0, 1) (1, 1)) [0.5, 0.5, 0.5]
    -- half dynRange
    equal (f (0.5, 1) (0, 1)) [0.5, 0.75, 1]
    equal (f (0.5, 1) (1, 1)) [0.75, 0.75, 0.75]

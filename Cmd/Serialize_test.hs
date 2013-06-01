-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Serialize_test where
import qualified Util.Serialize
import Util.Test
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import Types


test_serialize = do
    let (_, state) =
            UiTest.run_mkview [("track", [(0, 1, "e0"), (1, 1, "e1")])]
    let run f = (f state, recode (f state))
    uncurry equal $ run State.state_config
    uncurry equal $ run State.state_views
    uncurry equal $ run State.state_blocks
    uncurry equal $ run State.state_tracks
    uncurry equal $ run State.state_rulers

test_negative_zero = do
    -- make sure negative zero is encoded properly
    equal (recode (0 :: ScoreTime)) 0
    equal (recode (-0 :: ScoreTime)) (-0.0)

recode :: (Util.Serialize.Serialize a) => a -> a
recode = either error id . Util.Serialize.decode . Util.Serialize.encode

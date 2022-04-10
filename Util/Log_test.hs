-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Log_test where
import qualified Control.Monad.Identity as Identity

import qualified Util.Log as Log
import Util.Test
import qualified Ui.Id as Id
import qualified Derive.Stack as Stack


test_serialize :: Test
test_serialize = do
    let roundtrip = Log.deserialize . Log.serialize
    let msg = Log.msg Log.Notice (Just stack) "hi"
        stack = Stack.from_outermost
            [ Stack.Block block_id, Stack.Track track_id, Stack.Region 1 2
            , Stack.Call "call"
            ]
        block_id = Id.BlockId $ Id.read_id "foo/bar"
        track_id = Id.TrackId $ Id.read_id "t/t2"
    equal (roundtrip msg) (Right msg)

test_id :: Test
test_id = do
    let run_id = Identity.runIdentity . Log.run
    let logs = snd $ run_id $ do
        Log.debug "oh noes"
    equal (map Log.msg_text logs) ["oh noes"]

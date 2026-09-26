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
        stack = Stack.fromOutermost
            [ Stack.Block blockId, Stack.Track trackId, Stack.Region 1 2
            , Stack.Call "call"
            ]
        blockId = Id.BlockId $ Id.readId "foo/bar"
        trackId = Id.TrackId $ Id.readId "t/t2"
    equal (roundtrip msg) (Right msg)

test_id :: Test
test_id = do
    let runId = Identity.runIdentity . Log.run
    let logs = snd $ runId $ do
        Log.debug "oh noes"
    equal (map Log.msgText logs) ["oh noes"]

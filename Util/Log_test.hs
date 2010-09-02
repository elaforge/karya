module Util.Log_test where
import qualified Control.Monad.Identity as Identity

import Util.Test
import qualified Util.Log as Log


test_id = do
    let run_id = Identity.runIdentity . Log.run
    let v = snd $ run_id $ do
        Log.debug "oh noes"
    pprint v

test_io = do
    let run_io = Log.run
    (_, logs) <- run_io $ do
        Log.warn "oh noes"
    pprint logs
    mapM_ Log.write logs

-- test_io :: IO ()
-- test_io = do
--     debug "oh noes"
--     debug_srcpos (Just ("hi", Just "test_io", 4)) "oh noes"
-- 
-- test_logt :: IO ()
-- test_logt = do
--     let (val, msgs) = Identity.runIdentity . run $ do
--         debug "oh noes"
--         debug "and again"
--     mapM_ write msgs

module Util.Log_test where
import qualified Control.Monad.Identity as Identity

import Util.Test
import Util.Log

test_io :: IO ()
test_io = do
    debug "oh noes"
    debug_srcpos (Just ("hi", Just "test_io", 4)) "oh noes"

test_logt :: IO ()
test_logt = do
    let (val, msgs) = Identity.runIdentity . run $ do
        debug "oh noes"
        debug "and again"
    mapM_ write msgs

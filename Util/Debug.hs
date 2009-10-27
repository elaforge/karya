module Util.Debug where

import qualified Debug.Trace as Trace
import qualified Util.Log as Log


trace :: String -> b -> b
trace = Trace.trace

traces :: (Show a) => a -> b -> b
traces a b = trace (show a) b

trace_ret :: (Show a, Show b) => a -> b -> b
trace_ret a ret = trace (show a ++ " -> " ++ show ret) ret

tracev :: (Show a) => a -> a
tracev x = Trace.trace ("**trace: " ++ show x) x

tracem :: (Show a) => String -> a -> a
tracem msg x = Trace.trace ("**" ++ msg ++ ": " ++ show x) x

traceM :: (Show a, Monad m) => a -> m ()
traceM msg = trace ("** " ++ show msg) (return ())

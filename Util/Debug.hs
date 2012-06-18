module Util.Debug (
    trace, traces, tracef, trace_ret, tracem, traceM, traceMs
) where

import qualified Debug.Trace as Trace
import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq


trace :: String -> b -> b
trace s = Trace.trace (prefix ++ s)

-- | Print a showable value.
traces :: (Show a) => a -> b -> b
traces a b = trace (pshow a) b

-- | Print a value prefixed with a msg.
tracem :: (Show a) => String -> a -> a
tracem msg x = trace (msg ++ ": " ++ pshow x) x

-- | Print a value in a monad.  The monad will force it to be printed.
traceM :: (Show a, Monad m) => a -> m ()
traceM val = trace (pshow val) (return ())

traceMs :: (Monad m) => String -> m ()
traceMs msg = trace msg (return ())

-- | Print a value after applying a function to it.
tracef :: (Show b) => (a -> b) -> a -> a
tracef f val = trace (pshow (f val)) val

-- | Trace input and output of a function.
trace_ret :: (Show a, Show b) => a -> b -> b
trace_ret a ret = trace (pshow a ++ " -> " ++ pshow ret) ret

prefix = "** "

pshow :: (Show a) => a -> String
pshow = Seq.strip . PPrint.pshow

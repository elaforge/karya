module Util.Debug (
    -- * non-monadic
    trace, traces, tracef, trace_ret, tracem, traceM, traceMs
    -- * IO
    , puts, put, putp
) where
import qualified Control.Monad.Trans as Trans
import qualified Debug.Trace as Trace

import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
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
traceM :: (Show a, Monad m) => String -> a -> m ()
traceM msg val = Trace.trace (prefix ++ msg ++ ": " ++ pshow val) (return ())

traceMs :: (Monad m) => String -> m ()
traceMs msg = trace msg (return ())

-- | Print a value after applying a function to it.
tracef :: (Show b) => (a -> b) -> a -> a
tracef f val = trace (pshow (f val)) val

-- | Trace input and output of a function.
trace_ret :: (Show a, Show b) => a -> b -> b
trace_ret a ret = trace (pshow a ++ " -> " ++ pshow ret) ret

prefix :: String
prefix = "** "

pshow :: (Show a) => a -> String
pshow = Seq.strip . PPrint.pshow


-- These are like putStrLn, but more easily greppable.

puts :: (Trans.MonadIO m) => String -> m ()
puts = Trans.liftIO . putStrLn . (prefix++)

put :: (Trans.MonadIO m, Show a) => String -> a -> m ()
put msg = Trans.liftIO . putStrLn . ((prefix ++ msg ++ ": ") ++) . pshow

putp :: (Trans.MonadIO m, Pretty.Pretty a) => String -> a -> m ()
putp msg = Trans.liftIO . putStrLn . ((prefix ++ msg ++ ": ") ++)
    . Pretty.formatted

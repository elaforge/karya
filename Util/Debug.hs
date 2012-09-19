module Util.Debug (
    -- * forced by evaluation
    trace, tracep, traces
    , tracef, trace_ret
    -- * forced by monad
    , traceM, tracepM, tracesM
    -- in IO
    , puts, put, putp
) where
import qualified Control.Monad.Trans as Trans
import qualified Debug.Trace as Trace
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq


-- * forced by evaluation

trace :: (Show a) => String -> a -> a
trace msg val = Trace.trace (with_msg msg (pshow val)) val

tracep :: (Pretty.Pretty a) => String -> a -> a
tracep msg val = Trace.trace (with_msg msg (Pretty.formatted val)) val

-- | Print a showable value.
traces :: String -> a -> a
traces =  Trace.trace . (prefix++)

-- | Print a value after applying a function to it.
tracef :: (Show b) => (a -> b) -> a -> a
tracef f val = traces (pshow (f val)) val

-- | Trace input and output of a function.
trace_ret :: (Show a, Show b) => String -> a -> b -> b
trace_ret function a ret = traces (function ++ " " ++ pa ++ arrow ++ pret) ret
    where
    arrow = if '\n' `elem` pa || '\n' `elem` pret then "\t\t=>\n" else " => "
    pa = pshow a
    pret = pshow ret

-- * forced by monad

-- | Print a value in a monad.  The monad will force it to be printed.
traceM :: (Show a, Monad m) => String -> a -> m ()
traceM msg val = Trace.trace (with_msg msg (pshow val)) (return ())

tracepM :: (Pretty.Pretty a, Monad m) => String -> a -> m ()
tracepM msg val = Trace.trace (with_msg msg (Pretty.formatted val)) (return ())

tracesM :: (Monad m) => String -> m ()
tracesM msg = Trace.trace msg (return ())

-- * in IO
-- These are like putStrLn, but more easily greppable.

puts :: (Trans.MonadIO m) => String -> m ()
puts = put_line . (prefix++)

put :: (Trans.MonadIO m, Show a) => String -> a -> m ()
put msg = put_line . (with_msg msg) . pshow

putp :: (Trans.MonadIO m, Pretty.Pretty a) => String -> a -> m ()
putp msg = put_line . (with_msg msg) . Pretty.formatted

put_line :: (Trans.MonadIO m) => String -> m ()
put_line s = Trans.liftIO $ do
    putStrLn s
    IO.hFlush IO.stdout


-- * implementation

with_msg :: String -> String -> String
with_msg msg text = Seq.strip $ prefix ++ msg ++ ": " ++ text

prefix :: String
prefix = "** "

pshow :: (Show a) => a -> String
pshow = Seq.strip . PPrint.pshow

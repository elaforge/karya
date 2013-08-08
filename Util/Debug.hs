-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Debug (
    full, fullM
    -- * forced by evaluation
    , trace, tracep, traces, traceps
    , tracef, tracefp, trace_ret, trace_retp
    -- * forced by monad
    , traceM, tracepM, tracesM
    -- in IO
    , puts, put, putp
) where
import qualified Control.Monad.Trans as Trans
import qualified Data.Monoid as Monoid
import qualified Debug.Trace as Trace
import qualified System.IO as IO

import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq


-- | Only apply the function if the val is non-mempty.  Useful for the trace
-- family.
full :: (Eq a, Monoid.Monoid a) => (msg -> a -> a) -> msg -> a -> a
full f msg val
    | val == Monoid.mempty = val
    | otherwise = f msg val

-- | Like 'full' but useful for the traceM and put family.
fullM :: (Monad m, Eq a, Monoid.Monoid a) =>
    (msg -> a -> m ()) -> msg -> a -> m ()
fullM f msg val
    | val == Monoid.mempty = return ()
    | otherwise = f msg val

-- * forced by evaluation

-- | Print a showable value en passant.
trace :: (Show a) => String -> a -> a
trace msg val = traces msg val val

-- | Pretty print a value en passant.
tracep :: (Pretty.Pretty a) => String -> a -> a
tracep msg val = Trace.trace (with_msg msg (Pretty.formatted val)) val

-- | Print a showable value.
traces :: (Show b) => String -> b -> a -> a
traces msg val = Trace.trace (with_msg msg (pshow val))

-- | Pretty print a value.
traceps :: (Pretty.Pretty b) => String -> b -> a -> a
traceps msg traced = Trace.trace (with_msg msg (Pretty.formatted traced))

-- | Print a value after applying a function to it.
tracef :: (Show b) => String -> (a -> b) -> a -> a
tracef msg f val = Trace.trace (with_msg msg (pshow (f val))) val

tracefp :: (Pretty.Pretty b) => String -> (a -> b) -> a -> a
tracefp msg f val = Trace.trace (with_msg msg (Pretty.pretty (f val))) val

-- | Trace input and output of a function.
trace_ret :: (Show a, Show b) => String -> a -> b -> b
trace_ret function a ret =
    trace_str (function ++ " " ++ pa ++ arrow ++ pret) ret
    where
    arrow = if '\n' `elem` pa || '\n' `elem` pret then "\t\t=>\n" else " => "
    pa = pshow a
    pret = pshow ret

trace_retp :: (Pretty.Pretty a, Pretty.Pretty b) => String -> a -> b -> b
trace_retp function a ret =
    trace_str (function ++ " " ++ pa ++ arrow ++ pret) ret
    where
    arrow = if '\n' `elem` pa || '\n' `elem` pret then "\t\t=>\n" else " => "
    pa = Pretty.formatted a
    pret = Pretty.formatted ret

trace_str :: String -> a -> a
trace_str = Trace.trace . (prefix++)

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
put msg = put_line . with_msg msg . pshow

putp :: (Trans.MonadIO m, Pretty.Pretty a) => String -> a -> m ()
putp msg = put_line . with_msg msg . Pretty.formatted

put_line :: (Trans.MonadIO m) => String -> m ()
put_line s = Trans.liftIO $ do
    putStrLn s
    IO.hFlush IO.stdout


-- * implementation

with_msg :: String -> String -> String
with_msg msg text_ =
    prefix ++ msg ++ (if multiline then ":\n" else ": ") ++ text
    where
    text = Seq.strip text_
    multiline = Seq.count '\n' text > 2

prefix :: String
prefix = "** "

pshow :: (Show a) => a -> String
pshow = Seq.strip . PPrint.pshow

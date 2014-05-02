-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Debug (
    activate, deactivate
    , full, fullM
    -- * forced by evaluation
    , trace, tracep, traces, tracesp
    , tracef, tracefp, trace_ret, trace_retp
    , trace_str
    -- * forced by monad
    , traceM, tracepM, tracesM
    -- in IO
    , puts, put, putp
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad.Trans as Trans
import qualified Data.IORef as IORef
import qualified Data.Monoid as Monoid
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq


{-# NOINLINE active #-}
active :: IORef.IORef (Maybe IO.Handle)
active = Unsafe.unsafePerformIO (IORef.newIORef (Just IO.stdout))

-- | Write debug msgs to the given handle.
activate :: IO.Handle -> IO ()
activate hdl = IORef.writeIORef active (Just hdl)

-- | Suppress debug msgs.
deactivate :: IO ()
deactivate = IORef.writeIORef active Nothing

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
tracep msg val = write (with_msg msg (Pretty.formatted val)) val

-- | Print a showable value.
traces :: (Show b) => String -> b -> a -> a
traces msg val = write (with_msg msg (pshow val))

-- | Pretty print a value.
tracesp :: (Pretty.Pretty b) => String -> b -> a -> a
tracesp msg traced = write (with_msg msg (Pretty.formatted traced))

-- | Print a value after applying a function to it.
tracef :: (Show b) => String -> (a -> b) -> a -> a
tracef msg f val = write (with_msg msg (pshow (f val))) val

tracefp :: (Pretty.Pretty b) => String -> (a -> b) -> a -> a
tracefp msg f val = write (with_msg msg (Pretty.pretty (f val))) val

-- | Trace input and output of a function.
trace_ret :: (Show a, Show b) => String -> a -> b -> b
trace_ret function a ret = trace_str text ret
    where
    text = concat
        [ function
        , if multiline then "\n" else " "
        , pa
        , if multiline then "\n\t\t=>\n" else " => "
        , pret
        ]
    multiline = '\n' `elem` pa || '\n' `elem` pret
    pa = pshow a
    pret = pshow ret

trace_retp :: (Pretty.Pretty a, Pretty.Pretty b) => String -> a -> b -> b
trace_retp function a ret = trace_str text ret
    where
    text = concat
        [ function
        , if multiline then "\n" else " "
        , pa
        , if multiline then "\n\t\t=>\n" else " => "
        , pret
        ]
    multiline = '\n' `elem` pa || '\n' `elem` pret
    pa = Seq.strip $ Pretty.formatted a
    pret = Seq.strip $ Pretty.formatted ret

-- | Show a raw string, equivalent to 'Debug.Trace.trace'.
trace_str :: String -> a -> a
trace_str = write . (prefix++)

-- * forced by monad

-- | Print a value in a monad.  The monad will force it to be printed.
traceM :: (Show a, Monad m) => String -> a -> m ()
traceM msg val = write (with_msg msg (pshow val)) (return ())

tracepM :: (Pretty.Pretty a, Monad m) => String -> a -> m ()
tracepM msg val = write (with_msg msg (Pretty.formatted val)) (return ())

tracesM :: (Monad m) => String -> m ()
tracesM msg = write msg (return ())

-- * in IO
-- These are like putStrLn, but more easily greppable.

puts :: (Trans.MonadIO m) => String -> m ()
puts = writeIO . (prefix++)

put :: (Trans.MonadIO m, Show a) => String -> a -> m ()
put msg = writeIO . with_msg msg . pshow

putp :: (Trans.MonadIO m, Pretty.Pretty a) => String -> a -> m ()
putp msg = writeIO . with_msg msg . Pretty.formatted


-- * implementation

write :: String -> a -> a
write msg val = msg `DeepSeq.deepseq` Unsafe.unsafePerformIO
    (writeIO msg >> return val)
    -- deepseq to prevent debug msgs from being interleaved.

writeIO :: Trans.MonadIO m => String -> m ()
writeIO msg = Trans.liftIO $ IORef.readIORef active >>= \x -> case x of
    Nothing -> return ()
    Just hdl -> IO.hPutStrLn hdl msg

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

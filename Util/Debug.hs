-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
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
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Util.CallStack as CallStack
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty


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
full :: (Eq a, Monoid a) => (a -> a) -> a -> a
full f val
    | val == mempty = val
    | otherwise = f val

-- | Like 'full' but useful for the traceM and put family.
fullM :: (Monad m, Eq a, Monoid a) => (a -> m()) -> a -> m ()
fullM f val
    | val == mempty = return ()
    | otherwise = f val

-- * forced by evaluation

-- | Print a showable value en passant.
trace :: (CallStack.Stack, Show a) => Text -> a -> a
trace msg val = traces msg val val

-- | Pretty print a value en passant.
tracep :: (CallStack.Stack, Pretty.Pretty a) => Text -> a -> a
tracep msg val = write (with_msg msg (Pretty.formatted val)) val

-- | Print a showable value.
traces :: (CallStack.Stack, Show b) => Text -> b -> a -> a
traces msg val = write (with_msg msg (pshow val))

-- | Pretty print a value.
tracesp :: (CallStack.Stack, Pretty.Pretty b) => Text -> b -> a -> a
tracesp msg traced = write (with_msg msg (Pretty.formatted traced))

-- | Print a value after applying a function to it.
tracef :: (CallStack.Stack, Show b) => Text -> (a -> b) -> a -> a
tracef msg f val = write (with_msg msg (pshow (f val))) val

tracefp :: (CallStack.Stack, Pretty.Pretty b) => Text -> (a -> b) -> a -> a
tracefp msg f val = write (with_msg msg (Pretty.formatted (f val))) val

-- | Trace input and output of a function.
trace_ret :: (CallStack.Stack, Show a, Show b) => Text -> a -> b -> b
trace_ret function a ret = trace_str text ret
    where
    text = Monoid.mconcat
        [ function
        , if multiline then "\n" else " "
        , pa
        , if multiline then "\n\t\t=>\n" else " => "
        , pret
        ]
    multiline = "\n" `Text.isInfixOf` pa || "\n" `Text.isInfixOf` pret
    pa = pshow a
    pret = pshow ret

trace_retp :: (CallStack.Stack, Pretty.Pretty a, Pretty.Pretty b) =>
    Text -> a -> b -> b
trace_retp function a ret = trace_str text ret
    where
    text = Monoid.mconcat
        [ function
        , if multiline then "\n" else " "
        , pa
        , if multiline then "\n\t\t=>\n" else " => "
        , pret
        ]
    multiline = "\n" `Text.isInfixOf` pa || "\n" `Text.isInfixOf` pret
    pa = Text.strip $ Pretty.formatted a
    pret = Text.strip $ Pretty.formatted ret

-- | Show a raw string, equivalent to 'Debug.Trace.trace'.
trace_str :: CallStack.Stack => Text -> a -> a
trace_str = write . (prefix<>)

-- * forced by monad

-- | Print a value in a monad.  The monad will force it to be printed.
traceM :: (CallStack.Stack, Show a, Monad m) => Text -> a -> m ()
traceM msg val = write (with_msg msg (pshow val)) (return ())

tracepM :: (CallStack.Stack, Pretty.Pretty a, Monad m) => Text -> a -> m ()
tracepM msg val = write (with_msg msg (Pretty.formatted val)) (return ())

tracesM :: Monad m => Text -> m ()
tracesM msg = write msg (return ())

-- * in IO
-- These are like putStrLn, but more easily greppable.

puts :: (CallStack.Stack, Trans.MonadIO m) => Text -> m ()
puts = writeIO . (prefix<>)

put :: (CallStack.Stack, Trans.MonadIO m, Show a) => Text -> a -> m ()
put msg = writeIO . with_msg msg . pshow

putp :: (CallStack.Stack, Trans.MonadIO m, Pretty.Pretty a) => Text -> a -> m ()
putp msg = writeIO . with_msg msg . Pretty.formatted


-- * implementation

{-# NOINLINE write #-}
write :: Text -> a -> a
write msg val = msg `DeepSeq.deepseq`
    Unsafe.unsafePerformIO (writeIO msg >> return val)
    -- deepseq to prevent debug msgs from being interleaved.

writeIO :: Trans.MonadIO m => Text -> m ()
writeIO msg = Trans.liftIO $ IORef.readIORef active >>= \x -> case x of
    Nothing -> return ()
    Just hdl -> Text.IO.hPutStrLn hdl msg

with_msg :: CallStack.Stack => Text -> Text -> Text
with_msg msg text_ =
    prefix <> msg <> (if multiline then ":\n" else ": ") <> text
    where
    text = Text.strip text_
    multiline = Text.count "\n" text > 2

prefix :: CallStack.Stack => Text
prefix = "** " <> CallStack.getStack <> ": "

pshow :: Show a => a -> Text
pshow = Text.strip . Text.pack . PPrint.pshow

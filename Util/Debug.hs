-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
module Util.Debug (
    full, fullM
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
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans as Trans

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import           GHC.Stack (HasCallStack)
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Timeout as Timeout

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import           Util.Pretty (Pretty)


-- | Only apply the function if the val is non-mempty.  Useful for the trace
-- family.
full :: (Eq a, Monoid a) => (a -> a) -> a -> a
full f val
    | val == mempty = val
    | otherwise = f val

-- | Like 'full' but useful for the traceM and put family.
fullM :: (Monad m, Eq a, Monoid a) => (a -> m ()) -> a -> m ()
fullM f val
    | val == mempty = return ()
    | otherwise = f val

-- * forced by evaluation

-- | Print a showable value en passant.
trace :: (HasCallStack, Show a) => Text -> a -> a
trace msg val = traces msg val val

-- | Pretty print a value en passant.
tracep :: (HasCallStack, Pretty a) => Text -> a -> a
tracep msg val = write (with_msg msg (Pretty.formatted val)) val

-- | Print a showable value.
traces :: (HasCallStack, Show b) => Text -> b -> a -> a
traces msg val = write (with_msg msg (pshow val))

-- | Pretty print a value.
tracesp :: (HasCallStack, Pretty b) => Text -> b -> a -> a
tracesp msg traced = write (with_msg msg (Pretty.formatted traced))

-- | Print a value after applying a function to it.
tracef :: (HasCallStack, Show b) => Text -> (a -> b) -> a -> a
tracef msg f val = write (with_msg msg (pshow (f val))) val

tracefp :: (HasCallStack, Pretty b) => Text -> (a -> b) -> a -> a
tracefp msg f val = write (with_msg msg (Pretty.formatted (f val))) val

-- | Trace input and output of a function.
trace_ret :: (HasCallStack, Show a, Show b) => Text -> a -> b -> b
trace_ret function a ret = trace_str text ret
    where
    text = mconcat
        [ function
        , if multiline then "\n" else " "
        , pa
        , if multiline then "\n\t\t=>\n" else " => "
        , pret
        ]
    multiline = "\n" `Text.isInfixOf` pa || "\n" `Text.isInfixOf` pret
    pa = pshow a
    pret = pshow ret

trace_retp :: (HasCallStack, Pretty a, Pretty b) =>
    Text -> a -> b -> b
trace_retp function a ret = trace_str text ret
    where
    text = mconcat
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
trace_str :: HasCallStack => Text -> a -> a
trace_str = write . (prefix<>)

-- * forced by monad

-- | Print a value in a monad.  The monad will force it to be printed.
traceM :: (HasCallStack, Show a, Monad m) => Text -> a -> m ()
traceM msg val = write (with_msg msg (pshow val)) (return ())

tracepM :: (HasCallStack, Pretty a, Monad m) => Text -> a -> m ()
tracepM msg val = write (with_msg msg (Pretty.formatted val)) (return ())

tracesM :: (HasCallStack, Monad m) => Text -> m ()
tracesM msg = write msg (return ())

-- * in IO
-- These are like putStrLn, but more easily greppable.

puts :: (HasCallStack, Trans.MonadIO m) => Text -> m ()
puts = writeIO . (prefix<>)

put :: (HasCallStack, Trans.MonadIO m, Show a) => Text -> a -> m ()
put msg = writeIO . with_msg msg . pshow

putp :: (HasCallStack, Trans.MonadIO m, Pretty a) => Text -> a -> m ()
putp msg = writeIO . with_msg msg . Pretty.formatted


-- * implementation

{-# NOINLINE write #-}
write :: HasCallStack => Text -> a -> a
write msg val = Unsafe.unsafePerformIO $ writeIO msg >> return val

writeIO :: (HasCallStack, Trans.MonadIO m) => Text -> m ()
writeIO msg = Trans.liftIO $ do
    -- deepseq to prevent debug msgs from being interleaved.
    ok <- timeout 1 $ Exception.evaluate (DeepSeq.deepseq msg msg)
    -- I could catch exceptions, but I don't want to catch async exceptions.
    -- `Exception.catch` \(exc :: Exception.SomeException) ->
    --     return $ prefix <> "<exception: " <> Text.pack (show exc) <> ">"
    Log.with_stdio_lock $ case ok of
        Nothing ->
            Text.IO.hPutStrLn IO.stdout $ prefix <> "<evalutaion timed out>"
        Just msg -> Text.IO.hPutStrLn IO.stdout msg

timeout :: Double -> IO a -> IO (Maybe a)
timeout = Timeout.timeout . to_usec
    where to_usec = round . (*1000000)

with_msg :: HasCallStack => Text -> Text -> Text
with_msg msg text_ =
    prefix <> msg <> (if multiline then ":\n" else ": ") <> text
    where
    text = Text.strip text_
    multiline = Text.count "\n" text > 2

prefix :: HasCallStack => Text
prefix = "** " <> CallStack.getStack1 <> ": "

pshow :: Show a => a -> Text
pshow = Text.strip . Text.pack . PPrint.pshow

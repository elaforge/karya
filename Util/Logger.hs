{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
-- | LoggerT is a specialization of WriterT which only supports 'record', and
-- uses 'DList' for efficient appends.
module Util.Logger where
import Prelude hiding (log)
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer as Writer
import qualified Data.DList as DList


type LoggerM w m = Writer.WriterT (DList.DList w) m
newtype Monad m => LoggerT w m a = LoggerT (LoggerM w m a)
    deriving (Functor, Monad, Trans.MonadIO)
run_logger_t (LoggerT x) = x

-- | Record a msg to the log.
record :: (Monad m) => w -> LoggerT w m ()
record = LoggerT . Writer.tell . DList.singleton

run :: Monad m => LoggerT w m a -> m (a, [w])
run m = do
    (val, msgs) <- (Writer.runWriterT . run_logger_t) m
    return (val, DList.toList msgs)

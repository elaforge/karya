{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{- | A monad like WriterT, but intended for logging.

    WriterT is not actually all that good for logging because its (>>=) is not
    tail recursive.
-}
module Util.Logger (
    LoggerT, run, exec, MonadLogger(..), logs
) where

import Prelude hiding (log)
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer as Writer
import qualified Data.Monoid as Monoid


-- | This uses a plain list for now.  DList is not actually very efficient for
-- appends because appending nil doesn't strictly eliminate the nil.
newtype LoggerT w m a = LoggerT { runLoggerT :: State.StateT [w] m a }
    deriving (Functor, Monad, Trans.MonadTrans, Trans.MonadIO,
        Error.MonadError e, Reader.MonadReader r)

run :: (Monad m) => LoggerT w m a -> m (a, [w])
run m = do
    (result, logs) <- State.runStateT (runLoggerT m) []
    return (result, reverse logs)

exec :: (Monad m) => LoggerT w m a -> m [w]
exec m = return . snd =<< run m

class (Monad m) => MonadLogger w m | m -> w where
    log :: w -> m ()
    peek :: m [w]

logs :: (MonadLogger w m) => [w] -> m ()
logs msgs = mapM_ log msgs

instance (Monad m) => MonadLogger w (LoggerT w m) where
    log msg = LoggerT $ do
        ms <- State.get
        State.put $! (msg:ms)
    peek = LoggerT $ do
        ms <- State.get
        return $! reverse ms

-- | I think I can't automatically derive this because LoggerT itself is
-- a StateT.
instance (State.MonadState s m) => State.MonadState s (LoggerT w m) where
    get = Trans.lift State.get
    put = Trans.lift . State.put

-- mtl instances

instance (MonadLogger w m) => MonadLogger w (State.StateT s m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance (Error.Error e, MonadLogger w m) =>
        MonadLogger w (Error.ErrorT e m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance (MonadLogger w m) => MonadLogger w (Reader.ReaderT r m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance (Monoid.Monoid w, MonadLogger log m) =>
        MonadLogger log (Writer.WriterT w m) where
    log = Trans.lift . log
    peek = Trans.lift peek

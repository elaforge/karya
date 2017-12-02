-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-} -- Monad.Error
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{- | A monad like WriterT, but intended for logging.

    WriterT is not actually all that good for logging because its (>>=) is not
    tail recursive.
-}
module Util.Logger (
    LoggerT, Logger, run, runId, exec, MonadLogger(..), logs
    , mapLogs
) where
import Prelude hiding (log)
import qualified Control.Applicative as Applicative
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Writer as Writer

import qualified Data.Monoid as Monoid


-- | This uses a plain list for now.  DList is not actually very efficient for
-- appends because appending nil doesn't strictly eliminate the nil.
newtype LoggerT w m a = LoggerT { runLoggerT :: Strict.StateT [w] m a }
    deriving (Applicative.Applicative, Functor, Monad, Trans.MonadTrans,
        Trans.MonadIO, Except.MonadError e, Reader.MonadReader r)

type Logger w a = LoggerT w Identity.Identity a

run :: Monad m => LoggerT w m a -> m (a, [w])
run m = do
    (result, logs) <- Strict.runStateT (runLoggerT m) []
    return (result, reverse logs)

runId :: Logger w a -> (a, [w])
runId = Identity.runIdentity . run

exec :: Monad m => LoggerT w m a -> m [w]
exec m = return . snd =<< run m

class Monad m => MonadLogger w m | m -> w where
    log :: w -> m ()
    peek :: m [w]

logs :: (MonadLogger w m) => [w] -> m ()
logs = mapM_ log

mapLogs :: (w -> w) -> Logger w a -> Logger w a
mapLogs f (LoggerT m) = LoggerT $ Strict.withState (map f) m

instance Monad m => MonadLogger w (LoggerT w m) where
    log msg = LoggerT $ do
        ms <- Strict.get
        Strict.put $! (msg:ms)
    peek = LoggerT $ do
        ms <- Strict.get
        return $! reverse ms

-- | I think I can't automatically derive this because LoggerT itself is
-- a StateT.
instance (Strict.MonadState s m) => Strict.MonadState s (LoggerT w m) where
    get = Trans.lift Strict.get
    put = Trans.lift . Strict.put

-- mtl instances

instance MonadLogger w m => MonadLogger w (Strict.StateT s m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance MonadLogger w m => MonadLogger w (Lazy.StateT s m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance (Error.Error e, MonadLogger w m) =>
        MonadLogger w (Error.ErrorT e m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance MonadLogger w m => MonadLogger w (Except.ExceptT e m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance MonadLogger w m => MonadLogger w (Reader.ReaderT r m) where
    log = Trans.lift . log
    peek = Trans.lift peek

instance (Monoid.Monoid w, MonadLogger log m) =>
        MonadLogger log (Writer.WriterT w m) where
    log = Trans.lift . log
    peek = Trans.lift peek

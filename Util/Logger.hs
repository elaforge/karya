{-# LANGUAGE FlexibleContexts #-} -- needed for record type sig
{- | LoggerT is a specialization of WriterT which only supports 'record', and
    uses 'DList' for efficient appends.
-}
module Util.Logger where
import qualified Control.Monad.Writer as Writer
import qualified Data.DList as DList


-- | Doing this as a type instead of newtype means I don't have to write
-- instances for all the other monads.
type LoggerT w m = Writer.WriterT (DList.DList w) m
run :: (Monad m) => LoggerT w m a -> m (a, [w])
run m = do
    (val, msgs) <- Writer.runWriterT m
    return (val, DList.toList msgs)

record :: (Writer.MonadWriter (DList.DList w) m) => w -> m ()
record = Writer.tell . DList.singleton

record_list :: (Writer.MonadWriter (DList.DList w) m) => [w] -> m ()
record_list = Writer.tell . DList.fromList

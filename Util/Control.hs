{- | Control flow and monadic utilities.
-}
module Util.Control (
    (<$>), (<*>), (<*), (*>), (<|>)
    , first, second
    , (<>), mempty
    , while, while_
    , whenM, when_just, ifM

    , finally
) where
import Control.Monad
import qualified Control.Monad.Error.Class as Error
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<|>))
import qualified Control.Applicative as Applicative
import qualified Data.Monoid as Monoid
import Data.Monoid (mempty)
import qualified Text.ParserCombinators.Parsec as P


-- | Like the Arrow combinators, but specialized to functions for clearer
-- error messages.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (a -> b) -> (c, a) -> (c, b)
second f (c, a) = (c, f a)

(<>) :: (Monoid.Monoid a) => a -> a -> a
(<>) = Monoid.mappend

while :: (Monad m) => m Bool -> m a -> m [a]
while cond op = do
    b <- cond
    case b of
        True -> do
            val <- op
            rest <- while cond op
            return (val:rest)
        False -> return []

while_ :: (Monad m) => m Bool -> m a -> m ()
while_ cond op = do
    b <- cond
    if b then op >> while_ cond op else return ()

whenM :: (Monad m) => m Bool -> m a -> m ()
whenM cond op = do
    b <- cond
    if b then op >> return () else return ()

when_just :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
when_just val f = maybe (return ()) f val

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM cond consequent alternative = do
    b <- cond
    if b then consequent else alternative


-- Parsec2 doesn't have these, but parsec3 does.
instance Applicative.Applicative (P.GenParser s a) where
    pure = return
    (<*>) = ap

instance Applicative.Alternative (P.GenParser s a) where
    empty = mzero
    (<|>) = mplus

-- | Finally a finally for MonadError.
finally :: (Error.MonadError e m) => m a -> m () -> m a
finally action handler =
    Error.catchError (action >>= \v -> handler >> return v) $
        \exc -> handler >> Error.throwError exc

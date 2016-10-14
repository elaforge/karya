-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE Rank2Types #-}
-- | This is the core of the Deriver monad, instantiated in detail in
-- "Derive.Deriver.Monad".
module Derive.Deriver.DeriveM (
    Deriver, RunResult, run, write
    , throw, modify, get, gets, put
) where
import qualified Control.Applicative as Applicative

import qualified Util.Log as Log


newtype Deriver st err a = Deriver
    { runD :: forall r. st -> [Log.Msg] -> Failure st err r
        -> Success st err a r -> RunResult st err r
    }

type Failure st err r = st -> [Log.Msg] -> err -> RunResult st err r
type Success st err a r = st -> [Log.Msg] -> a -> RunResult st err r
type RunResult st err a = (Either err a, st, [Log.Msg])

run :: st -> Deriver st err a -> RunResult st err a
run st m = runD m st []
    (\st logs err -> (Left err, st, reverse logs))
    (\st logs a -> (Right a, st, reverse logs))

write :: Log.Msg -> Deriver st err ()
write msg = Deriver $ \st logs _ win -> win st (msg:logs) ()

{-# INLINE pureC #-}
pureC :: a -> Deriver st err a
pureC a = Deriver $ \st logs _ win -> win st logs a

{-# INLINE bindC #-}
bindC :: Deriver st err a -> (a -> Deriver st err b) -> Deriver st err b
bindC m f = Deriver $ \st1 logs1 lose win ->
    runD m st1 logs1 lose (\st2 logs2 a -> runD (f a) st2 logs2 lose win)

{-# INLINE fmapC #-}
fmapC :: (a -> b) -> Deriver st err a -> Deriver st err b
fmapC f m = Deriver $ \st1 logs1 lose win ->
    runD m st1 logs1 lose (\st2 logs2 a -> win st2 logs2 (f a))

instance Functor (Deriver st err) where
    fmap = fmapC

throw :: err -> Deriver st err a
throw err = Deriver $ \st logs lose _ -> lose st logs err

{-# INLINE modify #-}
modify :: (st -> st) -> Deriver st err ()
modify f = Deriver $ \st1 logs _ win -> let !st2 = f st1 in win st2 logs ()

{-# INLINE get #-}
get :: Deriver st err st
get = Deriver $ \st logs _ win -> win st logs st

{-# INLINE put #-}
put :: st -> Deriver st err ()
put !st = Deriver $ \_ logs _ win -> win st logs ()

instance Monad (Deriver st err) where
    (>>=) = bindC
    -- fail = throw

{-# INLINE gets #-}
gets :: (st -> a) -> Deriver st err a
gets f = do
    st <- get
    return $! f st

{-# INLINE apC #-}
apC :: Deriver st err (a -> b) -> Deriver st err a -> Deriver st err b
apC mf ma = do
    f <- mf
    a <- ma
    return (f a)

instance Applicative.Applicative (Deriver st err) where
    pure = pureC
    (<*>) = apC

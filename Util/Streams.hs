-- Copyright 2023 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for @streaming@.
module Util.Streams (
    mapAccumL, mapAccumL_
) where
import qualified Streaming.Prelude as S

import           Global


mapAccumL :: Monad m => (state -> a -> m (state, b)) -> state
    -> S.Stream (S.Of a) m r -> S.Stream (S.Of b) m r
mapAccumL f = go
    where
    go state0 as = lift (S.next as) >>= \case
        Left r -> pure r
        Right (a, as) -> do
            (state1, b) <- lift $ f state0 a
            S.cons b (go state1 as)

-- | Like `S.mapM_` but with state.
mapAccumL_ :: Monad m => (state -> a -> m state) -> state
    -> S.Stream (S.Of a) m r -> m r
mapAccumL_ f = go
    where
    go state0 xs = S.next xs >>= \case
        Left r -> return r
        Right (x, xs) -> do
            state1 <- f state0 x
            go state1 xs

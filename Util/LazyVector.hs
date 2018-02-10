-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}
module Util.LazyVector (
    Lazy, lazyChunks, toStrict
    -- * builder
    , Builder, build, singleton, fromList
) where
import qualified Control.Monad.ST as ST
import qualified Control.Monad.ST.Unsafe as ST.Unsafe
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as Mutable

import qualified Util.Pretty as Pretty


newtype Lazy a = Lazy [Vector.Vector a]
    deriving (Show, Pretty.Pretty)

instance Functor Lazy where
    fmap f (Lazy chunks) = Lazy (map (Vector.map f) chunks)

lazyChunks :: Lazy a -> [Vector.Vector a]
lazyChunks (Lazy chunks) = chunks

toStrict :: Lazy a -> Vector.Vector a
toStrict = mconcat . lazyChunks

-- cons :: Vector.Vector a -> Lazy a -> Lazy a
-- cons v (Lazy vs)
--     | Vector.null v = Lazy vs
--     | otherwise = Lazy (v : vs)

-- * builder

newtype Builder a = Builder {
    -- Invariant: The lists include no null Vectors.
    runBuilder :: forall s. (Buffer s a -> ST.ST s [Vector.Vector a])
         -> Buffer s a -> ST.ST s [Vector.Vector a]
    }

instance (Show a) => Show (Builder a) where
    show = show . build

instance (Pretty.Pretty a) => Pretty.Pretty (Builder a) where
    format = Pretty.format . build

instance Semigroup (Builder a) where
    (<>) = append

instance Monoid (Builder a) where
    mempty = empty
    mappend = (<>)

build :: Builder a -> Lazy a
build builder = Lazy $ ST.runST $ do
    buf <- newBuffer chunkSize
    runBuilder (builder `append` flush) (const (return [])) buf

empty :: Builder a
empty = Builder (\cont buf -> cont buf)

append :: Builder a -> Builder a -> Builder a
append (Builder f) (Builder g) = Builder (f . g)

singleton :: a -> Builder a
singleton val = writeN 1 $ \mvec offset ->
    Mutable.unsafeWrite mvec offset val

fromList :: [a] -> Builder a
fromList xs = Builder build
    where
    build cont (Buffer mvec offset left) = loop mvec offset left xs
        where
        loop !mvec !offset !left [] = cont (Buffer mvec offset left)
        loop mvec offset left (x:xs)
            | left == 0 = do
                chunk <- Vector.unsafeFreeze mvec
                mvec <- Mutable.new chunkSize
                chunks <- ST.Unsafe.unsafeInterleaveST $
                    loop mvec 0 chunkSize (x:xs)
                return $! chunk : chunks
            | otherwise = do
                Mutable.unsafeWrite mvec offset x
                loop mvec (offset+1) (left-1) xs

flush :: Builder a
flush = Builder f
    where
    f cont buf@(Buffer mvec used left)
        | used == 0 = cont buf
        | otherwise = do
            vec <- Vector.unsafeFreeze mvec
            -- TODO why is it I can use mvec after freezing?
            -- I think because the offset writes after the frozen point.
            -- But in any case, ensureFree calls const newBuffer afterwards.
            -- Why not do the newBuffer in flush?
            let !buf2 = Buffer mvec used left
                -- The buffer may have unused space on the end, so only take
                -- the part that has data.
                !chunk = Vector.take used vec
            -- Text uses inlineInterleaveST and says it's faster
            chunks <- ST.Unsafe.unsafeInterleaveST (cont buf2)
            return $! chunk : chunks


-- ** buffer

data Buffer s a = Buffer
    {-# UNPACK #-} !(Mutable.STVector s a)
    {-# UNPACK #-} !Int -- elements used
    {-# UNPACK #-} !Int -- space left

writeAtMost :: Int -> (forall s. Mutable.MVector s a -> Int -> ST.ST s Int)
    -> Builder a
writeAtMost n f = ensureFree n `append` withBuffer (writeBuffer f)

writeN :: Int -> (forall s. Mutable.MVector s a -> Int -> ST.ST s ())
    -> Builder a
writeN n f = writeAtMost n (\mvec offset -> f mvec offset >> return n)

ensureFree :: Int -> Builder a
ensureFree !n = withSize $ \left ->
    if n <= left then empty
        else flush `append` withBuffer (const (newBuffer (max n chunkSize)))

writeBuffer :: (Mutable.MVector s a -> Int -> ST.ST s Int) -> Buffer s a
    -> ST.ST s (Buffer s a)
writeBuffer f (Buffer mvec used left) = do
    written <- f mvec used
    return $! Buffer mvec (used + written) (left - written)

withBuffer :: (forall s. Buffer s a -> ST.ST s (Buffer s a)) -> Builder a
withBuffer f = Builder $ \cont buf -> cont =<< f buf

withSize :: (Int -> Builder a) -> Builder a
withSize f = Builder $ \cont buf@(Buffer _ _ left) ->
    runBuilder (f left) cont buf

newBuffer :: Int -> ST.ST s (Buffer s a)
newBuffer size = do
    mvec <- Mutable.new size
    return $! Buffer mvec 0 size

-- 8 bytes for an int, so 4k = 512 elts, 1k = 128 elts
chunkSize :: Int
chunkSize = 128 - chunkOverhead

-- | Overhead of one chunk, in words.
chunkOverhead :: Int
chunkOverhead = 4 -- not sure what MVector is, but should be this more or less

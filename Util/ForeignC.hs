-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{- | This is a drop-in replacement for "Foreign" and "Foreign.C".  The
    difference is that it uses a 'CStorable' class instead of
    'Foreign.Storable', and only C types are in CStorable.  Otherwise, it's
    easy to corrupt memory by accidentally marshalling a haskell type into a
    C struct.

    It tries to export all the same things that Foreign and Foreign.C do, but
    is incomplete.
-}
module Util.ForeignC (
    module Util.ForeignC
    , module Data.Int, module Data.Word
    , module Foreign.C
    , module Foreign.Ptr, module Foreign.StablePtr, module Foreign.ForeignPtr
    , module Foreign.Marshal.Alloc, module Foreign.Marshal.Utils
) where
import Data.Int
import Data.Word
import qualified Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
    ( allocaBytes, allocaBytesAligned, mallocBytes, reallocBytes, free
    , finalizerFree
    )
import Foreign.Marshal.Utils hiding (with, new)
import GHC.Base


-- * CStorable

class CStorable a where
    sizeOf      :: a -> Int
    alignment   :: a -> Int
    peekElemOff :: Ptr a -> Int      -> IO a
    pokeElemOff :: Ptr a -> Int -> a -> IO ()
    peekByteOff :: Ptr b -> Int      -> IO a
    pokeByteOff :: Ptr b -> Int -> a -> IO ()
    peek        :: Ptr a      -> IO a
    poke        :: Ptr a -> a -> IO ()

    peekElemOff = peekElemOff_ undefined
        where
        peekElemOff_ :: a -> Ptr a -> Int -> IO a
        peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
    pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val
    peekByteOff ptr off = peek (ptr `plusPtr` off)
    pokeByteOff ptr off = poke (ptr `plusPtr` off)
    peek ptr = peekElemOff ptr 0
    poke ptr = pokeElemOff ptr 0

-- ** basic types

instance CStorable CDouble where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable CFloat where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable CInt where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable CChar where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable CUChar where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

-- ** words

instance CStorable Word8 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Word16 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Word32 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Word64 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

-- ** ints

instance CStorable Int8 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Int16 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Int32 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke
instance CStorable Int64 where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

-- ** ptrs

instance CStorable (Ptr a) where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

instance CStorable (FunPtr a) where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

instance CStorable (StablePtr a) where
    sizeOf = Foreign.sizeOf
    alignment = Foreign.alignment
    peek = Foreign.peek
    poke = Foreign.poke

-- * Foreign.Marshal.Alloc

alloca :: forall a b. (CStorable a) => (Ptr a -> IO b) -> IO b
alloca = allocaBytesAligned (sizeOf dummy) (alignment dummy)
    where
    dummy :: a
    dummy = undefined

malloc :: (CStorable a) => IO (Ptr a)
malloc = doMalloc undefined
    where
    doMalloc :: (CStorable b) => b -> IO (Ptr b)
    doMalloc dummy = mallocBytes (sizeOf dummy)

-- * Foreign.Marshal.Array

mallocArray :: (CStorable a) => Int -> IO (Ptr a)
mallocArray = doMalloc undefined
    where
    doMalloc :: (CStorable a') => a' -> Int -> IO (Ptr a')
    doMalloc dummy size = mallocBytes (size * sizeOf dummy)

allocaArray :: (CStorable a) => Int -> (Ptr a -> IO b) -> IO b
allocaArray = doAlloca undefined
    where
    doAlloca :: (CStorable a') => a' -> Int -> (Ptr a' -> IO b') -> IO b'
    doAlloca dummy size =
        allocaBytesAligned (size * sizeOf dummy) (alignment dummy)

pokeArray :: (CStorable a) => Ptr a -> [a] -> IO ()
pokeArray ptr vals0 = go vals0 0#
    where
    go [] _ = return ()
    go (val:vals) n# = do
        pokeElemOff ptr (I# n#) val
        go vals (n# +# 1#)

peekArray :: (CStorable a) => Int -> Ptr a -> IO [a]
peekArray size ptr
    | size <= 0 = return []
    | otherwise = f (size-1) []
    where
    f 0 acc = do
        e <- peekElemOff ptr 0
        return (e:acc)
    f n acc = do
        e <- peekElemOff ptr n
        f (n-1) (e:acc)


newArray :: (CStorable a) => [a] -> IO (Ptr a)
newArray vals = do
    ptr <- mallocArray (length vals)
    pokeArray ptr vals
    return ptr

withArray :: (CStorable a) => [a] -> (Ptr a -> IO b) -> IO b
withArray vals f = withArrayLen vals (const f)

withArrayLen :: (CStorable a) => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLen vals f = allocaArray len $ \ptr -> do
    pokeArray ptr vals
    f len ptr
    where len = length vals

-- | Like 'withArrayLen', except if the list is null, then pass (0, nullPtr).
withArrayLenNull :: CStorable a => [a] -> (Int -> Ptr a -> IO b) -> IO b
withArrayLenNull vals f
    | null vals = f 0 nullPtr
    | otherwise = withArrayLen vals f

copyArray :: (CStorable a) => Ptr a -> Ptr a -> Int -> IO ()
copyArray = doCopy undefined
    where
    doCopy :: (CStorable a') => a' -> Ptr a' -> Ptr a' -> Int -> IO ()
    doCopy dummy dest src size = copyBytes dest src (size * sizeOf dummy)


-- ** Foreign.Marshal.Utils

with :: (CStorable a) => a -> (Ptr a -> IO b) -> IO b
with val f = alloca $ \ptr -> do
    poke ptr val
    f ptr

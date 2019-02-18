-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Data.Vector that dispatch to C.
module Util.VectorC (
    mixFloats
) where
import qualified Control.Monad.ST.Unsafe as ST.Unsafe
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import qualified Foreign
import           Foreign (Ptr)
import qualified Foreign.C as C


mixFloats :: Int -> [V.Vector Float] -> V.Vector Float
mixFloats minLen vs = V.create $ do
    let vsLen = maximum $ 0 : map V.length vs
    v <- VM.new (max minLen vsLen)
    VM.set v 0
    ST.Unsafe.unsafeIOToST $ withFPtrM v $ \vp vp_len ->
        withFPtrs vs $ \inspp lenspp ->
        c_mix_vectors (Foreign.castPtr vp) vp_len (Foreign.castPtr inspp) lenspp
            (fromIntegral (length vs))
    return v

-- void
-- mix_vectors(float *out, size_t out_len,
--     const float **ins, size_t *in_lens, size_t ins_len)
foreign import ccall "mix_vectors"
    c_mix_vectors :: Ptr C.CFloat -> C.CSize
        -> Ptr (Ptr C.CFloat) -> Ptr C.CSize -> C.CSize -> IO ()


-- * foreign utils

withFPtrs :: Foreign.Storable a => [V.Vector a]
    -> (Ptr (Ptr a) -> Ptr C.CSize -> IO b) -> IO b
withFPtrs vs action = go [] vs
    where
    go accum [] = Foreign.withArray vps $ \vpp ->
        Foreign.withArray vpLens $ \lenpp -> action vpp lenpp
        where (vps, vpLens) = unzip (reverse accum)
    go accum (v:vs) = withFPtr v $
        \vp len -> go ((vp, len) : accum) vs

withFPtrM :: Foreign.Storable a => VM.MVector s a
    -> (Ptr a -> C.CSize -> IO b) -> IO b
withFPtrM = withFPtr_ VM.unsafeToForeignPtr0

withFPtr :: Foreign.Storable a => V.Vector a
    -> (Ptr a -> C.CSize -> IO b) -> IO b
withFPtr = withFPtr_ V.unsafeToForeignPtr0

withFPtr_ :: (v -> (Foreign.ForeignPtr a, Int)) -> v
    -> (Ptr a -> C.CSize -> IO b) -> IO b
withFPtr_ toFptr v action =
    Foreign.withForeignPtr fptr $ \ptr -> action ptr (fromIntegral len)
    where (fptr, len) = toFptr v

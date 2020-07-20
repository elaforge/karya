-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Utilities for C wrappers.

    - Functions to convert between haskell and c types.

    - Generic UI debugging functions.
-}
module Util.CUtil where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Word as Word

import qualified Foreign
import qualified ForeignC

import qualified Util.Num as Num

import           Foreign.C
import           Global


-- * convert

c_int :: Int -> CInt
c_int = fromIntegral -- c ints should be at least as big as hs ones

c_nat :: Int -> CInt
c_nat = c_int -- I don't check for > 0 yet, I should catch the c++ exception

c_uchar :: Integral a => a -> CUChar
c_uchar = fromIntegral . Num.clamp 0 255

c_char :: Char -> CChar
c_char = fromIntegral . Num.clamp 0 127 . fromEnum

-- | This corresponds to utf8::rune.
c_rune :: Char -> Word.Word32
c_rune = fromIntegral . fromEnum

c_double :: Double -> CDouble
c_double = CDouble

c_float :: Float -> CFloat
c_float = CFloat

hs_double :: CDouble -> Double
hs_double (CDouble d) = d

-- | bool is C++, not C, so I represent bools as chars.
c_bool :: Bool -> CChar
c_bool True = 1
c_bool False = 0

-- | Copy the bytestring to a null-terminated cstring, in malloc'd space.
-- ByteString only has an alloca version of this.
bytesToCString0 :: ByteString.ByteString -> IO CString
bytesToCString0 bs = Unsafe.unsafeUseAsCStringLen bs $ \(str, len) -> do
    new <- Foreign.mallocBytes (len+1)
    Foreign.copyBytes new str len
    Foreign.poke (new `Foreign.plusPtr` len) (0 :: Word.Word8)
    return new

-- | Allocate a new UTF8-encoded null-terminated CString.
--
-- This copies the string twice, but I think I'd need a encodeUtf8 that can
-- write directly to a pointer to solve that.
newCString0 :: Text -> IO CString
newCString0 = bytesToCString0 . Encoding.encodeUtf8

-- | Like 'newCString0', but optimize "" to nullptr.  The C++ side has to
-- be prepared for this.
newCStringNull0 :: Text -> IO CString
newCStringNull0 t
    | Text.null t = return Foreign.nullPtr
    | otherwise = newCString0 t

peekCString :: CString -> IO Text
peekCString cstr
    | cstr == Foreign.nullPtr = return Text.empty
    | otherwise = fmap decodeUtf8 $ ByteString.packCString cstr

withText :: Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . Encoding.encodeUtf8

decodeUtf8 :: ByteString.ByteString -> Text
decodeUtf8 = Encoding.decodeUtf8With Encoding.Error.lenientDecode

-- * ForeignPtr

withForeignPtrs :: [Foreign.ForeignPtr a] -> ([Foreign.Ptr a] -> IO b) -> IO b
withForeignPtrs fps = withfp [] fps
    where
    withfp ps [] action = action (reverse ps)
    withfp ps (fp:rest) action =
        Foreign.withForeignPtr fp (\p -> withfp (p:ps) rest action)

-- * FunPtr

-- | Forgetting to call freeHaskellFunPtr is an easy way to leak memory.
-- So all FunPtrs should be created with this function, and always bee freed
-- with 'freeFunPtr'.  That way I can log creates and frees to ensure they
-- are balanced.  Use @tools/track_funptr.py@ to automate that.
makeFunPtr :: String -> IO (Foreign.FunPtr a) -> IO (Foreign.FunPtr a)
makeFunPtr _name make = do
    fptr <- make
    -- putStrLn $ "+ " ++ show fptr ++ " " ++ _name
    return fptr

freeFunPtr :: Foreign.FunPtr a -> IO ()
freeFunPtr fptr = do
    -- putStrLn $ "- " ++ show fptr
    Foreign.freeHaskellFunPtr fptr

-- | This should be in c-storable, but updating via hackage is such a pain I'll
-- inline it for now.
new :: ForeignC.CStorable a => a -> IO (Foreign.Ptr a)
new val  = do
    ptr <- ForeignC.malloc
    ForeignC.poke ptr val
    return ptr

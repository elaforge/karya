-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Utilities for the SomethingC layer.

    - Fltk monad

    - Functions to convert between haskell and c types.

    - Generic UI debugging functions.
-}
module Ui.Util where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text.Encoding.Error as Encoding.Error
import qualified Data.Word as Word

import qualified Foreign
import Foreign.C

import qualified Util.Num as Num


-- | Throw if 'n' isn't between the half open range lo--hi.
-- Do a fromIntegral as a bonus so it's easy to convert types at the same time.
in_range :: (Show a, Integral a, Num b) => String -> a -> a -> a -> b
in_range desc lo hi n
    | lo <= n && n < hi = fromIntegral n
        -- TODO: throw a specific error?
    | otherwise = error $ desc ++ ": number " ++ show n ++ " out of range ("
        ++ show lo ++ "--" ++ show hi ++ ")"

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

hs_double :: CDouble -> Double
hs_double (CDouble d) = d

-- | bool is C++, not C, so I represent bools as chars.
c_bool :: Bool -> CChar
c_bool True = 1
c_bool False = 0

with_foreign_ptrs :: [Foreign.ForeignPtr a] -> ([Foreign.Ptr a] -> IO b)
    -> IO b
with_foreign_ptrs fps f = withfp [] fps f
    where
    withfp ps [] f = f (reverse ps)
    withfp ps (fp:rest) f = Foreign.withForeignPtr
        fp (\p -> withfp (p:ps) rest f)

-- | Forgetting to call freeHaskellFunPtr is an easy way to leak memory.
-- So all FunPtrs should be created with this function, and always bee freed
-- with 'free_fun_ptr'.  That way I can log creates and frees to ensure they
-- are balanced.  Use @tools/track_funptr.py@ to automate that.
make_fun_ptr :: String -> IO (Foreign.FunPtr a) -> IO (Foreign.FunPtr a)
make_fun_ptr _name make = do
    fptr <- make
    -- putStrLn $ "+ " ++ show fptr ++ " " ++ _name
    return fptr

free_fun_ptr :: Foreign.FunPtr a -> IO ()
free_fun_ptr fptr = do
    -- putStrLn $ "- " ++ show fptr
    Foreign.freeHaskellFunPtr fptr

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
textToCString0 :: Text.Text -> IO CString
textToCString0 = bytesToCString0 . Encoding.encodeUtf8

peekCString :: CString -> IO Text.Text
peekCString cstr
    | cstr == Foreign.nullPtr = return Text.empty
    | otherwise = fmap decodeUtf8 $ ByteString.packCString cstr

withText :: Text.Text -> (CString -> IO a) -> IO a
withText = ByteString.useAsCString . Encoding.encodeUtf8

decodeUtf8 :: ByteString.ByteString -> Text.Text
decodeUtf8 = Encoding.decodeUtf8With Encoding.Error.lenientDecode

{- | Utilities for the SomethingC layer.

    - Fltk monad

    - Functions to convert between haskell and c types.

    - Generic UI debugging functions.
-}
module Ui.Util where
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as Unsafe
import qualified Data.Word as Word

import qualified Foreign
import Foreign.C

import qualified Util.Num as Num


-- | This is similar to Ui.Types.UI, except that it's intended for the low
-- level fltk operations.  The difference is that all fltk operations must be
-- executed serially in the ui thread.
-- Currently this is just a synonym for IO, but in the future I could guarantee
-- only the thread gets to run them with some newtype deriving magic.
type Fltk = IO

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

-- | Unpack the bytestring to a null-terminated cstring, in malloc'd space.
-- ByteString only has an alloca version of this.
unpackCString0 :: ByteString.ByteString -> IO CString
unpackCString0 bs = Unsafe.unsafeUseAsCStringLen bs $ \(str, len) -> do
    new <- Foreign.mallocBytes (len+1)
    Foreign.copyBytes new str len
    Foreign.poke (new `Foreign.plusPtr` len) (0 :: Word.Word8)
    return new

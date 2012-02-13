{- | Utilities for the SomethingC layer.

    - Fltk monad

    - Functions to convert between haskell and c types.

    - Generic UI debugging functions.
-}
module Ui.Util where
import Foreign
import Foreign.C


-- | This is similar to Ui.Types.UI, except that it's intended for the low
-- level fltk operations.  The difference is that all fltk operations must be
-- executed serially in the ui thread.
-- Currently this is just a synonym for IO, but in the future I could guarantee
-- only the thread gets to run them with some newtype deriving magic.
type Fltk = IO

bounded_list def max_len xs = take max_len xs
    ++ replicate (max_len - length xs) def

-- make this a hard bound and throw an exception if its out of range?
bounded lo hi n = max lo (min hi n)

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
c_uchar = fromIntegral . bounded 0 255

withForeignPtrs fps f = withfp [] fps f
    where
    withfp ps [] f = f (reverse ps)
    withfp ps (fp:rest) f = withForeignPtr fp (\p -> withfp (p:ps) rest f)

make_fun_ptr :: String -> IO (FunPtr a) -> IO (FunPtr a)
make_fun_ptr _name make = do
    fptr <- make
    -- putStrLn $ "alloc " ++ show name ++ ": " ++ show fptr
    -- IO.hFlush IO.stdout
    return fptr

free_fun_ptr :: FunPtr a -> IO ()
free_fun_ptr fptr = do
    -- putStrLn $ "free " ++ show fptr
    -- IO.hFlush IO.stdout
    freeHaskellFunPtr fptr

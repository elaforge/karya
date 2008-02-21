module Interface.Util where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Foreign
import Foreign.C
-- import qualified Interface.Types as Types

bounded_list def max_len lst = take max_len lst
    ++ replicate (max_len - length lst) def

-- make this a hard bound and throw an exception if its out of range?
bounded lo hi n = max lo (min hi n)

c_int :: Int -> CInt
c_int i = fromIntegral i -- c ints should be at least as big as hs ones

c_double :: Double -> CDouble
c_double = realToFrac

c_uchar :: Integral a => a -> CUChar
c_uchar = fromIntegral . bounded 0 255

withForeignPtrs fps f = withfp [] fps f
    where
    withfp ps [] f = f (reverse ps)
    withfp ps (fp:rest) f = withForeignPtr fp (\p -> withfp (p:ps) rest f)

start_thread = do_start_thread Concurrent.forkIO
start_os_thread = do_start_thread Concurrent.forkOS

do_start_thread fork name th = fork $ Exception.bracket_
    (putStrLn $ "thread start: " ++ name)
    (putStrLn $ "thread exit: " ++ name)
    th

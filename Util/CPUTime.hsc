-- | System.CPUTime is buggy, as of base 4.3.1.0.  It gets tv_usec as a time_t
-- when it's really a suseconds_t.  On 64bit OS X these are different sizes.
module Util.CPUTime (getCPUTime) where
import Foreign
import Foreign.C


#include <sys/time.h>
#include <sys/resource.h>

getCPUTime :: IO Integer
getCPUTime =
    allocaBytes (#const sizeof(struct rusage)) $ \ p_rusage -> do
    throwErrnoIfMinus1_ "getrusage" $ getrusage (#const RUSAGE_SELF) p_rusage

    let ru_utime = (#ptr struct rusage, ru_utime) p_rusage
    let ru_stime = (#ptr struct rusage, ru_stime) p_rusage
    u_sec  <- (#peek struct timeval,tv_sec)  ru_utime :: IO CTime
    u_usec <- (#peek struct timeval,tv_usec) ru_utime :: IO Int32
    s_sec  <- (#peek struct timeval,tv_sec)  ru_stime :: IO CTime
    s_usec <- (#peek struct timeval,tv_usec) ru_stime :: IO Int32
    return ((realToInteger u_sec * 1000000 + realToInteger u_usec +
             realToInteger s_sec * 1000000 + realToInteger s_usec)
                * 1000000)

foreign import ccall unsafe getrusage :: CInt -> Ptr () -> IO CInt

realToInteger :: Real a => a -> Integer
realToInteger ct = round (realToFrac ct :: Double)

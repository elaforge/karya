-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.FLock (
    withLockedWarning, withLocked_, withLocked
) where
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.IORef as IORef
import Foreign.C
import qualified Foreign.C.Error as C.Error
import qualified System.Posix.IO as IO
import System.Posix.Types (Fd(..))


type Seconds = Double

withLockedWarning :: FilePath -> IO () -> Seconds -> IO a -> IO a
withLockedWarning fn warn timeout action = do
    done <- IORef.newIORef False
    Concurrent.forkIO $ do
        Concurrent.threadDelay $ floor (timeout * 10^6)
        b <- IORef.readIORef done
        unless b warn
    withLocked_ fn $ IORef.atomicWriteIORef done True >> action

withLocked_ :: FilePath -> IO a -> IO a
withLocked_ fn = withLocked fn . const

withLocked :: FilePath -> (Fd -> IO a) -> IO a
withLocked fn = Exception.bracket lock unlock
    where
    lock = do
        fd <- IO.openFd fn IO.ReadWrite Nothing IO.defaultFileFlags
        flock fd lock_ex
        return fd
    unlock fd = do
        flock fd lock_un
        IO.closeFd fd

flock :: Fd -> CInt -> IO ()
flock (Fd fd) flags =
    C.Error.throwErrnoIfMinus1Retry_ "flock" (c_flock fd flags)

_lock_sh, lock_ex, _lock_nb, lock_un :: CInt
_lock_sh = 1
lock_ex = 2
_lock_nb = 4
lock_un = 8

foreign import ccall "flock" c_flock :: CInt -> CInt -> IO CInt

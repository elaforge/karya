-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | libsndfile isn't thread-safe because it uses a global sf_errno.
-- The haskell binding doesn't do any locking, so I need to serialize it until
-- it can check sf_errno.
module Util.Audio.Sndfile (openFile, hSeek, module Sndfile) where
import qualified Control.Concurrent.MVar as MVar
import qualified System.IO.Unsafe as Unsafe
import qualified Sound.File.Sndfile as Sndfile
import           Sound.File.Sndfile hiding (openFile, hSeek)


{-# NOINLINE lock #-}
lock :: MVar.MVar ()
lock = Unsafe.unsafePerformIO (MVar.newMVar ())

withLock :: IO a -> IO a
withLock action = MVar.withMVar lock (const action)

openFile :: FilePath -> IOMode -> Info -> IO Handle
openFile fname mode info = withLock $ Sndfile.openFile fname mode info

hSeek :: Handle -> SeekMode -> Count -> IO Count
hSeek hdl mode count = withLock $ Sndfile.hSeek hdl mode count

-- also lock hGetBuffer?

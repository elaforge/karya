{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Utilities dealing with memory usage.
module Util.Memory where
import qualified System.Posix.Process as Posix.Process
import qualified System.Process as Process

import qualified Util.Pretty as Pretty


newtype K = K Int deriving (Eq, Ord, Num)

instance Show K where
    show (K k) = Pretty.pretty (fromIntegral k / 1024 :: Double) ++ "m"

from_bytes :: Int -> K
from_bytes = K . (`div` 1024)

-- | Return (RSS, VSIZE).
memory_usage :: IO (K, K)
memory_usage = do
    pid <- Posix.Process.getProcessID
    out <- Process.readProcess "ps" ["-p", show pid, "-orss,vsize"] ""
    let [_, [rss, vsize]] = map words (lines out)
    return (K (read rss), K (read vsize))

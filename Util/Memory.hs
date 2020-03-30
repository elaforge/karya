-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Utilities dealing with memory usage.
--
-- TODO the SizeOf part can maybe be replaced by the weigh package.
module Util.Memory where
import qualified Data.Map as Map
import qualified Data.Word as Word

import qualified Foreign
import qualified GHC.Stats
import qualified System.Mem
import qualified System.Posix.Process as Posix.Process
import qualified System.Process as Process

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty


-- * Size

-- | Size in bytes.
newtype Size = Size Integer deriving (Eq, Ord, Num, Show)

instance Pretty.Pretty Size where
    pretty (Size n) =
        Pretty.pretty (fromIntegral n / 1024 / 1024 :: Double) <> "m"

fromBytes :: Integral a => a -> Size
fromBytes = Size . fromIntegral

toBytes :: Size -> Integer
toBytes (Size b) = b

fromK :: RealFrac a => a -> Size
fromK = fromBytes . round . (*1024)

toK :: Size -> Double
toK = (/1024) . fromIntegral . toBytes

fromM :: RealFrac a => a -> Size
fromM = fromK . (*1024)

toM :: Size -> Double
toM = (/1024) . toK

fromWords :: Integral a => a -> Size
fromWords = fromBytes . (* fromIntegral (Foreign.sizeOf Foreign.nullPtr))

-- * SizeOf

class SizeOf a where
    sizeOf :: a -> Size

instance SizeOf a => SizeOf [a] where
    sizeOf xs = fromBytes (length xs) * taggedBox + Num.sum (map sizeOf xs)

instance SizeOf Char where
    sizeOf _ = fromWords 0 -- chars from 0--255 are interned

instance SizeOf Int where sizeOf = boxedStorable
instance SizeOf Word.Word8 where sizeOf = boxedStorable
instance SizeOf Double where sizeOf = boxedStorable
instance SizeOf Float where sizeOf = boxedStorable

instance (SizeOf k, SizeOf a) => SizeOf (Map.Map k a) where
    sizeOf m = fromBytes nodes * taggedBox + Num.sum (map sizeOf (Map.keys m))
            + Num.sum (map sizeOf (Map.elems m))
        where
        nodes = Map.size m + ceiling (logBase 2 (fromIntegral (Map.size m)))

taggedBox :: Size
taggedBox = fromWords 3 -- gc overhead, tag, ptr

box :: Size
box = fromWords 2 -- gc overhead, ptr

-- | Boxed word-sized datatype.
boxedStorable :: (Foreign.Storable a) => a -> Size
boxedStorable v =
    -- I think all types have to be word aligned.
    box + fromBytes (max (Foreign.sizeOf Foreign.nullPtr) (Foreign.sizeOf v))

-- * usage

-- | Return OS-reported (RSS, VSIZE).
rssVsize :: IO (Size, Size)
rssVsize = do
    pid <- Posix.Process.getProcessID
    out <- Process.readProcess "ps" ["-p", show pid, "-orss,vsize"] ""
    let [_, [rss, vsize]] = map words (lines out)
    return (fromK (read rss), fromK (read vsize))

-- * RTS

-- | Allocated memory according to the GHC RTS.
rtsAllocated :: IO Size

#if GHC_VERSION >= 80401
rtsAllocated = do
    System.Mem.performMajorGC
    stats <- GHC.Stats.getRTSStats
    return $ fromBytes $ GHC.Stats.gcdetails_live_bytes $ GHC.Stats.gc stats

stats :: IO GHC.Stats.RTSStats
stats = do
    System.Mem.performMajorGC
    GHC.Stats.getRTSStats

#else

rtsAllocated = do
    System.Mem.performMajorGC
    stats <- GHC.Stats.getGCStats
    return $ fromBytes $ GHC.Stats.currentBytesUsed stats

stats :: IO GHC.Stats.GCStats
stats = do
    System.Mem.performMajorGC
    GHC.Stats.getGCStats

#endif

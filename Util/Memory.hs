-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Utilities dealing with memory usage.
module Util.Memory where
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Word as Word
import qualified Foreign
import qualified System.Posix.Process as Posix.Process
import qualified System.Process as Process

import qualified Util.Pretty as Pretty


class Bytes a where
    bytes :: a -> Size

instance (Bytes a) => Bytes [a] where
    bytes xs = Size (length xs) * tagged_box + sum (map bytes xs)

instance Bytes Char where
    bytes _ = from_words 0 -- chars from 0--255 are interned

instance Bytes Int where bytes = boxed_storable
instance Bytes Word.Word8 where bytes = boxed_storable
instance Bytes Double where bytes = boxed_storable
instance Bytes Float where bytes = boxed_storable

instance (Bytes k, Bytes a) => Bytes (Map.Map k a) where
    bytes m = Size nodes * tagged_box + sum (map bytes (Map.keys m))
            + sum (map bytes (Map.elems m))
        where
        nodes = Map.size m + ceiling (logBase 2 (fromIntegral (Map.size m)))

-- | Size in bytes.
newtype Size = Size Int deriving (Eq, Ord, Num, Show)

instance Pretty.Pretty Size where
    pretty (Size n) =
        Pretty.pretty (fromIntegral n / 1024 / 1024 :: Double) <> "m"

tagged_box :: Size
tagged_box = from_words 3 -- gc overhead, tag, ptr

box :: Size
box = from_words 2 -- gc overhead, ptr

-- | Boxed word-sized datatype.
boxed_storable :: (Foreign.Storable a) => a -> Size
boxed_storable v =
    -- I think all types have to be word aligned.
    box + from_bytes (max (Foreign.sizeOf Foreign.nullPtr) (Foreign.sizeOf v))

from_bytes :: Int -> Size
from_bytes = Size

from_k :: Int -> Size
from_k = from_bytes . (*1024)

from_words :: Int -> Size
from_words = from_bytes . (* Foreign.sizeOf Foreign.nullPtr)

-- | Return (RSS, VSIZE).
memory_usage :: IO (Size, Size)
memory_usage = do
    pid <- Posix.Process.getProcessID
    out <- Process.readProcess "ps" ["-p", show pid, "-orss,vsize"] ""
    let [_, [rss, vsize]] = map words (lines out)
    return (from_k (read rss), from_k (read vsize))

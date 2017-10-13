-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
module Util.Crc32Instances where
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import Data.Digest.CRC32
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Word as Word

import qualified Foreign
import qualified System.IO.Unsafe as Unsafe


instance CRC32 a => CRC32 [a] where crc32Update = List.foldl' crc32Update
instance (CRC32 a, CRC32 b) => CRC32 (a, b) where
    crc32Update n (a, b) = n `crc32Update` a `crc32Update` b
instance (CRC32 a, CRC32 b, CRC32 c) => CRC32 (a, b, c) where
    crc32Update n (a, b, c) = n `crc32Update` a `crc32Update` b `crc32Update` c

instance CRC32 Bool where crc32Update n = crc32Update n . fromEnum
instance CRC32 Char where crc32Update n = crc32Update n . fromEnum

instance CRC32 Int where crc32Update = crcStorable
instance CRC32 Word.Word8 where crc32Update = crcStorable
instance CRC32 Word.Word16 where crc32Update = crcStorable
instance CRC32 Word.Word32 where crc32Update = crcStorable
instance CRC32 Word.Word64 where crc32Update = crcStorable
instance CRC32 Int.Int8 where crc32Update = crcStorable
instance CRC32 Int.Int16 where crc32Update = crcStorable
instance CRC32 Int.Int32 where crc32Update = crcStorable
instance CRC32 Int.Int64 where crc32Update = crcStorable
instance CRC32 Double where crc32Update = crcStorable
instance CRC32 Float where crc32Update = crcStorable

instance CRC32 Text.Text where crc32Update = Text.foldl' crc32Update

crcStorable :: forall a. Foreign.Storable a => Word.Word32 -> a -> Word.Word32
crcStorable n d = Unsafe.unsafePerformIO $ Foreign.alloca $
    \(buf :: Foreign.Ptr a) -> do
        Foreign.poke (Foreign.castPtr buf) d
        bytes <- ByteString.Unsafe.unsafePackCStringLen
            (Foreign.castPtr buf, Foreign.sizeOf d)
        return $ crc32Update n bytes

crcBytes :: forall a. Foreign.Storable a => Word.Word32 -> a -> IO Word.Word32
crcBytes n d = Foreign.alloca $ \(buf :: Foreign.Ptr a) -> do
    Foreign.poke (Foreign.castPtr buf) d
    bytes <- ByteString.Unsafe.unsafePackCStringLen
        (Foreign.castPtr buf, Foreign.sizeOf d)
    return $ crc32Update n bytes

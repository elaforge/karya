{-# LANGUAGE ScopedTypeVariables #-}
module Util.Crc32Instances where
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import Data.Digest.CRC32
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Word as Word

import qualified Foreign
import qualified System.IO.Unsafe as Unsafe


instance (CRC32 a) => CRC32 [a] where crc32Update = List.foldl' crc32Update

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

crcStorable :: forall a. (Foreign.Storable a) => Word.Word32 -> a -> Word.Word32
crcStorable n d = Unsafe.unsafePerformIO $ Foreign.alloca $
    \(buf :: Foreign.Ptr a) -> do
        Foreign.poke (Foreign.castPtr buf) d
        bytes <- ByteString.Unsafe.unsafePackCStringLen
            (Foreign.castPtr buf, Foreign.sizeOf d)
        return $ crc32Update n bytes

crcBytes :: forall a. (Foreign.Storable a) => Word.Word32 -> a -> IO Word.Word32
crcBytes n d = Foreign.alloca $ \(buf :: Foreign.Ptr a) -> do
    Foreign.poke (Foreign.castPtr buf) d
    bytes <- ByteString.Unsafe.unsafePackCStringLen
        (Foreign.castPtr buf, Foreign.sizeOf d)
    return $ crc32Update n bytes

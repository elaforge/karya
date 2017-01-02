-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
{- | This module implements a Serialize class and serializers for basic types.

    It duplicates a lot from the standard Serialize class, but this one at
    least is under my control.  The other one is not guaranteed to remain
    compatible.  Of course, it's unlikely to change incompatibly and I use it
    myself rather than re-implementing String and Integer encoding, but that's
    the theory anyway.

    At the least it lets me use a direct float encoding rather than hacking
    around the large and buggy default implementation.
-}
module Util.Serialize (
    module Util.Serialize
    , Get, Put
    , getWord8, putWord8
) where
import qualified Control.Exception as Exception
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import Data.Serialize (Get, Put, getWord8, putWord8)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Foreign
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IO.Error
import qualified System.IO.Unsafe as Unsafe

import qualified Util.CallStack as CallStack
import qualified Util.File as File
import qualified Util.Pretty as Pretty

import Global


encode :: Serialize a => a -> ByteString
encode = Serialize.runPut . put

decode :: Serialize a => ByteString -> Either String a
decode = Serialize.runGet get

class Serialize a where
    put :: Serialize.Putter a
    get :: Get a

-- * magic

-- | This is a four byte prefix to identify a particular file type, tagged with
-- the serialized type.  The Chars are just for syntactic convenience only, and
-- must be ASCII.
--
-- The constructor is not exported, so all magics have to be defined here,
-- which should make it easy to avoid collisions.
data Magic a = Magic !Char !Char !Char !Char deriving (Show)

magicBytes :: Magic a -> ByteString
magicBytes (Magic c1 c2 c3 c4) = Char8.pack [c1, c2, c3, c4]

magicLength :: Int
magicLength = 4

serialize :: Serialize a => Magic a -> FilePath -> a -> IO ()
serialize magic fname state = do
    backupFile fname
    makeDir fname
    File.writeGz fname $ magicBytes magic <> encode state

data UnserializeError = BadMagic ByteString ByteString
    | IOError IO.Error.IOError | UnserializeError String
    deriving (Show)

unserialize :: Serialize a => Magic a -> FilePath
    -> IO (Either UnserializeError a)
unserialize magic fname = catch $ do
    bytes <- either (Exception.throw . IO.Error.userError) return
        =<< File.readGz fname
    let (file_magic, rest) = ByteString.splitAt magicLength bytes
    if file_magic /= magicBytes magic
        then return $ Left $ BadMagic (magicBytes magic) file_magic
        else first UnserializeError <$> Exception.evaluate (decode rest)
            -- Apparently decode can still throw an exception unless
            -- the contents of the Either is forced to whnf.
    where catch = fmap (either (Left . IOError) id) . Exception.try

instance Pretty.Pretty UnserializeError where
    pretty e = case e of
        BadMagic expected got -> "expected file magic " <> showt expected
            <> " but got " <> showt got
        IOError exc -> "io error: " <> showt exc
        UnserializeError err -> "unserialize error: " <> txt err

-- | Move @file@ to @file.last@.  Do this before writing a new one that may
-- fail.
backupFile :: FilePath -> IO ()
backupFile fname = do
    File.requireWritable fname
    File.requireWritable (fname ++ ".last")
    void $ File.ignoreEnoent $ Directory.renameFile fname (fname ++ ".last")

makeDir :: FilePath -> IO ()
makeDir = Directory.createDirectoryIfMissing True . FilePath.takeDirectory

-- * numeric

instance Serialize Integer where
    put = Serialize.put
    get = Serialize.get

instance Serialize Int where
    put i = put (fromIntegral i :: Int.Int64)
    get = fromIntegral <$> (get :: Get Int.Int64)

instance Serialize Int.Int64 where
    put i = put (fromIntegral i :: Word.Word64)
    get = fromIntegral <$> (get :: Get Word.Word64)

instance Serialize Word.Word8 where
    put = putWord8
    get = getWord8

instance Serialize Word.Word32 where
    put = Serialize.putWord32le
    get = Serialize.getWord32le

instance Serialize Word.Word64 where
    put = Serialize.putWord64le
    get = Serialize.getWord64le

instance Serialize Double where
    put = put . encode_double
    get = decode_double <$> get

instance Serialize Float where
    put = put . encode_float
    get = decode_float <$> get

encode_double :: Double -> Word.Word64
encode_double = _encodef

encode_float :: Float -> Word.Word32
encode_float = _encodef

_encodef :: (Foreign.Storable float, Foreign.Storable word) => float -> word
_encodef d = Unsafe.unsafePerformIO $ Foreign.alloca $ \buf -> do
    Foreign.poke (Foreign.castPtr buf) d
    Foreign.peek buf

decode_double :: Word.Word64 -> Double
decode_double = _decodef

decode_float :: Word.Word32 -> Float
decode_float = _decodef

_decodef :: (Foreign.Storable float, Foreign.Storable word) => word -> float
_decodef word = Unsafe.unsafePerformIO $ Foreign.alloca $ \buf -> do
    Foreign.poke (Foreign.castPtr buf) word
    Foreign.peek buf

-- * util

get_tag :: Get Word.Word8
get_tag = getWord8

put_tag :: Word.Word8 -> Put
put_tag = putWord8

bad_tag :: String -> Word.Word8 -> Get a
bad_tag typ tag = fail $ "unknown tag for " ++ typ ++ ": " ++ show tag

put_enum :: Enum a => a -> Serialize.Put
put_enum = put . fromEnum

get_enum :: (Bounded a, Enum a) => Serialize.Get a
get_enum = get >>= \n ->
    maybe (fail $ "enum value out of range: " ++ show n) return (to_enum n)

-- | A safe version of 'toEnum'.
to_enum :: forall a. (Enum a, Bounded a) => Int -> Maybe a
to_enum n
    | fromEnum (minBound :: a) <= n && n <= fromEnum (maxBound :: a) =
        Just (toEnum n)
    | otherwise = Nothing

-- * basic types

instance Serialize () where
    put () = return ()
    get = return ()

instance Serialize Bool where
    put False = put_tag 0
    put True = put_tag 1
    get = (/= 0) <$> get_tag

instance Serialize Char where
    put = Serialize.put
    get = Serialize.get

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= return . read

-- * tuples

instance (Serialize a, Serialize b) => Serialize (a, b) where
    put (a, b) = put a >> put b
    get = (,) <$> get <*> get

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c) where
    put (a, b, c) = put a >> put b >> put c
    get = (,,) <$> get <*> get <*> get

-- * containers

instance Serialize a => Serialize [a] where
    put = Serialize.putListOf put
    get = Serialize.getListOf get

instance Serialize a => Serialize (NonEmpty a) where
    put = put . NonEmpty.toList
    get = fmap NonEmpty.fromList get

instance Serialize a => Serialize (Maybe a) where
    put Nothing = put_tag 0
    put (Just a) = put_tag 1 >> put a
    get = do
        tag <- get_tag
        if tag == 0 then return Nothing else Just <$> get

instance (Ord a, Serialize a) => Serialize (Set.Set a) where
    put = put . Set.toAscList
    get = Set.fromAscList <$> get

instance (Ord k, Serialize k, Serialize v) => Serialize (Map.Map k v) where
    put = put . Map.toAscList
    get = Map.fromAscList <$> get

instance (Serialize i, IArray.Ix i, Serialize e) =>
        Serialize (IArray.Array i e) where
    put = Serialize.putIArrayOf put put
    get = Serialize.getIArrayOf get get

instance Serialize ByteString where
    put bs = do
        put $ ByteString.length bs
        Serialize.putByteString bs
    get = get >>= Serialize.getByteString

instance Serialize Text.Text where
    put = put . Text.Encoding.encodeUtf8
    get = Text.Encoding.decodeUtf8 <$> get

instance (Serialize a, Unboxed.Unbox a) => Serialize (Unboxed.Vector a) where
    put v = do
        put (Unboxed.length v)
        Unboxed.mapM_ put v
    get = do
        len :: Int <- get
        Unboxed.replicateM len get

instance Serialize a => Serialize (Vector.Vector a) where
    put v = do
        put (Vector.length v)
        Vector.mapM_ put v
    get = do
        len :: Int <- get
        Vector.replicateM len get

instance (Serialize a, Foreign.Storable a) =>
        Serialize (Vector.Storable.Vector a) where
    put v = do
        put (Vector.Storable.length v)
        Vector.Storable.mapM_ put v
    get = do
        len :: Int <- get
        Vector.Storable.replicateM len get

-- This has to be here instead of in CallStack to avoid a circular import.
instance Serialize CallStack.Caller where
    put (CallStack.Caller a b) = put_tag 0 >> put a >> put b
    put CallStack.NoCaller = put_tag 1
    get = get_tag >>= \tag -> case tag of
        0 -> CallStack.Caller <$> get <*> get
        1 -> return CallStack.NoCaller
        _ -> bad_tag "Caller" tag

-- * versions

put_version :: Word.Word8 -> Put
put_version = putWord8

get_version :: Get Word.Word8
get_version = getWord8

bad_version :: CallStack.Stack => String -> Word.Word8 -> a
bad_version typ ver = errorStack $
    "unknown version " <> showt ver <> " for " <> showt typ

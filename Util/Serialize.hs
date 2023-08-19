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
    encode, decode
    , Serialize(..)
    , Get, Put
    -- * magic
    , Magic(..)
    , magicBytes
    , serialize, serialize_rotate
    , UnserializeError(..)
    , unserialize
    -- * util
    , get_tag, put_tag, bad_tag
    , get_enum, put_enum, bad_enum
    , get_enum_unsafe, put_enum_unsafe
    -- * versions
    , get_version, put_version, bad_version
) where
import qualified Control.Exception as Exception
import qualified Data.Array.IArray as IArray
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Serialize as Serialize
import           Data.Serialize (getWord8, putWord8, Get, Put)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as Vector.Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Word as Word

import qualified Foreign
import qualified GHC.Float as Float
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IO.Error

import qualified Util.CallStack as CallStack
import qualified Util.Files as Files

import           Global


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

serialize :: Serialize a => Magic a -> FilePath -> a -> IO Bool
    -- ^ result of 'Files.writeGz'.
serialize = serialize_rotate 1

serialize_rotate :: Serialize a => Int -> Magic a -> FilePath -> a -> IO Bool
serialize_rotate rotations magic fname state = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory fname
    Files.writeGz rotations fname $ magicBytes magic <> encode state

data UnserializeError = BadMagic ByteString ByteString
    | IOError IO.Error.IOError | UnserializeError String
    deriving (Show)

unserialize :: Serialize a => Magic a -> FilePath
    -> IO (Either UnserializeError a)
unserialize magic fname = catch $ do
    bytes <- either (Exception.throw . IO.Error.userError) return
        =<< Files.readGz fname
    let (file_magic, rest) = ByteString.splitAt magicLength bytes
    if file_magic /= magicBytes magic
        then return $ Left $ BadMagic (magicBytes magic) file_magic
        else first UnserializeError <$> Exception.evaluate (decode rest)
            -- Apparently decode can still throw an exception unless
            -- the contents of the Either is forced to whnf.
    where catch = fmap (either (Left . IOError) id) . Exception.try

instance Pretty UnserializeError where
    pretty e = case e of
        BadMagic expected got -> "expected file magic " <> showt expected
            <> " but got " <> showt got
        IOError exc -> "io error: " <> showt exc
        UnserializeError err -> "unserialize error: " <> txt err

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
    put = put . Float.castDoubleToWord64
    get = Float.castWord64ToDouble <$> get

instance Serialize Float where
    put = put . Float.castFloatToWord32
    get = Float.castWord32ToFloat <$> get

-- * util

get_tag :: Get Word.Word8
get_tag = getWord8

put_tag :: Word.Word8 -> Put
put_tag = putWord8

bad_tag :: String -> Word.Word8 -> Get a
bad_tag typ tag = fail $ "unknown tag for " ++ typ ++ ": " ++ show tag

-- | These are convenient but dangerous.  If they are are used in a context
-- where backward compatibility matters ("Cmd.Serialize") then it's too easy
-- to break compatibility by adding or removing an enum.
--
-- But they're fine if used in an enum that will never change, or where
-- compatibility doesn't matter.
get_enum_unsafe :: (Bounded a, Enum a) => Serialize.Get a
get_enum_unsafe = get >>= \n ->
    maybe (fail $ "enum value out of range: " ++ show n) return (to_enum n)

put_enum_unsafe :: Enum a => a -> Serialize.Put
put_enum_unsafe = put . fromEnum

-- | A safe version of 'toEnum'.
to_enum :: forall a. (Enum a, Bounded a) => Int -> Maybe a
to_enum n
    | fromEnum (minBound :: a) <= n && n <= fromEnum (maxBound :: a) =
        Just (toEnum n)
    | otherwise = Nothing

get_enum :: Serialize.Get Word.Word8
get_enum = get

-- | It's just put, but make sure it's using Int.  Word8 would be more
-- suitable.
put_enum :: Word.Word8 -> Serialize.Put
put_enum = put

bad_enum :: String -> Word.Word8 -> Get a
bad_enum name val = fail $ "unknown enum val for " <> name <> ": " <> show val

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

-- * sums and products

instance (Serialize a, Serialize b) => Serialize (Either a b) where
    put (Left a) = put_tag 0 >> put a
    put (Right b) = put_tag 1 >> put b
    get = get_tag >>= \case
        0 -> Left <$> get
        1 -> Right <$> get
        tag -> bad_tag "Either" tag

instance Serialize a => Serialize (Maybe a) where
    put Nothing = put_tag 0
    put (Just a) = put_tag 1 >> put a
    get = get_tag >>= \case
        0 -> return Nothing
        1 -> Just <$> get
        tag -> bad_tag "Maybe" tag

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

instance (Ord a, Serialize a) => Serialize (Set a) where
    put = put . Set.toAscList
    get = Set.fromAscList <$> get

instance (Ord k, Serialize k, Serialize v) => Serialize (Map k v) where
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
    get = get_tag >>= \case
        0 -> CallStack.Caller <$> get <*> get
        1 -> return CallStack.NoCaller
        tag -> bad_tag "Caller" tag

-- * versions

get_version :: Get Word.Word8
get_version = getWord8

put_version :: Word.Word8 -> Put
put_version = putWord8

bad_version :: CallStack.Stack => String -> Word.Word8 -> a
bad_version typ ver = errorStack $
    "unknown version " <> showt ver <> " for " <> showt typ

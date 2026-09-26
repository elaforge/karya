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
    , serialize, serializeRotate
    , UnserializeError(..)
    , unserialize
    -- * util
    , getTag, putTag, badTag
    , getEnum, putEnum, badEnum
    , getEnumUnsafe, putEnumUnsafe
    -- * versions
    , getVersion, putVersion, badVersion
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
import           Data.Serialize (Get, Put, getWord8, putWord8)
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
import           GHC.Stack (HasCallStack)
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
serialize = serializeRotate 1

serializeRotate :: Serialize a => Int -> Magic a -> FilePath -> a -> IO Bool
serializeRotate rotations magic fname state = do
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory fname
    Files.writeGz rotations fname $ magicBytes magic <> encode state

data UnserializeError = BadMagic ByteString ByteString
    | IOError IO.Error.IOError | UnserializeError String
    deriving (Show)

unserialize :: Serialize a => Magic a -> FilePath
    -> IO (Either UnserializeError a)
unserialize magic fname = catch $ do
    bytes <- either (Exception.throw . IO.Error.userError) pure
        =<< Files.readGz fname
    let (fileMagic, rest) = ByteString.splitAt magicLength bytes
    if fileMagic /= magicBytes magic
        then pure $ Left $ BadMagic (magicBytes magic) fileMagic
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

getTag :: Get Word.Word8
getTag = getWord8

putTag :: Word.Word8 -> Put
putTag = putWord8

badTag :: String -> Word.Word8 -> Get a
badTag typ tag = fail $ "unknown tag for " ++ typ ++ ": " ++ show tag

-- | These are convenient but dangerous.  If they are are used in a context
-- where backward compatibility matters ("Cmd.Serialize") then it's too easy
-- to break compatibility by adding or removing an enum.
--
-- But they're fine if used in an enum that will never change, or where
-- compatibility doesn't matter.
getEnumUnsafe :: (Bounded a, Enum a) => Serialize.Get a
getEnumUnsafe = get >>= \n ->
    maybe (fail $ "enum value out of range: " ++ show n) pure (toEnumS n)

putEnumUnsafe :: Enum a => a -> Serialize.Put
putEnumUnsafe = put . fromEnum

-- | A safe version of 'toEnum'.
toEnumS :: forall a. (Enum a, Bounded a) => Int -> Maybe a
toEnumS n
    | fromEnum (minBound :: a) <= n && n <= fromEnum (maxBound :: a) =
        Just (toEnum n)
    | otherwise = Nothing

getEnum :: Serialize.Get Word.Word8
getEnum = get

-- | It's just put, but make sure it's using Int.  Word8 would be more
-- suitable.
putEnum :: Word.Word8 -> Serialize.Put
putEnum = put

badEnum :: String -> Word.Word8 -> Get a
badEnum name val = fail $ "unknown enum val for " <> name <> ": " <> show val

-- * basic types

instance Serialize () where
    put () = pure ()
    get = pure ()

instance Serialize Bool where
    put False = putTag 0
    put True = putTag 1
    get = (/= 0) <$> getTag

instance Serialize Char where
    put = Serialize.put
    get = Serialize.get

instance Serialize Time.UTCTime where
    put time = put (show time)
    get = get >>= pure . read

-- * sums and products

instance (Serialize a, Serialize b) => Serialize (Either a b) where
    put (Left a) = putTag 0 >> put a
    put (Right b) = putTag 1 >> put b
    get = getTag >>= \case
        0 -> Left <$> get
        1 -> Right <$> get
        tag -> badTag "Either" tag

instance Serialize a => Serialize (Maybe a) where
    put Nothing = putTag 0
    put (Just a) = putTag 1 >> put a
    get = getTag >>= \case
        0 -> pure Nothing
        1 -> Just <$> get
        tag -> badTag "Maybe" tag

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
    put (CallStack.Caller a b) = putTag 0 >> put a >> put b
    put CallStack.NoCaller = putTag 1
    get = getTag >>= \case
        0 -> CallStack.Caller <$> get <*> get
        1 -> pure CallStack.NoCaller
        tag -> badTag "Caller" tag

-- * versions

getVersion :: Get Word.Word8
getVersion = getWord8

putVersion :: Word.Word8 -> Put
putVersion = putWord8

badVersion :: HasCallStack => String -> Word.Word8 -> a
badVersion typ ver = errorStack $
    "unknown version " <> showt ver <> " for " <> showt typ

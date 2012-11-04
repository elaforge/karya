{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{- | Support for generating and parsing sysex files from a "spec" file.

    TODO I need to support disjoint subsections, e.g. the different effects
    blocks depending on the value of an enum.
-}
module Instrument.Sysex where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Serialize.Get as Get
import Data.Word (Word8)

import qualified Numeric
import qualified System.FilePath as FilePath

import Util.Control
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Tag as Tag


-- * parse files

parse_dir :: (ByteString -> Either String Instrument.Patch) -> FilePath
    -> IO [Instrument.Patch]
parse_dir parser dir = do
    fns <- File.list_dir dir
    results <- mapM (parse_file parser) fns
    sequence_ [Log.warn $ "parsing " ++ fn ++ ": " ++ err
        | (fn, Left err) <- zip fns results]
    return [patch | Right patch <- results]

parse_file :: (ByteString -> Either String Instrument.Patch) -> FilePath
    -> IO (Either String Instrument.Patch)
parse_file parser fn = do
    bytes <- B.readFile fn
    case B.uncons (B.drop 1 bytes) of
        Nothing -> return $ Left $ "not a valid sysex: " ++ show bytes
        Just (manuf, rest) -> do
            let init = Instrument.InitializeMidi
                    [Midi.CommonMessage (Midi.SystemExclusive manuf rest)]
            return $ annotate init <$> parser bytes
    where
    annotate init patch = patch
        { Instrument.patch_file = fn
        , Instrument.patch_tags = (Tag.file, FilePath.takeFileName fn)
            : Instrument.patch_tags patch
        , Instrument.patch_initialize = init
        }

-- * record

data Record =
    -- | A List is represented as an RMap with numbered keys.
    RMap (Map.Map Name Record)
    -- | Which one this is is determined by an REnum elsewhere.
    | RUnion Record
    | RNum Int | RStr String | REnum EnumName
    | RUnparsed ByteString
    deriving (Eq, Show)
type Error = String
type EnumName = String

data RecordType = TMap | TUnion | TNum | TStr | TEnum | TUnparsed
    deriving (Show)

-- | Create a Record from a Spec, defaulting everything to 0, "", or the first
-- enum val.
spec_to_record :: [Spec] -> Record
spec_to_record = RMap . Map.delete "" . List.foldl' add Map.empty
    where
    add rec (Bits bits) = List.foldl' add_bit rec bits
    add rec (Str name _) = Map.insert name (RStr "") rec
    add rec (SubSpec name specs) = Map.insert name (spec_to_record specs) rec
    add rec (List name elts specs) = Map.insert name
        (RMap $ Map.fromList $
            zip (map show [0..elts-1]) (repeat (spec_to_record specs)))
        rec
    add rec (Union name _enum_name _bytes ((_, specs) : _)) =
        Map.insert name (RUnion (spec_to_record specs)) rec
    add _ (Union name _enum_name _bytes []) =
        error $ name ++ " had an empty Union"
    add rec (Unparsed name nbytes) =
        Map.insert name (RUnparsed (B.replicate nbytes 0)) rec
    add_bit rec (name, (_, Range {})) = Map.insert name (RNum 0) rec
    add_bit rec (name, (_, Enum (enum : _))) = Map.insert name (REnum enum) rec
    add_bit _ (name, (_, Enum [])) = error $ name ++ " had an empty Enum"

instance Pretty.Pretty Record where
    format rec = case rec of
        RMap x -> Pretty.format x
        RNum x -> Pretty.format x
        RStr x -> Pretty.format x
        REnum x -> Pretty.text x
        RUnion x -> Pretty.format x
        RUnparsed x -> Pretty.text $ show (B.length x) ++ " unparsed bytes"

class RecordVal a where
    from_val :: a -> Record
    to_val :: Record -> Maybe a

instance RecordVal Int where
    from_val = RNum
    to_val (RNum x) = Just x
    to_val _ = Nothing

instance RecordVal String where
    from_val = RStr
    to_val (RStr x) = Just x
    to_val _ = Nothing

newtype EnumVal = EnumVal EnumName deriving (Show)

instance RecordVal EnumVal where
    from_val (EnumVal x) = REnum x
    to_val (REnum x) = Just (EnumVal x)
    to_val _ = Nothing

val_type :: (RecordVal a) => a -> RecordType
val_type = record_type . from_val

record_type :: Record -> RecordType
record_type r = case r of
    RMap {} -> TMap
    RUnion {} -> TUnion
    RNum {} -> TNum
    RStr {} -> TStr
    REnum {} -> TEnum
    RUnparsed {} -> TUnparsed

lookup_record :: forall a. (RecordVal a) => String -> Record -> Either String a
lookup_record path record =
    to_val_error =<< lookup1 (Seq.split "." path) record
    where
    lookup1 [] record = Right record
    lookup1 (field : fields) record = case record of
        RMap m -> lookup1 fields
            =<< maybe (Left $ "not found: " ++ show field)
                Right (Map.lookup field m)
        RUnion rec -> lookup1 (field:fields) rec
        _ -> Left $ "can't lookup " ++ show field ++ " in non-map"
    to_val_error v = case to_val v of
        Nothing -> Left $ path ++ ": expected a "
            ++ show (val_type rtype) ++ " but got " ++ show v
        Just val -> Right val
    rtype :: a
    rtype = error "unevaluated"

put_record :: (RecordVal a) => String -> a -> Record -> Record
put_record path val record = put (Seq.split "." path) val record
    where
    put [] val _ = from_val val
    put (field : fields) val record = case record of
        RMap m ->
            let sub = Map.findWithDefault (RMap mempty) field m
            in RMap $ Map.insert field (put fields val sub) m
        RUnion rec -> put (field:fields) val rec
        _ -> RMap $ Map.singleton field (put fields val (RMap mempty))

-- * util

expect_bytes :: ByteString -> ByteString -> Either String ByteString
expect_bytes bytes prefix
    | pre == prefix = Right post
    | otherwise = Left $ "expected " ++ hex prefix ++ " but got " ++ hex pre
    where (pre, post) = B.splitAt (B.length prefix) bytes

hex :: ByteString -> String
hex = unwords . map (\b -> Numeric.showHex b "") . B.unpack

-- * encode

type EncodeM a = Error.ErrorT Error (Writer.Writer Builder.Builder) a

encode :: [Spec] -> Record -> Either Error ByteString
encode specs record = run_encode (mapM_ (encode_spec [] record) specs)

run_encode :: EncodeM () -> Either Error ByteString
run_encode m = case Writer.runWriter (Error.runErrorT m) of
    (Left err, _) -> Left err
    (Right (), builder) ->
        Right $ Lazy.toStrict $ Builder.toLazyByteString builder

encode_spec :: [String] -> Record -> Spec -> EncodeM ()
encode_spec path record spec = case spec of
    Bits bits -> do
        b <- either (uncurry throw) return $ encode_byte record bits
        Writer.tell (Builder.word8 b)
    Str name chars -> do
        str <- lookup_field name >>= \x -> case x of
            RStr str -> return str
            val -> throw name $ "expected RStr, but got " ++ show val
        let diff = chars - length str
        padded <- if diff >= 0
            then return $ str ++ replicate diff ' '
            else throw name $
                "too many characters, expected " ++ show chars
                ++ ": " ++ show str
        Writer.tell (Builder.string7 padded)
    SubSpec name specs -> do
        sub_record <- lookup_field name
        mapM_ (encode_spec (name:path) sub_record) specs
    List name elts specs -> do
        records <- lookup_field name >>= \x -> case x of
            RMap records
                | Map.size records == elts ->
                    mapM lookup_field (map show [0..elts-1])
                | otherwise -> throw name $ "expected RMap list of length "
                    ++ show elts ++ " but got length "
                    ++ show (Map.size records)
            val -> throw name $ "expected RMap list, but got " ++ show val
        forM_ (zip [0..] records) $ \(i, rec) ->
            mapM_ (encode_spec ((name ++ show i) : path) rec) specs
    Union name enum_name nbytes enum_specs -> do
        union_record <- lookup_field name >>= \x -> case x of
            RUnion union_record -> return union_record
            val -> throw name $ "expected RUnion, but got " ++ show val
        enum <- lookup_field enum_name >>= \x -> case x of
            REnum enum -> return enum
            val -> throw name $ "expeted REnum, but got " ++ show val
        specs <- case lookup enum enum_specs of
            Just specs -> return specs
            Nothing -> throw name $ "not found in union "
                ++ show (map fst enum_specs) ++ ": " ++ enum
        bytes <- either Error.throwError return $
            run_encode (mapM_ (encode_spec (name:path) union_record) specs)
        Writer.tell $ Builder.byteString $ bytes
            <> B.replicate (nbytes - B.length bytes) 0
    Unparsed name nbytes -> do
        bytes <- lookup_field name >>= \x -> case x of
            RUnparsed bytes
                | B.length bytes /= nbytes -> throw name $
                    "Unparsed expected " ++ show nbytes ++ " bytes but got "
                    ++ show (B.length bytes)
                | otherwise -> return bytes
            val -> throw name $ "expected RUnparsed, but got " ++ show val

        Writer.tell (Builder.byteString bytes)
    where
    lookup_field = either (uncurry throw) return . rmap_lookup record
    throw name msg = Error.throwError $ show_path (name:path) ++ msg

encode_byte :: Record -> [(Name, BitField)] -> Either (Name, Error) Word8
encode_byte record bits = do
    let (names, fields) = unzip bits
    vals <- mapM (rmap_lookup record) names
    bs <- zipWithM encode1 (zip names fields) vals
    return $ encode_bits (map fst fields) bs
    where
    encode1 (name, (width, Range low high)) (RNum val)
        | not (low <= val && val <= high) =
            Left (name, "val out of range " ++ show (low, high) ++ ": "
                ++ show val)
        | low < 0 = Right $ from_signed width val
        | otherwise = Right (fromIntegral val)
    encode1 (name, (_, Enum enums)) (REnum enum) =
        case List.elemIndex enum enums of
            Nothing -> Left (name, "unknown enum: " ++ enum)
            Just i -> return $ fromIntegral i
    encode1 (name, field) val = Left
        (name, "field " ++ show field ++ " not appropriate for " ++ show val)

rmap_lookup :: Record -> Name -> Either (Name, Error) Record
rmap_lookup _ "" = Right $ RNum 0 -- null name means it's a reserved section
rmap_lookup record name = case record of
    RMap rmap -> case Map.lookup name rmap of
        Nothing -> Left (name, "not found")
        Just val -> Right val
    _ -> Left (name, "can't lookup name in non-map " ++ show record)

-- * decode

decode :: [Spec] -> ByteString -> Either Error (Record, ByteString)
decode = decode_from []
    where
    decode_from path specs bytes = Get.runGetState (rmap path [] specs) bytes 0
    rmap _ collect [] = return $ RMap (Map.fromList collect)
    rmap path collect (spec:specs) = do
        vals <- field path collect spec
        -- Debug.tracepM "vals" vals
        rmap path (vals ++ collect) specs

    field :: [Name] -> [(Name, Record)] -> Spec -> Get.Get [(String, Record)]
    field path _ (Bits bits) =
        either (throw path) return . decode_byte path bits =<< Get.getWord8
    field _ _ (Str name chars) = do
        str <- Get.getByteString chars
        return [(name, RStr (Seq.strip (Char8.unpack str)))]
    field path _ (SubSpec name specs) = do
        subs <- rmap (name : path) [] specs
        return [(name, subs)]
    field path _ (List name elts specs) = do
        subs <- forM [0 .. elts-1] $ \i ->
            rmap ((name ++ show i) : path) [] specs
        return [(name, RMap $ Map.fromList $ zip (map show [0..elts-1]) subs)]
    field path prev_record (Union name enum_name bytes enum_specs) = do
        path <- return (name : path)
        enum <- case lookup enum_name prev_record of
            Just (REnum enum) -> return enum
            _ -> throw path $ "previous enum not found: " ++ enum_name
        specs <- case lookup enum enum_specs of
            Just specs -> return specs
            Nothing -> throw path $
                "union doesn't contain enum: " ++ enum
        bytes <- Get.getByteString bytes
        record <- either (throw path) (return . fst) $
            decode_from path specs bytes
        return [(name, RUnion record)]
    field path _ (Unparsed name nbytes) = do
        bytes <- Get.getByteString nbytes
        when (B.length bytes < nbytes) $
            throw path $ "expected " ++ show nbytes ++ " bytes, but got "
                ++ show (B.length bytes)
        return [(name, RUnparsed bytes)]
    throw path = fail . (show_path path ++)

decode_byte :: [Name] -> [(Name, BitField)] -> Word8
    -> Either String [(String, Record)]
decode_byte path bits byte =
    -- Null names are reserved bytes.
    filter (not . null . fst) <$> zipWithM extract bits signed
    where
    vals = decode_bits [width | (_, (width, _)) <- bits] byte
    signed = zipWith convert_signs (map snd bits) vals
    extract (name, (_, field)) val = case field of
        Range low high
            | low <= val && val <= high -> return (name, RNum val)
            | otherwise -> Left $
                show_path (name:path) ++ "out of range: " ++ show val
        Enum enums
            | Just enum <- Seq.at enums val -> return (name, REnum enum)
            | otherwise -> Left $
                show_path (name:path) ++ " bit of byte " ++ show byte
                    ++ ": not a valid enum index: " ++ show val
    convert_signs :: BitField -> Word8 -> Int
    convert_signs (width, range) val
        | is_signed range = to_signed width val
        | otherwise = fromIntegral val
    is_signed (Range low _) = low < 0
    is_signed _ = False

show_path :: [Name] -> String
show_path = (++": ") . Seq.join "." . reverse

-- ** bit fiddling

encode_bits :: [Int] -> [Word8] -> Word8
encode_bits widths vals =
    List.foldl' (.|.) 0 $ zipWith Bits.shiftL vals offsets
    where offsets = scanl (+) 0 widths

decode_bits :: [Int] -> Word8 -> [Word8]
decode_bits widths byte = zipWith extract bs (drop 1 bs)
    where
    bs = scanl (+) 0 widths
    extract start end = Bits.shiftR (set start end .&. byte) start
    set start end = List.foldl' Bits.setBit 0 [start .. end-1]

-- | Convert an n bit 2s complement word to a signed integer.
to_signed :: Int -> Word8 -> Int
to_signed bits b
    | Bits.testBit b (bits-1) = negate $
        fromIntegral $ (Bits.complement b .&. (2^bits - 1)) + 1
    | otherwise = fromIntegral b

-- | Convert a signed integer to an n bit 2s complement word.
from_signed :: Int -> Int -> Word8
from_signed bits num
    | clamped < 0 = fromIntegral (2^bits + clamped)
    | otherwise = fromIntegral clamped
    where clamped = Num.clamp (-2^(bits-1)) (2^(bits-1) - 1) num


-- * spec

data Spec =
    Bits [(Name, BitField)] | Str Name Bytes | SubSpec Name [Spec]
    | List Name Int [Spec]
    -- | The content of this section depends on a previous enum value.
    -- Name of this field, name of the enum to reference
    | Union Name Name Bytes [(EnumName, [Spec])]
    | Unparsed Name Bytes
    deriving (Show)

data Range = Range Int Int | Enum [EnumName]
    deriving (Show)

type BitField = (Bits, Range)
type Name = String
type Bits = Int
type Bytes = Int

validate :: [Spec] -> Maybe String
validate specs = msum (map check specs)
    where
    check (Bits bits)
        | total /= 8 = Just $
            show (map fst bits) ++ " - bits should sum to 8: " ++ show total
        | otherwise = Nothing
        where total = sum [n | (_, (n, _)) <- bits]
    check (Union name enum_name _bytes fields) = case lookup_spec enum_name of
        Just (Left (_, Enum enums))
            | List.sort enums /= List.sort (map fst fields) -> Just $
                name ++ ": enums not equal: " ++ show (enums, map fst fields)
            -- TODO fields sum up to < bytes
            -- check recursively on specs
        _ -> Just $ name ++ ": enum not found"
    check _ = Nothing
    lookup_spec wanted = msum (map find specs)
        where
        find spec = case spec of
            Bits bits -> msum (map find_bit bits)
            Str name _ | name == wanted -> Just $ Right spec
            SubSpec name _ | name == wanted -> Just $ Right spec
            List name _ _ | name == wanted -> Just $ Right spec
            Union name _ _ _ | name == wanted -> Just $ Right spec
            _ -> Nothing
        find_bit (name, field)
            | name == wanted = Just $ Left field
            | otherwise = Nothing

-- | Hokey runtime check to make sure Bits constructors all add up to one byte.
assert_valid :: String -> [Spec] -> [Spec]
assert_valid name spec =
    maybe spec (error . ((name ++ ": ") ++)) (validate spec)

bits :: Int -> BitField
bits n = (n, Range 0 (2^n))

ranged_bits :: Int -> (Int, Int) -> BitField
ranged_bits n (min, max) = (n, Range min max)

ranged_byte :: Name -> (Int, Int) -> Spec
ranged_byte name range = Bits [(name, ranged_bits 8 range)]

byte :: Name -> Int -> Spec
byte name max = Bits [(name, (8, Range 0 max))]

reserved_space :: Int -> [Spec]
reserved_space bytes = replicate bytes (byte "" 256)

enum_byte :: Name -> [String] -> Spec
enum_byte name vals = Bits [(name, (8, Enum vals))]

enum_bits :: Int -> [String] -> BitField
enum_bits n vals = (n, Enum vals)

boolean :: Range
boolean = Enum ["off", "on"]

boolean1 :: BitField
boolean1 = (1, boolean)

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Support for generating and parsing sysex files from a "spec" file.

    TODO I need to support disjoint subsections, e.g. the different effects
    blocks depending on the value of an enum.
-}
module Instrument.Sysex where
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Bits as Bits
import           Data.Bits ((.&.), (.|.))
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Serialize.Get as Get
import qualified Data.Text as Text
import           Data.Word (Word8)

import qualified Numeric
import qualified System.FilePath as FilePath

import qualified Util.FFI as FFI
import qualified Util.Files as Files
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Strings as Strings

import qualified Instrument.Common as Common
import qualified Instrument.Tag as Tag
import qualified Midi.Encode
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global


-- * parse files

type Parser a = ByteString -> Either String a
type Patch = (Patch.Patch, Common.Common ())

-- | For every file below the directory ending with .syx, try all of the
-- given parsers on it.
parse_dir :: [Parser [Patch]] -> FilePath -> IO [Patch]
parse_dir parsers dir = do
    fns <- filter ((==".syx") . FilePath.takeExtension) <$>
        Files.listRecursive (const True) dir
    results <- mapM (\fn -> parse_file parsers fn <$> B.readFile fn) fns
    sequence_ [Log.warn $ "parsing " <> txt fn <> ": " <> txt err
        | (fn, Left err) <- zip fns results]
    return $ concat [patches | Right patches <- results]

parse_file :: [Parser [Patch]] -> FilePath -> ByteString
    -> Either String [Patch]
parse_file parsers fn bytes =
    map (bimap (initialize bytes) (add_file fn)) <$> try_parsers parsers bytes
    where
    -- Only add the sysex if the parser hasn't already added one.  This is
    -- because some parsers may parse things that aren't actually sysexes.
    initialize bytes patch = case Patch.initialize #$ patch of
        Patch.NoInitialization -> initialize_sysex bytes patch
        _ -> patch

-- | Parse a file just like 'parse_file'.  But this file is expected to be
-- the dump of the patches currently loaded in the synthesizer, and will be
-- given ProgramChange msgs for initialization rather than sysex dumps.
parse_builtins :: Int -> Parser [Patch] -> FilePath -> IO [Patch]
parse_builtins bank parser fn = do
    bytes <- B.readFile fn
    case parser bytes of
        Left err -> do
            Log.warn $ "parsing " <> txt fn <> ": " <> txt err
            return []
        Right patches ->
            return $ zipWith (initialize_program bank) [0..] patches

-- | Try each parser in turn, and fail only if they all fail.
try_parsers :: [Parser a] -> ByteString -> Either String a
try_parsers parsers bytes = case Either.rights results of
    patches : _ -> Right patches
    _ -> Left $ "didn't match any parsers: "
        <> Lists.join "; " (map Strings.strip (Either.lefts results))
    where results = map ($bytes) parsers

-- | Assume the sysex midi channel is 0.
initialize_program :: Int -> Midi.Program -> Patch -> Patch
initialize_program bank n = first $
    Patch.initialize #= Patch.initialize_midi (Midi.program_change bank n)

initialize_sysex :: ByteString -> Patch.Patch -> Patch.Patch
initialize_sysex bytes =
    Patch.initialize #= Patch.InitializeMidi [Midi.Encode.decode bytes]

add_file :: FilePath -> Common.Common a -> Common.Common a
add_file fn = Common.tags %= ((Tag.file, txt (FilePath.takeFileName fn)) :)

-- * record

type RMap = Map Name Record
data Record =
    -- | A List is represented as an RMap with numbered keys.
    RMap RMap
    -- | Which one this is is determined by an RStr elsewhere.
    | RUnion RMap
    | RNum Int | RStr Text
    | RUnparsed ByteString
    deriving (Eq, Show)
type Error = String
type EnumName = Text

data RecordType = TMap | TUnion | TNum | TStr | TUnparsed
    deriving (Eq, Show)

-- | Create a Record from a Spec, defaulting everything to 0, \"\", or the
-- first enum val.
spec_to_rmap :: Specs -> RMap
spec_to_rmap = foldl' add Map.empty
    where
    spec_to_record = RMap . spec_to_rmap
    add rec (name, spec) = case spec of
        Num (Range {}) -> Map.insert name (RNum 0) rec
        Num (Enum (enum:_)) -> Map.insert name (RStr enum) rec
        Num (Enum []) -> error $ name <> " had an empty Enum"

        Bits bits -> foldl' add_bit rec bits
        Str _ -> Map.insert name (RStr "") rec
        SubSpec specs -> Map.insert name (spec_to_record specs) rec
        List elts specs -> Map.insert name
            (RMap $ Map.fromList $
                zip (map show [0..elts-1]) (repeat (spec_to_record specs)))
            rec
        Union _enum_name _bytes ((_, specs) : _) ->
            Map.insert name (RUnion (spec_to_rmap specs)) rec
        Union _enum_name _bytes [] ->
            error $ name <> " had an empty Union"
        Unparsed nbytes
            | null name -> rec
            | otherwise ->
                Map.insert name (RUnparsed (B.replicate nbytes 0)) rec
        Constant {} -> rec

    add_bit rec (name, (_, range)) = case range of
        Range {} -> Map.insert name (RNum 0) rec
        Enum (enum : _) -> Map.insert name (RStr enum) rec
        Enum [] -> error $ name <> " had an empty Enum"

instance Pretty Record where
    format rec = case rec of
        RMap x -> Pretty.format x
        RNum x -> Pretty.format x
        RStr x -> Pretty.format x
        RUnion x -> Pretty.format x
        RUnparsed x -> Pretty.text $ showt (B.length x) <> " unparsed bytes"

-- | Show an RMap as a flat list of paths and values.
show_flat :: RMap -> [String]
show_flat = show_map []
    where
    show_map fields = concatMap (uncurry (show1 fields)) . Map.toAscList
    show1 fields field val = case val of
            RMap rmap -> show_map (field : fields) rmap
            RUnion rmap -> show_map (field : fields) rmap
            RNum n -> [path <> show n]
            RStr s -> [path <> untxt s]
            RUnparsed {} -> []
        where
        path = Lists.join "." (reverse (field : fields)) <> ": "

class RecordVal a where
    from_val :: a -> Record
    to_val :: Record -> Maybe a

instance RecordVal Int where
    from_val = RNum
    to_val (RNum x) = Just x
    to_val _ = Nothing

instance RecordVal Word8 where
    from_val = RNum . fromIntegral
    to_val (RNum x) = Just (fromIntegral x)
    to_val _ = Nothing

instance RecordVal Text where
    from_val = RStr
    to_val (RStr x) = Just x
    to_val _ = Nothing

instance RecordVal ByteString where
    from_val = RUnparsed
    to_val (RUnparsed x) = Just x
    to_val _ = Nothing

val_type :: (RecordVal a) => a -> RecordType
val_type = record_type . from_val

record_type :: Record -> RecordType
record_type r = case r of
    RMap {} -> TMap
    RUnion {} -> TUnion
    RNum {} -> TNum
    RStr {} -> TStr
    RUnparsed {} -> TUnparsed

get_rmap :: forall a. (RecordVal a) => String -> RMap -> Either String a
get_rmap path rmap = to_val_error $ lookup1 (Lists.split "." path) rmap
    where
    lookup1 [] _ = Left ([], "can't lookup empty field")
    lookup1 (field : fields) rmap = case Map.lookup field rmap of
        Nothing -> Left ([field], "not found")
        Just record
            | null fields -> Right record
            | otherwise -> case record of
                RMap submap -> case lookup1 fields submap of
                    Left (children, msg) -> Left (field : children, msg)
                    Right val -> Right val
                RUnion submap -> lookup1 (field:fields) submap
                _ -> Left ([field], "can't lookup field in non-map")
    to_val_error (Left (fields, msg)) =
        Left $ Lists.join "." fields <> ": " <> msg
    to_val_error (Right v) = case to_val v of
        Nothing -> Left $ path <> ": expected a "
            <> show (val_type rtype) <> " but got " <> show v
        Just val -> Right val
    rtype :: a
    rtype = error "unevaluated"

-- | Put the given val into the rmap at a certain path.  This only modifies
-- existing fields, it won't create new ones, and you can't change the type
-- of a field.
put_rmap :: (Show a, RecordVal a) => String -> a -> RMap -> Either String RMap
put_rmap path val rmap = format_err $ put (Lists.split "." path) val rmap
    where
    put [] _ _ = Left ([], "can't put empty field")
    put (field : fields) val rmap = case Map.lookup field rmap of
        Nothing -> Left ([field], "not found")
        Just record
            | next_field : _ <- fields -> case record of
                RMap submap -> case put fields val submap of
                    Left (children, msg) -> Left (field:children, msg)
                    Right submap -> Right $ Map.insert field (RMap submap) rmap
                RUnion submap -> put (field:fields) val submap
                _ -> Left ([field], "can't lookup field " <> show next_field
                    <> " in non-map")
            | record_type record /= record_type (from_val val) ->
                Left ([field], "old val " <> show record
                    <> " is a different type than " <> show val)
            | otherwise -> Right $ Map.insert field (from_val val) rmap
    format_err (Left (fields, msg)) =
        Left $ Lists.join "." fields <> ": " <> msg
    format_err (Right val) = Right val

-- * util

expect_bytes :: ByteString -> ByteString -> Either String ByteString
expect_bytes bytes prefix
    | pre == prefix = Right post
    | otherwise = Left $ "expected " <> hex prefix <> " but got " <> hex pre
    where (pre, post) = B.splitAt (B.length prefix) bytes

hex :: ByteString -> String
hex = unwords . map (\b -> Numeric.showHex b "") . B.unpack

-- | Extract substrings delimited by sox_byte and eox_byte.  Bytes not within
-- the delimeters are stripped.
extract_sysex :: ByteString -> [ByteString]
extract_sysex bytes
    | B.null bytes = []
    | not $ B.singleton Midi.Encode.eox_byte `B.isSuffixOf` sysex = []
    | B.null sysex = extract_sysex post
    | otherwise = sysex : extract_sysex post
    where
    (sysex, post) = break_after (==Midi.Encode.eox_byte) $
        B.dropWhile (/=Midi.Encode.sox_byte) bytes

break_after :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break_after f bytes = case B.findIndex f bytes of
    Nothing -> (bytes, mempty)
    Just i -> B.splitAt (i+1) bytes

-- * config

data Config = Config {
    decode_num :: NumRange -> ByteString -> Int
    , encode_num :: NumRange -> Int -> ByteString
    -- | The number of bytes needd to encode a number in the given range.
    -- This should agree with 'decode_num', since it will be given that many
    -- bytes to decode.
    , range_bytes :: NumRange -> Int
    }

-- | Encode for 8bit bytes, where numbers are never more than 1 byte.
config_8bit :: Config
config_8bit = Config decode_8bit_num encode_8bit_num (const 1)
    where
    decode_8bit_num :: NumRange -> ByteString -> Int
    decode_8bit_num (low, _) bytes = case B.uncons bytes of
        Nothing -> 0
        Just (b, _)
            | low < 0 -> to_signed 8 b
            | otherwise -> fromIntegral b
    encode_8bit_num :: NumRange -> Int -> ByteString
    encode_8bit_num (low, _) num
        | low < 0 = B.singleton $ from_signed 8 num
        | otherwise = B.singleton (fromIntegral num)

-- * encode

type EncodeM a = Except.ExceptT Error (Writer.Writer Builder.Builder) a

encode :: Config -> Specs -> RMap -> Either Error ByteString
encode config specs rmap = run_encode (mapM_ (encode_spec config [] rmap) specs)

run_encode :: EncodeM () -> Either Error ByteString
run_encode m = case Writer.runWriter (Except.runExceptT m) of
    (Left err, _) -> Left err
    (Right (), builder) ->
        Right $ Lazy.toStrict $ Builder.toLazyByteString builder

encode_spec :: Config -> [String] -> RMap -> (Name, Spec) -> EncodeM ()
encode_spec config path rmap (name, spec) = case spec of
    Num range -> do
        num <- either throw return . encode_range range
            =<< lookup_field name
        Writer.tell $ Builder.byteString $
            encode_num config (num_range range) num
    Bits bits -> do
        b <- either (uncurry throw_with) return $ encode_byte rmap bits
        Writer.tell (Builder.word8 b)
    Str chars -> do
        str <- lookup_field name >>= \case
            RStr str -> return str
            val -> throw $ "expected RStr, but got " <> show val
        let diff = chars - Text.length str
        padded <- if diff >= 0
            then return $ str <> Text.replicate diff " "
            else throw $ "too many characters, expected " <> show chars
                <> ": " <> show str
        Writer.tell (Builder.string7 $ untxt padded)
    SubSpec specs -> do
        sub_record <- lookup_field name >>= \case
            RMap rmap -> return rmap
            rec -> throw $ "non-RMap child of a SubSpec: " <> prettys rec
        mapM_ (encode_spec config (name:path) sub_record) specs
    List elts specs -> do
        records <- lookup_field name >>= \case
            RMap rmap
                | Map.size rmap == elts ->
                    mapM (\k -> lookup_map (show k) rmap) [0..elts-1]
                | otherwise -> throw $ "expected RMap list of length "
                    <> show elts <> " but got length "
                    <> show (Map.size rmap)
            val -> throw $ "expected RMap list, but got " <> prettys val
        rmaps <- forM records $ \case
            RMap rmap -> return rmap
            rec -> throw $ "non-RMap child of an RMap list: " <> prettys rec
        forM_ (zip [0..] rmaps) $ \(i, rmap) ->
            mapM_ (encode_spec config (name : show i : path) rmap) specs
    Union enum_name nbytes enum_specs -> do
        union_rmap <- lookup_field name >>= \case
            RUnion union_rmap -> return union_rmap
            val -> throw $ "expected RUnion RMap, but got " <> prettys val
        enum <- lookup_field enum_name >>= \case
            RStr enum -> return enum
            val -> throw $ "expeted RStr, but got " <> prettys val
        specs <- case lookup enum enum_specs of
            Just specs -> return specs
            Nothing -> throw $ "not found in union "
                <> show (map fst enum_specs) <> ": " <> untxt enum
        bytes <- tryRight $ run_encode $
            mapM_ (encode_spec config (name:path) union_rmap) specs
        Writer.tell $ Builder.byteString $ bytes
            <> B.replicate (nbytes - B.length bytes) 0
    Unparsed nbytes
        | null name -> Writer.tell $ Builder.byteString $ B.replicate nbytes 0
        | otherwise -> do
            bytes <- lookup_field name >>= \case
                RUnparsed bytes
                    | B.length bytes /= nbytes -> throw $
                        "Unparsed expected " <> show nbytes
                        <> " bytes but got " <> show (B.length bytes)
                    | otherwise -> return bytes
                val -> throw $ "expected RUnparsed, but got " <> prettys val
            Writer.tell (Builder.byteString bytes)
    Constant bytes -> Writer.tell (Builder.byteString bytes)
    where
    lookup_map k rmap = maybe (throw (k <> " not found")) return $
        Map.lookup k rmap
    lookup_field field = case rmap_lookup rmap field of
            Left err -> Except.throwError $ prefix <> err
            Right val -> return val
        where
        prefix
            | field == name = show_path (name : path)
            | otherwise = show_path (field : name : path)
    throw msg = Except.throwError $ show_path (name:path) <> msg
    throw_with field msg = Except.throwError $ show_path (field:path) <> msg

encode_byte :: RMap -> [(Name, BitField)] -> Either (Name, Error) Word8
encode_byte rmap bits = do
    let (names, fields) = unzip bits
    vals <- mapM (\name -> add_name name $ rmap_lookup rmap name) names
    bs <- zipWithM encode1 (zip names fields) vals
    return $ encode_bits (map fst fields) bs
    where
    add_name name = first ((,) name)
    encode1 (name, (width, range)) rec = add_name name $ do
        num <- encode_range range rec
        return $ if range_signed range then from_signed width num
            -- encode_range should have already checked the range of 'num'.
            else fromIntegral num

encode_range :: Range -> Record -> Either String Int
encode_range (Range low high) (RNum num)
    | low <= num && num <= high = Right num
    | otherwise = Left $ "num out of range " <> show (low, high) <> ": "
        <> show num
encode_range (Enum enums) (RStr enum)
    | Just i <- List.elemIndex enum enums = Right i
    | otherwise = Left $ "unknown enum: " <> untxt enum
encode_range _ record =
    Left $ "expected a num or str, but got " <> show record

rmap_lookup :: RMap -> Name -> Either Error Record
rmap_lookup rmap name = case Map.lookup name rmap of
    Nothing -> Left $ "not found in: " <> prettys (Map.keys rmap)
    Just val -> Right val

-- * decode

decode :: Config -> Specs -> ByteString -> Either Error (RMap, ByteString)
decode config = decode_from []
    where
    decode_from path specs bytes = first Strings.strip $
        Get.runGetState (rmap path [] specs) bytes 0
    rmap _ collect [] = return (Map.fromList collect)
    rmap path collect (spec:specs) = do
        vals <- field path collect spec
        rmap path (vals ++ collect) specs

    field :: [Name] -> [(Name, Record)] -> (Name, Spec)
        -> Get.Get [(String, Record)]
    field path prev_record (name, spec) = case spec of
        Bits bits ->
            either fail return . decode_byte path bits =<< Get.getWord8
        Num range -> do
            bytes <- Get.getBytes $ range_bytes config (num_range range)
            let num = decode_num config (num_range range) bytes
            either throw (return . (:[]) . (,) name) $ decode_range num range
        Str chars -> do
            str <- Get.getByteString chars
            return [(name, RStr $ Text.strip $ FFI.decodeUtf8 str)]
        SubSpec specs -> do
            subs <- rmap (name : path) [] specs
            return [(name, RMap subs)]
        List elts specs -> do
            subs <- forM [0 .. elts-1] $ \i ->
                RMap <$> rmap (show i : name : path) [] specs
            return [(name, RMap $ Map.fromList $
                zip (map show [0..elts-1]) subs)]
        Union enum_name bytes enum_specs -> do
            path <- return (name : path)
            enum <- case lookup enum_name prev_record of
                Just (RStr enum) -> return enum
                _ -> throw $ "previous enum not found: " <> enum_name
            specs <- case lookup enum enum_specs of
                Just specs -> return specs
                Nothing -> throw $ "union doesn't contain enum: " <> untxt enum
            bytes <- Get.getByteString bytes
            record <- either fail (return . fst) $ decode_from path specs bytes
            return [(name, RUnion record)]
        Unparsed nbytes
            | null name -> Get.skip nbytes >> return []
            | otherwise -> do
                bytes <- Get.getByteString nbytes
                when (B.length bytes < nbytes) $
                    throw $ "expected " <> show nbytes <> " bytes, but got "
                        <> show (B.length bytes)
                return [(name, RUnparsed bytes)]
        Constant expected -> do
            bytes <- Get.getByteString (B.length expected)
            if bytes == expected then return []
                else throw $ "expected " <> hex expected <> " but got "
                    <> hex bytes
        where
        throw = fail . (show_path (name:path) <>)

decode_range :: Int -> Range -> Either String Record
decode_range num (Range low high)
    | low <= num && num <= high = Right (RNum num)
    | otherwise = Left $ "out of range " <> show (low, high) <> ": "
        <> show num
decode_range num (Enum enums)
    | Just enum <- Lists.at enums num = Right (RStr enum)
    | otherwise = Left $ "out of range for enum: " <> show num

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
                show_path (name:path) <> "out of range: " <> show val
        Enum enums
            | Just enum <- Lists.at enums val -> return (name, RStr enum)
            | otherwise -> Left $
                show_path (name:path) <> "bit of byte " <> show byte
                    <> ": not a valid enum index: " <> show val
    convert_signs :: BitField -> Word8 -> Int
    convert_signs (width, range) val
        | range_signed range = to_signed width val
        | otherwise = fromIntegral val

show_path :: [Name] -> String
show_path = (<>": ") . Lists.join "." . reverse

-- ** bit fiddling

encode_bits :: [Int] -> [Word8] -> Word8
encode_bits widths vals = foldl' (.|.) 0 $ zipWith Bits.shiftL vals offsets
    where offsets = scanl (+) 0 widths

decode_bits :: [Int] -> Word8 -> [Word8]
decode_bits widths byte = zipWith extract bs (drop 1 bs)
    where
    bs = scanl (+) 0 widths
    extract start end = Bits.shiftR (set start end .&. byte) start
    set start end = foldl' Bits.setBit 0 [start .. end-1]

-- | Convert an n bit 2s complement word to a signed integer.
to_signed :: (Integral a, Bits.Bits a) => Int -> a -> Int
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

type Specs = [(Name, Spec)]
data Spec =
    -- | A set of bitfields encoded in a single byte.  The BitField widths must
    -- add up to 8.  They start at the least significant end of the byte, so
    -- given @[(\"a\", (1, Range 0 1)), (\"b\", (7, Range 0 1))]@, @a@ is from
    -- the least significant bit.
    Bits [(Name, BitField)]
    | Num Range | Str Bytes | SubSpec Specs
    | List Int Specs
    -- | The content of this section depends on a previous enum value.
    -- The Name is the name of the enum to reference
    | Union Name Bytes [(EnumName, Specs)]
    -- | A chunk of unparsed bytes.  If the name is \"\", then its considered
    -- unused padding.  On input it will be ignored, and on output will become
    -- zeros.
    | Unparsed Bytes
    -- | Assert that these bytes are set literally.  This is useful for
    -- failing quickly when the required header isn't found.
    | Constant ByteString
    deriving (Show)

data Range = Range Int Int | Enum [EnumName]
    deriving (Show)
type NumRange = (Int, Int)

num_range :: Range -> NumRange
num_range (Range low high) = (low, high)
num_range (Enum enums) = (0, length enums - 1)

range_signed :: Range -> Bool
range_signed (Range low _) = low < 0
range_signed _ = False

type BitField = (Bits, Range)
type Name = String
type Bits = Int
type Bytes = Int

spec_bytes :: Config -> Specs -> Int
spec_bytes config = Num.sum . map (bytes_of . snd)
    where
    bytes_of (Bits {}) = 1
    bytes_of (Num range) = range_bytes config (num_range range)
    bytes_of (Str n) = n
    bytes_of (SubSpec specs) = spec_bytes config specs
    bytes_of (List n specs) = spec_bytes config specs * n
    bytes_of (Union _ n _) = n
    bytes_of (Unparsed n) = n
    bytes_of (Constant bytes) = B.length bytes

validate :: Specs -> Maybe String
validate specs = msum (map check specs)
    where
    -- TODO assert each name is unique
    -- names can't have dots
    check (_, Bits bits)
        | total /= 8 = Just $
            show (map fst bits) <> " - bits should sum to 8: " <> show total
        | otherwise = Nothing
        where total = Num.sum [n | (_, (n, _)) <- bits]
    check (name, Union enum_name _bytes fields) = case lookup_spec enum_name of
        Just (Left (_, Enum enums))
            | List.sort enums /= List.sort (map fst fields) -> Just $
                name <> ": enums not equal: " <> show (enums, map fst fields)
            -- TODO fields sum up to < bytes
            -- check recursively on specs
        _ -> Just $ name <> ": enum not found"
    check _ = Nothing
    lookup_spec wanted = msum (map find specs)
        where
        find (name, spec)
            | Bits bits <- spec = msum (map find_bit bits)
            | name == wanted = Just $ Right spec
            | otherwise = Nothing
        find_bit (name, field)
            | name == wanted = Just $ Left field
            | otherwise = Nothing

-- | Hokey runtime check to make sure the Specs is valid and has the expected
-- size.
assert_valid :: Config -> String -> Int -> Specs -> Specs
assert_valid config name size specs
    | Just err <- validate specs = crash err
    | actual_size /= size = crash $ "expected " <> show size
        <> " bytes, but was " <> show actual_size
    | otherwise = specs
    where
    actual_size = spec_bytes config specs
    crash msg = error $ name <> ": " <> msg

-- * convenience

-- ** num

unsigned :: Int -> Spec
unsigned max = Num (Range 0 max)

ranged :: Int -> Int -> Spec
ranged low high = Num (Range low high)

signed :: Int -> Spec
signed high = ranged (-high) high

enum :: [EnumName] -> Spec
enum enums = Num (Enum enums)

bool :: Spec
bool = enum ["off", "on"]

-- ** bits

bits :: Int -> BitField
bits n = (n, Range 0 (2^n))

ranged_bits :: Int -> (Int, Int) -> BitField
ranged_bits n (low, high) = (n, Range low high)

enum_bits :: Int -> [EnumName] -> BitField
enum_bits n vals = (n, Enum vals)

bool_bit :: BitField
bool_bit = (1, Enum ["off", "on"])

unparsed_bits :: Int -> (String, BitField)
unparsed_bits n = ("", bits n)

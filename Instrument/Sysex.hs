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
parseDir :: [Parser [Patch]] -> FilePath -> IO [Patch]
parseDir parsers dir = do
    fns <- filter ((==".syx") . FilePath.takeExtension) <$>
        Files.listRecursive (const True) dir
    results <- mapM (\fn -> parseFile parsers fn <$> B.readFile fn) fns
    sequence_ [Log.warn $ "parsing " <> txt fn <> ": " <> txt err
        | (fn, Left err) <- zip fns results]
    pure $ concat [patches | Right patches <- results]

parseFile :: [Parser [Patch]] -> FilePath -> ByteString
    -> Either String [Patch]
parseFile parsers fn bytes =
    map (bimap (initialize bytes) (addFile fn)) <$> tryParsers parsers bytes
    where
    -- Only add the sysex if the parser hasn't already added one.  This is
    -- because some parsers may parse things that aren't actually sysexes.
    initialize bytes patch = case Patch.initialize #$ patch of
        Patch.NoInitialization -> initializeSysex bytes patch
        _ -> patch

-- | Parse a file just like 'parse_file'.  But this file is expected to be
-- the dump of the patches currently loaded in the synthesizer, and will be
-- given ProgramChange msgs for initialization rather than sysex dumps.
parseBuiltins :: Int -> Parser [Patch] -> FilePath -> IO [Patch]
parseBuiltins bank parser fn = do
    bytes <- B.readFile fn
    case parser bytes of
        Left err -> do
            Log.warn $ "parsing " <> txt fn <> ": " <> txt err
            pure []
        Right patches ->
            pure $ zipWith (initializeProgram bank) [0..] patches

-- | Try each parser in turn, and fail only if they all fail.
tryParsers :: [Parser a] -> ByteString -> Either String a
tryParsers parsers bytes = case Either.rights results of
    patches : _ -> Right patches
    _ -> Left $ "didn't match any parsers: "
        <> Lists.join "; " (map Strings.strip (Either.lefts results))
    where results = map ($bytes) parsers

-- | Assume the sysex midi channel is 0.
initializeProgram :: Int -> Midi.Program -> Patch -> Patch
initializeProgram bank n = first $
    Patch.initialize #= Patch.initializeMidi (Midi.programChange bank n)

initializeSysex :: ByteString -> Patch.Patch -> Patch.Patch
initializeSysex bytes =
    Patch.initialize #= Patch.InitializeMidi [Midi.Encode.decode bytes]

addFile :: FilePath -> Common.Common a -> Common.Common a
addFile fn = Common.tags %= ((Tag.file, txt (FilePath.takeFileName fn)) :)

-- * record

type RMap = Map Name Record
data Record
    -- | A List is represented as an RMap with numbered keys.
    = RMap RMap
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
specToRmap :: Specs -> RMap
specToRmap = foldl' add Map.empty
    where
    specToRecord = RMap . specToRmap
    add rec (name, spec) = case spec of
        Num (Range {}) -> Map.insert name (RNum 0) rec
        Num (Enum (enum:_)) -> Map.insert name (RStr enum) rec
        Num (Enum []) -> error $ name <> " had an empty Enum"

        Bits bits -> foldl' addBit rec bits
        Str _ -> Map.insert name (RStr "") rec
        SubSpec specs -> Map.insert name (specToRecord specs) rec
        List elts specs -> Map.insert name
            (RMap $ Map.fromList $
                zip (map show [0..elts-1]) (repeat (specToRecord specs)))
            rec
        Union _enumName _bytes ((_, specs) : _) ->
            Map.insert name (RUnion (specToRmap specs)) rec
        Union _enumName _bytes [] ->
            error $ name <> " had an empty Union"
        Unparsed nbytes
            | null name -> rec
            | otherwise ->
                Map.insert name (RUnparsed (B.replicate nbytes 0)) rec
        Constant {} -> rec

    addBit rec (name, (_, range)) = case range of
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
showFlat :: RMap -> [String]
showFlat = showMap []
    where
    showMap fields = concatMap (uncurry (show1 fields)) . Map.toAscList
    show1 fields field val = case val of
            RMap rmap -> showMap (field : fields) rmap
            RUnion rmap -> showMap (field : fields) rmap
            RNum n -> [path <> show n]
            RStr s -> [path <> untxt s]
            RUnparsed {} -> []
        where
        path = Lists.join "." (reverse (field : fields)) <> ": "

class RecordVal a where
    fromVal :: a -> Record
    toVal :: Record -> Maybe a

instance RecordVal Int where
    fromVal = RNum
    toVal (RNum x) = Just x
    toVal _ = Nothing

instance RecordVal Word8 where
    fromVal = RNum . fromIntegral
    toVal (RNum x) = Just (fromIntegral x)
    toVal _ = Nothing

instance RecordVal Text where
    fromVal = RStr
    toVal (RStr x) = Just x
    toVal _ = Nothing

instance RecordVal ByteString where
    fromVal = RUnparsed
    toVal (RUnparsed x) = Just x
    toVal _ = Nothing

valType :: (RecordVal a) => a -> RecordType
valType = recordType . fromVal

recordType :: Record -> RecordType
recordType r = case r of
    RMap {} -> TMap
    RUnion {} -> TUnion
    RNum {} -> TNum
    RStr {} -> TStr
    RUnparsed {} -> TUnparsed

getRmap :: forall a. (RecordVal a) => String -> RMap -> Either String a
getRmap path rmap = toValError $ lookup1 (Lists.split (=='.') path) rmap
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
    toValError (Left (fields, msg)) =
        Left $ Lists.join "." fields <> ": " <> msg
    toValError (Right v) = case toVal v of
        Nothing -> Left $ path <> ": expected a "
            <> show (valType rtype) <> " but got " <> show v
        Just val -> Right val
    rtype :: a
    rtype = error "unevaluated"

-- | Put the given val into the rmap at a certain path.  This only modifies
-- existing fields, it won't create new ones, and you can't change the type
-- of a field.
putRmap :: (Show a, RecordVal a) => String -> a -> RMap -> Either String RMap
putRmap path val rmap = formatErr $ put (Lists.split (=='.') path) val rmap
    where
    put [] _ _ = Left ([], "can't put empty field")
    put (field : fields) val rmap = case Map.lookup field rmap of
        Nothing -> Left ([field], "not found")
        Just record
            | nextField : _ <- fields -> case record of
                RMap submap -> case put fields val submap of
                    Left (children, msg) -> Left (field:children, msg)
                    Right submap -> Right $ Map.insert field (RMap submap) rmap
                RUnion submap -> put (field:fields) val submap
                _ -> Left ([field], "can't lookup field " <> show nextField
                    <> " in non-map")
            | recordType record /= recordType (fromVal val) ->
                Left ([field], "old val " <> show record
                    <> " is a different type than " <> show val)
            | otherwise -> Right $ Map.insert field (fromVal val) rmap
    formatErr (Left (fields, msg)) =
        Left $ Lists.join "." fields <> ": " <> msg
    formatErr (Right val) = Right val

-- * util

expectBytes :: ByteString -> ByteString -> Either String ByteString
expectBytes bytes prefix
    | pre == prefix = Right post
    | otherwise = Left $ "expected " <> hex prefix <> " but got " <> hex pre
    where (pre, post) = B.splitAt (B.length prefix) bytes

hex :: ByteString -> String
hex = unwords . map (\b -> Numeric.showHex b "") . B.unpack

-- | Extract substrings delimited by sox_byte and eox_byte.  Bytes not within
-- the delimeters are stripped.
extractSysex :: ByteString -> [ByteString]
extractSysex bytes
    | B.null bytes = []
    | not $ B.singleton Midi.Encode.eoxByte `B.isSuffixOf` sysex = []
    | B.null sysex = extractSysex post
    | otherwise = sysex : extractSysex post
    where
    (sysex, post) = breakAfter (==Midi.Encode.eoxByte) $
        B.dropWhile (/=Midi.Encode.soxByte) bytes

breakAfter :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakAfter f bytes = case B.findIndex f bytes of
    Nothing -> (bytes, mempty)
    Just i -> B.splitAt (i+1) bytes

-- * config

data Config = Config
    { decodeNum :: NumRange -> ByteString -> Int
    , encodeNum :: NumRange -> Int -> ByteString
    -- | The number of bytes needd to encode a number in the given range.
    -- This should agree with 'decode_num', since it will be given that many
    -- bytes to decode.
    , rangeBytes :: NumRange -> Int
    }

-- | Encode for 8bit bytes, where numbers are never more than 1 byte.
config8bit :: Config
config8bit = Config decode8bitNum encode8bitNum (const 1)
    where
    decode8bitNum :: NumRange -> ByteString -> Int
    decode8bitNum (low, _) bytes = case B.uncons bytes of
        Nothing -> 0
        Just (b, _)
            | low < 0 -> toSigned 8 b
            | otherwise -> fromIntegral b
    encode8bitNum :: NumRange -> Int -> ByteString
    encode8bitNum (low, _) num
        | low < 0 = B.singleton $ fromSigned 8 num
        | otherwise = B.singleton (fromIntegral num)

-- * encode

type EncodeM a = Except.ExceptT Error (Writer.Writer Builder.Builder) a

encode :: Config -> Specs -> RMap -> Either Error ByteString
encode config specs rmap = runEncode (mapM_ (encodeSpec config [] rmap) specs)

runEncode :: EncodeM () -> Either Error ByteString
runEncode m = case Writer.runWriter (Except.runExceptT m) of
    (Left err, _) -> Left err
    (Right (), builder) ->
        Right $ Lazy.toStrict $ Builder.toLazyByteString builder

encodeSpec :: Config -> [String] -> RMap -> (Name, Spec) -> EncodeM ()
encodeSpec config path rmap (name, spec) = case spec of
    Num range -> do
        num <- either throw pure . encodeRange range
            =<< lookupField name
        Writer.tell $ Builder.byteString $
            encodeNum config (numRange range) num
    Bits bits -> do
        b <- either (uncurry throwWith) pure $ encodeByte rmap bits
        Writer.tell (Builder.word8 b)
    Str chars -> do
        str <- lookupField name >>= \case
            RStr str -> pure str
            val -> throw $ "expected RStr, but got " <> show val
        let diff = chars - Text.length str
        padded <- if diff >= 0
            then pure $ str <> Text.replicate diff " "
            else throw $ "too many characters, expected " <> show chars
                <> ": " <> show str
        Writer.tell (Builder.string7 $ untxt padded)
    SubSpec specs -> do
        subRecord <- lookupField name >>= \case
            RMap rmap -> pure rmap
            rec -> throw $ "non-RMap child of a SubSpec: " <> prettys rec
        mapM_ (encodeSpec config (name:path) subRecord) specs
    List elts specs -> do
        records <- lookupField name >>= \case
            RMap rmap
                | Map.size rmap == elts ->
                    mapM (\k -> lookupMap (show k) rmap) [0..elts-1]
                | otherwise -> throw $ "expected RMap list of length "
                    <> show elts <> " but got length "
                    <> show (Map.size rmap)
            val -> throw $ "expected RMap list, but got " <> prettys val
        rmaps <- forM records $ \case
            RMap rmap -> pure rmap
            rec -> throw $ "non-RMap child of an RMap list: " <> prettys rec
        forM_ (zip [0..] rmaps) $ \(i, rmap) ->
            mapM_ (encodeSpec config (name : show i : path) rmap) specs
    Union enumName nbytes enumSpecs -> do
        unionRmap <- lookupField name >>= \case
            RUnion unionRmap -> pure unionRmap
            val -> throw $ "expected RUnion RMap, but got " <> prettys val
        enum <- lookupField enumName >>= \case
            RStr enum -> pure enum
            val -> throw $ "expeted RStr, but got " <> prettys val
        specs <- case lookup enum enumSpecs of
            Just specs -> pure specs
            Nothing -> throw $ "not found in union "
                <> show (map fst enumSpecs) <> ": " <> untxt enum
        bytes <- tryRight $ runEncode $
            mapM_ (encodeSpec config (name:path) unionRmap) specs
        Writer.tell $ Builder.byteString $ bytes
            <> B.replicate (nbytes - B.length bytes) 0
    Unparsed nbytes
        | null name -> Writer.tell $ Builder.byteString $ B.replicate nbytes 0
        | otherwise -> do
            bytes <- lookupField name >>= \case
                RUnparsed bytes
                    | B.length bytes /= nbytes -> throw $
                        "Unparsed expected " <> show nbytes
                        <> " bytes but got " <> show (B.length bytes)
                    | otherwise -> pure bytes
                val -> throw $ "expected RUnparsed, but got " <> prettys val
            Writer.tell (Builder.byteString bytes)
    Constant bytes -> Writer.tell (Builder.byteString bytes)
    where
    lookupMap k rmap = maybe (throw (k <> " not found")) pure $
        Map.lookup k rmap
    lookupField field = case rmapLookup rmap field of
            Left err -> Except.throwError $ prefix <> err
            Right val -> pure val
        where
        prefix
            | field == name = showPath (name : path)
            | otherwise = showPath (field : name : path)
    throw msg = Except.throwError $ showPath (name:path) <> msg
    throwWith field msg = Except.throwError $ showPath (field:path) <> msg

encodeByte :: RMap -> [(Name, BitField)] -> Either (Name, Error) Word8
encodeByte rmap bits = do
    let (names, fields) = unzip bits
    vals <- mapM (\name -> addName name $ rmapLookup rmap name) names
    bs <- zipWithM encode1 (zip names fields) vals
    pure $ encodeBits (map fst fields) bs
    where
    addName name = first ((,) name)
    encode1 (name, (width, range)) rec = addName name $ do
        num <- encodeRange range rec
        pure $ if rangeSigned range then fromSigned width num
            -- encode_range should have already checked the range of 'num'.
            else fromIntegral num

encodeRange :: Range -> Record -> Either String Int
encodeRange (Range low high) (RNum num)
    | low <= num && num <= high = Right num
    | otherwise = Left $ "num out of range " <> show (low, high) <> ": "
        <> show num
encodeRange (Enum enums) (RStr enum)
    | Just i <- List.elemIndex enum enums = Right i
    | otherwise = Left $ "unknown enum: " <> untxt enum
encodeRange _ record =
    Left $ "expected a num or str, but got " <> show record

rmapLookup :: RMap -> Name -> Either Error Record
rmapLookup rmap name = case Map.lookup name rmap of
    Nothing -> Left $ "not found in: " <> prettys (Map.keys rmap)
    Just val -> Right val

-- * decode

decode :: Config -> Specs -> ByteString -> Either Error (RMap, ByteString)
decode config = decodeFrom []
    where
    decodeFrom path specs bytes = first Strings.strip $
        Get.runGetState (rmap path [] specs) bytes 0
    rmap _ collect [] = pure (Map.fromList collect)
    rmap path collect (spec:specs) = do
        vals <- field path collect spec
        rmap path (vals ++ collect) specs

    field :: [Name] -> [(Name, Record)] -> (Name, Spec)
        -> Get.Get [(String, Record)]
    field path prevRecord (name, spec) = case spec of
        Bits bits ->
            either fail pure . decodeByte path bits =<< Get.getWord8
        Num range -> do
            bytes <- Get.getBytes $ rangeBytes config (numRange range)
            let num = decodeNum config (numRange range) bytes
            either throw (pure . (:[]) . (,) name) $ decodeRange num range
        Str chars -> do
            str <- Get.getByteString chars
            pure [(name, RStr $ Text.strip $ FFI.decodeUtf8 str)]
        SubSpec specs -> do
            subs <- rmap (name : path) [] specs
            pure [(name, RMap subs)]
        List elts specs -> do
            subs <- forM [0 .. elts-1] $ \i ->
                RMap <$> rmap (show i : name : path) [] specs
            pure [(name, RMap $ Map.fromList $
                zip (map show [0..elts-1]) subs)]
        Union enumName bytes enumSpecs -> do
            path <- pure (name : path)
            enum <- case lookup enumName prevRecord of
                Just (RStr enum) -> pure enum
                _ -> throw $ "previous enum not found: " <> enumName
            specs <- case lookup enum enumSpecs of
                Just specs -> pure specs
                Nothing -> throw $ "union doesn't contain enum: " <> untxt enum
            bytes <- Get.getByteString bytes
            record <- either fail (pure . fst) $ decodeFrom path specs bytes
            pure [(name, RUnion record)]
        Unparsed nbytes
            | null name -> Get.skip nbytes >> pure []
            | otherwise -> do
                bytes <- Get.getByteString nbytes
                when (B.length bytes < nbytes) $
                    throw $ "expected " <> show nbytes <> " bytes, but got "
                        <> show (B.length bytes)
                pure [(name, RUnparsed bytes)]
        Constant expected -> do
            bytes <- Get.getByteString (B.length expected)
            if bytes == expected then pure []
                else throw $ "expected " <> hex expected <> " but got "
                    <> hex bytes
        where
        throw = fail . (showPath (name:path) <>)

decodeRange :: Int -> Range -> Either String Record
decodeRange num (Range low high)
    | low <= num && num <= high = Right (RNum num)
    | otherwise = Left $ "out of range " <> show (low, high) <> ": "
        <> show num
decodeRange num (Enum enums)
    | Just enum <- Lists.at enums num = Right (RStr enum)
    | otherwise = Left $ "out of range for enum: " <> show num

decodeByte :: [Name] -> [(Name, BitField)] -> Word8
    -> Either String [(String, Record)]
decodeByte path bits byte =
    -- Null names are reserved bytes.
    filter (not . null . fst) <$> zipWithM extract bits signed
    where
    vals = decodeBits [width | (_, (width, _)) <- bits] byte
    signed = zipWith convertSigns (map snd bits) vals
    extract (name, (_, field)) val = case field of
        Range low high
            | low <= val && val <= high -> pure (name, RNum val)
            | otherwise -> Left $
                showPath (name:path) <> "out of range: " <> show val
        Enum enums
            | Just enum <- Lists.at enums val -> pure (name, RStr enum)
            | otherwise -> Left $
                showPath (name:path) <> "bit of byte " <> show byte
                    <> ": not a valid enum index: " <> show val
    convertSigns :: BitField -> Word8 -> Int
    convertSigns (width, range) val
        | rangeSigned range = toSigned width val
        | otherwise = fromIntegral val

showPath :: [Name] -> String
showPath = (<>": ") . Lists.join "." . reverse

-- ** bit fiddling

encodeBits :: [Int] -> [Word8] -> Word8
encodeBits widths vals = foldl' (.|.) 0 $ zipWith Bits.shiftL vals offsets
    where offsets = scanl (+) 0 widths

decodeBits :: [Int] -> Word8 -> [Word8]
decodeBits widths byte = zipWith extract bs (drop 1 bs)
    where
    bs = scanl (+) 0 widths
    extract start end = Bits.shiftR (set start end .&. byte) start
    set start end = foldl' Bits.setBit 0 [start .. end-1]

-- | Convert an n bit 2s complement word to a signed integer.
toSigned :: (Integral a, Bits.Bits a) => Int -> a -> Int
toSigned bits b
    | Bits.testBit b (bits-1) = negate $
        fromIntegral $ (Bits.complement b .&. (2^bits - 1)) + 1
    | otherwise = fromIntegral b

-- | Convert a signed integer to an n bit 2s complement word.
fromSigned :: Int -> Int -> Word8
fromSigned bits num
    | clamped < 0 = fromIntegral (2^bits + clamped)
    | otherwise = fromIntegral clamped
    where clamped = Num.clamp (-2^(bits-1)) (2^(bits-1) - 1) num


-- * spec

type Specs = [(Name, Spec)]
data Spec
    -- | A set of bitfields encoded in a single byte.  The BitField widths must
    -- add up to 8.  They start at the least significant end of the byte, so
    -- given @[(\"a\", (1, Range 0 1)), (\"b\", (7, Range 0 1))]@, @a@ is from
    -- the least significant bit.
    = Bits [(Name, BitField)]
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

numRange :: Range -> NumRange
numRange (Range low high) = (low, high)
numRange (Enum enums) = (0, length enums - 1)

rangeSigned :: Range -> Bool
rangeSigned (Range low _) = low < 0
rangeSigned _ = False

type BitField = (Bits, Range)
type Name = String
type Bits = Int
type Bytes = Int

specBytes :: Config -> Specs -> Int
specBytes config = Num.sum . map (bytesOf . snd)
    where
    bytesOf (Bits {}) = 1
    bytesOf (Num range) = rangeBytes config (numRange range)
    bytesOf (Str n) = n
    bytesOf (SubSpec specs) = specBytes config specs
    bytesOf (List n specs) = specBytes config specs * n
    bytesOf (Union _ n _) = n
    bytesOf (Unparsed n) = n
    bytesOf (Constant bytes) = B.length bytes

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
    check (name, Union enumName _bytes fields) = case lookupSpec enumName of
        Just (Left (_, Enum enums))
            | List.sort enums /= List.sort (map fst fields) -> Just $
                name <> ": enums not equal: " <> show (enums, map fst fields)
            -- TODO fields sum up to < bytes
            -- check recursively on specs
        _ -> Just $ name <> ": enum not found"
    check _ = Nothing
    lookupSpec wanted = msum (map find specs)
        where
        find (name, spec)
            | Bits bits <- spec = msum (map findBit bits)
            | name == wanted = Just $ Right spec
            | otherwise = Nothing
        findBit (name, field)
            | name == wanted = Just $ Left field
            | otherwise = Nothing

-- | Hokey runtime check to make sure the Specs is valid and has the expected
-- size.
assertValid :: Config -> String -> Int -> Specs -> Specs
assertValid config name size specs
    | Just err <- validate specs = crash err
    | actualSize /= size = crash $ "expected " <> show size
        <> " bytes, but was " <> show actualSize
    | otherwise = specs
    where
    actualSize = specBytes config specs
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

rangedBits :: Int -> (Int, Int) -> BitField
rangedBits n (low, high) = (n, Range low high)

enumBits :: Int -> [EnumName] -> BitField
enumBits n vals = (n, Enum vals)

boolBit :: BitField
boolBit = (1, Enum ["off", "on"])

unparsedBits :: Int -> (String, BitField)
unparsedBits n = ("", bits n)

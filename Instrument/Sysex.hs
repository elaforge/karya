{- | Support for generating and parsing sysex files from a "spec" file.

    TODO I need to support disjoint subsections, e.g. the different effects
    blocks depending on the value of an enum.
-}
module Instrument.Sysex where
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Writer.Strict as Writer
import qualified Data.Bits as Bits
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Builder as Builder
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Serialize.Get as Get
import Data.Word (Word8)

import Util.Control
import qualified Util.Num as Num
import qualified Util.Seq as Seq


data Record =
    RMap (Map.Map String Record)
    | RList [Record]
    | RNum Int | RStr String | REnum String
    deriving (Eq, Show)
type Error = String

-- | Create a Record from a Spec, defaulting everything to 0, "", or the first
-- enum val.
spec_to_record :: [Spec] -> Record
spec_to_record = RMap . Map.delete "" . List.foldl' add Map.empty
    where
    add rec (Bits bits) = List.foldl' add_bit rec bits
    add rec (Str name _) = Map.insert name (RStr "") rec
    add rec (SubSpec name specs) = Map.insert name (spec_to_record specs) rec
    add rec (List name elts specs) =
        Map.insert name (RList (replicate elts (spec_to_record specs))) rec
    add_bit rec (name, (_, Range {})) = Map.insert name (RNum 0) rec
    add_bit rec (name, (_, Enum (enum : _))) = Map.insert name (REnum enum) rec
    add_bit _ (name, (_, Enum [])) = error $ name ++ " had an empty Enum"

-- * encode

type Put a = Error.ErrorT Error (Writer.Writer Builder.Builder) a

encode :: [Spec] -> Record -> Either Error ByteString
encode specs record = run_encode (mapM_ (encode_spec [] record) specs)

run_encode :: Put () -> Either Error ByteString
run_encode m = case Writer.runWriter (Error.runErrorT m) of
    (Left err, _) -> Left err
    (Right (), builder) ->
        Right $ Lazy.toStrict $ Builder.toLazyByteString builder

encode_spec :: [String] -> Record -> Spec -> Put ()
encode_spec path record spec = case spec of
    Bits bits -> do
        b <- either (uncurry throw) return $ encode_byte record bits
        Writer.tell (Builder.word8 b)
    Str name chars -> do
        str <- lookup name >>= \x -> case x of
            RStr str -> return str
            val -> throw name $ "expected a string, but got " ++ show val
        let diff = chars - length str
        padded <- if diff >= 0
            then return $ str ++ replicate diff ' '
            else throw name $
                "too many characters, expected " ++ show chars
                ++ ": " ++ show str
        Writer.tell (Builder.string7 padded)
    SubSpec name specs -> do
        sub_record <- lookup name
        mapM_ (encode_spec (name:path) sub_record) specs
    List name elts specs -> do
        records <- lookup name >>= \x -> case x of
            RList records
                | length records == elts -> return records
                | otherwise -> throw name $ "expected list of length "
                    ++ show elts ++ " but got length " ++ show (length records)
            val -> throw name $ "expected a list, but got " ++ show val
        forM_ (zip [0..] records) $ \(i, rec) ->
            mapM_ (encode_spec ((name ++ show i) : path) rec) specs
    where
    lookup = either (uncurry throw) return . rmap_lookup record
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
            Nothing -> Left (name, "unknown enum: " ++ show enum)
            Just i -> return $ fromIntegral i
    encode1 (name, field) val = Left
        (name, "field " ++ show field ++ " not appropriate for " ++ show val)

rmap_lookup :: Record -> Name -> Either (Name, Error) Record
rmap_lookup _ "" = Right $ RNum 0 -- null name means it's a reserved section
rmap_lookup record name = case record of
    RMap rmap -> case Map.lookup name rmap of
        Nothing -> Left (name, "name not found")
        Just val -> Right val
    _ -> Left (name, "can't lookup name in non-map " ++ show record)

-- * decode

decode :: [Spec] -> ByteString -> Either Error (Record, ByteString)
decode specs bytes = Get.runGetState (rmap [] specs) bytes 0
    where
    rmap path specs = RMap . Map.fromList <$> concatMapM (field path) specs
    field :: [Name] -> Spec -> Get.Get [(String, Record)]
    field path (Bits bits) =
        either (fail . (show_path path ++)) return
            . decode_byte path bits =<< Get.getWord8
    field _ (Str name chars) = do
        str <- Get.getByteString chars
        return [(name, RStr (Seq.strip (Char8.unpack str)))]
    field path (SubSpec name specs) = do
        subs <- rmap (name : path) specs
        return [(name, subs)]
    field path (List name elts specs) = do
        subs <- forM [0 .. elts-1] $ \i ->
            rmap ((name ++ show i) : path) specs
        return [(name, RList subs)]

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
                show_path (name:path) ++ "not a valid enum index: " ++ show val
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
    List.foldl' (.|.) 0 $ zipWith Bits.shiftL (reverse vals) offsets
    where offsets = scanl (+) 0 (reverse widths)

decode_bits :: [Int] -> Word8 -> [Word8]
decode_bits widths byte = reverse $ zipWith extract bs (drop 1 bs)
    where
    bs = scanl (+) 0 (reverse widths)
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
    Bits [(Name, BitField)] | Str Name Int | SubSpec Name [Spec]
    | List Name Int [Spec]
    deriving (Show)

data Range = Range Int Int | Enum [String]
    deriving (Show)

type BitField = (Bits, Range)
type Name = String
type Bits = Int

validate :: [Spec] -> Maybe String
validate = msum . map check
    where
    check (Bits bits)
        | total /= 8 = Just $
            show (map fst bits) ++ " - bits should sum to 8: " ++ show total
        | otherwise = Nothing
        where total = sum [n | (_, (n, _)) <- bits]
    check _ = Nothing

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

{- | Functions to help parse MIDI patch files.
-}
module Instrument.Parse where
import Control.Monad
import Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Numeric
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Pos as Parsec.Pos
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import qualified Util.File as File
import qualified Midi.Midi as Midi
import qualified Derive.Parse as Parse
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument


-- * patch file

patch_file :: FilePath -> IO [Instrument.Patch]
patch_file fn = do
    parsed <- parse_patch_file fn
    case parsed of
        Left err -> error $ "parse patches: " ++ show err
        Right patches -> return patches

parse_patch_file fn = do
    contents <- readFile fn
    return $ Parsec.runParser p_patch_file initial_state fn contents

p_patch_file = do
    plines <- p_patch_lines
    return $ map (make_patch (-2, 2)) plines

make_patch pb_range (PatchLine name cat bank patch_num) =
    Instrument.patch inst
        (Instrument.InitializeMidi (Midi.program_change bank patch_num))
        tags
        ""
    where
    inst = Instrument.instrument name Controller.empty_map pb_range Nothing
    tags = [Instrument.tag "category" cat]

p_patch_lines = fmap Maybe.catMaybes $ Parsec.many p_line
p_line = Parsec.try p_bank_decl <|> p_rest_of_line <|> fmap Just p_patch_line

p_patch_line = do
    st <- Parsec.getState
    name <- word <?> "name"
    cat <- opt (state_prev st) word
    Parsec.setState $
        st { state_prev = cat, state_patch_num = state_patch_num st + 1 }
    p_rest_of_line
    return $ PatchLine name cat (state_bank st) (state_patch_num st)
    where
    opt def p = Parsec.option def (comma >> p)
    word = Parsec.many1 (Parsec.noneOf "\n,")
    comma = Parsec.string ", "

p_bank_decl = do
    Parsec.string "*bank"
    Parsec.skipMany1 Parsec.space
    n <- Parse.p_nat
    st <- Parsec.getState
    Parsec.setState (st { state_bank = n, state_patch_num = 0 })
    return Nothing

p_rest_of_line = do
    spaces
    Parsec.optional (Parsec.char '#' >> Parsec.skipMany (Parsec.noneOf "\n"))
    Parsec.newline
    return Nothing
    where spaces = Parsec.skipMany (Parsec.oneOf " \t")

data State = State {
    state_prev :: String
    , state_bank :: Integer
    , state_patch_num :: Integer
    }
initial_state = State "" 0 0

data PatchLine = PatchLine String String Integer Integer deriving (Show)


-- * sysex

type ByteParser = Parsec.GenParser (Parsec.Pos.SourcePos, Word.Word8)

parse_sysex_dir :: ByteParser () Instrument.Patch -> FilePath
    -> IO [Instrument.Patch]
parse_sysex_dir parser dir = do
    fns <- File.list_dir dir
    warn_parses =<< mapM (parse_sysex_file parser) fns

warn_parses :: [Either Parsec.ParseError a] -> IO [a]
warn_parses parses = fmap Maybe.catMaybes $ forM parses $ \p -> case p of
    Left err -> do
        -- This is only run by make_db, so I guess prints are ok.
        putStrLn $ "error parsing sysex: " ++ show err
        return Nothing
    Right parse -> return (Just parse)

parse_sysex_file :: ByteParser () Instrument.Patch -> FilePath
    -> IO (Either Parsec.ParseError Instrument.Patch)
parse_sysex_file parser fn = do
    bytes <- File.read_binary fn
    return $ fmap (add_sysex bytes . add_file_text fn)
        (parse_sysex parser fn bytes)

parse_sysex :: ByteParser () a -> FilePath -> [Word.Word8]
    -> Either Parsec.ParseError a
parse_sysex parser fn bytes = Parsec.parse parser fn (annotate bytes)
    where
    annotate bytes =
        [(Parsec.Pos.newPos fn 1 n, byte) | (n, byte) <- zip [1..] bytes]

add_file_text :: FilePath -> Instrument.Patch -> Instrument.Patch
add_file_text fn patch = patch { Instrument.patch_text =
    Instrument.patch_text patch ++ "\n\nFile: " ++ fn }

-- | Tack the sysex on to the patch's initialize field.
add_sysex :: [Word.Word8] -> Instrument.Patch -> Instrument.Patch
add_sysex bytes patch =
    patch { Instrument.patch_initialize = make_sysex_init bytes }
make_sysex_init bytes = Instrument.InitializeSysex (ByteString.pack bytes)
    -- Instrument.InitializeMidi
    --     [Midi.CommonMessage (Midi.SystemExclusive manuf rest)]
    -- where
    -- If the msg is broken there's not much I can do here.
    -- manuf = bytes !! 1
    -- rest = drop 2 bytes

byte_tok :: (Word.Word8 -> Maybe a) -> ByteParser st a
byte_tok f = Parsec.token show_tok tok_pos test_tok
    where
    show_tok (_, n) = hex n
    tok_pos (pos, _) = pos
    test_tok (_, n) = f n

hex :: (Integral a) => a -> String
hex n = Numeric.showHex n ""

byte_sat f = byte_tok $ \b -> if f b then Just b else Nothing

byte b = byte_sat (==b) <?> ("byte " ++ hex b)
match_bytes [] = return []
match_bytes (b:bs) = byte b >> match_bytes bs

n_bytes n = Parsec.count n any_byte
one_byte = fmap head (n_bytes 1)
any_byte = byte_sat (const True)

start_sysex manuf = byte Midi.sox_byte >> byte manuf
end_sysex = byte Midi.eox_byte >> Parsec.eof
to_eox :: ByteParser st [Word.Word8]
to_eox = Parsec.many (byte_sat (/=Midi.eox_byte))

-- * decode sysex bytes

to_string :: [Word.Word8] -> String
to_string = map (toEnum . fromIntegral)

from_signed_7bit :: Word.Word8 -> Integer
from_signed_7bit b = fromIntegral b .&. 0x3f - fromIntegral b .&. 0x40
from_signed_8bit :: (Integral a) => a -> Integer
from_signed_8bit b = fromIntegral b .&. 0x7f - fromIntegral b .&. 0x80
-- 76543210
-- 10000000 0x80
-- x1000000 0x40
-- x0111111 0x3f
-- x0100000 0x20

korg_code, yamaha_code :: Word.Word8
korg_code = 0x42
yamaha_code = 0x43

-- | TODO get a more complete list
manufacturer_codes :: [(Word.Word8, String)]
manufacturer_codes =
    [(korg_code, "korg"), (yamaha_code, "yamaha")]

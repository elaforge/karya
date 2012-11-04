{- | Functions to help parse MIDI patch files.
-}
module Instrument.Parse where
import Data.Bits as Bits
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Word (Word8)

import qualified Numeric
import qualified System.FilePath as FilePath
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import qualified Text.ParserCombinators.Parsec.Pos as Parsec.Pos

import Util.Control hiding ((<|>))
import qualified Util.File as File
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Tag as Tag


-- * patch file

-- | This module uses parsec.  The rest of the modules use attoparsec so it
-- would be nice to entirely get rid of parsec dependency by using it here too,
-- but I use state to have nicer error msgs and attoparsec doesn't support
-- user state.
type Parser a = Parsec.GenParser Char State a

data State = State {
    state_bank :: Int
    , state_patch_num :: Midi.Program
    }

empty_state :: State
empty_state = State 0 0

-- | name, category, bank, patch_num
data PatchLine = PatchLine {
    patch_name :: String
    , patch_bank :: Int
    , patch_program :: Midi.Program
    , patch_tags :: Map.Map Instrument.TagKey Instrument.TagVal
    } deriving (Show)

patch_file :: FilePath -> IO [Instrument.Patch]
patch_file fn =
    either (errorIO . ("parse patches: " ++) . show) return
        =<< parse_patch_file fn

-- | Parse a simple ad-hoc text file format to describe a synth's built-in
-- patches.
--
-- If a line looks like @<word>, <word>@, the first word will be the patch
-- name and the second will be be the @category@ tag.  If there is only one
-- word, the patch inherits the previous line's category.
--
-- The patch's program change is incremented for each patch.  A line like
-- @*bank <num>@ sets the bank number and resets the program change to 0.
--
-- Comments start with @#@, and blank lines are ignored.
--
-- There is no support currently for other attributes, but they could be added
-- later if needed.
parse_patch_file :: String -> IO (Either Parsec.ParseError [Instrument.Patch])
parse_patch_file fn = do
    contents <- readFile fn
    let result = Parsec.runParser p_patch_file empty_state fn contents
    return $ map (add_file fn) <$> result

p_patch_file :: Parser [Instrument.Patch]
p_patch_file = do
    patches <- p_patch_lines
    return $ map (make_patch (-2, 2)) (inherit_prev_category patches)
    where
    inherit_prev_category = snd . List.mapAccumL inherit Nothing
    inherit maybe_prev patch = case Map.lookup Tag.category tags of
            Nothing -> case maybe_prev of
                Nothing -> (Nothing, patch)
                Just prev -> (Just prev,
                    patch { patch_tags = Map.insert Tag.category prev tags })
            Just cat -> (Just cat, patch)
        where tags = patch_tags patch

make_patch :: Control.PbRange -> PatchLine -> Instrument.Patch
make_patch pb_range (PatchLine name bank patch_num tags) =
    (Instrument.patch inst)
        { Instrument.patch_initialize = Instrument.InitializeMidi $
            map (Midi.ChannelMessage 0) (Midi.program_change bank patch_num)
        , Instrument.patch_tags = Map.toList tags
        }
    where inst = Instrument.instrument name [] pb_range

p_patch_lines :: Parser [PatchLine]
p_patch_lines = fmap Maybe.catMaybes $ Parsec.many p_line

p_line :: Parser (Maybe PatchLine)
p_line = Parsec.try p_bank_decl <|> p_rest_of_line <|> fmap Just p_patch_line

-- | name, category, tag=val
p_patch_line :: Parser PatchLine
p_patch_line = do
    st <- Parsec.getState
    name <- Parsec.many1 (Parsec.noneOf "\n,")
    tags <- Map.fromList <$>
        Parsec.option [] (comma >> Parsec.sepBy1 p_tag comma)
    Parsec.setState $ st { state_patch_num = state_patch_num st + 1 }
    p_rest_of_line
    return $ PatchLine name (state_bank st) (state_patch_num st) tags
    where
    comma = Parsec.char ',' >> spaces
    spaces = Parsec.skipMany (Parsec.char ' ')

p_tag :: Parser (Instrument.TagKey, Instrument.TagVal)
p_tag = do
    key <- Parsec.many1 (Parsec.alphaNum <|> Parsec.char '-')
    val <- Parsec.option Nothing $ do
        Parsec.char '='
        Just <$> Parsec.many (Parsec.noneOf " \n")
    return $ case val of
        Nothing -> (Tag.category, key)
        Just val -> (key, val)

p_bank_decl :: Parser (Maybe PatchLine)
p_bank_decl = do
    Parsec.string "*bank"
    Parsec.skipMany1 Parsec.space
    n <- p_nat
    st <- Parsec.getState
    Parsec.setState (st { state_bank = fromIntegral n, state_patch_num = 0 })
    return Nothing

p_rest_of_line :: Parser (Maybe PatchLine)
p_rest_of_line = do
    spaces
    Parsec.optional (Parsec.char '#' >> Parsec.skipMany (Parsec.noneOf "\n"))
    Parsec.newline
    return Nothing
    where spaces = Parsec.skipMany (Parsec.oneOf " \t")

p_nat :: Parsec.CharParser st Integer
p_nat = do
    i <- Parsec.many Parsec.digit
    case Numeric.readDec i of
        (n, _) : _ -> return n
        _ -> Parsec.pzero -- this should never happen
    <?> "natural int"

-- * sysex

-- | Parse a sysex file as a stream of Word8s.
-- TODO this should be ByteString, but it's surprisingly not easy to do this.
-- Attoparsec doesn't keep track of the byte offset at all so the errors
-- are unhelpful.  Parsec can now read ByteStrings, but it's still Char
-- oriented so I still have to write combinators for bytes, and Parsec.token
-- still wants a 'tok_pos' function, which implies the byte number still has
-- to be in the token stream.
type ByteParser = Parsec.GenParser (Parsec.Pos.SourcePos, Word8) ()

parse_sysex_dir :: ByteParser Instrument.Patch -> FilePath
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

parse_sysex_file :: ByteParser Instrument.Patch -> FilePath
    -> IO (Either Parsec.ParseError Instrument.Patch)
parse_sysex_file parser fn = do
    bytes <- File.read_binary fn
    return $ add_sysex bytes . add_file fn <$> parse_sysex parser fn bytes

parse_sysex :: ByteParser a -> FilePath -> [Word8]
    -> Either Parsec.ParseError a
parse_sysex parser fn bytes = Parsec.parse parser fn (annotate bytes)
    where
    annotate bytes =
        [(Parsec.Pos.newPos fn 1 n, byte) | (n, byte) <- zip [1..] bytes]

add_file :: FilePath -> Instrument.Patch -> Instrument.Patch
add_file fn patch = patch
    { Instrument.patch_file = fn
    , Instrument.patch_tags =
        (Tag.file, FilePath.takeFileName fn) : Instrument.patch_tags patch
    }

-- | Tack the sysex on to the patch's initialize field.
add_sysex :: [Word8] -> Instrument.Patch -> Instrument.Patch
add_sysex bytes patch =
    patch { Instrument.patch_initialize = make_sysex_init bytes }

make_sysex_init :: [Word8] -> Instrument.InitializePatch
make_sysex_init bytes = Instrument.InitializeMidi
        [Midi.CommonMessage (Midi.SystemExclusive manuf rest)]
    where
    -- If the msg is broken there's not much I can do here.
    manuf = bytes !! 1
    rest = ByteString.pack (drop 2 bytes)

byte_tok :: (Word8 -> Maybe a) -> ByteParser a
byte_tok f = Parsec.token show_tok tok_pos test_tok
    where
    show_tok (_, n) = hex n
    tok_pos (pos, _) = pos
    test_tok (_, n) = f n

hex :: (Show a, Integral a) => a -> String
hex n = Numeric.showHex n ""

byte_sat :: (Word8 -> Bool) -> ByteParser Word8
byte_sat f = byte_tok $ \b -> if f b then Just b else Nothing

byte :: Word8 -> ByteParser Word8
byte b = byte_sat (==b) <?> ("byte " ++ hex b)

match_bytes :: [Word8] -> ByteParser ()
match_bytes = mapM_ byte

n_bytes :: Int -> ByteParser [Word8]
n_bytes n = Parsec.count n any_byte

one_byte :: ByteParser Word8
one_byte = fmap head (n_bytes 1)

any_byte :: ByteParser Word8
any_byte = byte_sat (const True)

start_sysex :: Word8 -> ByteParser Word8
start_sysex manuf = byte Midi.sox_byte >> byte manuf

end_sysex :: ByteParser ()
end_sysex = byte Midi.eox_byte >> Parsec.eof

to_eox :: ByteParser [Word8]
to_eox = Parsec.many (byte_sat (/=Midi.eox_byte))

-- * decode sysex bytes

to_string :: [Word8] -> String
to_string = map (toEnum . fromIntegral)

from_signed_7bit :: Word8 -> Int
from_signed_7bit b = fromIntegral b .&. 0x3f - fromIntegral b .&. 0x40

from_signed_8bit :: (Integral a) => a -> Int
from_signed_8bit b = fromIntegral b .&. 0x7f - fromIntegral b .&. 0x80
-- 76543210
-- 10000000 0x80
-- x1000000 0x40
-- x0111111 0x3f
-- x0100000 0x20

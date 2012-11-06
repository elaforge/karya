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
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>), (<?>))
import qualified Text.ParserCombinators.Parsec.Pos as Parsec.Pos

import Util.Control hiding ((<|>))
import qualified Util.File as File
import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Sysex as Sysex
import qualified Instrument.Tag as Tag


type Parser st a = Parsec.GenParser Char st a

-- * annotation file

type Annotation = Instrument.Tag

-- | Format is @synth/inst-name tag1=x tag2=y@.
--
-- TODO other attributes are not supported, but if there were, they could look
-- like @*pb-range=12 *flag=pressure@@
parse_annotations :: FilePath
    -> IO (Either String (Map.Map Score.Instrument [Annotation]))
parse_annotations fn = do
    result <- Parsec.parseFromFile p_annotation_file fn
    return $ either (Left . show) (Right . Map.fromListWith (++)) result

p_annotation_file :: Parser st [(Score.Instrument, [Annotation])]
p_annotation_file = concat <$> Parsec.many line <* Parsec.eof
    where
    line = ((:[]) <$> Parsec.try p_annotation_line) <|> (p_eol >> return [])

p_annotation_line :: Parser st (Score.Instrument, [Annotation])
p_annotation_line =
    ((,) <$> lexeme p_instrument <*> Parsec.many (lexeme p_tag)) <* p_eol

p_instrument :: Parser st Score.Instrument
p_instrument = do
    synth <- Parsec.many1 $ Parsec.oneOf Score.inst_valid_chars
    Parsec.char '/'
    name <- Parsec.many1 $ Parsec.oneOf Score.inst_valid_chars
    return $ Score.Instrument $ synth ++ '/' : name
    <?> "instrument"

p_tag :: Parser st Instrument.Tag
p_tag = (,) <$> Parsec.many1 tag_char
        <*> Parsec.option "" (Parsec.char '=' *> Parsec.many1 tag_char)
    where tag_char = Parsec.alphaNum <|> Parsec.char '-'

lexeme :: Parser st a -> Parser st a
lexeme p = p <* p_whitespace

p_eol :: Parser st ()
p_eol = p_whitespace <* Parsec.char '\n'

p_whitespace :: Parser st ()
p_whitespace = Parsec.skipMany (Parsec.oneOf " \t") >> Parsec.optional comment
    where comment = Parsec.string "#" >> Parsec.skipMany (Parsec.noneOf "\n")


-- * patch file

-- | Parse a simple ad-hoc text file format to describe a synth's built-in
-- patches.
--
-- Each line should look like @inst-name, tag=val, tag=val, ...@.
-- The instrument name can contain any character except a comma, but the
-- tags are restricted to [a-z0-9-].  The @category@ tag is treated specially:
-- if not set it will be inherited from the previous category.
--
-- The patch's program change is incremented for each patch.  A line like
-- @*bank <num>@ sets the bank number and resets the program change to 0.
--
-- Comments start with @#@, and blank lines are ignored.
patch_file :: FilePath -> IO [Instrument.Patch]
patch_file fn = either (errorIO . ("parse patches: " ++) . show) return
    =<< parse_patch_file fn

parse_patch_file :: String -> IO (Either Parsec.ParseError [Instrument.Patch])
parse_patch_file fn = do
    contents <- readFile fn
    let result = Parsec.runParser p_patch_file empty_state fn contents
    return $ map (Sysex.add_file fn) <$> result

data State = State {
    state_bank :: Int
    , state_patch_num :: Midi.Program
    } deriving (Show)

empty_state :: State
empty_state = State 0 0

data PatchLine = PatchLine {
    patch_name :: String
    , patch_bank :: Int
    , patch_program :: Midi.Program
    , patch_tags :: [Instrument.Tag]
    } deriving (Show)

p_patch_file :: Parser State [Instrument.Patch]
p_patch_file = do
    patches <- Maybe.catMaybes <$> Parsec.many p_line
    return $ map (make_patch (-2, 2)) (inherit_prev_category patches)
    where
    inherit_prev_category = snd . List.mapAccumL inherit Nothing
    inherit maybe_prev patch = case lookup Tag.category tags of
            Nothing -> case maybe_prev of
                Nothing -> (Nothing, patch)
                Just prev -> (Just prev,
                    patch { patch_tags = (Tag.category, prev) : tags })
            Just cat -> (Just cat, patch)
        where tags = patch_tags patch

make_patch :: Control.PbRange -> PatchLine -> Instrument.Patch
make_patch pb_range (PatchLine name bank patch_num tags) =
    (Instrument.patch inst)
        { Instrument.patch_initialize = Instrument.InitializeMidi $
            map (Midi.ChannelMessage 0) (Midi.program_change bank patch_num)
        , Instrument.patch_tags = tags
        }
    where inst = Instrument.instrument name [] pb_range

p_line :: Parser State (Maybe PatchLine)
p_line = Parsec.try (p_bank_decl >> return Nothing)
    <|> (p_eol >> return Nothing) <|> fmap Just p_patch_line

p_patch_line :: Parser State PatchLine
p_patch_line = do
    name <- Parsec.many1 (Parsec.noneOf "\n,")
    tags <- Parsec.option [] $ comma >> Parsec.sepBy1 p_tag comma
    p_eol
    st <- Parsec.getState
    Parsec.setState $ st { state_patch_num = state_patch_num st + 1 }
    return $ PatchLine name (state_bank st) (state_patch_num st) tags
    where
    comma = lexeme (Parsec.char ',')

p_bank_decl :: Parser State ()
p_bank_decl = do
    Parsec.string "*bank"
    Parsec.skipMany1 Parsec.space
    n <- p_nat
    st <- Parsec.getState
    Parsec.setState (st { state_bank = fromIntegral n, state_patch_num = 0 })

p_nat :: Parser st Integer
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
    return $ add_sysex bytes . Sysex.add_file fn <$> parse_sysex parser fn bytes

parse_sysex :: ByteParser a -> FilePath -> [Word8]
    -> Either Parsec.ParseError a
parse_sysex parser fn bytes = Parsec.parse parser fn (annotate bytes)
    where
    annotate bytes =
        [(Parsec.Pos.newPos fn 1 n, byte) | (n, byte) <- zip [1..] bytes]

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

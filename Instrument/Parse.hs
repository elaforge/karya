{- | Functions to help parse MIDI patch files.

TODO the program change stuff will have to get more complicated to account for
idiosyncratic handling of program change msb, lsb.
-}
module Instrument.Parse where
import Control.Monad
import qualified Data.List as List
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import qualified Midi.Midi as Midi
import qualified Derive.Parse as Parse
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument


-- | Read the given file and return errors and parsed Patches.
patches :: FilePath -> IO (Either Parsec.ParseError [Instrument.Patch])
patches fn = Parsec.parseFromFile p_patch_file fn


-- * parsers

p_patch_file :: Parsec.CharParser st [Instrument.Patch]
p_patch_file = do
    patch_lines <- Parsec.many (Parsec.skipMany p_rest_of_line >> p_patch_line)
    return $ map make_patch $ set_defaults ("", (-2, 2), Nothing) patch_lines

make_patch :: PatchSpec -> Instrument.Patch
make_patch (PatchSpec name tags patch_num controllers pb_range decay) =
    Instrument.Patch inst (map (uncurry Instrument.tag) tags)
        (Instrument.InitializeMsg [program_change patch_num])
    where
    inst = Instrument.instrument name (Controller.controller_map controllers)
        pb_range decay

program_change n = Midi.ChannelMessage 0 (Midi.ProgramChange (fromIntegral n))

-- name tags patch_num controllers pb_range decay
data PatchSpec = PatchSpec String [(String, String)] Integer
        [(Integer, String)] Controller.PbRange (Maybe Double)
    deriving (Eq, Show)

-- Nothing values carry over defaults from the previous line.
set_defaults defs patch_lines =
    snd $ List.mapAccumL go defs (zip [0..] patch_lines)
    where
    go (d_cat, d_pb_range, d_decay) (i, (name, m_cat, m_pb_range, m_decay)) =
        let (cat, pb_range, decay) =
                (d_cat >+ m_cat, d_pb_range >+ m_pb_range, d_decay >+ m_decay)
            cat_assoc = if null cat then [] else [("category", cat)]
        in ((cat, pb_range, decay),
            PatchSpec name cat_assoc i [] pb_range decay)
    _ >+ (Just y) = y
    x >+ _ = x

type PatchLine = (String, Maybe String, Maybe Controller.PbRange,
    Maybe (Maybe Double))
p_patch_line :: Parsec.CharParser st PatchLine
p_patch_line = do
    name <- word <?> "name"
    cat <- opt word
    pb_range <- opt p_pb_range
    decay <- opt ((Parsec.char '_' >> return Nothing)
        <|> fmap Just Parse.p_float)
    p_rest_of_line
    return (name, cat, pb_range, decay)
    where
    word = Parsec.many1 (Parsec.noneOf "\n,")
    opt p = fmap join $ Parsec.optionMaybe (comma >> Parsec.optionMaybe p)
    comma = Parsec.string ", "

p_rest_of_line = do
    spaces
    Parsec.optional (Parsec.char '#' >> Parsec.skipMany (Parsec.noneOf "\n"))
    Parsec.newline
    where spaces = Parsec.skipMany (Parsec.oneOf " \t")

p_pb_range :: Parsec.CharParser st Controller.PbRange
p_pb_range = do
    low <- Parse.p_int
    Parsec.char ','
    high <- Parse.p_int
    return (low, high)

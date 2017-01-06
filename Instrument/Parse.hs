-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to parse MIDI patch files.
module Instrument.Parse where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))

import qualified Util.Parse as Parse
import qualified Midi.Midi as Midi
import qualified Ui.Id as Id
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.InstTypes as InstTypes
import qualified Instrument.Sysex as Sysex
import qualified Instrument.Tag as Tag

import Global


type Parser st a = Parsec.Parsec Text st a

-- * annotation file

type Annotation = Tag.Tag

-- | Format is @synth\/inst-name tag1=x tag2=y@.
--
-- TODO other attributes are not supported, but if there were, they could look
-- like @*pb-range=12 *flag=pressure@
parse_annotations :: FilePath
    -> IO (Either String (Map InstTypes.Qualified [Annotation]))
parse_annotations fn = do
    result <- Parse.file mempty p_annotation_file () fn
    return $ (show *** Map.fromListWith (++)) result

p_annotation_file :: Parser st [(InstTypes.Qualified, [Annotation])]
p_annotation_file = concat <$> Parsec.many line <* Parsec.eof
    where
    line = ((:[]) <$> Parsec.try p_annotation_line) <|> (p_eol >> return [])

p_annotation_line :: Parser st (InstTypes.Qualified, [Annotation])
p_annotation_line =
    ((,) <$> lexeme p_qualified <*> Parsec.many (lexeme p_tag)) <* p_eol

p_qualified :: Parser st InstTypes.Qualified
p_qualified =
    InstTypes.Qualified <$> chars <*> (Parsec.char '/' *> chars) <?> "qualified"
    where chars = txt <$> Parsec.many1 (Parsec.satisfy Id.is_id_char)

p_tag :: Parser st Tag.Tag
p_tag = (,) <$> (txt <$> Parsec.many1 tag_char)
        <*> (txt <$> Parsec.option "" (Parsec.char '=' *> Parsec.many1 tag_char))
    where tag_char = Parsec.alphaNum <|> Parsec.char '-'

lexeme :: Parser st a -> Parser st a
lexeme p = p <* p_whitespace

p_eol :: Parser st ()
p_eol = p_whitespace <* Parsec.char '\n'

p_whitespace :: Parser st ()
p_whitespace = Parsec.skipMany (Parsec.oneOf " \t") >> Parsec.optional comment
    where comment = Parsec.string "#" >> Parsec.skipMany (Parsec.noneOf "\n")


-- * patch file

{- | Parse a simple ad-hoc text file format to describe a synth's built-in
    patches.

    Each line should look like @inst-name, tag=val, tag=val, ...@.
    The instrument name can contain any character except a comma, but the
    tags are restricted to [a-z0-9-].  The @category@ tag is treated specially:
    if not set it will be inherited from the previous category.

    The patch's program change is incremented for each patch.  A line like
    @*bank <num>@ sets the bank number and resets the program change to 0.

    Comments start with @#@, and blank lines are ignored.
-}
patch_file :: FilePath -> IO [Sysex.Patch]
patch_file fn = either (errorIO . ("parse patches: " <>) . showt) return
    =<< parse_patch_file fn

parse_patch_file :: String -> IO (Either Parsec.ParseError [Sysex.Patch])
parse_patch_file fn =
    fmap (map (second (Sysex.add_file fn))) <$>
        Parse.file mempty p_patch_file empty_state fn

data State = State {
    state_bank :: Int
    , state_patch_num :: Midi.Program
    } deriving (Show)

empty_state :: State
empty_state = State 0 0

data PatchLine = PatchLine {
    patch_name :: Text
    , patch_bank :: Int
    , patch_program :: Midi.Program
    , patch_tags :: [Tag.Tag]
    } deriving (Show)

p_patch_file :: Parser State [Sysex.Patch]
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

make_patch :: Control.PbRange -> PatchLine -> Sysex.Patch
make_patch pb_range (PatchLine name bank patch_num tags) = (patch, common)
    where
    patch = (Patch.patch pb_range name)
        { Patch.patch_initialize = Patch.InitializeMidi $
            map (Midi.ChannelMessage 0) (Midi.program_change bank patch_num)
        }
    common = (Common.common ()) { Common.common_tags = tags }

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
    return $ PatchLine (txt name) (state_bank st) (state_patch_num st) tags
    where
    comma = lexeme (Parsec.char ',')

p_bank_decl :: Parser State ()
p_bank_decl = do
    Parsec.string "*bank"
    Parsec.skipMany1 Parsec.space
    n <- Parse.p_nat
    st <- Parsec.getState
    Parsec.setState (st { state_bank = n, state_patch_num = 0 })

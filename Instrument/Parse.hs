-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to parse MIDI patch files.
module Instrument.Parse where
import qualified Control.Monad.State.Strict as State
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.P as P
import           Util.P ((<?>))
import qualified Util.ParseM as ParseM

import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Sysex as Sysex
import qualified Instrument.Tag as Tag

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Ui.Id as Id

import           Global


type Parser st a = ParseM.ParserS st a

-- * annotation file

type Annotation = Tag.Tag

-- | Format is @synth\/inst-name tag1=x tag2=y@.
--
-- TODO other attributes are not supported, but if there were, they could look
-- like @*pb-range=12 *flag=pressure@
parseAnnotations :: FilePath
    -> IO (Either String (Map InstT.Qualified [Annotation]))
parseAnnotations fn = do
    result <- ParseM.file mempty pAnnotationFile () fn
    pure $ bimap show (Map.fromListWith (++)) result

pAnnotationFile :: Parser st [(InstT.Qualified, [Annotation])]
pAnnotationFile = concat <$> P.many line <* P.eof
    where line = ((:[]) <$> P.try pAnnotationLine) <|> (pEol >> pure [])

pAnnotationLine :: Parser st (InstT.Qualified, [Annotation])
pAnnotationLine =
    ((,) <$> lexeme pQualified <*> P.many (lexeme pTag)) <* pEol

pQualified :: Parser st InstT.Qualified
pQualified =
    InstT.Qualified <$> chars <*> (P.char '/' *> chars) <?> "qualified"
    where chars = P.takeWhile1 Id.isIdChar

pTag :: Parser st Tag.Tag
pTag =
    (,) <$> P.takeWhile1 tagChar
    <*> P.option "" (P.char '=' *> P.takeWhile1 tagChar)
    where tagChar c = Char.isAlphaNum c || c == '-'

lexeme :: Parser st a -> Parser st a
lexeme p = p <* pWhitespace

pEol :: Parser st ()
pEol = pWhitespace <* P.char '\n'

pWhitespace :: Parser st ()
pWhitespace = P.skipMany (P.oneOfC " \t") >> P.option () comment
    where comment = P.char '#' >> P.skipMany (P.satisfy (/='\n'))


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
patchFile :: FilePath -> IO [Sysex.Patch]
patchFile fn = either (errorIO . ("parse patches: " <>)) pure
    =<< parsePatchFile fn

parsePatchFile :: String -> IO (Either Text [Sysex.Patch])
parsePatchFile fn =
    fmap (map (second (Sysex.addFile fn))) <$>
        ParseM.file mempty pPatchFile emptyState fn

data State = State
    { stateBank :: Int
    , statePatchNum :: Midi.Program
    } deriving (Show)

emptyState :: State
emptyState = State 0 0

data PatchLine = PatchLine
    { patchName :: Text
    , patchBank :: Int
    , patchProgram :: Midi.Program
    , patchTags :: [Tag.Tag]
    } deriving (Show)

pPatchFile :: Parser State [Sysex.Patch]
pPatchFile = do
    patches <- Maybe.catMaybes <$> P.many pLine
    pure $ map (makePatch (-2, 2)) (inheritPrevCategory patches)
    where
    inheritPrevCategory = snd . List.mapAccumL inherit Nothing
    inherit maybePrev patch = case lookup Tag.category tags of
            Nothing -> case maybePrev of
                Nothing -> (Nothing, patch)
                Just prev -> (Just prev,
                    patch { patchTags = (Tag.category, prev) : tags })
            Just cat -> (Just cat, patch)
        where tags = patchTags patch

makePatch :: Control.PbRange -> PatchLine -> Sysex.Patch
makePatch pbRange (PatchLine name bank patchNum tags) = (patch, common)
    where
    patch = (Patch.patch pbRange name)
        { Patch.patchInitialize =
            Patch.initializeMidi $ Midi.programChange bank patchNum
        }
    common = (Common.common ()) { Common.commonTags = tags }

pLine :: Parser State (Maybe PatchLine)
pLine =
    (pBankDecl >> pure Nothing)
    <|> (pEol >> pure Nothing)
    <|> fmap Just pPatchLine

pPatchLine :: Parser State PatchLine
pPatchLine = do
    name <- P.takeWhile1 (`notElem` ['\n', ','])
    tags <- P.option [] $ comma >> P.sepBy1 pTag comma
    pEol
    st <- State.get
    State.put $ st { statePatchNum = statePatchNum st + 1 }
    pure $ PatchLine name (stateBank st) (statePatchNum st) tags
    where
    comma = lexeme (P.char ',')

pBankDecl :: Parser State ()
pBankDecl = do
    P.string "*bank"
    P.space1
    n <- ParseM.pNat
    State.modify' $ \st -> st { stateBank = n, statePatchNum = 0 }

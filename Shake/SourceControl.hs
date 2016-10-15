-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Functions to deal with local source control.
module Shake.SourceControl (currentPatch, Entry(..), currentPatchParsed) where
import qualified Data.Char as Char
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import qualified System.Exit as Exit
import qualified System.Process as Process

import qualified Util.Regex as Regex


-- | Get human-readable darcs log output.
--
-- This assumes the current dir is in the darcs repo.
currentPatch :: IO (Either String String)
currentPatch = fmap strip <$> getCurrentPatch False

data Entry = Entry {
    _author :: !Text
    , _localDate :: !Text
    , _hash :: !Text
    } deriving (Show)

-- | Newer darcs have a patch hash.
--
-- I have to parse the stdout because the only machine readable option is xml
-- and who wants to parse that.
currentPatchParsed :: IO (Either String Entry)
currentPatchParsed = (parseXml . Text.pack =<<) <$> getCurrentPatch True

strip :: String -> String
strip = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

{- Example output:
    <changelog>
    <patch author='qdunkan@gmail.com' date='20161014175123' local_date='Fri Oct 14 10:51:23 PDT 2016' inverted='False' hash='ac46ad4bde5e6fa1a5a742ab6f6ae9415b2fdbfe'>
        <name>add LInst.set_addr</name>
        <comment>Ignore-this: 50324106664282aff81117720fd11f1f</comment>
    </patch>
    </changelog>
-}
parseXml :: Text -> Either String Entry
parseXml xml = Entry <$> match "author" <*> match "local_date" <*> match "hash"
    where match field = matchOne (field <> "='([^']*)'") xml

matchOne :: String -> Text -> Either String Text
matchOne regex text = do
    cRegex <- Regex.compileOptions [Regex.Multiline] regex
    case Regex.groups cRegex text of
        [(_, [group])] -> Right group
        matches -> Left $ "expected exactly one match: " <> show matches

getCurrentPatch :: Bool -> IO (Either String String)
getCurrentPatch xml = do
    (exit, stdout, stderr) <- Process.readProcessWithExitCode
        "darcs" (["log", "--last=1"] ++ if xml then ["--xml-output"] else []) ""
    return $ case exit of
        Exit.ExitFailure n ->
            Left $ "darcs failed with " <> show n <> ": " <> stderr
        Exit.ExitSuccess -> Right stdout

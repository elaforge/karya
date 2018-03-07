-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
-- | Functions to deal with local source control.
module Util.SourceControl (Entry(..), current, showDate) where
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Clock.POSIX

import qualified System.Directory as Directory
import qualified System.Exit as Exit
import System.FilePath ((</>))
import qualified System.Process as Process

import qualified Util.ParseText as ParseText
import qualified Util.Regex as Regex
import qualified Util.TextUtil as TextUtil


data SCM = Darcs | Git deriving (Show, Eq)

type Error = String

data Entry = Entry {
    _author :: !Text
    , _date :: !Time.UTCTime
    , _hash :: !Text
    , _summary :: !Text
    } deriving (Show)

current :: FilePath -> IO (Either Error Entry)
current dir = do
    scm <- inferScm dir
    case scm of
        Darcs -> currentPatchDarcs dir
        Git -> currentPatchGit dir

showDate :: Time.UTCTime -> Text
showDate = Text.pack
    . Time.formatTime Time.defaultTimeLocale
        (Time.iso8601DateFormat (Just "%H:%M:%S"))

inferScm :: FilePath -> IO SCM
inferScm dir = do
    git <- Directory.doesDirectoryExist (dir </> ".git")
    return $ if git then Git else Darcs

-- * darcs

currentPatchDarcs :: FilePath -> IO (Either Error Entry)
currentPatchDarcs dir = (parseXml . Text.pack =<<) <$> getCurrentPatchDarcs dir

{- Example output:
    <changelog>
    <patch author='qdunkan@gmail.com' date='20161014175123' local_date='Fri Oct 14 10:51:23 PDT 2016' inverted='False' hash='ac46ad4bde5e6fa1a5a742ab6f6ae9415b2fdbfe'>
        <name>add LInst.set_addr</name>
        <comment>Ignore-this: 50324106664282aff81117720fd11f1f</comment>
    </patch>
    </changelog>
-}
parseXml :: Text -> Either Error Entry
parseXml xml =
    Entry <$> attr "author"
        <*> (parseDate =<< attr "date")
        <*> attr "hash"
        <*> field "name"
    where
    attr name = matchOne ("\\b" <> name <> "='([^']*)'") xml
    field name = unquote <$>
        matchOne ("<" <> name <> ">([^<]*)</" <> name <> ">") xml

-- | Parse darcs date format, e.g. "20180127222545".
parseDate :: Text -> Either Error Time.UTCTime
parseDate = Time.parseTimeM False Time.defaultTimeLocale "%Y%m%d%H%M%S"
    . Text.unpack

unquote :: Text -> Text
unquote = TextUtil.replaceMany $ map (\(k, v) -> ("&" <> k <> ";", v))
    [ ("apos", "'")
    , ("lt", "<")
    , ("gt", ">")
    , ("amp", "&")
    ]

matchOne :: String -> Text -> Either Error Text
matchOne regex text = do
    cRegex <- Regex.compileOptions [Regex.Multiline] regex
    case Regex.groups cRegex text of
        [(_, [group])] -> Right group
        matches -> Left $ "expected exactly one match: " <> show matches

getCurrentPatchDarcs :: FilePath -> IO (Either Error String)
getCurrentPatchDarcs dir = do
    (exit, stdout, stderr) <- Process.readCreateProcessWithExitCode
        ((Process.proc "darcs" ["log", "--last=1", "--xml-output"])
            { Process.cwd = Just dir })
        ""
    return $ case exit of
        Exit.ExitFailure n ->
            Left $ "darcs failed with " <> show n <> ": " <> stderr
        Exit.ExitSuccess -> Right stdout

-- * git

currentPatchGit :: FilePath -> IO (Either Error Entry)
currentPatchGit dir = (parseGit . Text.pack =<<) <$> getCurrentPatchGit dir

getCurrentPatchGit :: FilePath -> IO (Either Error String)
getCurrentPatchGit dir = do
    (exit, stdout, stderr) <- Process.readCreateProcessWithExitCode
        ((Process.proc "git" ["log", "-n1", "--format=%H%n%ae%n%at%n%s"])
            { Process.cwd = Just dir })
        ""
    return $ case exit of
        Exit.ExitFailure n ->
            Left $ "git failed with " <> show n <> ": " <> stderr
        Exit.ExitSuccess -> Right stdout

-- 1c1aa1db8eb8ae5ec76507f27ab6a6a3107a76f6
-- qdunkan@gmail.com
-- 2018-02-16T02:49:11+00:00
-- clean up the last of the -Wmissing-monadfail-instances warnings
parseGit :: Text -> Either Error Entry
parseGit = parse . Text.lines
    where
    parse [hash, author, timestamp, summary] = do
        timestamp <- maybe
            (Left $ "can't parse timestamp: " <> Text.unpack timestamp) Right $
            ParseText.int timestamp
        let date = Clock.POSIX.posixSecondsToUTCTime (fromIntegral timestamp)
        return $ Entry
            { _author = author
            , _date = date
            , _hash = hash
            , _summary = summary
            }
    parse _ = Left "expected 4 lines"

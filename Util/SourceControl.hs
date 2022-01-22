-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE OverloadedStrings #-}
-- | Functions to deal with local source control.
module Util.SourceControl (Entry(..), current, showDate) where
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Clock.POSIX

import qualified System.Exit as Exit
import qualified System.Process as Process

import qualified Util.ParseText as ParseText


type Error = String

data Entry = Entry {
    _author :: !Text
    , _date :: !Time.UTCTime
    , _hash :: !Text
    , _summary :: !Text
    } deriving (Show)

current :: FilePath -> IO (Either Error Entry)
current = currentPatchGit

showDate :: Time.UTCTime -> Text
showDate = Text.pack
    . Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

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

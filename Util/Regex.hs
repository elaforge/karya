-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE FlexibleContexts #-}
-- | More user friendly regex api for PCRE regexes.
module Util.Regex (
    Regex
    -- * compile
    , Option(..)
    , compile, compileOptions, compileUnsafe, compileOptionsUnsafe

    -- * matching
    , matches, groups, groupRanges
    -- * substitute
    , substitute, substituteGroups

    -- * misc
    , escape
) where
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Encoding

import qualified Text.Regex.PCRE.Heavy as PCRE
import Text.Regex.PCRE.Heavy (Regex)
import qualified Text.Regex.PCRE.Light as PCRE

import qualified Util.CallStack as CallStack


-- * compile

fromText :: Text -> ByteString.ByteString
fromText = Encoding.encodeUtf8

data Option = CaseInsensitive | DotAll | Multiline
    deriving (Ord, Eq, Show)

compile :: String -> Either String Regex
compile = compileOptions []

compileOptions :: [Option] -> String -> Either String Regex
compileOptions options text =
    case PCRE.compileM (fromText (Text.pack text)) (convertOptions options) of
        Left msg -> Left $ "compiling regex " ++ show text ++ ": " ++ msg
        Right regex -> Right regex

convertOptions :: [Option] -> [PCRE.PCREOption]
convertOptions = (options++) . map convert
    where
    convert opt = case opt of
        CaseInsensitive -> PCRE.caseless
        DotAll -> PCRE.dotall
        Multiline -> PCRE.multiline
    options = [PCRE.utf8, PCRE.no_utf8_check]

-- | Will throw a runtime error if the regex has an error!
compileUnsafe :: CallStack.Stack => String -> Regex
compileUnsafe = compileOptionsUnsafe []

-- | Will throw a runtime error if the regex has an error!
compileOptionsUnsafe :: CallStack.Stack => [Option] -> String -> Regex
compileOptionsUnsafe options =
    either (CallStack.errorStack . Text.pack) id . compileOptions options

-- * match

matches :: Regex -> Text -> Bool
matches = flip (PCRE.=~)

-- | Return (complete_match, [group_match]).
groups :: Regex -> Text -> [(Text, [Text])]
groups = PCRE.scan

-- | Half-open ranges of where the regex matches.
groupRanges :: Regex -> Text -> [((Int, Int), [(Int, Int)])]
    -- ^ (entire, [group])
groupRanges = PCRE.scanRanges

-- * substitute

-- | TODO this is not the usual thing where it replaces \1 \2 etc., but
-- it replaces the entire match.
substitute :: Regex -> Text -> Text -> Text
substitute regex sub = PCRE.gsub regex sub

substituteGroups :: Regex -> (Text -> [Text] -> Text)
    -- ^ (complete_match -> groups -> replacement)
    -> Text -> Text
substituteGroups = PCRE.gsub

-- * misc

-- | Escape a string so the regex matches it literally.
escape :: String -> String
escape "" = ""
escape (c : cs)
    | c `elem` ("\\^$.[|()?*+{" :: [Char]) = '\\' : c : escape cs
    | otherwise = c : escape cs

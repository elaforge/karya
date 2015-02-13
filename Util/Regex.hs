-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | More user friendly regex api for PCRE regexes.
module Util.Regex (
    Regex
    -- * compile
    , Option(..)
    , compile, compileUnsafe, compileOptionsUnsafe

    -- * matching
    , matches, groups, groupRanges

    -- * misc
    , escape
) where
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding

import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Text.Regex.PCRE.Light as PCRE
import Text.Regex.PCRE.Heavy (Regex)


-- * compile

fromText :: Text -> ByteString.ByteString
fromText = Encoding.encodeUtf8

-- toText :: ByteString.ByteString -> Text
-- toText = Encoding.decodeUtf8With Encoding.Error.lenientDecode

data Option = CaseInsensitive | DotAll
    deriving (Ord, Eq, Show)

-- TODO default options to []
compile :: [Option] -> String -> Either String Regex
compile options = compileOptions options

compileOptions :: [Option] -> String -> Either String Regex
compileOptions options text =
    -- TODO take Text instead of String
    case PCRE.compileM (fromText (Text.pack text)) (convertOptions options) of
        Left msg -> Left $ "compiling regex " ++ show text ++ ": " ++ msg
        Right regex -> Right regex

convertOptions :: [Option] -> [PCRE.PCREOption]
convertOptions = (options++) . map convert
    where
    convert opt = case opt of
        CaseInsensitive -> PCRE.caseless
        DotAll -> PCRE.dotall
    options = [PCRE.utf8, PCRE.no_utf8_check]

-- | Will throw a runtime error if the regex has an error!
compileUnsafe :: String -> String -> Regex
compileUnsafe caller = compileOptionsUnsafe caller []

-- | Will throw a runtime error if the regex has an error!
compileOptionsUnsafe :: String -> [Option] -> String -> Regex
compileOptionsUnsafe caller options =
    either (error . ((caller ++ ": ") ++)) id . compileOptions options

-- * match

matches :: Regex -> String -> Bool
matches = flip (PCRE.=~)

-- | Return (complete_match, [group_match]).
groups :: Regex -> String -> [(String, [String])]
groups = PCRE.scan

-- | Half-open ranges of where the regex matches.
-- TODO group matches should be included
groupRanges :: Regex -> String -> [(Int, Int)]
groupRanges regex = map fst . PCRE.scanRanges regex

-- * misc

-- | Escape a string so the regex matches it literally.
escape :: String -> String
escape "" = ""
escape (c : cs)
    | c `elem` "\\^$.[|()?*+{" = '\\' : c : escape cs
    | otherwise = c : escape cs

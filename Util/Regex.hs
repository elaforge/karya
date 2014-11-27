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
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe

import qualified Text.Regex.PCRE as PCRE


data Regex = Regex { regexText :: !String, regexCompiled :: !PCRE.Regex }

instance Show Regex where
    show regex = "Regex.compile " ++ show (regexText regex)

-- * compile

data Option = CaseInsensitive | DotAll
    deriving (Ord, Eq, Show)

compile :: [Option] -> String -> Either String Regex
compile options str =
    case PCRE.makeRegexOptsM (convertOptions options) 0 str of
        Left msg -> Left $ "compiling regex " ++ show str ++ ": " ++ msg
        Right reg -> Right (Regex str reg)

convertOptions :: [Option] -> PCRE.CompOption
convertOptions = foldr (Bits..|.) 0 . map convert
    where
    convert opt = case opt of
        CaseInsensitive -> PCRE.compCaseless
        DotAll -> PCRE.compDotAll

-- | Will throw a runtime error if the regex has an error!
compileUnsafe :: String -> String -> Regex
compileUnsafe caller = compileOptionsUnsafe caller []

-- | Will throw a runtime error if the regex has an error!
compileOptionsUnsafe :: String -> [Option] -> String -> Regex
compileOptionsUnsafe caller options =
    either (error . ((caller ++ ": ") ++)) id . compile options

-- * match

matches :: Regex -> String -> Bool
matches = PCRE.matchTest . regexCompiled

-- | Return (complete_match, [group_match]).
groups :: Regex -> String -> [(String, [String])]
groups reg str =
    Maybe.mapMaybe extract (PCRE.matchAllText (regexCompiled reg) str)
    where
    extract arr = case map fst (IArray.elems arr) of
        h : rest -> Just (h, rest)
        [] -> Nothing

-- | Half-open ranges of where the regex matches.
groupRanges :: Regex -> String -> [(Int, Int)]
groupRanges reg str = concatMap extract (PCRE.matchAll (regexCompiled reg) str)
    where extract arr = [(i, i+n) | (i, n) <- IArray.elems arr]

-- * misc

-- | Escape a string so the regex matches it literally.
escape :: String -> String
escape "" = ""
escape (c : cs)
    | c `elem` "\\^$.[|()?*+{" = '\\' : c : escape cs
    | otherwise = c : escape cs

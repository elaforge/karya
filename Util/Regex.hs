-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | More user friendly regex api.
module Util.Regex where
import qualified Data.Array.IArray as IArray
import qualified Data.Bits as Bits
import qualified Data.Maybe as Maybe

import qualified Text.Regex.PCRE as PCRE


data Regex = Regex String PCRE.Regex

regex :: Regex -> PCRE.Regex
regex (Regex _ r) = r

instance Show Regex where
    show (Regex reg _) = "Regex.make " ++ show reg

data Option = CaseInsensitive | DotAll
    deriving (Ord, Eq, Show)

makeM :: [Option] -> String -> Either String Regex
makeM options str = case PCRE.makeRegexOptsM (convert_options options) 0 str of
    Left msg -> Left $ "compiling regex " ++ show str ++ ": " ++ msg
    Right reg -> Right (Regex str reg)

convert_options :: [Option] -> PCRE.CompOption
convert_options = foldr (Bits..|.) 0 . map convert
    where
    convert opt = case opt of
        CaseInsensitive -> PCRE.compCaseless
        DotAll -> PCRE.compDotAll

-- | Will throw a runtime error if the regex has an error!
make :: String -> Regex
make = make_options []

-- | Like 'make', but you can provide options.
make_options :: [Option] -> String -> Regex
make_options options = either error id . makeM options

matches :: Regex -> String -> Bool
matches (Regex _ reg) = PCRE.matchTest reg

-- | Return (complete_match, [group_match]).
find_groups :: Regex -> String -> [(String, [String])]
find_groups reg str =
    Maybe.mapMaybe extract (PCRE.matchAllText (regex reg) str)
    where
    extract arr = case map fst (IArray.elems arr) of
        (h:rest) -> Just (h, rest)
        [] -> Nothing

find_ranges :: Regex -> String -> [(Int, Int)]
find_ranges reg str = concatMap extract (PCRE.matchAll (regex reg) str)
    where extract arr = [(i, i+n) | (i, n) <- IArray.elems arr]

-- | Escape a string so the regex matches it literally.
escape :: String -> String
escape "" = ""
escape (c : cs)
    | c `elem` "\\^$.[|()?*+{" = '\\' : c : escape cs
    | otherwise = c : escape cs

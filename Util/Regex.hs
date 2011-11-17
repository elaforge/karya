-- | More user friendly regex api.
module Util.Regex where
import qualified Data.Array.IArray as IArray
import qualified Data.Maybe as Maybe
import qualified Text.Regex.PCRE as PCRE


data Regex = Regex String PCRE.Regex

instance Show Regex where
    show (Regex reg _) = "Regex.make " ++ show reg

makeM :: String -> Either String Regex
makeM str = case PCRE.makeRegexM str of
    Left msg -> Left $ "compiling regex " ++ show str ++ ": " ++ msg
    Right reg -> Right (Regex str reg)

make :: String -> Regex
make = either error id . makeM

matches :: Regex -> String -> Bool
matches (Regex _ reg) str = PCRE.matchTest reg str

-- | Return (complete_match, [group_match]).
find_groups :: Regex -> String -> [(String, [String])]
find_groups (Regex _ reg) str =
    Maybe.mapMaybe extract (PCRE.matchAllText reg str)
    where
    extract arr = case map fst (IArray.elems arr) of
        (h:rest) -> Just (h, rest)
        [] -> Nothing

find_ranges :: Regex -> String -> [(Int, Int)]
find_ranges (Regex _ reg) str = concatMap extract (PCRE.matchAll reg str)
    where extract arr = [(i, i+n) | (i, n) <- IArray.elems arr]

-- | Escape a string so the regex matches it literally.
escape :: String -> String
escape "" = ""
escape (c : cs)
    | c `elem` "\\^$.[|()?*+{" = '\\' : c : escape cs
    | otherwise = c : escape cs

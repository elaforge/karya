#!/usr/local/bin/runghc
{-# LANGUAGE OverloadedStrings #-}

-- Run cabal freeze, then use this to take out the base deps, so it has a
-- chance of working with another ghc version.
import qualified Data.Text as Text
import qualified System.Process as Process
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO


main :: IO ()
main = do
    constraints <- parseConstraints . Text.lines <$>
        Text.IO.readFile "cabal.config"
    global <- map (Text.dropWhileEnd (=='-') . Text.dropWhileEnd (/='-')
        . Text.strip) . drop 1 . Text.lines
        . Text.pack <$> Process.readProcess "ghc-pkg" ["list", "--global"] ""
    mapM_ Text.IO.putStrLn $ ("constraints:":) $
        mapInit (<>",") $ map (\(a, b) -> "    " <> Text.unwords [a, b]) $
        filter ((`notElem` global) . fst) constraints

parseConstraints :: [Text] -> [(Text, Text)]
parseConstraints (line:lines) =
    map (split . strip) (Text.dropWhile (/=' ') line : lines)
    where
    strip = Text.dropWhileEnd (==',') . Text.strip
    split s = case Text.words s of
        [a, b] -> (a, b)

mapInit :: (a -> a) -> [a] -> [a]
mapInit _ [] = []
mapInit _ [x] = [x]
mapInit f (x:xs) = f x : mapInit f xs

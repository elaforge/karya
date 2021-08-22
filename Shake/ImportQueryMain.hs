-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Program to query the module graph.
module Shake.ImportQueryMain where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tree as Tree

import qualified System.Environment as Environment

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Shake.ImportQuery as ImportQuery
import           Shake.ImportQuery (Module)

import           Global


main :: IO ()
main = do
    Environment.getArgs >>= \case
        cmd : args -> case Map.lookup cmd commands of
            Nothing -> die "unknown command"
            Just parse -> case parse args of
                Left expected -> die $ "expected args: " <> show expected
                    <> " but got: " <> show args
                Right cmd -> cmd
        _ -> die "expected 1 command"
    where
    die msg = do
        putStrLn msg
        putStrLn $ unwords $ "cmds:" : Map.keys commands

commands :: Map String ([String] -> Either [String] (IO ()))
commands = Map.fromList
    [ ("cache", parse0 ImportQuery.cacheGraph)
    , ("weak", parse0 cWeak)
    , ("rm", parse2 "parent" "removed" cRm)
    , ("add", parse2 "parent" "new" cAdd)
    ]
    where
    parse0 cmd = \case
        [] -> Right cmd
        _ -> Left []
    parse2 a1 a2 cmd = \case
        [arg1, arg2] -> Right $ cmd (fnameToModule arg1) (fnameToModule arg2)
        _ -> Left [a1, a2]

cWeak :: IO ()
cWeak = do
    ImportQuery.cacheGraph
    ImportQuery.findWeakLinks =<< ImportQuery.loadCachedGraph

cRm :: Module -> Module -> IO ()
cRm parent removed = do
    graph <- ImportQuery.loadCachedGraph
    let rms = ImportQuery.rmDep graph parent removed
    if  | parent `Map.notMember` ImportQuery._graph graph ->
            Text.IO.putStrLn $ "no parent module: " <> parent
        | null rms -> Text.IO.putStrLn $ parent <> " doesn't import " <> removed
        | otherwise -> do
            mapM_ (Text.IO.putStrLn . prettyRmDep) $
                Seq.sort_on (Set.size . snd) rms
            putStrLn $ "total lost: "
                <> show (Num.sum (map (Set.size . snd) rms))

prettyRmDep :: (Module, Set Module) -> Text
prettyRmDep (mod, lost) = mod <> " - "
    <> (if Set.size lost > 2 then showt (Set.size lost) <> ":" else "")
    <> Text.intercalate "," (Set.toList lost)

cAdd :: Module -> Module -> IO ()
cAdd parent new = do
    graph <- ImportQuery.loadGraph
    case ImportQuery.addDep graph parent new of
        Left paths -> do
            putStrLn $ "already imported via paths:"
            mapM_ (Text.IO.putStrLn . Text.intercalate "->") paths
        Right tree -> do
            -- TODO improve
            putStrLn $ Tree.drawTree $ untxt <$> tree

fnameToModule :: String -> ImportQuery.Module
fnameToModule =
    txt . Seq.replace1 '/' "." . dropSuffixes [".hs", ".hsc", ".chs"]

dropSuffixes :: [String] -> FilePath -> String
dropSuffixes suffixes str =
    maybe str (fst . flip Seq.drop_suffix str) $
        List.find (\suf -> suf `List.isSuffixOf` str) suffixes

-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Program to query the module graph.
module Shake.ImportQueryMain where
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tree as Tree

import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Text.Read as Read

import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Trees as Trees

import qualified Shake.ImportQuery as ImportQuery
import           Shake.ImportQuery (Module)

import           Global


main :: IO ()
main = do
    Environment.getArgs >>= \case
        cmd : args -> case Map.lookup cmd commands of
            Nothing -> die "unknown command"
            Just (_, parse) -> case parse args of
                Left expected -> die $ "expected args: " <> show expected
                    <> " but got: " <> show args
                Right cmd -> cmd
        _ -> die "expected 1 command"
    where
    die msg = do
        putStrLn msg
        mapM_ putStrLn
            [ cmd <> " - " <> doc
            | (cmd, (doc, _)) <- Map.toAscList commands
            ]

type Doc = String

commands :: Map String (Doc, ([String] -> Either [String] (IO ())))
commands = Map.fromList $ map (\(name, doc, cmd) -> (name, (doc, cmd)))
    [ ( "cache"
      , "recreate cache in " <> ImportQuery.cacheFile
      , parse0 ImportQuery.cacheGraph
      )
    , ( "weak"
      , "Report weak links, which are imports that would reduce the closure\
        \ by a lot if broken."
      , parse0 cWeak
      )
    , ( "rm"
      , "Show what would happen to the import closure if an import was removed."
      , parse2 "parent" "removed" cRm
      )
    , ( "add"
      , "Show what would happen to the import closure if an import was added."
      , parse2 "parent" "new" cAdd
      )
    , ( "path"
      , "Show the way the parent imports the child."
      , parse2 "parent" "child" cPath
      )
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
    scores <- ImportQuery.findWeakLinks <$> ImportQuery.loadCachedGraph
    total <- mapStateM fmt 0 scores
    -- Make the total line parseable by parseWeaks
    putStrLn $ show total <> " TOTAL TOTAL"
    putStrLn $ "total: " <> show total
    where
    fmt total (score, ((parent, rm), _rms)) = do
        when (score > 3) $ do
            Text.IO.putStrLn $ Text.unwords [showt score, parent, rm]
            IO.hFlush IO.stdout
        return $ total + score

mapStateM :: Monad m => (state -> a -> m state) -> state -> [a] -> m state
mapStateM action state = \case
    [] -> pure state
    x : xs -> do
        !state <- action state x
        mapStateM action state xs

cRm :: Module -> Module -> IO ()
cRm parent removed = do
    graph <- ImportQuery.loadCachedGraph
    let rms = ImportQuery.rmDep graph parent removed
    if  | parent `Map.notMember` ImportQuery._graph graph ->
            Text.IO.putStrLn $ "no parent module: " <> parent
        | null rms -> Text.IO.putStrLn $ parent <> " doesn't import " <> removed
        | otherwise -> do
            mapM_ (Text.IO.putStrLn . prettyRmDep) $
                Lists.sortOn (Set.size . snd) rms
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
            putStrLn "New modules added, * marks already imported ones:"
            putStrLn $ draw tree
            putStrLn "Adds only:"
            putStrLn $ maybe "nothing?" draw $
                Trees.filter (not . ("*" `Text.isSuffixOf`)) tree

cPath :: Module -> Module -> IO ()
cPath parent child = do
    graph <- ImportQuery.loadGraph
    mapM_ Text.IO.putStrLn $ case ImportQuery.paths graph parent child of
        [] -> ["no path from " <> parent <> " to " <> child]
        paths -> map (Text.intercalate " -> ") paths

draw :: Tree.Tree Text -> String
draw = Tree.drawTree . fmap untxt

fnameToModule :: String -> ImportQuery.Module
fnameToModule =
    txt . Lists.replace1 '/' "." . dropSuffixes [".hs", ".hsc", ".chs"]

dropSuffixes :: [String] -> FilePath -> String
dropSuffixes suffixes str =
    maybe str (fst . flip Lists.dropSuffix str) $
        List.find (\suf -> suf `List.isSuffixOf` str) suffixes

-- Call from ghci.
updateWeaks :: IO ()
updateWeaks = do
    old <- parseWeaks <$> Text.IO.readFile "weak-links"
    new <- parseWeaks <$> Text.IO.readFile "weak-links2"
    Text.IO.writeFile "weak-links-merged" $ unparseWeaks (mergeWeaks old new)

mergeWeaks :: Ord k => Map k Parsed -> Map k Parsed -> Map k Parsed
mergeWeaks old new = merge m old new
    where
    m = \case
        Lists.Both (Parsed sigil count1 comments) (Parsed _ count _) -> Parsed
            { _sigil = sigil
            , _count = count
            , _comments = filter (/="") $ if count1 /= count
                then "[" <> showt count1 <> "]" : comments
                else comments
            }
        Lists.Second a -> a
        -- '_' means this dep disappeared.
        Lists.First (Parsed _ count comments) -> Parsed '_' count comments

unparseWeaks :: Map (Text, Text) Parsed -> Text
unparseWeaks =
    mconcat . map (uncurry unparseWeak) . List.sortOn (_count . snd)
    . Map.toList

data Parsed = Parsed {
    _sigil :: Char
    , _count :: Int
    , _comments :: [Text]
    } deriving (Show)

unparseWeak :: (Text, Text) -> Parsed -> Text
unparseWeak (parent, child) (Parsed sigil count comments_) = Text.unlines $
    Text.unwords ([Text.singleton sigil, showt count, parent, child] ++ status)
    : comments
    where
    -- Following comments lines start with ' '.
    (status, comments) = case comments_ of
        c : cs | Text.all (/=' ') c -> ([c], cs)
        _ -> ([], comments_)

parseWeaks :: Text -> Map (Text, Text) Parsed
parseWeaks =
    Map.fromList . map convert . collectJust . Lists.keyOn parseSigil
    . Text.lines
    where
    convert ((k, (sigil, count, comment1)), comments) =
        (k, Parsed sigil count (comment1 : comments))

parseSigil :: Text -> Maybe ((Text, Text), (Char, Int, Text))
parseSigil line = case Text.uncons line of
    Just (c, line)
        | isSigil c, ds : parent : child : c1 <- Text.words line,
            Just d <- Read.readMaybe (untxt ds) ->
        Just ((parent, child), (c, d, Text.unwords c1))
    _ -> Nothing
    where
    isSigil c = c == ' ' || Char.isPunctuation c


-- * util

collectJust :: [(Maybe a, b)] -> [(a, [b])]
collectJust ((Just a, _b) : xs) = (a, map snd pre) : collectJust post
    where (pre, post) = span (Maybe.isNothing . fst) xs
collectJust ((Nothing, _b) : xs) = collectJust xs
collectJust [] = []

-- | Data.Map has a merge which can probably do this, but it's so complicated
-- I gave up on it.
merge :: Ord k => (Lists.Paired a1 b -> a2) -> Map k a1 -> Map k b -> Map k a2
merge merger m1 m2 = Map.fromAscList $ map (second merger) $
    Lists.pairSorted (Map.toAscList m1) (Map.toAscList m2)

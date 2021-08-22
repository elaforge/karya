-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities to generate and query the import graph.
module Shake.ImportQuery where
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified System.FilePath as FilePath
import qualified System.Process as Process

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Shake.HsDeps as HsDeps

import           Global


{-
    queries:
    If I add or remove an import, how does that change the transitive imports?
        . Get transitive closure before and after, show difference.
    What is the transitive closure of the imports of this module?
    What are the paths from A to B?

    I want to not just show as a list, but as a tree.
      . For each import, print the number of transitive things it imports.
-}

type Graph = Map Module [Module] -- TODO Set
type Module = Text

data CachedGraph = CachedGraph {
    _graph :: Graph
    , _closures :: Map Module (Set Module)
    } deriving (Show)

cachedGraph :: Graph -> CachedGraph
cachedGraph g = CachedGraph
    { _graph = g
    , _closures = Map.mapWithKey (\m _ -> closure g m) g
    }

loadCachedGraph :: IO CachedGraph
loadCachedGraph = cachedGraph <$> loadGraph

-- | If I add some import, what do I now depend on that I didn't before?
--
-- It's the new closure minus the previous one.
-- If I already imported that module, it's Left and the paths that show I
-- already depend.
-- If I didn't, I added it, plus whatever non-zero adds it implies.
-- TODO If it's circular, report that!
addDep :: Graph -> Module -> Module -> Either [[Module]] (Tree.Tree Module)
addDep graph parent new
    | Set.member new oldClosure = Left $ paths graph parent new
    | otherwise = Right $ trimTree oldClosure $ closureTree graph new
    where oldClosure = closure graph parent

-- | If I remove Perform.Signal from Derive.ScoreT, who now has fewer deps?
rmDep :: CachedGraph -> Module -> Module -> [(Module, Set Module)]
rmDep graph parent removed =
    filter ((/= mempty) . snd) . Seq.key_on_snd gone $ Map.keys (_graph graph)
    -- TODO this builds the complete closure of every module twice, only
    -- to filter out the ones that wind up empty.  Is there a way to
    -- find out there is no difference to short-circuit?
    where
    -- TODO emit this as a tree!
    gone mod = get mod (_closures graph) `Set.difference` closure without mod
    without = Map.adjust (filter (/=removed)) parent (_graph graph)

{-
    It would be interesting to find all the single drops with the highest lost
    score.  It won't reveal when I could drop two things... seems like there
    should be some kind of graph algorithm to find the weakest links holding
    the heaviest subgraph.

    Brute force way would be to do rmDep on each import of each module, and
    sort by scale.  That's probably too expensive if I want to try to remove 2.

    TODO brute force way is slow even for just one link.
-}

-- | This implements the brute-force way of seeing which single imports can be
-- removed for the greatest reduction in dependencies.
findWeakLinks :: CachedGraph -> [(Int, ((Text, Text), [(Module, Set Module)]))]
findWeakLinks graph = concatMap get $ filter wanted $ Map.keys (_graph graph)
    where
    get parent = map (get1 parent) (importsOf parent (_graph graph))
    get1 parent removed = (scoreOf rms, ((parent, removed), rms))
        where rms = rmDep graph parent removed
    scoreOf = Num.sum . map (Set.size . snd)
    wanted mod = not $ any (`Text.isSuffixOf` mod) ["_test", "_profile"]


-- TODO common up the prefixes into a tree?
-- TODO very slow, could I save time by memoizing branches I already searched?
-- Surely this is a standard DAG algorithm.
paths :: Graph -> Module -> Module -> [[Module]]
paths graph from to
    | from == to = [[to]]
    | otherwise = case Map.lookup from graph of
        Nothing -> []
        Just mods -> [from:path | m <- mods, path <- paths graph m to]

closureTree :: Graph -> Module -> Tree.Tree Module
closureTree graph mod =
    Tree.Node mod (map (closureTree graph) (importsOf mod graph))

trimTree :: Set Module -> Tree.Tree Module -> Tree.Tree Module
trimTree seen = snd . go seen
    where
    go seen (Tree.Node mod subs)
        | Set.member mod seen = (seen, Tree.Node (mod <> "*") [])
        | otherwise = Tree.Node mod <$>
            List.mapAccumL go (Set.insert mod seen) subs

-- | Closure as a list so you can abort as soon as you find something.
closureList :: Graph -> Module -> [Module]
closureList graph = Tree.flatten . closureTree graph

closure :: Graph -> Module -> Set Module
closure graph = go Set.empty
    where
    go seen mod
        | mod `Set.member` seen = seen
        | otherwise = List.foldl' go (Set.insert mod seen) (importsOf mod graph)

importsOf :: Module -> Graph -> [Module]
importsOf = get

get :: (Ord k, Monoid a) => k -> Map k a -> a
get = Map.findWithDefault mempty

-- * generate

cacheGraph :: IO ()
cacheGraph = Aeson.encodeFile "build/imports.json" =<< generateGraph

loadGraph :: IO Graph
loadGraph = fromMaybe (error "no parse") <$>
    Aeson.decodeFileStrict' "build/imports.json"

generateGraph :: IO Graph
generateGraph =
    fmap (Map.fromList . map (bimap fileToModule (map fileToModule))
        . Maybe.catMaybes . snd) $
        Seq.mapAccumLM get Set.empty =<< getAllHs
    where
    get seen fname
        | Set.member fname seen = return (seen, Nothing)
        | otherwise = do
            deps <- HsDeps.importsOfIO generatedSrc (cppFlags fname) fname
            return (Set.insert fname seen, Just (fname, deps))

-- From Shakefile, but simplified.

getAllHs :: IO [FilePath]
getAllHs =
    filter isHs . lines <$>
        Process.readProcess "git" ["ls-tree", "--name-only", "-r", "HEAD"] ""
    where isHs fn = any (`List.isSuffixOf` fn) [".hs", ".hsc", ".chs"]

generatedSrc :: HsDeps.Generated
generatedSrc = HsDeps.Generated
    { _generatedHs = Set.fromList ["Solkattu/All.hs"]
    , _generatedExtensions = [".hsc", ".chs"]
    }

cppFlags :: FilePath -> Maybe [String]
cppFlags fn
    | fn `Set.member` cppInImports = Just
        [ "-Ibuild/debug"
        , "-DTESTING"
        , "-DBUILD_DIR=\"\""
        , "-DGHC_VERSION=90001"
        ]
    | otherwise = Nothing

fileToModule :: FilePath -> Module
fileToModule = txt . map replace . FilePath.dropExtensions
    where
    replace '/' = '.'
    replace c = c

-- | Hardcoded list of modules that use CPP to determine their imports.
-- TODO duplicated with Shakefile.ccInImports.
cppInImports :: Set.Set FilePath
cppInImports = Set.fromList
    [ "App/Main.hs"
    , "Cmd/Repl.hs"
    , "Midi/MidiDriver.hs"
    , "App/LoadInstruments.hs"
    ]

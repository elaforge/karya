-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Collect tests from the given modules and generate a haskell module that
    calls the tests.  Test functions are any function starting with @test_@ or
    @profile_@ and immediately followed by @=@ (implying the function has no
    arguments).  This module doesn't distinguish between tests and profiles,
    but they should presumably be compiled separately since they require
    different flags.

    If a module has a function called @initialize@, it will be called as
    @IO ()@ prior to the tests.
-}
module Util.Test.GenerateRunTests (main) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Environment

import qualified Util.ExtractHs as ExtractHs
import qualified Util.Regex as Regex
import Global


main :: IO ()
main = do
    args <- System.Environment.getArgs
    ExtractHs.process args (extract . ExtractHs.stripComments)
        (\_ -> Right . generate)

generate :: Map FilePath ([Test], HasMeta) -> ([Text], Text)
generate fnameTests = (,) warnings $
    testTemplate
        (Text.unlines $ map ExtractHs.makeImport (Map.keys fnameTests))
        (Text.intercalate "\n    , " $ makeTests fnameTests)
    where
    noTests = map fst $ filter (null . fst . snd) $ Map.toList fnameTests
    -- I tend to have empty test modules with hand tests.  I used to filter out
    -- the empty modules, but at least this way they get type-checked.
    --
    -- Most likely ghc will already warn with -Wunused-imports but let's warn
    -- here too.
    warnings = map (("Warning: no (test|profile)_* defs in " <>) . txt) noTests

testTemplate :: Text -> Text -> Text
testTemplate imports allTests =
    "import qualified Util.Test.RunTests as RunTests\n\
    \import Util.Test.RunTests (Test(..))\n\
    \\n"
    <> imports <> "\n\
    \\n\
    \tests :: [Test]\n\
    \tests = \n\
    \    [ " <> allTests <> "\n\
    \    ]\n\
    \\n\
    \main :: IO ()\n\
    \main = RunTests.run tests\n"

data Test  = Test {
    testLineNumber :: !LineNumber
    , testName :: !Text
    } deriving (Show)
type LineNumber = Int

-- * extract

type HasMeta = Maybe Text

-- | Extract test functions and possible metadata from the file.
extract :: Text -> ([Test], HasMeta)
extract content = (extractTests content, hasMeta content)

hasMeta :: Text -> HasMeta
hasMeta =
    (\b -> if b then Just "meta" else Nothing) . Regex.matches reg
    where reg = Regex.compileOptionsUnsafe [Regex.Multiline] "^meta\\b"

extractTests :: Text -> [Test]
extractTests = go . zip [1..] . Text.lines
    where
    go ((i, line) : lines)
        | Just def <- hasTestFunction line = Test
            { testLineNumber = i
            , testName = def
            } : go rest
        | otherwise = go lines
        where
        rest = dropWhile (isIndented . snd) lines
        -- TODO does this get fooled by empty lines?
        isIndented t = " " `Text.isPrefixOf` t || t == "\n"
    go [] = []

hasTestFunction :: Text -> Maybe Text
hasTestFunction line
    | Regex.matches reg line = Just $ head (Text.words line)
    | otherwise = Nothing
    where
    reg = Regex.compileUnsafe "^(?:test|profile)_[a-zA-Z0-9_]+ \\="

-- * make

makeTests :: Map.Map FilePath ([Test], HasMeta) -> [Text]
makeTests fnameTests =
    [ makeTestLine fname test meta
    | (fname, (tests, meta)) <- Map.toList fnameTests, test <- tests
    ]

makeTestLine :: FilePath -> Test -> HasMeta -> Text
makeTestLine fname test meta = Text.unwords
    [ "Test", showt name, "(" <> name <> " >> return ())"
    , showt fname, showt (testLineNumber test)
    , case meta of
        Nothing -> "Nothing"
        Just fn -> "(Just " <> ExtractHs.pathToModule fname <> "." <> fn <> ")"
    ]
    where name = ExtractHs.pathToModule fname <> "." <> testName test

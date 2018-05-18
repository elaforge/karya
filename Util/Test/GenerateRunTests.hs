-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Collect tests from the given modules and generate a haskell module that
    calls the tests.  Test functions are any function starting with @test_@ or
    @profile_@ and immediately followed by @=@ (implying the function has no
    arguments).  This module doesn't distinguish between tests and profiles,
    but they should presumably be compiled separately since they required
    different flags.

    If a module has a function called @initialize@, it will be called as
    @IO ()@ prior to the tests.

    Tests are divided into interactive and auto variants.  Interactive tests
    want to have a conversation with the user, so they're not appropriate to
    run frequently.  Auto tests get an auto- prefix so you can avoid the
    interactive ones.  TODO interactive should be removed
-}
module Util.Test.GenerateRunTests (main) where
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified System.Environment
import qualified System.FilePath as FilePath

import qualified Util.ExtractHs as ExtractHs
import qualified Util.Regex as Regex
import qualified Util.TextUtil as TextUtil

import Global


main :: IO ()
main = do
    args <- System.Environment.getArgs
    ExtractHs.process args (extract . ExtractHs.stripComments) generate

generate :: FilePath -> Map FilePath ([Test], HasMeta)
    -> Either Text ([Text], Text)
generate outFname extracted = fmap (warnings,) $
    TextUtil.interpolate testTemplate $ Map.fromList
        [ ("imports", Text.unlines $
            map ExtractHs.makeImport (Map.keys fnameTests))
        , ("all_tests", Text.intercalate "\n    , " $ makeTests fnameTests)
        , ("argv0", showt (FilePath.dropExtension outFname))
        ]
    where
    (empty, fnameTests) = Map.partition (null . fst) extracted
    warnings = map (("Warning: no (test|profile)_* defs in " <>) . txt)
        (Map.keys empty)

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

testTemplate :: Text
testTemplate =
    "import qualified Util.Test.RunTests as RunTests\n\
    \import Util.Test.RunTests (Test(..))\n\
    \\n\
    \${imports}\n\
    \\n\
    \-- System.Environment.getProgName strips the dir, so I can't use it to\n\
    \-- reinvoke.\n\
    \argv0 :: String\n\
    \argv0 = ${argv0}\n\
    \\n\
    \tests :: [Test]\n\
    \tests = \n\
    \    [ ${all_tests}\n\
    \    ]\n\
    \\n\
    \main :: IO ()\n\
    \main = RunTests.run argv0 tests\n"

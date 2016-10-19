-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Collect tests from the given modules and generate a haskell module that
    calls the tests.  Test functions are any function starting with
    'large_test_', 'test_' or 'profile_' and immediately followed by '='
    (implying the function has no arguments).  This module doesn't distinguish
    between tests and profiles, but they should presumably be compiled
    separately since they required different flags.

    If a module has a function called 'initialize', it will be called as 'IO ()'
    prior to the tests.

    Tests are divided into interactive and auto variants.  Interactive tests
    want to have a conversation with the user, so they're not appropriate to
    run frequently.  Auto tests get an auto- prefix so you can avoid the
    interactive ones.  TODO interactive should be removed
-}
module Util.GenerateRunTests (main) where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Util.Regex as Regex
import qualified Util.TextUtil as TextUtil


main :: IO ()
main = do
    args <- System.Environment.getArgs
    (out_fname, tests) <- case args of
        out_fname : tests -> return (out_fname, tests)
        _ -> error "usage: generate_run_tests output.hs input_test1.hs\
            \ input_test2.hs ..."
    (empty, fnameTests) <- Map.partition (null . fst) <$> extractFiles tests
    forM (Map.keys empty) $ \fname ->
        IO.hPutStrLn IO.stderr $
            "Warning: no (test|profile)_* defs in " ++ show fname
    progName <- System.Environment.getProgName
    let output = either (error . Text.unpack) id $
            TextUtil.interpolate testTemplate $ Map.fromList
            [ ("generator", Text.pack progName)
            , ("imports", Text.unlines $ map makeImport (Map.keys fnameTests))
            , ("all_tests", Text.intercalate "\n    , " $ makeTests fnameTests)
            , ("argv0", showt (FilePath.dropExtension out_fname))
            ]
    Directory.createDirectoryIfMissing True $ FilePath.takeDirectory out_fname
    Text.IO.writeFile out_fname output

extractFiles :: [FilePath] -> IO (Map.Map FilePath ([Test], Maybe Text))
extractFiles tests = fmap mconcat $ forM tests $ \fname -> do
    content <- Text.IO.readFile fname
    return $ Map.singleton fname (extract fname content)

data Test  = Test {
    testLineNumber :: !LineNumber
    , testName :: !Text
    , testInteractive :: !Bool
    } deriving (Show)
type LineNumber = Int

-- * extract

-- | Extract test functions and a possible initilaize function from the file.
extract :: FilePath -> Text -> ([Test], Maybe Text)
extract fname content =
    (extractTests stripped, extractInitialize fname stripped)
    where stripped = stripComments content

-- | This will be fooled by a {- or -} inside a string.  I don't strip --
-- comments because the extract functions look for left justified text.
stripComments :: Text -> Text
stripComments = mconcat . go 0
    where
    go nesting text
        | Text.null post = [text]
        | "{-" `Text.isPrefixOf` post = (if nesting > 0 then id else (pre:))
            (go (nesting+1) (Text.drop 2 post))
        | otherwise = (if nesting == 0 then (pre <> Text.take 2 post :) else id)
            (go (max 0 (nesting-1)) (Text.drop 2 post))
        where (pre, post) = breakOnFirst "{-" "-}" text

breakOnFirst :: Text -> Text -> Text -> (Text, Text)
breakOnFirst a b text
    | Text.length aPre <= Text.length bPre = (aPre, aPost)
    | otherwise = (bPre, bPost)
    where
    (aPre, aPost) = Text.breakOn a text
    (bPre, bPost) = Text.breakOn b text

extractInitialize :: FilePath -> Text -> Maybe Text
extractInitialize fname content
    | Regex.matches reg content = Just $ pathToModule fname <> ".initialize"
    | otherwise = Nothing
    where reg = Regex.compileOptionsUnsafe [Regex.Multiline] "^initialize .*="

pathToModule :: FilePath -> Text
pathToModule =
    Text.map dot . Text.pack . FilePath.dropExtension . FilePath.normalise
    where dot c = if c == '/' then '.' else c

extractTests :: Text -> [Test]
extractTests = go . zip [1..] . Text.lines
    where
    go ((i, line) : lines)
        | Just def <- hasTestFunction line =
            Test i def (isInteractive (map snd body)) : go rest
        | otherwise = go lines
        where
        (body, rest) = span (isIndented . snd) lines
        isIndented t = " " `Text.isPrefixOf` t || t == "\n"
    go [] = []
    isInteractive = any ("io_human" `Text.isInfixOf`)

hasTestFunction :: Text -> Maybe Text
hasTestFunction line
    | Regex.matches reg line = Just $ head (Text.words line)
    | otherwise = Nothing
    where
    reg = Regex.compileUnsafe "^(?:large_test|test|profile)_[a-zA-Z0-9_]+ \\="

-- * make

makeImport :: FilePath -> Text
makeImport fname = "import qualified " <> pathToModule fname

makeTests :: Map.Map FilePath ([Test], Maybe Text) -> [Text]
makeTests fnameTests =
    [ makeTestLine fname test init
    | (fname, (tests, init)) <- Map.toList fnameTests, test <- tests
    ]

makeTestLine :: FilePath -> Test -> Maybe Text -> Text
makeTestLine fname test init = Text.unwords
    [ "Test", showt name, "(" <> name <> " >> return ())"
    , showt fname, showt (testLineNumber test)
    , maybe "Nothing" (("(Just "<>) . (<>")")) init
    , showt $ testType (Maybe.isJust init) (testName test)
        (testInteractive test)
    ]
    where name = pathToModule fname <> "." <> testName test

testType :: Bool -> Text -> Bool -> Text
testType hasInitialize name interactive
    | interactive = "interactive"
    | hasInitialize = "gui"
    | "large_test_" `Text.isPrefixOf` name = "large"
    | otherwise = "normal"

showt :: Show a => a -> Text
showt = Text.pack . show

testTemplate :: Text
testTemplate =
    "-- automatically generated by ${generator} --\n\
    \import qualified Util.RunTests as RunTests\n\
    \import Util.RunTests (Test(..))\n\
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

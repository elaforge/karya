module Util.RunTests where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.IORef as IORef
import qualified System.Environment
import qualified System.Exit
import qualified System.Console.GetOpt as GetOpt
import qualified System.Process as Process

import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.Test as Test


data Test = Test
    { testSymName :: String
    , testRun :: IO ()
    , testFilename :: FilePath
    , testLine :: Int
    , testInitialize :: Maybe (IO () -> IO ())
    , testType :: String
    }

testName :: Test -> String
testName test = testType test ++ "-" ++ testSymName test

data Flag = List | Noninteractive deriving (Eq, Show)

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["list"] (GetOpt.NoArg List) "display but don't run"
    , GetOpt.Option [] ["noninteractive"] (GetOpt.NoArg Noninteractive)
        "run though interactive tests without asking"
    ]

run :: String -> [Test] -> IO ()
run argv0 tests = do
    args <- System.Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (opts, n, []) -> return (opts, n)
        (_, _, errs) -> error $ "errors:\n" ++ concat errs
    runTests argv0 tests flags args

runTests :: String -> [Test] -> [Flag] -> [String] -> IO ()
runTests argv0 tests flags args
    | List `elem` flags = printTests
    | otherwise = do
        when (Noninteractive `elem` flags) $
            IORef.writeIORef Test.skip_human True
        printTests
        let (initTests, nonInitTests) =
                List.partition (Maybe.isJust . testInitialize) matches
        mapM_ runTest nonInitTests
        case initTests of
            [test] -> runTest test
            _ -> mapM_ (runSubprocess argv0) initTests
    where
    matches = matchingTests args tests
    printTests
        | null matches = putStrLn $ "no tests match: " ++ show args
        | otherwise = mapM_ putStrLn (List.sort (map testName matches))

runSubprocess :: String -> Test -> IO ()
runSubprocess argv0 test = do
    putStrLn $ "subprocess: " ++ show argv0 ++ " " ++ show [testName test]
    val <- Process.rawSystem argv0 [testName test]
    case val of
        System.Exit.ExitFailure code -> void $ Test.failure_srcpos Nothing $
            "test returned " ++ show code ++ ": " ++ testName test
        _ -> return ()

-- | Match all tests whose names match any regex, or if a test is an exact
-- match, just that test.
matchingTests :: [String] -> [Test] -> [Test]
matchingTests regexes tests = concatMap match regexes
    where
    byName = Map.fromList (zip (map testName tests) tests)
    match reg = case Map.lookup reg byName of
        Just test -> [test]
        Nothing -> filter (Regex.matches (Regex.make reg) . testName) tests

runTest :: Test -> IO ()
runTest test = do
    putStrLn $ "---------- run test "
        ++ testFilename test ++ ": " ++ testName test
    let name = last (Seq.split "." (testName test))
    maybe id id (testInitialize test) $ Test.catch_srcpos
        (Just (testFilename test, Just name, testLine test)) (testRun test)
    return ()

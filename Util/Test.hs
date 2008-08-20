-- | Basic testing utilities.
module Util.Test (
    skip_human
    -- * tests
    -- ** pure checks
    , check, check_srcpos, check_msg, check_msg_srcpos
    , equal, equal_srcpos
    , strings_like, strings_like_srcpos
    -- ** exception checks
    , throws, throws_srcpos, catch_srcpos
    , exc_like

    -- ** io checks
    , io_equal, io_equal_srcpos
    , io_human, io_human_srcpos

    -- * pretty printing
    , plist, pslist, pmlist
    , module PPrint
) where

import qualified Control.Exception as Exception
import Control.Monad
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import Text.Printf
import qualified Text.Regex as Regex

import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos
import Util.PPrint as PPrint


-- | If this is True, skip through human-feedback tests.  That way I can at
-- least get the coverage and check for crashes, even if I can't verify the
-- results.
skip_human :: IORef.IORef Bool
skip_human = Unsafe.unsafePerformIO (IORef.newIORef False)

-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

check = check_srcpos Nothing
check_srcpos srcpos False = failure srcpos "assertion false"
check_srcpos srcpos True = success srcpos "assertion true"

check_msg = check_msg_srcpos Nothing

check_msg_srcpos srcpos (False, msg) =
    failure srcpos ("assertion false: " ++ msg)
check_msg_srcpos srcpos (True, msg) =
    success srcpos ("assertion true: " ++ msg)

equal :: (Show a, Eq a) => a -> a -> IO ()
equal = equal_srcpos Nothing

equal_srcpos :: (Show a, Eq a) => SrcPos.SrcPos -> a -> a -> IO ()
equal_srcpos srcpos a b
    | a == b = success srcpos $ "== " ++ show a
    | otherwise = failure srcpos $ pa ++ "\t/=\n" ++ pb
    where
    pa = PPrint.pshow a
    pb = PPrint.pshow b

strings_like :: [String] -> [String] -> IO ()
strings_like = strings_like_srcpos Nothing

strings_like_srcpos :: SrcPos.SrcPos -> [String] -> [String] -> IO ()
strings_like_srcpos srcpos gotten expected =
    mapM_ (uncurry string_like) (zip gotten expected)
    where
    string_like a b
        | Maybe.isJust (Regex.matchRegex (Regex.mkRegex b) a) =
            success srcpos $ show a ++ " =~ " ++ show b
        | otherwise = failure srcpos $ show a ++ " !~ " ++ show b

-- | The given pure value should throw an exception that matches the predicate.
throws :: (Show a) => (Exception.Exception -> Bool) -> a -> IO ()
throws = throws_srcpos Nothing

throws_srcpos :: (Show a) => SrcPos.SrcPos -> (Exception.Exception -> Bool)
    -> a -> IO ()
throws_srcpos srcpos f val =
    (Exception.evaluate val >> failure srcpos ("didn't throw: " ++ show val))
    `Exception.catch` \exc ->
        if f exc
            then success srcpos ("caught exc: " ++ show exc)
            else failure srcpos ("exception didn't match: " ++ show exc)

catch_srcpos :: SrcPos.SrcPos -> IO () -> IO ()
catch_srcpos srcpos op = Exception.catch op
    (\e -> failure srcpos ("test threw exception: " ++ show e))

exc_like :: String -> Exception.Exception -> Bool
exc_like expected exc = expected `List.isInfixOf` show exc

-- IO oriented checks, the first value is pulled from IO.

io_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_equal = io_equal_srcpos Nothing

io_equal_srcpos :: (Eq a, Show a) => SrcPos.SrcPos -> IO a -> a -> IO ()
io_equal_srcpos srcpos io_val expected = do
    val <- io_val
    if val == expected
        then success srcpos ("== " ++ show val)
        else failure srcpos $
            "expected: " ++ show expected ++ ", got: " ++ show val

-- Only a human can check these things.
io_human :: String -> IO a -> IO a
io_human = io_human_srcpos Nothing
io_human_srcpos srcpos expected_msg op = do
    putStrLn $ "should see: " ++ expected_msg
    human_getch
    result <- op
    putStr $ "  ... ok? "
    c <- human_getch
    putChar '\n'
    if c /= 'y'
        then failure srcpos $ "didn't see " ++ show expected_msg
        else success srcpos $ "saw " ++ show expected_msg
    return result

-- * util

-- | Print a list with newlines between its elements.
plist :: Show a => [a] -> IO ()
plist xs = do
    mapM_ (\(i, x) -> putStr (show i ++ ": ") >> print x) (Seq.enumerate xs)
    putChar '\n'
pslist :: [String] -> IO ()
pslist xs = putStr $
    concatMap (\(i, x) -> printf "%02d. %s\n" i x) (Seq.enumerate xs)

pmlist msg xs = putStrLn (msg++":") >> plist xs


-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

-- | Print a msg with a special tag indicating a passing test.
success :: SrcPos.SrcPos -> String -> IO ()
success srcpos msg =
    hPrintf IO.stdout "++-> %s- %s\n" (SrcPos.show_srcpos srcpos) msg

-- | Print a msg with a special tag indicating a failing test.
failure :: SrcPos.SrcPos -> String -> IO ()
failure srcpos msg =
    hPrintf IO.stdout "__-> %s- %s\n" (SrcPos.show_srcpos srcpos) msg

-- getChar with no buffering
human_getch :: IO Char
human_getch = do
    skip <- IORef.readIORef skip_human
    if skip
        then return 'y'
        else do
            IO.hFlush IO.stdout
            mode <- IO.hGetBuffering IO.stdin
            IO.hSetBuffering IO.stdin IO.NoBuffering
            do { c <- getChar; putChar ' '; return c}
                `Exception.finally` (IO.hSetBuffering IO.stdin mode)

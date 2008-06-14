-- | Basic testing utilities.
module Util.Test where

import qualified Control.Exception as Exception
import qualified Data.List as List
import qualified System.IO as IO
import Text.Printf

import qualified Util.Seq as Seq
import qualified Util.Misc as Misc


-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

check = check_srcpos Nothing
check_srcpos srcpos False = failure srcpos "assertion false"
check_srcpos srcpos True = success srcpos "assertion true"

equal :: (Show a, Eq a) => a -> a -> IO ()
equal = equal_srcpos Nothing

-- | The given pure value should throw an exception that matches the predicate.
throws :: (Show a) => (Exception.Exception -> Bool) -> a -> IO ()
throws = throws_srcpos Nothing

throws_srcpos :: (Show a) => Misc.SrcPos -> (Exception.Exception -> Bool)
    -> a -> IO ()
throws_srcpos srcpos f val =
    (Exception.evaluate val >> failure srcpos ("didn't throw: " ++ show val))
    `Exception.catch` \exc ->
        if f exc
            then success srcpos ("caught exc: " ++ show exc)
            else failure srcpos ("exception didn't match: " ++ show exc)

exc_like :: String -> Exception.Exception -> Bool
exc_like expected exc = expected `List.isInfixOf` show exc

equal_srcpos :: (Show a, Eq a) => Misc.SrcPos -> a -> a -> IO ()
equal_srcpos srcpos a b
    | a == b = success srcpos $ "== " ++ show a
    | otherwise = failure srcpos $ show a ++ " /= " ++ show b

catch_srcpos :: Misc.SrcPos -> IO () -> IO ()
catch_srcpos srcpos op = Exception.catch op
    (\e -> failure srcpos ("test threw exception: " ++ show e))

-- IO oriented checks, the first value is pulled from IO.

io_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_equal = io_equal_srcpos Nothing

io_equal_srcpos :: (Eq a, Show a) => Misc.SrcPos -> IO a -> a -> IO ()
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
    getch
    result <- op
    putStr $ "  ... ok? "
    c <- getch
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

-- This goes before printed results when they are as expected.
success :: Misc.SrcPos -> String -> IO ()
success srcpos msg =
    hPrintf IO.stderr "++-> %s- %s\n" (Misc.show_srcpos srcpos) msg
failure :: Misc.SrcPos -> String -> IO ()
failure srcpos msg =
    hPrintf IO.stderr "**-> %s- %s\n" (Misc.show_srcpos srcpos) msg

-- getChar with no buffering
getch :: IO Char
getch = do
    IO.hFlush IO.stdout
    mode <- IO.hGetBuffering IO.stdin
    IO.hSetBuffering IO.stdin IO.NoBuffering
    do { c <- getChar; putChar ' '; return c}
        `Exception.finally` (IO.hSetBuffering IO.stdin mode)

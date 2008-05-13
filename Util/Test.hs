-- | Basic testing utilities.
module Util.Test where

import Prelude hiding (catch)
import qualified Control.Exception as Exception
import qualified System.IO as IO
import Text.Printf

import qualified Util.Misc as Misc


-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

check = check_srcpos Nothing
check_srcpos srcpos False = failure srcpos "assertion false"
check_srcpos srcpos True = success srcpos "assertion true"

equal :: (Show a, Eq a) => a -> a -> IO ()
equal = equal_srcpos Nothing

equal_srcpos :: (Show a, Eq a) => Misc.SrcPos -> a -> a -> IO ()
equal_srcpos srcpos a b
    | a == b = success srcpos $ "== " ++ show a
    | otherwise = failure srcpos $ show a ++ " /= " ++ show b

catch_srcpos :: Misc.SrcPos -> IO () -> IO ()
catch_srcpos srcpos op = Exception.catch op
    (\e -> failure srcpos ("threw exception: " ++ show e))

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
plist xs = mapM_ (\(i, x) -> putStr ("--" ++ show i ++ " ") >> print x)
    (zip [0..] xs)

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

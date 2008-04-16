-- | Basic testing utilities.
module Util.Test where

import Prelude hiding (catch)
import qualified Control.Exception as Exception
import qualified System.IO as IO
import Text.Printf


-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

type SrcPos = Maybe (String, Int) -- file, lineno
show_srcpos Nothing = ""
show_srcpos (Just (file, line)) = file ++ ":" ++ show line ++ " "

equal :: (Show a, Eq a) => a -> a -> IO ()
equal = equal_line Nothing

equal_line :: (Show a, Eq a) => SrcPos -> a -> a -> IO ()
equal_line srcpos a b
    | a == b = success srcpos $ "== " ++ show a
    | otherwise = failure srcpos $ show a ++ " /= " ++ show b

catch_line :: SrcPos -> IO () -> IO ()
catch_line srcpos op = Exception.catch op
    (\e -> failure srcpos ("threw exception: " ++ show e))

-- IO oriented checks, the first value is pulled from IO.

io_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_equal = io_equal_line Nothing

io_equal_line :: (Eq a, Show a) => SrcPos -> IO a -> a -> IO ()
io_equal_line srcpos io_val expected = do
    val <- io_val
    if val == expected
        then success srcpos ("== " ++ show val)
        else failure srcpos $
            "expected: " ++ show expected ++ ", got: " ++ show val

-- Only a human can check these things.
io_human expected_msg op = io_human_line Nothing
io_human_line srcpos expected_msg op = do
    putStrLn $ "should see: " ++ expected_msg
    getch
    op
    putStr $ "  ... ok? "
    c <- getch
    putChar '\n'
    if c /= 'y'
        then failure srcpos $ "didn't see " ++ expected_msg
        else success srcpos $ "saw " ++ expected_msg

-- * util

-- This goes before printed results when they are as expected.
success :: SrcPos -> String -> IO ()
success srcpos msg = hPrintf IO.stderr "++-> %s- %s\n" (show_srcpos srcpos) msg
failure :: SrcPos -> String -> IO ()
failure srcpos msg = hPrintf IO.stderr "**-> %s- %s\n" (show_srcpos srcpos) msg

-- getChar with no buffering
getch :: IO Char
getch = do
    IO.hFlush IO.stdout
    mode <- IO.hGetBuffering IO.stdin
    IO.hSetBuffering IO.stdin IO.NoBuffering
    do { c <- getChar; putChar ' '; return c}
        `Exception.finally` (IO.hSetBuffering IO.stdin mode)

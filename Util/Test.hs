-- | Basic testing utilities.
module Util.Test where

import Prelude hiding (catch)
import qualified Control.Exception as Exception
import qualified System.IO as IO
import Text.Printf


-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.


equal_line file line a b
    | a == b = good_result_line file line $ "== " ++ show a
    | otherwise = bad_result_line file line $ show a ++ " /= " ++ show b
equal a b
    | a == b = good_result a
    | otherwise = bad_result $ show a ++ " /= " ++ show b

catch_line :: String -> Int -> IO () -> IO ()
catch_line file line op = Exception.catch op
    (\e -> bad_result_line file line ("threw exception: " ++ show e))

-- This goes before printed results when they are as expected.
good_result val = putStr "++-> " >> print val
bad_result msg = putStr "**-> " >> putStrLn msg

good_result_line :: String -> Int -> String -> IO ()
good_result_line file line msg = printf "++-> %s:%d - %s\n" file line msg
bad_result_line :: String -> Int -> String -> IO ()
bad_result_line file line msg = printf "**-> %s:%d - %s\n" file line msg

-- IO oriented checks, the first value is pulled from IO.

io_check_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_check_equal io_val expected = do
    val <- io_val
    if val == expected
        then good_result val
        else error $ "expected: " ++ show expected ++ ", got: " ++ show val

-- Only a human can check these things.
io_human expected_msg op = do
    putStr $ "should see: " ++ expected_msg
    getch
    op
    putStr $ "  ... ok? "
    c <- getch
    if c /= 'y'
        then bad_result $ "didn't see " ++ expected_msg
        else return ()

io_human_line file line expected_msg op = do
    putStr $ "should see: " ++ expected_msg
    getch
    op
    putStr $ "  ... ok? "
    c <- getch
    if c /= 'y'
        then bad_result_line file line $ "didn't see " ++ expected_msg
        else return ()

-- getChar with no buffering
getch :: IO Char
getch = do
    IO.hFlush IO.stdout
    mode <- IO.hGetBuffering IO.stdin
    IO.hSetBuffering IO.stdin IO.NoBuffering
    do { c <- getChar; putChar ' '; return c}
        `Exception.finally` (IO.hSetBuffering IO.stdin mode)

-- | Basic testing utilities.
module Util.Test where

-- import qualified Control.Exception as Exception
import qualified System.IO as IO


-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

{-
assert = Exception.assert
assert_ :: Bool -> a
assert_ bool = assert bool undefined
-}

-- This goes before printed results when they are as expected.
good_result val = putStr "++-> " >> print val

check_equal a b
    | a == b = good_result a
    | otherwise = putStrLn $ "not equal: " ++ show a

-- IO oriented checks, the first value is pulled from IO.

io_check_equal :: (Eq a, Show a) => IO a -> a -> IO ()
io_check_equal io_val expected = do
    val <- io_val
    if val == expected
        then good_result val
        else error $ "expected: " ++ show expected ++ ", got: " ++ show val

-- Only a human can check these things.
io_human expected_msg op = do
    op
    putStr $ "should see: " ++ expected_msg
    IO.hFlush IO.stdout >> getLine

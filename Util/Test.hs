{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE BangPatterns #-}
-- | Basic testing utilities.
module Util.Test (
    skip_human
    -- * tests
    -- ** pure checks
    , check, check_srcpos, check_msg, check_msg_srcpos
    , equal, equal_srcpos
    , equalf, equalf_srcpos
    , strings_like, strings_like_srcpos
    , has_string, has_string_srcpos
    , check_right, check_right_srcpos
    , map_left, left_like, left_like_srcpos
    -- ** exception checks
    , throws, throws_srcpos, catch_srcpos

    -- ** io checks
    , io_equal, io_equal_srcpos
    , io_human, io_human_srcpos
    , pause

    , success, failure, success_srcpos, failure_srcpos

    -- * extracting
    , expect_right

    -- * profiling
    , timer, print_timer
    , force

    -- * pretty printing
    , printf
    , plist, pslist, pmlist
    , prettyp, PPrint.pprint

    -- * debugging
    , module Debug
) where
import Control.Applicative ((<$>))
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad

import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Time as Time

import qualified System.CPUTime as CPUTime
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Terminal as Terminal

import Text.Printf

import qualified Util.ApproxEq as ApproxEq
import Util.Debug as Debug
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos


-- | If this is True, skip through human-feedback tests.  That way I can at
-- least get the coverage and check for crashes, even if I can't verify the
-- results.
skip_human :: IORef.IORef Bool
skip_human = Unsafe.unsafePerformIO (IORef.newIORef False)

-- "Asserts" abort computation if they are false.  "Checks" print an unhappy
-- msg and keep going.

check = check_srcpos Nothing
check_srcpos srcpos False = failure_srcpos srcpos "assertion false"
check_srcpos srcpos True = success_srcpos srcpos "assertion true"

check_msg :: Bool -> String -> IO Bool
check_msg = check_msg_srcpos Nothing

check_msg_srcpos :: SrcPos.SrcPos -> Bool -> String -> IO Bool
check_msg_srcpos srcpos ok msg
    | ok = success_srcpos srcpos ("assertion true: " ++ msg)
    | otherwise = failure_srcpos srcpos ("assertion false: " ++ msg)

-- * equal and diff

equal :: (Show a, Eq a) => a -> a -> IO Bool
equal = equal_srcpos Nothing

equal_srcpos :: (Show a, Eq a) => SrcPos.SrcPos -> a -> a -> IO Bool
equal_srcpos srcpos a b
    | a == b = success_srcpos srcpos $ "== " ++ ellipse (show a)
    | otherwise = failure_srcpos srcpos msg
    where
    maxlen = 200
    ellipse s
        | len > maxlen = take maxlen s ++ "... {" ++ show len ++ "}"
        | otherwise = s
        where len = length s
    pa = Seq.strip $ PPrint.pshow a
    pb = Seq.strip $ PPrint.pshow b
    msg
        | Seq.count '\n' pa > 7 = diff_values pa pb
        | '\n' `elem` pa || '\n' `elem` pb || length pa + length pb >= 60 =
            "\n" ++ pa ++ "\n\t/=\n" ++ pb
        | otherwise = pa ++ " /= " ++ pb

diff_values :: String -> String -> String
diff_values pa pb =
    highlight_lines first_lines pa ++ "\n\t/=\n"
    ++ highlight_lines second_lines pb ++ "\ndiff:\n"
    ++ highlight_red diff_text
    where
    (first_lines, second_lines, diff_text) = diff pa pb

highlight_lines :: [Int] -> String -> String
highlight_lines nums = unlines . map hi . zip [0..] . lines
    where
    hi (i, line)
        | i `elem` nums = highlight_red line
        | otherwise = line

diff :: String -> String -> ([Int], [Int], String)
diff xs ys = (concatMap fnums diffs, concatMap snums diffs,
        unlines $ filter (not.null) $ map to_lines diffs)
    where
    fnums (Diff.F, nlines) = map num_of nlines
    fnums _ = []
    snums (Diff.S, nlines) = map num_of nlines
    snums _ = []
    to_lines (d, nlines) = case d of
        Diff.B -> ""
        Diff.F -> "\t---- " ++ show (num_of (head nlines)) ++ "\n"
            ++ unlines (map (('<':) . text_of) nlines)
        Diff.S -> "\t---- " ++ show (num_of (head nlines)) ++ "\n"
            ++ unlines (map (('>':) . text_of) nlines)
    num_of (NumberedLine (i, _)) = i
    text_of (NumberedLine (_, s)) = s
    diffs = Diff.getGroupedDiff (numbered (lines xs)) (numbered (lines ys))
    numbered = map NumberedLine . zip [0..]

-- | Numbered lines don't compare their numbers so the diff won't count
-- everytihng as different just because the line number changed.
newtype NumberedLine = NumberedLine (Int, String)
instance Eq NumberedLine where
    NumberedLine (_, s1) == NumberedLine (_, s2) = s1 == s2

-- * approximately equal

equalf :: (Show a, ApproxEq.ApproxEq a) => Double -> a -> a -> IO Bool
equalf = equalf_srcpos Nothing

equalf_srcpos :: (Show a, ApproxEq.ApproxEq a) => SrcPos.SrcPos -> Double
    -> a -> a -> IO Bool
equalf_srcpos srcpos eta a b
    | ApproxEq.approx_eq eta a b = success_srcpos srcpos $ "~~ " ++ show a
    | otherwise = failure_srcpos srcpos $ show a ++ " !~ " ++ show b

-- * other assertions

-- | Strings in the first list match regexes in the second list.
strings_like :: [String] -> [String] -> IO Bool
strings_like = strings_like_srcpos Nothing

strings_like_srcpos :: SrcPos.SrcPos -> [String] -> [String] -> IO Bool
strings_like_srcpos srcpos gotten expected
    | null gotten && null expected = success_srcpos srcpos "[] =~ []"
    | otherwise = foldl (&&) True <$>
        mapM string_like (Seq.padded_zip gotten expected)
    where
    string_like (Seq.Second reg) =
        failure_srcpos srcpos $ "gotten list too short: expected " ++ show reg
    string_like (Seq.First gotten) =
        failure_srcpos srcpos $ "expected list too short: got " ++ show gotten
    string_like (Seq.Both gotten reg)
        | pattern_matches reg gotten = success_srcpos srcpos $
            gotten ++ " =~ " ++ reg
        | otherwise = failure_srcpos srcpos $ gotten ++ " !~ " ++ reg

-- | The given list of strings contains the given pattern.  Useful to make sure
-- a certain message was logged.
has_string :: [String] -> String -> IO Bool
has_string = has_string_srcpos Nothing

has_string_srcpos :: SrcPos.SrcPos -> [String] -> String -> IO Bool
has_string_srcpos srcpos strings expected =
    case [str | (str, True) <- zip strings matches] of
        m : _ -> success_srcpos srcpos $
            quoted m ++ " =~ " ++ quoted expected
        [] -> failure_srcpos srcpos $
            show strings ++ " doesn't contain " ++ quoted expected
    where matches = map (pattern_matches expected) strings

quoted :: String -> String
quoted s = "'" ++ s ++ "'"

check_right :: (Show err) => Either err a -> IO Bool
check_right = check_right_srcpos Nothing

check_right_srcpos :: (Show err) => SrcPos.SrcPos -> Either err a -> IO Bool
check_right_srcpos srcpos (Left err) = failure_srcpos srcpos $
    "expected Right: Left " ++ show err
check_right_srcpos srcpos (Right _) = success_srcpos srcpos "Right"

map_left f (Left a) = Left (f a)
map_left _ (Right a) = Right a

left_like :: (Show a) => Either String a -> String -> IO Bool
left_like = left_like_srcpos Nothing

-- | It's common for Left to be an error msg, or be something that can be
-- converted to one.
left_like_srcpos :: (Show a) =>
    SrcPos.SrcPos -> Either String a -> String -> IO Bool
left_like_srcpos srcpos gotten expected = case gotten of
    Left msg
        | pattern_matches expected msg -> success_srcpos srcpos $
            "Left (" ++ msg ++ ") =~ Left (" ++ expected ++ ")"
        | otherwise -> failure_srcpos srcpos $
            "Left (" ++ msg ++ ") !~ Left (" ++ expected ++ ")"
    Right a -> failure_srcpos srcpos $
        "Right (" ++ show a ++ ") !~ " ++ expected

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
pattern_matches :: String -> String -> Bool
pattern_matches pattern s = not $ null $
    Regex.find_groups (pattern_to_reg pattern) s

pattern_to_reg :: String -> Regex.Regex
pattern_to_reg = Regex.make . mkstar . Regex.escape
    where
    mkstar "" = ""
    mkstar ('\\' : '\\' : '\\' : '*' : cs) = '\\' : '*' : mkstar cs
    mkstar ('\\' : '*' : cs) = '.' : '*' : '?' : mkstar cs
    mkstar (c : cs) = c : mkstar cs

-- | The given pure value should throw an exception that matches the predicate.
throws :: (Show a) => a -> String -> IO Bool
throws = throws_srcpos Nothing

throws_srcpos :: (Show a) => SrcPos.SrcPos -> a -> String -> IO Bool
throws_srcpos srcpos val exc_like =
    (Exception.evaluate val
        >> failure_srcpos srcpos ("didn't throw: " ++ show val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if exc_like `List.isInfixOf` show exc
            then success_srcpos srcpos ("caught exc: " ++ show exc)
            else failure_srcpos srcpos $
                "exception <" ++ show exc ++ "> didn't match " ++ show exc_like

catch_srcpos :: SrcPos.SrcPos -> IO a -> IO ()
catch_srcpos srcpos op = do
    result <- Exception.try op
    case result of
        Left (exc :: Exception.SomeException) ->
            void $ failure_srcpos srcpos ("test threw exception: " ++ show exc)
        Right _ -> return ()

-- IO oriented checks, the first value is pulled from IO.

io_equal :: (Eq a, Show a) => IO a -> a -> IO Bool
io_equal = io_equal_srcpos Nothing

io_equal_srcpos :: (Eq a, Show a) => SrcPos.SrcPos -> IO a -> a -> IO Bool
io_equal_srcpos srcpos io_val expected = do
    val <- io_val
    equal_srcpos srcpos val expected

-- Only a human can check these things.
io_human :: String -> IO a -> IO a
io_human = io_human_srcpos Nothing

io_human_srcpos :: SrcPos.SrcPos -> String -> IO a -> IO a
io_human_srcpos srcpos expected_msg op = do
    putStrLn $ "should see: " ++ expected_msg
    human_getch
    result <- op
    putStr "  ... ok (y/n/q)? "
    c <- human_getch
    putChar '\n'
    case c of
        'y' -> success_srcpos srcpos $ "saw " ++ show expected_msg
        'q' -> error "quit test"
        _ -> failure_srcpos srcpos $ "didn't see " ++ show expected_msg
    return result

pause :: IO ()
pause = do
    putStr "pausing, hit almost any key... "
    human_getch
    putStr "\n"


-- * unpacking

expect_right :: (Show a) => String -> Either a b -> b
expect_right msg (Left v) = error $ msg ++ ": " ++ show v
expect_right _ (Right v) = v


-- * profiling

-- | Run an action and report the time in CPU seconds.
timer :: IO a -> IO (a, Double)
timer op = do
    start_cpu <- CPUTime.getCPUTime
    !v <- op
    end_cpu <- CPUTime.getCPUTime
    return (v, cpu_to_sec (end_cpu - start_cpu))
    where
    cpu_to_sec :: Integer -> Double
    cpu_to_sec s = fromIntegral s / fromIntegral (10^12)

print_timer :: String -> (a -> String) -> IO a -> IO a
print_timer msg show_val op = do
    start <- now
    printf "%s - " msg
    IO.hFlush IO.stdout
    (val, secs) <- timer op
    end <- now
    printf "time: %.2fcpu / %.2fs - %s\n" secs (double (end-start))
        (show_val val)
    IO.hFlush IO.stdout
    return val
    where
    double :: Time.DiffTime -> Double
    double = realToFrac

now :: IO Time.DiffTime
now = fmap Time.utctDayTime Time.getCurrentTime

force :: (DeepSeq.NFData a) => a -> IO ()
force x = x `DeepSeq.deepseq` return ()

-- * util

-- | Print a list with newlines between its elements.
plist :: Show a => [a] -> IO ()
plist [] = putStrLn "[]"
plist xs = do
    mapM_ (\(i, x) -> putStr (show i ++ ": ") >> print x) (Seq.enumerate xs)
    putChar '\n'

pslist :: [String] -> IO ()
pslist [] = putStrLn "[]"
pslist xs = putStr $
    concatMap (\(i, x) -> printf "%02d. %s\n" i x) (Seq.enumerate xs)

pmlist :: (Show a) => String -> [a] -> IO ()
pmlist msg xs
    | null xs = return ()
    | otherwise = putStrLn (msg++":") >> plist xs

prettyp :: (Pretty.Pretty a) => a -> IO ()
prettyp val = s `DeepSeq.deepseq` putStr s -- ensure log tracing happens first
    where s = Pretty.formatted val


-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

success, failure :: String -> IO Bool
success = success_srcpos Nothing
failure = failure_srcpos Nothing

-- | Print a msg with a special tag indicating a passing test.
success_srcpos :: SrcPos.SrcPos -> String -> IO Bool
success_srcpos srcpos msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    hPrintf IO.stdout "++-> %s - %s\n" (SrcPos.show_srcpos srcpos) msg
    return True

-- | Print a msg with a special tag indicating a failing test.
failure_srcpos :: SrcPos.SrcPos -> String -> IO Bool
failure_srcpos srcpos msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    -- A little magic to make failures more obvious in tty output.
    isatty <- Terminal.queryTerminal IO.stdOutput
    putStrLn $ colorify isatty $ "__-> " ++ SrcPos.show_srcpos srcpos
        ++ " - " ++ msg
    return False
    where
    -- Highlight the line unless the text already has highlighting in it.
    colorify isatty text
        | isatty = if vt100_red `List.isInfixOf` text then text
            else highlight_red text
        | otherwise =
            Seq.replace vt100_red "" $ Seq.replace vt100_normal "" text

highlight_red :: String -> String
highlight_red text = vt100_red ++ text ++ vt100_normal

vt100_red :: String
vt100_red = "\ESC[31m"

vt100_normal :: String
vt100_normal = "\ESC[m\ESC[m"

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
                `Exception.finally` IO.hSetBuffering IO.stdin mode

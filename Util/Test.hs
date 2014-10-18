-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE BangPatterns #-}
-- | Basic testing utilities.
module Util.Test (
    skip_human
    -- * tests
    -- ** pure checks
    , check, check_srcpos
    , equal, equal_srcpos
    , equalf, equalf_srcpos
    , strings_like, strings_like_srcpos
    , has_string, has_string_srcpos
    , check_right, check_right_srcpos
    , map_left, left_like, left_like_srcpos
    , match, match_srcpos
    -- ** exception checks
    , throws, throws_srcpos

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
    , prettyp, pprint

    -- * filesystem
    , unique_tmp_dir, tmp_dir

    -- * debugging
    , module Debug
) where
import Control.Applicative ((<$>))
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception

import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.List as List
import Data.Monoid ((<>))

import qualified System.CPUTime as CPUTime
import qualified System.Directory as Directory
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
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
{-# NOINLINE skip_human #-}
skip_human :: IORef.IORef Bool
skip_human = Unsafe.unsafePerformIO (IORef.newIORef False)

check :: Bool -> IO Bool
check = check_srcpos Nothing

check_srcpos :: SrcPos.SrcPos -> Bool -> IO Bool
check_srcpos srcpos False = failure_srcpos srcpos "assertion false"
check_srcpos srcpos True = success_srcpos srcpos "assertion true"

-- * equal and diff

equal :: (Show a, Eq a) => a -> a -> IO Bool
equal = equal_srcpos Nothing

equal_srcpos :: (Show a, Eq a) => SrcPos.SrcPos -> a -> a -> IO Bool
equal_srcpos srcpos a b
    | a == b = success_srcpos srcpos $ pretty True
    | otherwise = failure_srcpos srcpos $ pretty False
    where pretty = pretty_compare "==" "/=" a b

-- | Show the values nicely, whether they are equal or not.
pretty_compare :: Show a => String -> String -> a -> a -> Bool -> String
pretty_compare equal inequal a b is_equal
    | is_equal = equal <> " " <> ellipse (show a)
    | Seq.count '\n' pa >= 5 = diff_values pa pb
    | '\n' `elem` pa || '\n' `elem` pb || length pa + length pb >= 60 =
        "\n" <> pa <> "\n\t" <> inequal <> "\n" <> pb
    | otherwise = pa <> " " <> inequal <> " " <> pb
    where
    maxlen = 200
    ellipse s
        | len > maxlen = take maxlen s ++ "... {" ++ show len ++ "}"
        | otherwise = s
        where len = length s
    pa = Seq.strip $ PPrint.pshow a
    pb = Seq.strip $ PPrint.pshow b

diff_values :: String -> String -> String
diff_values pa pb =
    '\n' : highlight_lines first_lines pa ++ "\n\t/=\n"
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
    fnums (Diff.First nlines) = map num_of nlines
    fnums _ = []
    snums (Diff.Second nlines) = map num_of nlines
    snums _ = []
    to_lines (Diff.Both {}) = ""
    to_lines (Diff.First nlines) =
        "\t---- " ++ show (num_of (head nlines)) ++ "\n"
            ++ unlines (map (('<':) . text_of) nlines)
    to_lines (Diff.Second nlines) =
        "\t---- " ++ show (num_of (head nlines)) ++ "\n"
            ++ unlines (map (('>':) . text_of) nlines)
    num_of (NumberedLine i _) = i
    text_of (NumberedLine _ s) = s
    diffs = Diff.getGroupedDiff (numbered (lines xs)) (numbered (lines ys))
    numbered = map (uncurry NumberedLine) . zip [0..]

-- | Numbered lines don't compare their numbers so the diff won't count
-- everytihng as different just because the line number changed.
data NumberedLine = NumberedLine Int String
instance Eq NumberedLine where
    NumberedLine _ s1 == NumberedLine _ s2 = s1 == s2

-- * approximately equal

equalf :: (Show a, ApproxEq.ApproxEq a) => Double -> a -> a -> IO Bool
equalf = equalf_srcpos Nothing

equalf_srcpos :: (Show a, ApproxEq.ApproxEq a) => SrcPos.SrcPos -> Double
    -> a -> a -> IO Bool
equalf_srcpos srcpos eta a b
    | ApproxEq.eq eta a b = success_srcpos srcpos $ pretty True
    | otherwise = failure_srcpos srcpos $ pretty False
    where pretty = pretty_compare "~~" "/~" a b

-- * other assertions

-- | Strings in the first list match regexes in the second list.
strings_like :: [String] -> [String] -> IO Bool
strings_like = strings_like_srcpos Nothing

strings_like_srcpos :: SrcPos.SrcPos -> [String] -> [String] -> IO Bool
strings_like_srcpos srcpos gotten expected
    | null gotten && null expected = success_srcpos srcpos "[] =~ []"
    | otherwise = foldl (&&) True <$>
        mapM string_like (zip [0..] (Seq.padded_zip gotten expected))
    where
    string_like (n, Seq.Second reg) = failure_srcpos srcpos $
        show n ++ ": gotten list too short: expected " ++ show reg
    string_like (n, Seq.First gotten) = failure_srcpos srcpos $
        show n ++ ": expected list too short: got " ++ show gotten
    string_like (n, Seq.Both gotten reg)
        | pattern_matches reg gotten = success_srcpos srcpos $
            show n ++ ": " ++ gotten ++ " =~ " ++ reg
        | otherwise = failure_srcpos srcpos $
            show n ++ ": " ++ gotten ++ " !~ " ++ reg

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
            "Left " ++ msg ++ " =~ Left " ++ expected
        | otherwise -> failure_srcpos srcpos $
            "Left " ++ msg ++ " !~ Left " ++ expected
    Right a -> failure_srcpos srcpos $
        "Right (" ++ show a ++ ") !~ Left " ++ expected

match :: String -> String -> IO Bool
match = match_srcpos Nothing

match_srcpos :: SrcPos.SrcPos -> String -> String -> IO Bool
match_srcpos srcpos gotten pattern
    | pattern_matches pattern gotten = success_srcpos srcpos $
        gotten ++ "\n\t=~\n" ++ pattern
    | otherwise = failure_srcpos srcpos $
        gotten ++ "\n\t!~\n" ++ pattern

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
pattern_matches :: String -> String -> Bool
pattern_matches pattern s = not $ null $
    Regex.find_groups (pattern_to_reg pattern) s

pattern_to_reg :: String -> Regex.Regex
pattern_to_reg = Regex.make_options [Regex.DotAll] . mkstar . Regex.escape
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

pause :: String -> IO ()
pause msg = do
    putStr $ "pausing, hit almost any key... "
        ++ if null msg then "" else " -- " ++ msg
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
    cpu_to_sec s = fromIntegral s / 10^12

print_timer :: String -> (Double -> a -> String) -> IO a -> IO a
print_timer msg show_val op = do
    printf "%s - " msg
    IO.hFlush IO.stdout
    (val, secs) <- timer $ do
        !val <- op
        -- let showed = show_val val
        -- force showed
        return val
    printf "time: %.2f - %s\n" secs (show_val secs val)
    IO.hFlush IO.stdout
    return val

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

pmlist :: Show a => String -> [a] -> IO ()
pmlist msg xs
    | null xs = return ()
    | otherwise = putStrLn (msg++":") >> plist xs

prettyp :: Pretty.Pretty a => a -> IO ()
prettyp val = s `DeepSeq.deepseq` putStr s -- ensure log tracing happens first
    where s = Pretty.formatted val

pprint :: Show a => a -> IO ()
pprint val = s `DeepSeq.deepseq` putStr s
    where s = PPrint.pshow val

-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

success, failure :: String -> IO Bool
success = success_srcpos Nothing
failure = failure_srcpos Nothing

-- | Print a msg with a special tag indicating a passing test.
success_srcpos :: SrcPos.SrcPos -> String -> IO Bool
success_srcpos srcpos msg = do
    print_test_line srcpos vt100_green "++-> " msg
    return True

-- | Print a msg with a special tag indicating a failing test.
failure_srcpos :: SrcPos.SrcPos -> String -> IO Bool
failure_srcpos srcpos msg = do
    print_test_line srcpos vt100_red "__-> " msg
    return False

print_test_line :: SrcPos.SrcPos -> String -> String -> String -> IO ()
print_test_line srcpos color_code prefix msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    -- A little magic to make failures more obvious in tty output.
    isatty <- Terminal.queryTerminal IO.stdOutput
    putStrLn $ highlight isatty color_code $ prefix
        ++ SrcPos.show_srcpos srcpos ++ " - " ++ msg

-- | Highlight the line unless the text already has highlighting in it.
highlight :: Bool -> String -> String -> String
highlight isatty code text
    | isatty = if code `List.isInfixOf` text then text
        else code ++ text ++ vt100_normal
    | otherwise = Seq.replace code "" $ Seq.replace vt100_normal "" text

highlight_red :: String -> String
highlight_red = (vt100_red++) . (++vt100_normal)

-- | These codes should probably come from termcap, but I can't be bothered.
vt100_red :: String
vt100_red = "\ESC[31m"

vt100_green :: String
vt100_green = "\ESC[32m"

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


-- * filesystem

-- | Get a tmp dir, which will be unique for each test run.
unique_tmp_dir :: String -> IO FilePath
unique_tmp_dir prefix = do
    Directory.createDirectoryIfMissing True "build/test/tmp"
    Temp.mkdtemp $ "build/test/tmp/" ++ prefix ++ "-"

-- | Get a tmp dir, which is the same on each test run.
tmp_dir :: String -> IO FilePath
tmp_dir name = do
    let dir = "build/test/tmp/" ++ name
    Directory.createDirectoryIfMissing True dir
    return dir

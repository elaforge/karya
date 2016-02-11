-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-} -- for pattern type sig in catch
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams, ConstraintKinds #-}
-- | Basic testing utilities.
module Util.Test (
    Config(..), modify_config, with_name
    -- * tests
    -- ** pure checks
    , check, equal, right_equal, equalf, strings_like
    , left_like , match
    -- ** exception checks
    , throws

    -- ** io checks
    , io_equal, io_human, pause

    -- ** low level
    , success, failure

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
    , unique_tmp_dir, tmp_dir, tmp_base_dir

    -- * debugging
    , module Debug
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified GHC.SrcLoc as SrcLoc
import qualified GHC.Stack as Stack

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
import qualified System.Posix.Terminal as Terminal

import Text.Printf

import qualified Util.ApproxEq as ApproxEq
import Util.Debug as Debug
import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq


{-# NOINLINE test_config #-}
test_config :: IORef.IORef Config
test_config = Unsafe.unsafePerformIO (IORef.newIORef (Config "no-test" False))

modify_config :: (Config -> Config) -> IO ()
modify_config = IORef.modifyIORef test_config

with_name :: String -> IO a -> IO a
with_name name action = do
    modify_config (\config -> config { config_test_name = name })
    action

data Config = Config {
    config_test_name :: !String
    -- | If True, skip through human-feedback tests.  That way I can at least
    -- get the coverage and check for crashes, even if I can't verify the
    -- results.
    , config_skip_human :: !Bool
    } deriving (Show)

check :: Stack => Bool -> IO Bool
check False = failure "assertion false"
check True = success "assertion true"

type Stack = (?stack :: Stack.CallStack)

-- * equal and diff

equal :: (Stack, Show a, Eq a) => a -> a -> IO Bool
equal a b
    | a == b = success $ pretty True
    | otherwise = failure $ pretty False
    where pretty = pretty_compare "==" "/=" a b

right_equal :: (Stack, Show err, Show a, Eq a) => Either err a -> a -> IO Bool
right_equal (Right a) b = equal a b
right_equal (Left err) _ = failure $ "Left: " <> PPrint.pshow err

-- | Show the values nicely, whether they are equal or not.
pretty_compare :: Show a =>
    String -- ^ equal operator
    -> String -- ^ inequal operator
    -> a -> a -> Bool -> String
pretty_compare equal inequal a b is_equal
    | is_equal = equal <> " " <> ellipse (show a)
    | big_values = '\n' : diff_values inequal pa pb
    | otherwise = pa <> " " <> inequal <> " " <> pb
    where
    -- If the values are a bit long, run the diff highlighter on them.
    big_values = '\n' `elem` pa || '\n' `elem` pb || length pa + length pb >= 60
    -- Equal values are usually not interesting, so abbreviate if they're too
    -- long.
    ellipse s
        | len > maxlen = take maxlen s ++ "... {" ++ show len ++ "}"
        | otherwise = s
        where len = length s
    maxlen = 200
    pa = Seq.strip $ PPrint.pshow a
    pb = Seq.strip $ PPrint.pshow b

-- | Diff two strings and highlight the different parts.
diff_values :: String -> String -> String -> String
diff_values inequal first second = concat
    [ Seq.strip $ highlight_lines firsts first
    , "\n\t" ++ inequal ++ "\n"
    , Seq.strip $ highlight_lines seconds second
    ]
    where (firsts, seconds) = diff first second

highlight_lines :: IntMap.IntMap [CharRange] -> String -> String
highlight_lines nums = unlines . zipWith hi [0..] . lines
    where
    hi i line = case IntMap.lookup i nums of
        Just ranges -> highlight_red_ranges ranges line
        Nothing -> line

highlight_red_ranges :: [CharRange] -> String -> String
highlight_red_ranges ranges text = concatMap hi (split_ranges ranges text)
    where hi (outside, inside) = outside ++ highlight_red inside

split_ranges :: [(Int, Int)] -> [a] -> [([a], [a])] -- ^ (out, in) pairs
split_ranges ranges = go 0 ranges
    where
    go _ _ [] = []
    go _ [] xs = [(xs, [])]
    go prev ((s, e) : ranges) xs = (pre, within) : go e ranges post
        where
        (pre, rest) = splitAt (s-prev) xs
        (within, post) = splitAt (e - s) rest

type CharRange = (Int, Int)

diff :: String -> String
    -> (IntMap.IntMap [CharRange], IntMap.IntMap [CharRange])
diff first second =
    to_map $ Seq.partition_paired $ map diff_line $
        Util.Map.pairs first_by_line second_by_line
    where
    to_map (as, bs) = (IntMap.fromList as, IntMap.fromList bs)
    diff_line (num, d) = case d of
        Seq.Both line1 line2 -> Seq.Both (num, d1) (num, d2)
            where (d1, d2) = char_diff line1 line2
        Seq.First line1 -> Seq.First (num, [(0, length line1)])
        Seq.Second line2 -> Seq.Second (num, [(0, length line2)])
    first_by_line = Map.fromList
        [(n, text) | Diff.First (Numbered n text) <- diffs]
    second_by_line = Map.fromList
        [(n, text) | Diff.Second (Numbered n text) <- diffs]
    diffs = numbered_diff (lines first) (lines second)

char_diff :: String -> String -> ([CharRange], [CharRange])
char_diff first second
    | too_different first_cs || too_different second_cs =
        ([(0, length first)], [(0, length second)])
    | otherwise = (first_cs, second_cs)
    where
    first_cs = to_ranges [n | Diff.First (Numbered n _) <- diffs]
    second_cs = to_ranges [n | Diff.Second (Numbered n _) <- diffs]
    diffs = numbered_diff first second
    -- If there are too many diff ranges let's just mark the whole thing
    -- different.  Perhaps I should ignore spaces that are the same, but let's
    -- see how this work first.
    too_different ranges = length ranges > 2

to_ranges :: [Int] -> [(Int, Int)]
to_ranges xs = Ranges.merge_sorted [(n, n+1) | n <- xs]

numbered_diff :: Eq a => [a] -> [a] -> [Diff.Diff (Numbered a)]
numbered_diff a b =
    Diff.getDiffBy (\a b -> numbered_val a == numbered_val b)
        (number a) (number b)
    where number = zipWith Numbered [0..]

data Numbered a = Numbered {
    _numbered :: !Int
    , numbered_val :: !a
    } deriving (Show)

-- * approximately equal

equalf :: (Stack, Show a, ApproxEq.ApproxEq a) => Double -> a -> a -> IO Bool
equalf eta a b
    | ApproxEq.eq eta a b = success $ pretty True
    | otherwise = failure $ pretty False
    where pretty = pretty_compare "~~" "/~" a b

-- * other assertions

-- | Strings in the first list match regexes in the second list.
strings_like :: Stack => [String] -> [String] -> IO Bool
strings_like gotten expected
    | null gotten && null expected = success "[] =~ []"
    | otherwise = and <$>
        mapM string_like (zip [0..] (Seq.zip_padded gotten expected))
    where
    string_like (n, Seq.Second reg) = failure $
        show n ++ ": gotten list too short: expected " ++ show reg
    string_like (n, Seq.First gotten) = failure $
        show n ++ ": expected list too short: got " ++ show gotten
    string_like (n, Seq.Both gotten reg)
        | pattern_matches reg gotten = success $
            show n ++ ": " ++ gotten ++ " =~ " ++ reg
        | otherwise = failure $ show n ++ ": " ++ gotten ++ " !~ " ++ reg

-- | It's common for Left to be an error msg, or be something that can be
-- converted to one.
left_like :: (Stack, Show a) => Either String a -> String -> IO Bool
left_like gotten expected = case gotten of
    Left msg
        | pattern_matches expected msg -> success $
            "Left " ++ msg ++ " =~ Left " ++ expected
        | otherwise -> failure $ "Left " ++ msg ++ " !~ Left " ++ expected
    Right a -> failure $ "Right (" ++ show a ++ ") !~ Left " ++ expected

match :: Stack => String -> String -> IO Bool
match gotten pattern
    | pattern_matches pattern gotten = success $
        gotten ++ "\n\t=~\n" ++ pattern
    | otherwise = failure $ gotten ++ "\n\t!~\n" ++ pattern

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
pattern_matches :: String -> String -> Bool
pattern_matches pattern =
    not . null . Regex.groups (pattern_to_regex pattern) . Text.pack

pattern_to_regex :: String -> Regex.Regex
pattern_to_regex =
    Regex.compileOptionsUnsafe "Test.pattern_to_regex" [Regex.DotAll] . mkstar
        . Regex.escape
    where
    mkstar "" = ""
    mkstar ('\\' : '\\' : '\\' : '*' : cs) = '\\' : '*' : mkstar cs
    mkstar ('\\' : '*' : cs) = '.' : '*' : '?' : mkstar cs
    mkstar (c : cs) = c : mkstar cs

-- | The given pure value should throw an exception that matches the predicate.
throws :: (Stack, Show a) => a -> String -> IO Bool
throws val exc_like =
    (Exception.evaluate val >> failure ("didn't throw: " ++ show val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if exc_like `List.isInfixOf` show exc
            then success ("caught exc: " ++ show exc)
            else failure $
                "exception <" ++ show exc ++ "> didn't match " ++ show exc_like

io_equal :: (Stack, Eq a, Show a) => IO a -> a -> IO Bool
io_equal io_val expected = do
    val <- io_val
    equal val expected

-- | Only a human can check these things.
io_human :: Stack => String -> IO a -> IO a
io_human expected_msg op = do
    putStrLn $ "should see: " ++ expected_msg
    human_get_char
    result <- op
    putStr "  ... ok (y/n/q)? "
    c <- human_get_char
    putChar '\n'
    case c of
        'y' -> success $ "saw " ++ show expected_msg
        'q' -> error "quit test"
        _ -> failure $ "didn't see " ++ show expected_msg
    return result

pause :: String -> IO ()
pause msg = do
    putStr $ "pausing, hit almost any key... "
        ++ if null msg then "" else " -- " ++ msg
    human_get_char
    putStr "\n"

expect_right :: (Stack, Show a) => String -> Either a b -> b
expect_right msg (Left v) =
    error $ show_stack msg ?stack ++ " - " ++ msg ++ ": " ++ show v
expect_right _ (Right v) = v


-- * profiling

-- | Run an action and report the time in CPU seconds and wall clock seconds.
timer :: IO a -> IO (a, Double, Double)
timer = Log.time_eval

print_timer :: String -> (Double -> Double -> a -> String) -> IO a -> IO a
print_timer msg show_val op = do
    printf "%s - " msg
    IO.hFlush IO.stdout
    (val, cpu_secs, secs) <- timer $ do
        !val <- op
        return val
    printf "time: %.2fs cpu %.2fs wall - %s\n" cpu_secs secs
        (show_val cpu_secs secs val)
    IO.hFlush IO.stdout
    return val

force :: DeepSeq.NFData a => a -> IO ()
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
prettyp val = s `DeepSeq.deepseq` Text.IO.putStr s
    -- deepseq to ensure log tracing happens first
    where s = Pretty.formatted val

pprint :: Show a => a -> IO ()
pprint val = s `DeepSeq.deepseq` putStr s
    where s = PPrint.pshow val

-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

-- | Print a msg with a special tag indicating a passing test.
success :: Stack => String -> IO Bool
success msg = do
    print_test_line ?stack vt100_green "++-> " msg
    return True

-- | Print a msg with a special tag indicating a failing test.
failure :: Stack => String -> IO Bool
failure msg = do
    print_test_line ?stack vt100_red "__-> " msg
    return False

print_test_line :: Stack.CallStack -> String -> String -> String -> IO ()
print_test_line stack color_code prefix msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    -- A little magic to make failures more obvious in tty output.
    isatty <- Terminal.queryTerminal IO.stdOutput
    test_name <- config_test_name <$> IORef.readIORef test_config
    putStrLn $ highlight isatty color_code $ prefix
        ++ show_stack test_name stack ++ " " ++ msg

show_stack :: String -> Stack.CallStack -> String
show_stack test_name =
    maybe "<empty-stack>" show_frame . Seq.last . Stack.getCallStack
    where
    show_frame (_, srcloc) =
        SrcLoc.srcLocFile srcloc ++ ":" ++ show (SrcLoc.srcLocStartLine srcloc)
        ++ " [" ++ test_name ++ "]"

-- | Highlight the line unless the text already has highlighting in it.
highlight :: Bool -> String -> String -> String
highlight isatty code text
    | isatty = if code `List.isInfixOf` text then text
        else code ++ text ++ vt100_normal
    | otherwise = Seq.replace code "" $ Seq.replace vt100_normal "" text

highlight_red :: String -> String
highlight_red text
    | null text = ""
    | otherwise = vt100_red ++ text ++ vt100_normal

-- | These codes should probably come from termcap, but I can't be bothered.
vt100_red :: String
vt100_red = "\ESC[31m"

vt100_green :: String
vt100_green = "\ESC[32m"

vt100_normal :: String
vt100_normal = "\ESC[m\ESC[m"

-- | getChar with no buffering.
human_get_char :: IO Char
human_get_char = do
    skip <- config_skip_human <$> IORef.readIORef test_config
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
    Directory.createDirectoryIfMissing True tmp_base_dir
    Temp.mkdtemp $ tmp_base_dir </> prefix ++ "-"

-- | Get a tmp dir, which is the same on each test run.
tmp_dir :: String -> IO FilePath
tmp_dir name = do
    let dir = tmp_base_dir </> name
    Directory.createDirectoryIfMissing True dir
    return dir

-- | All tmp files used by tests should go in this directory.
-- TODO instead of being hardcoded this should be configured per-project.
tmp_base_dir :: FilePath
tmp_base_dir = "build/test/tmp"

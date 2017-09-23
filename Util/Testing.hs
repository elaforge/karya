-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- | Basic testing utilities.
module Util.Testing (
    Config(..), modify_test_config, with_test_name
    -- * assertions
    , check, equal, equal_fmt, right_equal, not_equal, equalf, strings_like
    , left_like , match
    , Pattern
    -- ** exception assertions
    , throws

    -- ** io assertions
    , io_equal, io_human, pause

    -- ** low level
    , success, failure

    -- * extracting
    , expect_right

    -- * QuickCheck
    , quickcheck
    , q_equal

    -- * profiling
    , timer, print_timer
    , force

    -- * pretty printing
    , prettyp, pprint

    -- * filesystem
    , unique_tmp_dir, tmp_dir, tmp_base_dir
) where
import Control.Monad (unless)
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

#if GHC_VERSION < 80000
import qualified GHC.SrcLoc as Stack
#endif
import qualified GHC.Stack as Stack

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
import qualified System.Posix.Terminal as Terminal

import qualified Test.QuickCheck as QuickCheck
import qualified Text.Printf as Printf

import qualified Util.ApproxEq as ApproxEq
import Util.CallStack (Stack)
import qualified Util.CallStack as CallStack
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

modify_test_config :: (Config -> Config) -> IO ()
modify_test_config = IORef.modifyIORef test_config

with_test_name :: String -> IO a -> IO a
with_test_name name action = do
    modify_test_config (\config -> config { config_test_name = name })
    action

data Config = Config {
    config_test_name :: !String
    -- | If True, skip through human-feedback tests.  That way I can at least
    -- get the coverage and check for crashes, even if I can't verify the
    -- results.
    , config_skip_human :: !Bool
    } deriving (Show)

check :: Stack => Text -> Bool -> IO Bool
check msg False = failure ("failed: " <> msg)
check msg True = success msg

-- * equal and diff

equal :: (Stack, Show a, Eq a) => a -> a -> IO Bool
equal a b
    | a == b = success $ cmp True
    | otherwise = failure $ cmp False
    where cmp = pretty_compare "==" "/=" True a b

equal_fmt :: (Stack, Eq a, Show a) => (a -> Text) -> a -> a -> IO Bool
equal_fmt fmt a b = do
    ok <- equal a b
    let (pa, pb) = (fmt a, fmt b)
    unless (ok || Text.null pa && Text.null pb) $
        Text.IO.putStrLn $ show_diff pa pb
    return ok
    where
    show_diff pretty_a pretty_b = fmt_lines "/="
        (Text.lines $ highlight_lines color diff_a pretty_a)
        (Text.lines $ highlight_lines color diff_b pretty_b)
        where
        color = failure_color
        (diff_a, diff_b) = diff_ranges pretty_a pretty_b

not_equal :: (Stack, Show a, Eq a) => a -> a -> IO Bool
not_equal a b
    | a == b = failure $ cmp True
    | otherwise = success $ cmp False
    where cmp = pretty_compare "==" "/=" False a b

right_equal :: (Stack, Show err, Show a, Eq a) => Either err a -> a -> IO Bool
right_equal (Right a) b = equal a b
right_equal (Left err) _ = failure $ "Left: " <> pshowt err

-- | Show the values nicely, whether they are equal or not.
pretty_compare :: Show a =>
    Text -- ^ equal operator
    -> Text -- ^ inequal operator
    -> Bool -- ^ If True then equal is expected so inequal will be highlighted
    -- red.  Otherwise, inequal is expected and highlighted green.
    -> a -> a -> Bool -- ^ True if as are equal
    -> Text
pretty_compare equal inequal expect_equal a b is_equal
    | expect_equal == is_equal = equal <> " " <> ellipse (showt a)
    | otherwise = fmt_lines inequal
        (Text.lines $ highlight_lines color diff_a pretty_a)
        (Text.lines $ highlight_lines color diff_b pretty_b)
    where
    color = if expect_equal then failure_color else success_color
    (diff_a, diff_b) = diff_ranges pretty_a pretty_b
    pretty_a = Text.strip $ pshowt a
    pretty_b = Text.strip $ pshowt b
    -- Equal values are usually not interesting, so abbreviate if they're too
    -- long.
    ellipse s
        | len > maxlen = Text.take maxlen s <> "... {" <> showt len <> "}"
        | otherwise = s
        where len = Text.length s
    maxlen = 200

-- | Apply color ranges as produced by 'diff_ranges'.
highlight_lines :: ColorCode -> IntMap.IntMap [CharRange] -> Text -> Text
highlight_lines color nums = Text.unlines . zipWith hi [0..] . Text.lines
    where
    hi i line = case IntMap.lookup i nums of
        Just ranges -> highlight_ranges color ranges line
        Nothing -> line

highlight_ranges :: ColorCode -> [CharRange] -> Text -> Text
highlight_ranges color ranges = mconcat . map hi . split_ranges ranges
    where hi (outside, inside) = outside <> highlight color inside

split_ranges :: [(Int, Int)] -> Text -> [(Text, Text)] -- ^ (out, in) pairs
split_ranges ranges = go 0 ranges
    where
    go _ [] text
        | Text.null text = []
        | otherwise = [(text, mempty)]
    go prev ((s, e) : ranges) text = (pre, within) : go e ranges post
        where
        (pre, rest) = Text.splitAt (s-prev) text
        (within, post) = Text.splitAt (e - s) rest

type CharRange = (Int, Int)

diff_ranges :: Text -> Text
    -> (IntMap.IntMap [CharRange], IntMap.IntMap [CharRange])
diff_ranges first second =
    to_map $ Seq.partition_paired $ map diff_line $
        Util.Map.pairs first_by_line second_by_line
    where
    to_map (as, bs) = (IntMap.fromList as, IntMap.fromList bs)
    diff_line (num, d) = case d of
        Seq.Both line1 line2 -> Seq.Both (num, d1) (num, d2)
            where (d1, d2) = char_diff line1 line2
        Seq.First line1 -> Seq.First (num, [(0, Text.length line1)])
        Seq.Second line2 -> Seq.Second (num, [(0, Text.length line2)])
    first_by_line = Map.fromList
        [(n, text) | Diff.First (Numbered n text) <- diffs]
    second_by_line = Map.fromList
        [(n, text) | Diff.Second (Numbered n text) <- diffs]
    diffs = numbered_diff (==) (Text.lines first) (Text.lines second)

char_diff :: Text -> Text -> ([CharRange], [CharRange])
char_diff first second
    | too_different first_cs || too_different second_cs =
        ([(0, Text.length first)], [(0, Text.length second)])
    | otherwise = (first_cs, second_cs)
    where
    first_cs = to_ranges [n | Diff.First (Numbered n _) <- diffs]
    second_cs = to_ranges [n | Diff.Second (Numbered n _) <- diffs]
    diffs = numbered_diff (==) (Text.unpack first) (Text.unpack second)
    -- If there are too many diff ranges let's just mark the whole thing
    -- different.  Perhaps I should ignore spaces that are the same, but let's
    -- see how this work first.
    too_different ranges = length ranges > 2

to_ranges :: [Int] -> [(Int, Int)]
to_ranges xs = Ranges.merge_sorted [(n, n+1) | n <- xs]

numbered_diff :: (a -> a -> Bool) -> [a] -> [a] -> [Diff.Diff (Numbered a)]
numbered_diff equal a b =
    Diff.getDiffBy (\a b -> numbered_val a `equal` numbered_val b)
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
    where pretty = pretty_compare "~~" "/~" True a b

-- * other assertions

class Show a => TextLike a where to_text :: a -> Text
instance TextLike String where to_text = Text.pack
instance TextLike Text where to_text = id

-- | Strings in the first list match patterns in the second list, using
-- 'pattern_matches'.
strings_like :: forall txt. (Stack, TextLike txt) => [txt] -> [Pattern]
    -> IO Bool
strings_like gotten_ expected
    | all is_both diffs = success $ fmt_lines "=~" gotten expected
    | otherwise = failure $ fmt_lines "/~"
        (map (fmt_line (Set.fromList [_numbered a | Diff.Second a <- diffs]))
            (zip [0..] gotten))
        (map (fmt_line (Set.fromList [_numbered a | Diff.First a <- diffs]))
            (zip [0..] expected))
    where
    fmt_line failures (n, line)
        | Set.member n failures = highlight failure_color line
        | otherwise = line
    gotten = map to_text gotten_
    diffs = numbered_diff pattern_matches expected gotten
    is_both (Diff.Both {}) = True
    is_both _ = False

-- | Format multiple lines with an operator between them, on a single line if
-- they fit.
fmt_lines :: Text -> [Text] -> [Text] -> Text
fmt_lines operator [x] [y] | Text.length x + Text.length y <= 70 =
    x <> " " <> operator <> " " <> y
fmt_lines operator [] [y] = "<empty> " <> operator <> " " <> y
fmt_lines operator [x] [] = x <> " " <> operator <> " <empty>"
fmt_lines operator xs ys = ("\n"<>) $ Text.stripEnd $
    Text.unlines $ xs <> ["    " <> operator] <> ys

-- | It's common for Left to be an error msg, or be something that can be
-- converted to one.
left_like :: (Stack, Show a, TextLike txt) => Either txt a -> Pattern -> IO Bool
left_like gotten expected = case gotten of
    Left msg
        | pattern_matches expected msg -> success $
            "Left " <> to_text msg <> " =~ Left " <> to_text expected
        | otherwise ->
            failure $ "Left " <> to_text msg <> " !~ Left " <> to_text expected
    Right a ->
        failure $ "Right (" <> showt a <> ") !~ Left " <> to_text expected

match :: (Stack, TextLike txt) => txt -> Pattern -> IO Bool
match gotten pattern =
    (if matches then success else failure) $
        fmt_lines (if matches then "=~" else "!~")
            (Text.lines (to_text gotten)) (Text.lines pattern)
    where
    matches = pattern_matches pattern gotten

-- | Pattern as matched by 'pattern_matches'.
type Pattern = Text

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
pattern_matches :: TextLike txt => Pattern -> txt -> Bool
pattern_matches pattern = not . null . Regex.groups (pattern_to_regex pattern)
    . to_text

pattern_to_regex :: Stack => Text -> Regex.Regex
pattern_to_regex =
    Regex.compileOptionsUnsafe [Regex.DotAll] . mkstar . Regex.escape
        . Text.unpack
    where
    mkstar "" = ""
    mkstar ('\\' : '\\' : '\\' : '*' : cs) = '\\' : '*' : mkstar cs
    mkstar ('\\' : '*' : cs) = '.' : '*' : '?' : mkstar cs
    mkstar (c : cs) = c : mkstar cs

-- | The given pure value should throw an exception that matches the predicate.
throws :: (Stack, Show a) => a -> Pattern -> IO Bool
throws val exc_pattern =
    (Exception.evaluate val >> failure ("didn't throw: " <> showt val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if pattern_matches exc_pattern (showt exc)
            then success ("caught exc: " <> showt exc)
            else failure $ "exception <" <> showt exc <> "> didn't match "
                <> exc_pattern

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
        'y' -> success $ "saw " <> showt expected_msg
        'q' -> error "quit test"
        _ -> failure $ "didn't see " <> showt expected_msg
    return result

pause :: String -> IO ()
pause msg = do
    putStr $ "pausing, hit almost any key... "
        ++ if null msg then "" else " -- " ++ msg
    human_get_char
    putStr "\n"

expect_right :: (Stack, Show a) => Either a b -> b
expect_right (Left v) = CallStack.errorStack (showt v)
expect_right (Right v) = v

-- * QuickCheck

-- | Run a quickcheck property.
quickcheck :: (Stack, QuickCheck.Testable prop) => prop -> IO Bool
quickcheck prop = do
    result <- QuickCheck.quickCheckWithResult args prop
    case result of
        QuickCheck.Success { output = output } -> success (Text.pack output)
        QuickCheck.GaveUp { output = output } -> failure (Text.pack output)
        QuickCheck.Failure { output = output } -> failure (Text.pack output)
        QuickCheck.NoExpectedFailure { output = output } ->
            failure (Text.pack output)
        QuickCheck.InsufficientCoverage { output = output } ->
            failure (Text.pack output)
    where
    args = QuickCheck.stdArgs { QuickCheck.chatty = False }

-- | 'equal' for quickcheck.
q_equal :: (Show a, Eq a) => a -> a -> QuickCheck.Property
q_equal a b = QuickCheck.counterexample
    (Text.unpack $ pretty_compare "==" "/=" True a b False)
    (a == b)

-- * profiling

-- | Run an action and report the time in CPU seconds and wall clock seconds.
timer :: IO a -> IO (a, Double, Double)
timer = Log.time_eval

print_timer :: String -> (Double -> Double -> a -> String) -> IO a -> IO a
print_timer msg show_val op = do
    Printf.printf "%s - " msg
    IO.hFlush IO.stdout
    result <- Exception.try $ timer $ do
        !val <- op
        return val
    case result of
        Right (val, cpu_secs, secs) -> do
            Printf.printf "time: %.2fs cpu %.2fs wall - %s\n" cpu_secs secs
                (show_val cpu_secs secs val)
            return val
        Left (exc :: Exception.SomeException) -> do
            -- Complete the line so the exception doesn't interrupt it.  This
            -- is important if it's a 'failure' line!
            putStrLn $ "threw exception: " <> show exc
            Exception.throwIO exc

force :: DeepSeq.NFData a => a -> IO ()
force x = Exception.evaluate (DeepSeq.rnf x)

-- * util

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
success :: Stack => Text -> IO Bool
success msg = do
    print_test_line Stack.callStack success_color "++-> " msg
    return True

-- | Print a msg with a special tag indicating a failing test.
failure :: Stack => Text -> IO Bool
failure msg = do
    print_test_line Stack.callStack failure_color "__-> " msg
    return False

print_test_line :: Stack.CallStack -> ColorCode -> Text -> Text -> IO ()
print_test_line stack color prefix msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    isatty <- Terminal.queryTerminal IO.stdOutput
    test_name <- config_test_name <$> IORef.readIORef test_config
    -- If there is highlighting in the msg, then it's probably a diff so
    -- most of it may be unhighlighted.  So highlight the prefix to make
    -- the line visible.
    let full_prefix = (if is_highlighted msg then highlight color else id)
            (prefix <> show_stack test_name stack)
    let full_msg = full_prefix <> " " <> msg
        highlighted
            -- I only want colors in tty output.
            | not isatty = strip_colors full_msg
            -- Don't put on a color if it already has some.
            | is_highlighted full_msg = full_msg
            | otherwise = highlight color full_msg
    Text.IO.putStrLn highlighted
    where
    is_highlighted = (vt100_prefix `Text.isInfixOf`)

show_stack :: String -> Stack.CallStack -> Text
show_stack test_name =
    maybe "<empty-stack>" show_frame . Seq.last . Stack.getCallStack
    where
    show_frame (_, srcloc) =
        Text.pack (Stack.srcLocFile srcloc) <> ":"
        <> showt (Stack.srcLocStartLine srcloc)
        <> if null test_name then "" else " [" <> Text.pack test_name <> "]"

highlight :: ColorCode -> Text -> Text
highlight (ColorCode code) text
    | Text.null text = text
    | otherwise = code <> text <> vt100_normal

-- | Remove vt100 color codes.
strip_colors :: Text -> Text
strip_colors = mconcat . Seq.map_tail (Text.drop 1 . Text.dropWhile (/='m'))
    . Text.splitOn vt100_prefix

newtype ColorCode = ColorCode Text deriving (Show)

vt100_prefix :: Text
vt100_prefix = "\ESC["

vt100_normal :: Text
vt100_normal = "\ESC[m\ESC[m"

-- | These codes should probably come from termcap, but I can't be bothered.
failure_color :: ColorCode
failure_color = ColorCode "\ESC[31m" -- red

success_color :: ColorCode
success_color = ColorCode "\ESC[32m" -- green

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

showt :: Show a => a -> Text
showt = Text.pack . show

pshowt :: Show a => a -> Text
pshowt = Text.pack . PPrint.pshow

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

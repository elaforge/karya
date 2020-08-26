-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns, DisambiguateRecordFields #-}
-- | Basic testing utilities.
module Util.Test.Testing (
    Test
    , Config(..), modify_test_config, with_test_name
    -- * metadata
    , ModuleMeta(..), moduleMeta, Tag(..)
    -- * assertions
    , check, check_val
    , equal, equal_fmt, equal_on, right_equal, not_equal, equalf, strings_like
    , left_like, match
    , Pattern
    -- ** exception assertions
    , throws

    -- ** io assertions
    , io_equal, io_human, pause

    -- ** low level
    , success, failure

    -- * extracting
    , expect_right

    -- * hedgehog
    , hedgehog
    , property
    , (===), (/==)

    -- * QuickCheck
    , quickcheck
    , q_equal

    -- * pretty printing
    , pretty_compare
    , prettyp, pprint

    -- * filesystem
    , tmp_dir, in_tmp_dir, tmp_base_dir

    -- * util
    , force
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import Control.Monad (unless)

import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified GHC.Stack as Stack
import qualified Hedgehog
import Hedgehog ((===), (/==), property)
import qualified Hedgehog.Internal.Config as Internal.Config
import qualified Hedgehog.Internal.Property as Internal.Property
import qualified Hedgehog.Internal.Report as Report
import qualified Hedgehog.Internal.Runner as Internal.Runner
import qualified Hedgehog.Internal.Seed as Internal.Seed

import qualified System.Directory as Directory
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
import qualified System.Posix.Terminal as Terminal

import qualified Test.QuickCheck as QuickCheck

import qualified Util.CallStack as CallStack
import Util.CallStack (Stack)
import qualified Util.Maps as Maps
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Regex as Regex
import qualified Util.Seq as Seq
import qualified Util.Test.ApproxEq as ApproxEq


-- | The type of toplevel tests, which is the same as the type of individual
-- test functions.  It's just IO () for now, but some day I may be able to move
-- to something more specialized, so if tests have declared types it might as
-- well be one I can change in one place.
type Test = IO ()

{-# NOINLINE test_config #-}
test_config :: IORef.IORef Config
test_config = Unsafe.unsafePerformIO $ IORef.newIORef $ Config
    { config_test_name = "no-test"
    , config_human_agreeable = True
    }

modify_test_config :: (Config -> Config) -> IO ()
modify_test_config = IORef.modifyIORef test_config

-- | Set 'configTestName'.  This is a grody hack, but I need it because GHC
-- call stack is off by one, so you get the caller line number, but the
-- callee's function name: https://ghc.haskell.org/trac/ghc/ticket/11686
with_test_name :: Text -> IO a -> IO a
with_test_name name action = do
    modify_test_config (\config -> config { config_test_name = name })
    action

data Config = Config {
    -- | Keep the test name so I can report it in 'success' in 'failure'.
    config_test_name :: !Text
    -- | If True, 'io_human' will always return True.  That way I can at least
    -- get the coverage and check for crashes, even if I can't verify the
    -- results.
    , config_human_agreeable :: !Bool
    } deriving (Show)

check :: Stack => Text -> Bool -> Test
check msg False = failure ("failed: " <> msg)
check msg True = success msg

-- | Check against a function.  Use like:
--
-- > check_val (f x) $ \case -> ...
check_val :: Show a => Stack => a -> (a -> Bool) -> Test
check_val val f
    | f val = success $ "ok: " <> pshowt val
    | otherwise = failure $ "failed: " <> pshowt val

-- * metadata

data ModuleMeta = ModuleMeta {
    -- | Wrap each test with IO level setup and teardown.  Sync exceptions are
    -- caught from the test function, so this should only see async exceptions.
    initialize :: IO () -> IO ()
    , tags :: [Tag]
    }

moduleMeta :: ModuleMeta
moduleMeta = ModuleMeta
    { initialize = id
    , tags = []
    }

data Tag =
    -- | Especially expensive to run.
    Large
    -- | Wants to have a conversation.  This implies the tests must be
    -- serialized, since who wants to have a conversation in parallel.
    -- 'io_human' is one way to do this.
    | Interactive
    deriving (Eq, Show)

-- * equal and diff

equal :: (Stack, Show a, Eq a) => a -> a -> Test
equal a b = equal_ a b >> return ()

equal_ :: (Stack, Show a, Eq a) => a -> a -> IO Bool
equal_ a b
    | a == b = success (cmp True) >> return True
    | otherwise = failure (cmp False) >> return False
    where cmp = pretty_compare "==" "/=" True a b

equal_fmt :: (Stack, Eq a, Show a) => (a -> Text) -> a -> a -> Test
equal_fmt fmt a b = do
    ok <- equal_ a b
    let (pa, pb) = (fmt a, fmt b)
    unless (ok || Text.null pa && Text.null pb) $
        Text.IO.putStrLn $ show_diff pa pb
    where
    show_diff pretty_a pretty_b = fmt_lines "/="
        (Text.lines $ highlight_lines color diff_a pretty_a)
        (Text.lines $ highlight_lines color diff_b pretty_b)
        where
        color = failure_color
        (diff_a, diff_b) = diff_ranges pretty_a pretty_b

-- | Assert these things are equal after applying a function.  Print without
-- the function if they're not equal.  This is for cases when the extract
-- function loses information it would be nice to see if the test fails.
equal_on :: (Stack, Eq b, Show a, Show b) => (a -> b) -> a -> b -> Test
equal_on f a b = do
    ok <- equal_ (f a) b
    unless ok $ Text.IO.putStrLn (pshowt a)

not_equal :: (Stack, Show a, Eq a) => a -> a -> Test
not_equal a b
    | a == b = failure $ cmp True
    | otherwise = success $ cmp False
    where cmp = pretty_compare "==" "/=" False a b

right_equal :: (Stack, Show err, Show a, Eq a) => Either err a -> a -> Test
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
    | is_equal && expect_equal = equal <> " " <> ellipse (showt a)
    | otherwise = fmt_lines inequal
        (Text.lines $ highlight_lines color diff_a pretty_a)
        (Text.lines $ highlight_lines color diff_b pretty_b)
    where
    color = if expect_equal then failure_color else success_color
    (diff_a, diff_b) = diff_ranges pretty_a pretty_b
    pretty_a = pshowt a
    pretty_b = pshowt b
    -- Expected results are usually not interesting, so abbreviate if they're
    -- too long.
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
split_ranges = go 0
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
        Maps.pairs first_by_line second_by_line
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
    numbered :: !Int
    , numbered_val :: !a
    } deriving (Show)

-- * approximately equal

equalf :: (Stack, Show a, ApproxEq.ApproxEq a) => Double -> a -> a -> Test
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
strings_like :: forall txt. (Stack, TextLike txt) => [txt] -> [Pattern] -> Test
strings_like gotten_ expected
    | all is_both diffs = success $ fmt_lines "=~" gotten expected
    | otherwise = failure $ fmt_lines "/~"
        (map (fmt_line (Set.fromList [numbered a | Diff.Second a <- diffs]))
            (zip [0..] gotten))
        (map (fmt_line (Set.fromList [numbered a | Diff.First a <- diffs]))
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
left_like :: (Stack, Show a, TextLike txt) => Either txt a -> Pattern -> Test
left_like gotten expected = case gotten of
    Left msg
        | pattern_matches expected msg -> success $
            "Left " <> to_text msg <> " =~ Left " <> to_text expected
        | otherwise ->
            failure $ "Left " <> to_text msg <> " !~ Left " <> to_text expected
    Right a ->
        failure $ "Right (" <> showt a <> ") !~ Left " <> to_text expected

match :: (Stack, TextLike txt) => txt -> Pattern -> Test
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
throws :: (Stack, Show a) => a -> Pattern -> Test
throws val exc_pattern =
    (Exception.evaluate val >> failure ("didn't throw: " <> showt val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if pattern_matches exc_pattern (showt exc)
            then success ("caught exc: " <> showt exc)
            else failure $ "exception <" <> showt exc <> "> didn't match "
                <> exc_pattern

io_equal :: (Stack, Eq a, Show a) => IO a -> a -> Test
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

-- * hedgehog

hedgehog :: Hedgehog.Property -> Test
hedgehog prop = do
    seed <- Internal.Seed.random
    report <- Internal.Runner.checkReport config size seed
        (Internal.Property.propertyTest prop) updateUI
    (ok, msg) <- format_hedgehog_report report
    (if ok then success else failure) msg
    where
    -- TODO if it's interactive, give a progress report?
    updateUI _progress = return ()
    config = Internal.Property.propertyConfig prop
    size = 0

format_hedgehog_report :: Report.Report Report.Result -> IO (Bool, Text)
format_hedgehog_report report = do
    name <- Text.unpack . config_test_name <$> IORef.readIORef test_config
    str <- Report.renderResult Internal.Config.EnableColor
        (Just (Internal.Property.PropertyName name)) report
    return $ (, Text.pack str) $ case Report.reportStatus report of
        Report.OK -> True
        Report.GaveUp -> False
        Report.Failed {} -> False

-- * QuickCheck

-- | Run a quickcheck property.
quickcheck :: (Stack, QuickCheck.Testable prop) => prop -> Test
quickcheck prop = do
    (ok, msg) <- format_quickcheck_result <$>
        QuickCheck.quickCheckWithResult args prop
    (if ok then success else failure) msg
    where
    args = QuickCheck.stdArgs { QuickCheck.chatty = False }

format_quickcheck_result :: QuickCheck.Result -> (Bool, Text)
format_quickcheck_result result = fmap Text.strip $ case result of
    QuickCheck.Success { output } -> (True, Text.pack output)
    QuickCheck.GaveUp { output } -> (False, Text.pack output)
    QuickCheck.Failure { output } -> (False, Text.pack output)
    QuickCheck.NoExpectedFailure { output } -> (False, Text.pack output)

-- | 'equal' for quickcheck.
q_equal :: (Show a, Eq a) => a -> a -> QuickCheck.Property
q_equal a b = QuickCheck.counterexample
    (Text.unpack $ pretty_compare "==" "/=" True a b False)
    (a == b)

-- * util

-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

-- | Print a msg with a special tag indicating a passing test.
success :: Stack => Text -> Test
success msg = print_test_line Stack.callStack success_color "++-> " msg

-- | Print a msg with a special tag indicating a failing test.
failure :: Stack => Text -> Test
failure msg = print_test_line Stack.callStack failure_color "__-> " msg

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

show_stack :: Text -> Stack.CallStack -> Text
show_stack test_name =
    maybe "<empty-stack>" show_frame . Seq.last . Stack.getCallStack
    where
    show_frame (_, srcloc) =
        Text.pack (Stack.srcLocFile srcloc) <> ":"
        <> showt (Stack.srcLocStartLine srcloc)
        <> if Text.null test_name then "" else " [" <> test_name <> "]"

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
    agreeable <- config_human_agreeable <$> IORef.readIORef test_config
    if agreeable
        then return 'y'
        else do
            IO.hFlush IO.stdout
            mode <- IO.hGetBuffering IO.stdin
            IO.hSetBuffering IO.stdin IO.NoBuffering
            do { c <- getChar; putChar ' '; return c }
                `Exception.finally` IO.hSetBuffering IO.stdin mode

-- * pretty

prettyp :: Pretty.Pretty a => a -> IO ()
prettyp val = s `DeepSeq.deepseq` Text.IO.putStr s
    -- deepseq to ensure log tracing happens first
    where s = Pretty.formatted val

pprint :: Show a => a -> IO ()
pprint = putStrLn . pshow

showt :: Show a => a -> Text
showt = Text.pack . show

pshowt :: Show a => a -> Text
pshowt = Text.strip . Text.pack . pshow

-- | Strict pshow, so I don't get debug traces interleaved with printing.
pshow :: Show a => a -> String
pshow val = s `DeepSeq.deepseq` s
    where s = Seq.rstrip $ PPrint.format_str (show val)

-- * filesystem

-- | Get a tmp dir, which will be unique for each test run.
tmp_dir :: String -> IO FilePath
tmp_dir prefix = do
    Directory.createDirectoryIfMissing True tmp_base_dir
    dir <- Temp.mkdtemp $ tmp_base_dir </> prefix ++ "-"
    putStrLn $ "** tmp dir: " ++ dir
    return dir

-- | Run the computation with cwd in a new tmp dir.
in_tmp_dir :: String -> IO a -> IO a
in_tmp_dir prefix action = do
    dir <- tmp_dir prefix
    Directory.withCurrentDirectory dir action

-- | All tmp files used by tests should go in this directory.
-- TODO instead of being hardcoded this should be configured per-project.
tmp_base_dir :: FilePath
tmp_base_dir = "build/test/tmp"

-- * util

force :: DeepSeq.NFData a => a -> IO ()
force x = Exception.evaluate (DeepSeq.rnf x)

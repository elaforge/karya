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
    Test, Profile
    , Config(..), modifyTestConfig, withTestName
    -- * metadata
    , ModuleMeta(..), moduleMeta, Tag(..)
    -- * assertions
    , check, checkVal
    , equal, equalFmt, equalOn, rightEqual, notEqual, equalf, stringsLike
    , leftLike, match
    , Pattern
    -- ** exception assertions
    , throws

    -- ** io assertions
    , ioEqual, ioHuman, pause
    , io

    -- ** low level
    , success, failure

    -- * extracting
    , expectRight

    -- * hedgehog
    , hedgehog
    , property
    , (===), (/==)

    -- * QuickCheck
    , quickcheck
    , qEqual

    -- * pretty printing
    , prettyCompare
    , prettyp, pprint

    -- * filesystem
    , tmpDir, inTmpDir, tmpBaseDir

    -- * util
    , force
) where
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import           Control.Monad (unless)

import qualified Data.Algorithm.Diff as Diff
import qualified Data.IORef as IORef
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified GHC.Stack as Stack
import           GHC.Stack (HasCallStack)
import qualified Hedgehog
import           Hedgehog (property, (/==), (===))
import qualified Hedgehog.Internal.Config as Internal.Config
import qualified Hedgehog.Internal.Property as Internal.Property
import qualified Hedgehog.Internal.Report as Report
import qualified Hedgehog.Internal.Runner as Internal.Runner
import qualified Hedgehog.Internal.Seed as Internal.Seed

import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Posix.IO as IO
import qualified System.Posix.Temp as Temp
import qualified System.Posix.Terminal as Terminal

import qualified Test.QuickCheck as QuickCheck

import qualified Util.Diffs as Diffs
import qualified Util.Lists as Lists
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Regex as Regex
import qualified Util.Strings as Strings
import qualified Util.Test.ApproxEq as ApproxEq


-- | The type of toplevel tests, which is the same as the type of individual
-- test functions.  It's just IO () for now, but some day I may be able to move
-- to something more specialized, so if tests have declared types it might as
-- well be one I can change in one place.
type Test = IO ()

-- | Profiles are just like tests, but I'll use a different type just for
-- documentation at least.  The real determination is that profiles start with
-- "profile_" instead of "test_".
type Profile = IO ()

{-# NOINLINE test_config #-}
test_config :: IORef.IORef Config
test_config = Unsafe.unsafePerformIO $ IORef.newIORef $ Config
    { configTestName = "no-test"
    , configHumanAgreeable = True
    }

modifyTestConfig :: (Config -> Config) -> IO ()
modifyTestConfig = IORef.modifyIORef test_config

-- | Set 'config_test_name'.  This is a grody hack, but I need it because GHC
-- call stack is off by one, so you get the caller line number, but the
-- callee's function name: https://ghc.haskell.org/trac/ghc/ticket/11686
withTestName :: Text -> IO a -> IO a
withTestName name action = do
    modifyTestConfig (\config -> config { configTestName = name })
    action

data Config = Config
    -- | Keep the test name so I can report it in 'success' in 'failure'.
    { configTestName :: !Text
    -- | If True, 'io_human' will always return True.  That way I can at least
    -- get the coverage and check for crashes, even if I can't verify the
    -- results.
    , configHumanAgreeable :: !Bool
    } deriving (Show)

check :: HasCallStack => Text -> Bool -> Test
check msg False = failure ("failed: " <> msg)
check msg True = success msg

-- | Check against a function.  Use like:
--
-- > check_val (f x) $ \case -> ...
checkVal :: (Show a, HasCallStack) => a -> (a -> Bool) -> Test
checkVal val f
    | f val = success $ "ok: " <> pshowt val
    | otherwise = failure $ "failed: " <> pshowt val

-- * metadata

data ModuleMeta = ModuleMeta
    -- | Wrap each test with IO level setup and teardown.  Sync exceptions are
    -- caught from the test function, so this should only see async exceptions.
    { initialize :: IO () -> IO ()
    , tags :: [Tag]
    }

moduleMeta :: ModuleMeta
moduleMeta = ModuleMeta
    { initialize = id
    , tags = []
    }

data Tag
    -- | Especially expensive to run.
    = Large
    -- | Wants to have a conversation.  This implies the tests must be
    -- serialized, since who wants to have a conversation in parallel.
    -- 'io_human' is one way to do this.
    | Interactive
    deriving (Eq, Show)

-- * equal and diff

equal :: (HasCallStack, Show a, Eq a) => a -> a -> Test
equal a b = equal_ a b >> pure ()

equal_ :: (HasCallStack, Show a, Eq a) => a -> a -> IO Bool
equal_ a b
    | a == b = success (cmp True) >> pure True
    | otherwise = failure (cmp False) >> pure False
    where cmp = prettyCompare "==" "/=" True a b

notEqual :: (HasCallStack, Show a, Eq a) => a -> a -> Test
notEqual a b
    | a == b = failure $ cmp True
    | otherwise = success $ cmp False
    where cmp = prettyCompare "==" "/=" False a b

-- | Like equal, but show the inequality with a fmt function.
equalFmt :: (HasCallStack, Eq a, Show a) => (a -> Text) -> a -> a -> Test
equalFmt fmt a b = do
    ok <- equal_ a b
    let (pa, pb) = (fmt a, fmt b)
    unless (ok || Text.null pa && Text.null pb) $
        Text.IO.putStrLn $ showDiff pa pb
    where
    showDiff prettyA prettyB = fmtLines "/="
        (highlightLines color diffA $ Text.lines prettyA)
        (highlightLines color diffB $ Text.lines prettyB)
        where
        color = failureColor
        (diffA, diffB) = Diffs.ranges prettyA prettyB

-- | Assert these things are equal after applying a function.  Print without
-- the function if they're not equal.  This is for cases when the extract
-- function loses information it would be nice to see if the test fails.
equalOn :: (HasCallStack, Eq b, Show a, Show b) => (a -> b) -> a -> b -> Test
equalOn f a b = do
    ok <- equal_ (f a) b
    unless ok $ Text.IO.putStrLn (pshowt a)

rightEqual :: (HasCallStack, Show err, Show a, Eq a) => Either err a -> a
    -> Test
rightEqual (Right a) b = equal a b
rightEqual (Left err) _ = failure $ "Left: " <> pshowt err

-- | Show the values nicely, whether they are equal or not.
prettyCompare :: Show a =>
    Text -- ^ equal operator
    -> Text -- ^ inequal operator
    -> Bool -- ^ If True then equal is expected so inequal will be highlighted
    -- red.  Otherwise, inequal is expected and highlighted green.
    -> a -> a -> Bool -- ^ True if as are equal
    -> Text
prettyCompare equal inequal expectEqual a b isEqual
    | isEqual && expectEqual = equal <> " " <> ellipse (showt a)
    | otherwise = fmtLines inequal
        (highlightLines color diffA $ Text.lines prettyA)
        (highlightLines color diffB $ Text.lines prettyB)
    where
    color = if expectEqual then failureColor else successColor
    (diffA, diffB) = Diffs.ranges prettyA prettyB
    prettyA = pshowt a
    prettyB = pshowt b
    -- Expected results are usually not interesting, so abbreviate if they're
    -- too long.
    ellipse s
        | len > maxlen = Text.take maxlen s <> "... {" <> showt len <> "}"
        | otherwise = s
        where len = Text.length s
    maxlen = 400

highlightLines :: ColorCode -> IntMap.IntMap [Diffs.CharRange] -> [Text]
    -> [Text]
highlightLines (ColorCode color) =
    Diffs.highlightLines (Diffs.ColorCode color)

-- * approximately equal

equalf :: (HasCallStack, Show a, ApproxEq.ApproxEq a) => Double -> a -> a
    -> Test
equalf eta a b
    | ApproxEq.eq eta a b = success $ pretty True
    | otherwise = failure $ pretty False
    where pretty = prettyCompare "~~" "/~" True a b

-- * other assertions

class Show a => TextLike a where toText :: a -> Text
instance TextLike String where toText = Text.pack
instance TextLike Text where toText = id

-- | Strings in the first list match patterns in the second list, using
-- 'pattern_matches'.
stringsLike :: forall txt. (HasCallStack, TextLike txt) => [txt] -> [Pattern]
    -> Test
stringsLike gotten_ expected
    | all isBoth diffs = success $ fmtLines "=~" gotten expected
    | otherwise = failure $ fmtLines "/~"
        (map (fmtLine
                (Set.fromList [Diffs.numbered a | Diff.Second a <- diffs]))
            (zip [0..] gotten))
        (map (fmtLine
                (Set.fromList [Diffs.numbered a | Diff.First a <- diffs]))
            (zip [0..] expected))
    where
    fmtLine failures (n, line)
        | Set.member n failures = highlight failureColor line
        | otherwise = line
    gotten = map toText gotten_
    diffs = Diffs.numberedDiff patternMatches expected gotten
    isBoth (Diff.Both {}) = True
    isBoth _ = False

-- | Format multiple lines with an operator between them, on a single line if
-- they fit.
fmtLines :: Text -> [Text] -> [Text] -> Text
fmtLines operator [x] [y] | Text.length x + Text.length y <= 70 =
    x <> " " <> operator <> " " <> y
fmtLines operator [] [y] = "<empty> " <> operator <> " " <> y
fmtLines operator [x] [] = x <> " " <> operator <> " <empty>"
fmtLines operator xs ys = ("\n"<>) $ Text.stripEnd $
    Text.unlines $ xs <> ["    " <> operator] <> ys

-- | It's common for Left to be an error msg, or be something that can be
-- converted to one.
leftLike :: (HasCallStack, Show a, TextLike txt) => Either txt a -> Pattern
    -> Test
leftLike gotten expected = case gotten of
    Left msg
        | patternMatches expected msg -> success $
            "Left " <> toText msg <> " =~ Left " <> toText expected
        | otherwise ->
            failure $ "Left " <> toText msg <> " !~ Left " <> toText expected
    Right a ->
        failure $ "Right (" <> showt a <> ") !~ Left " <> toText expected

match :: (HasCallStack, TextLike txt) => txt -> Pattern -> Test
match gotten pattern =
    (if matches then success else failure) $
        fmtLines (if matches then "=~" else "!~")
            (Text.lines (toText gotten)) (Text.lines pattern)
    where
    matches = patternMatches pattern gotten

-- | Pattern as matched by 'pattern_matches'.
type Pattern = Text

-- | This is a simplified pattern that only has the @*@ operator, which is
-- equivalent to regex's @.*?@.  This reduces the amount of quoting you have
-- to write.  You can escape @*@ with a backslash.
patternMatches :: TextLike txt => Pattern -> txt -> Bool
patternMatches pattern = not . null . Regex.groups (patternToRegex pattern)
    . toText

patternToRegex :: HasCallStack => Text -> Regex.Regex
patternToRegex =
    Regex.compileOptionsUnsafe [Regex.DotAll] . mkstar . Regex.escape
        . Text.unpack
    where
    mkstar "" = ""
    mkstar ('\\' : '\\' : '\\' : '*' : cs) = '\\' : '*' : mkstar cs
    mkstar ('\\' : '*' : cs) = '.' : '*' : '?' : mkstar cs
    mkstar (c : cs) = c : mkstar cs

-- | The given pure value should throw an exception that matches the predicate.
throws :: (HasCallStack, Show a) => a -> Pattern -> Test
throws val excPattern =
    (Exception.evaluate val >> failure ("didn't throw: " <> showt val))
    `Exception.catch` \(exc :: Exception.SomeException) ->
        if patternMatches excPattern (showt exc)
            then success ("caught exc: " <> showt exc)
            else failure $ "exception <" <> showt exc <> "> didn't match "
                <> excPattern

ioEqual :: (HasCallStack, Eq a, Show a) => IO a -> a -> Test
ioEqual ioVal expected = do
    val <- ioVal
    equal val expected

io :: (a -> b -> IO c) -> IO a -> b -> IO c
io op ioVal expected = do
    val <- ioVal
    op val expected

-- | Only a human can check these things.
ioHuman :: HasCallStack => String -> IO a -> IO a
ioHuman expectedMsg op = do
    putStrLn $ "should see: " ++ expectedMsg
    humanGetChar
    result <- op
    putStr "  ... ok (y/n/q)? "
    c <- humanGetChar
    putChar '\n'
    case c of
        'y' -> success $ "saw " <> showt expectedMsg
        'q' -> error "quit test"
        _ -> failure $ "didn't see " <> showt expectedMsg
    pure result

pause :: String -> IO ()
pause msg = do
    putStr $ "pausing, hit almost any key... "
        ++ if null msg then "" else " -- " ++ msg
    humanGetChar
    putStr "\n"

expectRight :: (HasCallStack, Show a) => Either a b -> b
expectRight (Left v) = error (show v)
expectRight (Right v) = v

-- * hedgehog

hedgehog :: Hedgehog.Property -> Test
hedgehog prop = do
    seed <- Internal.Seed.random
    report <- Internal.Runner.checkReport config size seed
        (Internal.Property.propertyTest prop) updateUI
    (ok, msg) <- formatHedgehogReport report
    (if ok then success else failure) msg
    where
    -- TODO if it's interactive, give a progress report?
    updateUI _progress = pure ()
    config = Internal.Property.propertyConfig prop
    size = 0

formatHedgehogReport :: Report.Report Report.Result -> IO (Bool, Text)
formatHedgehogReport report = do
    name <- Text.unpack . configTestName <$> IORef.readIORef test_config
    str <- Report.renderResult Internal.Config.EnableColor
        (Just (Internal.Property.PropertyName name)) report
    pure $ (, Text.pack str) $ case Report.reportStatus report of
        Report.OK -> True
        Report.GaveUp -> False
        Report.Failed {} -> False

-- * QuickCheck

-- | Run a quickcheck property.
quickcheck :: (HasCallStack, QuickCheck.Testable prop) => prop -> Test
quickcheck prop = do
    (ok, msg) <- formatQuickcheckResult <$>
        QuickCheck.quickCheckWithResult args prop
    (if ok then success else failure) msg
    where
    args = QuickCheck.stdArgs { QuickCheck.chatty = False }

formatQuickcheckResult :: QuickCheck.Result -> (Bool, Text)
formatQuickcheckResult result = fmap Text.strip $ case result of
    QuickCheck.Success { output } -> (True, Text.pack output)
    QuickCheck.GaveUp { output } -> (False, Text.pack output)
    QuickCheck.Failure { output } -> (False, Text.pack output)
    QuickCheck.NoExpectedFailure { output } -> (False, Text.pack output)

-- | 'equal' for quickcheck.
qEqual :: (Show a, Eq a) => a -> a -> QuickCheck.Property
qEqual a b = QuickCheck.counterexample
    (Text.unpack $ prettyCompare "==" "/=" True a b False)
    (a == b)

-- * util

-- These used to write to stderr, but the rest of the diagnostic output goes to
-- stdout, and it's best these appear in context.

-- | Print a msg with a special tag indicating a passing test.
success :: HasCallStack => Text -> Test
success msg = printTestLine Stack.callStack successColor "++-> " msg

-- | Print a msg with a special tag indicating a failing test.
failure :: HasCallStack => Text -> Test
failure msg = printTestLine Stack.callStack failureColor "__-> " msg

printTestLine :: Stack.CallStack -> ColorCode -> Text -> Text -> IO ()
printTestLine stack color prefix msg = do
    -- Make sure the output doesn't get mixed with trace debug msgs.
    force msg
    isatty <- Terminal.queryTerminal IO.stdOutput
    test_name <- configTestName <$> IORef.readIORef test_config
    -- If there is highlighting in the msg, then it's probably a diff so
    -- most of it may be unhighlighted.  So highlight the prefix to make
    -- the line visible.
    let fullPrefix = (if isHighlighted msg then highlight color else id)
            (prefix <> showStack test_name stack)
    let fullMsg = fullPrefix <> " " <> msg
        highlighted
            -- I only want colors in tty output.
            | not isatty = stripColors fullMsg
            -- Don't put on a color if it already has some.
            | isHighlighted fullMsg = fullMsg
            | otherwise = highlight color fullMsg
    Text.IO.putStrLn highlighted
    where
    isHighlighted = (vt100Prefix `Text.isInfixOf`)

showStack :: Text -> Stack.CallStack -> Text
showStack test_name =
    maybe "<empty-stack>" showFrame . Lists.last . Stack.getCallStack
    where
    showFrame (_, srcloc) =
        Text.pack (Stack.srcLocFile srcloc) <> ":"
        <> showt (Stack.srcLocStartLine srcloc)
        <> if Text.null test_name then "" else " [" <> test_name <> "]"

highlight :: ColorCode -> Text -> Text
highlight (ColorCode code) text
    | Text.null text = text
    | otherwise = code <> text <> vt100Normal

-- | Remove vt100 color codes.
stripColors :: Text -> Text
stripColors = mconcat . Lists.mapTail (Text.drop 1 . Text.dropWhile (/='m'))
    . Text.splitOn vt100Prefix

newtype ColorCode = ColorCode Text deriving (Show)

vt100Prefix :: Text
vt100Prefix = "\ESC["

vt100Normal :: Text
vt100Normal = "\ESC[m\ESC[m"

-- | These codes should probably come from termcap, but I can't be bothered.
failureColor :: ColorCode
failureColor = ColorCode "\ESC[31m" -- red

successColor :: ColorCode
successColor = ColorCode "\ESC[32m" -- green

-- | getChar with no buffering.
humanGetChar :: IO Char
humanGetChar = do
    agreeable <- configHumanAgreeable <$> IORef.readIORef test_config
    if agreeable
        then pure 'y'
        else do
            IO.hFlush IO.stdout
            mode <- IO.hGetBuffering IO.stdin
            IO.hSetBuffering IO.stdin IO.NoBuffering
            do { c <- getChar; putChar ' '; pure c }
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
    where s = Strings.rstrip $ PPrint.formatStr (show val)

-- * filesystem

-- | Get a tmp dir, which will be unique for each test run.
tmpDir :: String -> IO FilePath
tmpDir prefix = do
    baseDir <- Directory.makeAbsolute tmpBaseDir
    Directory.createDirectoryIfMissing True baseDir
    dir <- Temp.mkdtemp $ baseDir </> prefix ++ "-"
    putStrLn $ "** tmp dir: " ++ dir
    pure dir

-- | Run the computation with cwd in a new tmp dir.
inTmpDir :: String -> IO a -> IO a
inTmpDir prefix action = do
    dir <- tmpDir prefix
    Directory.withCurrentDirectory dir action

-- | All tmp files used by tests should go in this directory.
-- TODO instead of being hardcoded this should be configured per-project.
tmpBaseDir :: FilePath
tmpBaseDir = "build/test/tmp"

-- * util

force :: DeepSeq.NFData a => a -> IO ()
force x = Exception.evaluate (DeepSeq.rnf x)

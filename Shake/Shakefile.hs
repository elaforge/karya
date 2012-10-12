{-# LANGUAGE CPP, FlexibleContexts, ViewPatterns #-}
{- | Shakefile for seq and associated binaries.

    - Setting the 'repl' env var will add the -DINTERPRETER_GHC flag.

    - Building RunTests-something will include only tests matching
    *something*_test.hs.

    - The same goes for RunProfile, except it searches for
    *something*_profile.hs.

    TODO

    - askOracle isn't working like I think it should, why does it only
    rebuild test_block.cc when I change fltk version?

    - If I update build/test/RunTests.hs, it gets regenerated, even though
    it's newer than everything else.  Why?
    Also, if I update generate_run_tests.py it doesn't rebuild anything.

    - Paper suggests, in "Unchanging files" that files that remain unchanged
    after a build should allow depending rules to be skipped.  Test this
    with ghc's recompilation skipper.
-}
module Shake.Shakefile where
import Control.Applicative ((<$>))
import qualified Control.Exception as Exception
import Control.Monad
import qualified Control.Monad.Trans as Trans

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import Data.Monoid (mempty)

import qualified Development.Shake as Shake
import Development.Shake ((?==), (?>), (*>), action, need)
import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Info
import qualified System.Process as Process

import qualified Util.PPrint as PPrint
import qualified Shake.CcDeps as CcDeps
import qualified Shake.HsDeps as HsDeps
import qualified Shake.Util as Util
import Shake.Util (system)


-- * config

-- Static constants.
build = "build"
fltkConfig = "/usr/local/src/fltk-1.3/fltk-config"
ghcBinary = "ghc"
hspp = modeToDir Opt </> "hspp"

shakeOptions :: Shake.ShakeOptions
shakeOptions = Shake.shakeOptions
    { Shake.shakeFiles = build </> "shake"
    , Shake.shakeVerbosity = Shake.Quiet
    , Shake.shakeThreads = 4
    , Shake.shakeReport = Just $ build </> "report.html"
    }

data Config = Config {
    buildDir :: FilePath
    , hscDir :: FilePath
    , ghcLib :: FilePath
    , fltkVersion :: String
    , configFlags :: Flags
    } deriving (Show)

-- | Root of .o and .hi hierarchy.
oDir :: Config -> FilePath
oDir = (</> "obj") . buildDir

docDir :: FilePath
docDir = build </> "doc"

-- * flags

data Flags = Flags {
    define :: [String]
    , midiLibs :: [String]
    , cInclude :: [String]
    , ccFlags :: [String]
    , fltkCc :: [String]
    , fltkLd :: [String]
    , hcFlags :: [String]
    , hLinkFlags :: [String]
    -- | Extra flags for ghci.
    , ghciFlags :: [String]
    } deriving (Show)

instance Monoid.Monoid Flags where
    mempty = Flags [] [] [] [] [] [] [] [] []
    mappend (Flags a1 b1 c1 d1 e1 f1 g1 h1 i1)
            (Flags a2 b2 c2 d2 e2 f2 g2 h2 i2) =
        Flags (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2) (f1<>f2) (g1<>g2)
            (h1<>h2) (i1<>i2)

-- * binaries

-- This section has project specific hardcoded lists of files.

-- ** hs

data HsBinary = HsBinary {
    hsName :: FilePath
    , hsMain :: FilePath -- ^ main module
    , hsDeps :: [FilePath] -- ^ additional deps, relative to obj dir
    , hsGui :: Maybe (Maybe FilePath) -- ^ GUI needs make_bundle + maybe icon
    } deriving (Show)

hsBinaries :: [HsBinary]
hsBinaries =
    [ gui "browser" "Instrument/Browser.hs" ["Instrument/browser_ui.cc.o"]
        Nothing
    , plain "dump" "App/Dump.hs"
    , plain "logcat" "LogView/LogCat.hs"
    , gui "logview" "LogView/LogView.hs" ["LogView/logview_ui.cc.o"] Nothing
    , plain "linkify" "Util/Linkify.hs"
    , plain "make_db" "Instrument/MakeDb.hs"
    , plain "pprint" "App/PPrint.hs"
    -- PrintKeymap wants the global keymap, which winds up importing cmds that
    -- directly call UI level functions.  Even though it doesn't call the
    -- cmds, they're packaged together with the keybindings, so I wind up
    -- having to link in all that stuff anyway.
    , HsBinary "print_keymap" "App/PrintKeymap.hs" ["fltk/fltk.a"] Nothing
    , plain "repl" "App/Repl.hs"
    , plain "send" "App/Send.hs"
    , plain "lilypond_click" "App/LilypondClick.hs"
    , gui "seq" "App/Main.hs" ["fltk/fltk.a"] (Just "doc/seq.icns")
    , plain "shakefile" "Shake/Shakefile.hs"
    , plain "test_midi" "Midi/TestMidi.hs"
    , plain "test_jack_midi" "Midi/TestJackMidi.hs"
    , plain "timer" "LogView/Timer.hs"
    , plain "update" "App/Update.hs"
    ]
    where
    plain name path = HsBinary name path [] Nothing
    gui name path deps icon = HsBinary name path deps (Just icon)

-- | Hardcoded list of files that should be processed with CPP when chasing
-- deps.  It would be more robust to generate this dynamically by looking
-- for 'LANGUAGE .*CPP' but there aren't many.
cppFlags :: Config -> FilePath -> Maybe [String]
cppFlags config fn
    | fn `elem` cppFiles = Just $
        cInclude (configFlags config) ++ define (configFlags config)
    | otherwise = Nothing
    where
    cppFiles = ["App/Main.hs", "Cmd/Lang.hs", "Midi/TestMidi.hs", "Ui/Sync.hs"]

-- | Module that define 'main' and should get linked to their own binaries,
-- and the names of their eventual binaries.
nameToMain :: Map.Map FilePath FilePath
nameToMain = Map.fromList [(hsName b, hsMain b) | b <- hsBinaries]

-- | Haskell files that use the FFI likely have dependencies on C++ source.
-- I could figure this out automatically by looking for @foreign import ...@
-- and searching for a neighboring .cc file with those symbols, but it's
-- simpler to give the dependency explicitly.
hsToCc :: Map.Map FilePath [FilePath]
hsToCc = Map.fromList $
    [ ("Midi/CoreMidi.hs", ["Midi/core_midi.cc"])
    , ("Midi/JackMidi.hsc", ["Midi/jack.cc"])
    , ("LogView/LogViewC.hsc", ["LogView/interface.cc"])
    , ("Instrument/BrowserC.hsc", ["Instrument/interface.cc"])
    , ("Util/Fltk.hs", ["Util/fltk_interface.cc"])
    , ("Util/Git/LibGit2.hsc", ["Util/Git/libgit_wrappers.cc"])
    ] ++ [(hsc, ["Ui/c_interface.cc"])
        | hsc <- ["Ui/BlockC.hsc", "Ui/RulerC.hsc", "Ui/StyleC.hsc",
            "Ui/SymbolC.hsc", "Ui/TrackC.hsc", "Ui/UiMsgC.hsc"]]

-- | Another hack.  hsc2hs isn't really meant to work with C++, but it
-- basically does, and that way I don't have to strictly separate all the FFI
-- headers from the C++.
--
-- Unfortunately, g++ breaks the macros used by bindings-dsl:
hsc2hsNeedsC :: [FilePath]
hsc2hsNeedsC = ["Util/Git/LibGit2.hsc"]

-- | Rather than trying to figure out which binary needs which packages, I
-- just union all the packages.  TODO can I ask ghc to infer packages
-- automatically like --make?
packages :: [String]
packages = words $ "fixed-list deepseq data-ordlist cereal "
    ++ "semigroups dlist parsec text stm network haskell-src regex-pcre "
    ++ "bytestring attoparsec utf8-string "
    ++ "mersenne-random-pure64 hashable random-shuffle "
    ++ "containers filepath transformers vector "
    ++ "QuickCheck fclabels syb "
    ++ "Diff " -- Util.Test
    ++ "ghc ghc-paths haskeline " -- repl
    ++ "shake " -- shakefile

-- ** cc

data CcBinary = CcBinary {
    ccName :: String
    , ccDeps :: [FilePath]
    } deriving (Show)

ccBinaries :: [CcBinary]
ccBinaries =
    [ CcBinary "test_block" ["fltk/test_block.cc.o", "fltk/fltk.a"]
    , CcBinary "test_browser" ["Instrument/test_browser.cc.o",
        "Instrument/browser_ui.cc.o", "fltk/f_util.cc.o"]
    , CcBinary "test_logview" ["LogView/test_logview.cc.o",
        "LogView/logview_ui.cc.o", "fltk/f_util.cc.o"]
    ]

fltkDeps :: Config -> [FilePath]
fltkDeps config = map (srcToObj config . ("fltk"</>))
    [ "Block.cc", "EventTrack.cc", "MoveTile.cc", "MsgCollector.cc"
    , "P9Scrollbar.cc", "Ruler.cc", "SeqInput.cc", "SimpleScroll.cc"
    , "SkeletonDisplay.cc", "StyleTable.cc", "SymbolOutput.cc"
    , "SymbolTable.cc", "Track.cc", "TrackTile.cc"
    , "alpha_draw.cc", "config.cc", "f_util.cc", "types.cc", "util.cc"
    ]

-- * mode

data Mode = Debug | Opt | Test | Profile deriving (Eq, Enum, Show)

allModes :: [Mode]
allModes = [Debug .. Profile]

modeToDir :: Mode -> FilePath
modeToDir mode = (build </>) $ case mode of
    Debug -> "debug"
    Opt -> "opt"
    Test -> "test"
    Profile -> "profile"

targetToMode :: FilePath -> Maybe Mode
targetToMode target = snd <$> List.find ((`List.isPrefixOf` target) . fst)
    (zip (map modeToDir [Debug ..]) [Debug ..])

configure :: IO (Mode -> Config)
configure = do
    ghcLib <- run ghcBinary ["--print-libdir"]
    fltkCs <- words <$> run fltkConfig ["--cflags"]
    fltkLds <- words <$> run fltkConfig ["--ldflags"]
    fltkVersion <- takeWhile (/='\n') <$> run fltkConfig ["--version"]
    bindingsInclude <- run "ghc-pkg" ["field", "bindings-DSL", "include-dirs"]
    bindingsInclude <- case words bindingsInclude of
        [_, path] -> return path
        words -> error $ "unexpected output from ghc-pkg: " ++ show words
    return $ \mode -> Config (modeToDir mode) (build </> "hsc") (strip ghcLib)
        fltkVersion $ setCcFlags $
            setConfigFlags fltkCs fltkLds mode osFlags bindingsInclude
    where
    setConfigFlags fltkCs fltkLds mode flags bindingsInclude = flags
        { define = define osFlags
            ++ (if mode `elem` [Test, Profile] then ["-DTESTING"] else [])
            ++ ["-DBUILD_DIR=\"" ++ modeToDir mode ++ "\""]
        , cInclude = ["-I.", "-Ifltk", "-I" ++ bindingsInclude]
        , fltkCc = fltkCs
        , fltkLd = fltkLds
        , hcFlags = words "-I. -threaded -W -fwarn-tabs -pgml g++"
            ++ ["-F", "-pgmF", hspp]
            ++ case mode of
                Debug -> []
                Opt -> ["-O"]
                Test -> ["-fhpc"]
                -- Omit -auto-all because it slows down the profile quite
                -- a bit.  Usually when profiling I'm looking for overall
                -- timing and stats, not individual cost centers.  I can turn
                -- those on when debugging.
                Profile -> ["-O", "-prof"]
        , hLinkFlags = libs ++ ["-rtsopts", "-threaded"]
            ++ if mode == Profile then ["-prof"] else []
        -- Hackery, make sure ghci gets link flags, otherwise it wants to
        -- load everything as bytecode and fails on missing symbols.  Actually,
        -- these only apply to loading the modules for the main app.  But
        -- that's where I care most about load time.
        , ghciFlags = libs ++ ["Util/Git/libgit_wrappers.cc"]
        }
    setCcFlags flags = flags
        { ccFlags =
            -- Always compile c++ with optimization because I don't have much
            -- of it and it compiles quickly.
            fltkCc flags ++ define flags ++ cInclude flags ++ ["-Wall", "-O2"]
        }
    libs = ["-lgit2"]
    osFlags = case System.Info.os of
        -- In C and C++ programs the OS specific defines like __APPLE__ and
        -- __linux__ are already defined, but ghc doesn't define them.
        "darwin" -> mempty
            -- These apparently control which APIs are visible.  But they
            -- make it slightly more awkward for ghci since it needs the
            -- same flags to load .o files, and things seem to work without
            -- them, so I'll omit them for the time being.
            -- { define = ["-DMAC_OS_X_VERSION_MAX_ALLOWED=1060",
            --     "-DMAC_OS_X_VERSION_MIN_REQUIRED=1050"]
            { define = ["-D__APPLE__"]
            , midiLibs = words $ "-framework CoreFoundation "
                ++ "-framework CoreMIDI -framework CoreAudio"
            }
        "linux" -> mempty
            { midiLibs = ["-ljack"]
            , define = ["-D__linux__"]
            }
        unknown -> error $ "unknown os: " ++ show unknown
    run cmd args = Process.readProcess cmd args ""

type InferConfig = FilePath -> Config

-- | Figure out the Config for a given target by looking at its directory.
inferConfig :: (Mode -> Config) -> InferConfig
inferConfig modeConfig fn =
    maybe (modeConfig Debug) modeConfig (targetToMode fn)

-- * rules

data Flag = Verbosity Shake.Verbosity | Jobs Int deriving (Eq, Show)

cmdOptions :: [GetOpt.OptDescr Flag]
cmdOptions =
    [ GetOpt.Option ['v'] []
        (GetOpt.OptArg (maybe (Verbosity Shake.Normal) readVerbosity)
            "verbosity") $ "Verbosity, from 0 to 4."
    , GetOpt.Option ['j'] [] (GetOpt.ReqArg (Jobs . read) "jobs") $
        "Number of jobs to run simultaneously."
    ]
    where
    readVerbosity s = Verbosity $ case read s of
        0 -> Shake.Silent
        1 -> Shake.Quiet
        2 -> Shake.Normal
        3 -> Shake.Loud
        4 -> Shake.Diagnostic
        n -> error $ "verbosity should be 0--4: " ++ show n

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    (flags, targets, errors) <- GetOpt.getOpt GetOpt.Permute cmdOptions <$>
        Environment.getArgs
    when (not (null errors)) $
        error $ "Errors parsing flags: " ++ unlines errors

    let target = case targets of
            [target] -> target
            _ -> error "expected one argument"
    modeConfig <- configure
    let options = shakeOptions
            { Shake.shakeThreads =
                Maybe.fromMaybe (Shake.shakeThreads shakeOptions) $
                    mlast [j | Jobs j <- flags]
            , Shake.shakeVerbosity =
                Maybe.fromMaybe (Shake.shakeVerbosity shakeOptions) $
                    mlast [v | Verbosity v <- flags]
            }
    writeGhciFlags modeConfig
    writeConfigH
    Shake.shake options $ do
        let infer = inferConfig modeConfig
        setupOracle (modeConfig Debug)
        -- hspp is depended on by all .hs files.  To avoid recursion, I
        -- build hspp itself with --make.
        hspp *> \fn -> do
            -- But I need to mark hspp's deps so it will rebuild.
            need =<< HsDeps.transitiveImportsOf (const Nothing) "Util/Hspp.hs"
            Util.cmdline $ makeHs (oDir (modeConfig Opt)) fn "Util/Hspp.hs"
        matchObj "fltk/fltk.a" ?> \fn -> do
            let config = infer fn
            need (fltkDeps config)
            system "ar" $ ["-rs", fn] ++ fltkDeps config
        forM_ ccBinaries $ \binary -> matchBinary (ccName binary) ?> \fn -> do
            let config = infer fn
            let objs = map (oDir config </>) (ccDeps binary)
            need objs
            Util.cmdline $ linkCc config fn objs
            makeBundle fn Nothing
        forM_ hsBinaries $ \binary -> matchBinary (hsName binary) ?> \fn -> do
            let config = infer fn
            hs <- maybe (errorIO $ "no main module for " ++ fn) return
                (Map.lookup (FilePath.takeFileName fn) nameToMain)
            buildHs config (map (oDir config </>) (hsDeps binary)) hs fn
            case hsGui binary of
                Just icon -> makeBundle fn icon
                _ -> return ()
        docDir </> "keymap.html" *> \fn -> do
            let bin = buildDir (modeConfig Debug) </> "print_keymap"
            need [bin]
            Util.shell $ bin ++ " >" ++ fn
        testRules (modeConfig Test)
        profileRules (modeConfig Profile)
        markdownRule (buildDir (modeConfig Opt) </> "linkify")
        hsRule (modeConfig Debug) -- hsc2hs only uses mode-independent flags
        hsORule infer
        -- 'hsORule' depends on .hi files instead of .o files, and this rule
        -- states that a .hi is created by creating its .hs.o.  This might
        -- reduce some recompilation because ghc will avoid updating the
        -- timestamp on the .hi file if things dependent on it don't need to
        -- be recompiled.
        "//*.hi" *> \hi -> need [hiToObj hi]
        ccORule infer
        dispatch (modeConfig Debug) target

setupOracle :: Config -> Shake.Rules ()
setupOracle config = do
    Shake.addOracle ["ghc"] $ return [ghcLib config]
    Shake.addOracle ["fltk"] $ return [fltkVersion config]

writeConfigH :: IO ()
writeConfigH = do
    useRepl <- fmap (("repl" `elem`) . map fst) Environment.getEnvironment
    if useRepl then mangle [ghc] []
        else mangle [] [ghc] -- use stub
    where
    mangle = CcDeps.enableDefines "hsconfig.h"
    ghc = "INTERPRETER_GHC"

-- | Match a file in @build/<mode>/obj/@.
matchObj :: Shake.FilePattern -> FilePath -> Bool
matchObj pattern fn =
    matchPrefix (map ((</> "obj") . modeToDir) allModes) pattern fn
    || matchPrefix (map modeToDir allModes) pattern fn

-- | Match a file in @build/<mode>/@.
matchBinary :: Shake.FilePattern -> FilePath -> Bool
matchBinary = matchPrefix (map modeToDir allModes)

matchPrefix :: [Shake.FilePattern] -> Shake.FilePattern -> FilePath -> Bool
matchPrefix prefixes pattern fn =
    case msum $ map (flip dropPrefix fn) prefixes of
        Nothing -> False
        Just rest -> pattern ?== (dropWhile (=='/') rest)

dispatch :: Config -> String -> Shake.Rules ()
dispatch config target = case target of
    "show-config" -> action $ Trans.liftIO $ PPrint.pprint config
    "clean" -> action $ do
        -- The shake database will remain because shake creates it after the
        -- shakefile runs, but that's probably ok.
        system "rm" ["-rf", build]
        system "mkdir" [build]
    "doc" -> action $ makeDocumentation config
    "haddock" -> action $ makeHaddock config
    "md" -> action $ need . map docToHtml =<< getMarkdown
    "checkin" -> do
        let debug = (modeToDir Debug </>)
        Shake.want [debug "browser", debug "logview", debug "make_db",
            debug "seq", debug "update", docDir </> "keymap.html",
            modeToDir Profile </> "RunProfile"]
        dispatch config "tests"
        -- The gui tests tend to wedge.
        -- dispatch config "complete-tests"
    "tests" -> action $ do
        need [runTests Nothing]
        system "test/run_tests" [runTests Nothing]
    (dropPrefix "tests-" -> Just tests) -> action $ do
        need [runTests (Just tests)]
        system "test/run_tests" [runTests (Just tests)]
    "complete-tests" -> action $ do
        need [runTests Nothing]
        system "test/run_tests" [runTests Nothing, "normal-", "gui-"]
    "profile" -> action $ do
        need [modeToDir Profile </> "RunProfile"]
        system "tools/summarize_profile.py" []
    _ -> Shake.want [target]
    where
    runTests tests = modeToDir Test </> ("RunTests" ++ maybe "" ('-':) tests)

-- | Make all documentation.
makeDocumentation :: Config -> Shake.Action ()
makeDocumentation config = do
    hscs <- filter haddock <$> Util.findHs "*.hsc" "."
    hs <- filter haddock <$> Util.findHs "*.hs" "."
    docs <- getMarkdown
    need $ (docDir </> "keymap.html")
        : map (hscToHs (hscDir config)) hscs ++ map docToHtml docs
    makeHaddock config
    -- TODO do these individually so they can be parallelized and won't run
    -- each time
    system "tools/colorize" $ (build </> "hscolour") : hs ++ hscs

getMarkdown :: Shake.Action [FilePath]
getMarkdown = map ("doc"</>) <$> Shake.getDirectoryFiles "doc" "*.md"

makeHaddock :: Config -> Shake.Action ()
makeHaddock config = do
    hscs <- filter haddock <$> Util.findHs "*.hsc" "."
    hs <- filter haddock <$> Util.findHs "*.hs" "."
    need $ map (hscToHs (hscDir config)) hscs
    let flags = configFlags config
    system "haddock" $
        [ "--html", "-B", ghcLib config
        , "--source-base=../hscolour/"
        , "--source-module=../hscolour/%{MODULE/.//}.html"
        , "--source-entity=../hscolour/%{MODULE/.//}.html#%{NAME}"
        , "--prologue=doc/prologue"
        -- This flag crashes ghc 7.0.3
        -- , "-q", "relative" -- Source references use qualified names.
        , "-o", build </> "haddock"
        ] ++ ["--optghc=" ++ flag | flag <- define flags ++ cInclude flags]
        ++ hs ++ map (hscToHs (hscDir config)) hscs

-- | Should this module have haddock documentation generated?
haddock :: FilePath -> Bool
haddock hs = not $ hs `elem` map hsMain hsBinaries
    || "_test.hs" `List.isSuffixOf` hs
    || "_profile.hs" `List.isSuffixOf` hs
    -- TODO Actually I would like to haddock these, but they rely on TESTING
    -- being set.  Apparently haddock has no way to set CPP defines, so I
    -- either have to add a way or stop using CPP for conditional exports.
    || "Test.hs" `List.isSuffixOf` hs
    -- This will crash haddock on OS X since jack.h is likely not present.
    -- TODO sorta hacky
    || hs == "Midi/JackMidi.hsc"

makeHs :: FilePath -> FilePath -> FilePath -> Util.Cmdline
makeHs dir out main = ("GHC-MAKE", out, cmdline)
    where
    cmdline = [ghcBinary, "--make", "-outputdir", dir, "-O2", "-o", out,
        "-main-is", pathToModule main, main]

-- | Build a haskell binary.
buildHs :: Config -> [FilePath] -> FilePath -> FilePath -> Shake.Action ()
buildHs config deps hs fn = do
    srcs <- HsDeps.transitiveImportsOf (cppFlags config) hs
    let ccs = List.nub $
            concat [Map.findWithDefault [] src hsToCc | src <- srcs]
        objs = List.nub (map (srcToObj config) (ccs ++ srcs)) ++ deps
    logDeps config "build" fn objs
    Util.cmdline $ linkHs config fn packages objs

makeBundle :: FilePath -> Maybe FilePath -> Shake.Action ()
makeBundle binary icon
    | System.Info.os == "darwin" =
        system "tools/make_bundle" (binary : maybe [] (:[]) icon)
    | otherwise = return ()

-- * tests and profiles

-- | Generate RunTests.hs and compile it.
testRules :: Config -> Shake.Rules ()
testRules config = do
    let binPrefix = modeToDir Test </> "RunTests"
    binPrefix ++ "*.hs" *> generateTestHs "_test"
    hasPrefix binPrefix ?> \fn -> do
        -- The UI tests use fltk.a.  It would be nicer to have it
        -- automatically added when any .o that uses it is linked in.
        buildHs config [oDir config </> "fltk/fltk.a"] (fn ++ ".hs") fn
        -- This sticks around and breaks hpc.
        system "rm" ["-f",
            FilePath.replaceExtension (FilePath.takeFileName fn) "tix"]
        -- This gets reset on each new test run.
        system "rm" ["-f", "test.output"]

profileRules :: Config -> Shake.Rules ()
profileRules config = do
    let binPrefix = modeToDir Profile </> "RunProfile"
    binPrefix ++ "*.hs" *> generateTestHs "_profile"
    hasPrefix binPrefix ?> \fn -> do
        buildHs config [oDir config </> "fltk/fltk.a"] (fn ++ ".hs") fn

-- | Match any filename that starts with the given prefix but doesn't have
-- an extension, i.e. binaries.
hasPrefix :: FilePath -> FilePath -> Bool
hasPrefix prefix fn =
    prefix `List.isPrefixOf` fn && null (FilePath.takeExtension fn)

generateTestHs :: FilePath -> FilePath -> Shake.Action ()
generateTestHs hsSuffix fn = do
    let contains = drop 1 (dropWhile (/='-') (FilePath.dropExtension fn))
        pattern = "*" ++ contains ++ "*" ++ hsSuffix ++ ".hs"
    tests <- Util.findHs pattern "."
    when (null tests) $
        errorIO $ "no tests match pattern: " ++ show pattern
    need $ "test/generate_run_tests.py" : tests
    system "test/generate_run_tests.py" (fn : tests)

-- * markdown

markdownRule :: FilePath -> Shake.Rules ()
markdownRule linkifyBin = docDir </> "*.md.html" *> \html -> do
    let doc = htmlToDoc html
    need [linkifyBin, doc]
    system "tools/convert_doc" [doc, html]

-- | build/doc/xyz.md.html -> doc/xyz.md
htmlToDoc :: FilePath -> FilePath
htmlToDoc = ("doc" </>) . FilePath.takeFileName . FilePath.dropExtension

-- | doc/xyz.md -> build/doc/xyz.md.html
docToHtml :: FilePath -> FilePath
docToHtml = (docDir </>) . FilePath.takeFileName . (++".html")

-- * hs

hsORule :: InferConfig -> Shake.Rules ()
hsORule infer = matchObj "//*.hs.o" ?> \obj -> do
    Shake.askOracle ["ghc"]
    let config = infer obj
    isHsc <- Trans.liftIO $
        Directory.doesFileExist (objToSrc config obj ++ "c")
    let hs = if isHsc then objToHscHs config obj else objToSrc config obj
    need [hspp]
    imports <- HsDeps.importsOf (cppFlags config hs) hs
    includes <- if Maybe.isJust (cppFlags config hs)
        then includesOf "hsORule" config hs else return []
    need includes
    let his = map (objToHi . srcToObj config) imports
    logDeps config "hs" obj (hs:his)
    Util.cmdline $ compileHs config hs
    -- FFI-using files with a "wrapper" callback generate a _stub.c file
    -- and compile it.  Merge it with the module's .o so I don't have to
    -- worry about linking it later.  This also makes ghci able to load the
    -- file.
    --
    -- A bug in ghc 7.0.3 causes the compiled stub .o files to have a
    -- strange name when -o is used:
    -- .../BlockC.hs.o -> .../BlockC.hs_stub.o
    -- should be .../BlockC_stub.o
    let stub = FilePath.dropExtension (srcToObj config hs) ++ "_stub.o"
    Util.whenM (Trans.liftIO $ Directory.doesFileExist stub) $ do
        Trans.liftIO $ Directory.renameFile obj (obj ++ "2")
        system "ld" ["-r", "-o", obj, obj ++ "2", stub]
        -- Get rid of the stub.o.  Otherwise if this rule fires again but
        -- ghc decides not to recompile, the old .o file will be left alone.
        -- Then the ld -r will try to merge the stub again and will fail with
        -- a duplicate symbol error.
        Trans.liftIO $ Directory.removeFile stub

compileHs :: Config -> FilePath -> Util.Cmdline
compileHs config hs = ("GHC", hs,
    [ghcBinary, "-c"] ++ ghcFlags config ++ hcFlags (configFlags config)
        ++ main_is ++ [hs, "-o", srcToObj config hs])
    where
    main_is = if hs `elem` Map.elems nameToMain
        then ["-main-is", pathToModule hs]
        else []

linkHs :: Config -> FilePath -> [String] -> [FilePath] -> Util.Cmdline
linkHs config output packages objs = ("LD-HS", output,
    ghcBinary : fltkLd flags ++ midiLibs flags ++ hLinkFlags flags
        ++ ["-lstdc++"]
        ++ ["-hide-all-packages"] ++ map ("-package="++) packages
        ++ objs ++ ["-o", output])
    where flags = configFlags config

-- | ghci has to be called with the same flags that the .o files were compiled
-- with or it won't load them.
writeGhciFlags :: (Mode -> Config) -> IO ()
writeGhciFlags modeConfig =
    forM_ (map modeConfig [Debug, Test, Opt]) $ \config -> do
        Directory.createDirectoryIfMissing True (buildDir config)
        writeFile (buildDir config </> "ghci-flags") $
            unwords (flags config) ++ "\n"
    where
    -- Make sure -osuf .hs.o is in the flags, otherwise ghci won't know
    -- how to find the .o files.  But it's redundant for the ghc compile,
    -- which uses -o.
    -- I have to add -I. manually too since it's in hcFlags along with
    -- non-ghci stuff, not ghcFlags.  I'd have to add a new config field for
    -- non-file-specific ghc flags, or put -I in 'define', where it doesn't
    -- belong.
    flags config = ["-I.", "-osuf", ".hs.o"]
        ++ ghcFlags config
        ++ map (resolveSrc config) (ghciFlags (configFlags config))
    resolveSrc config arg
        | FilePath.takeExtension arg == ".cc" = srcToObj config arg
        | otherwise = arg

-- | Get the file-independent flags for a haskell compile.
ghcFlags :: Config -> [String]
ghcFlags config =
    [ "-outputdir", oDir config
    , "-i" ++ oDir config ++ ":" ++ hscDir config
    ] ++ define (configFlags config)

-- * cc

ccORule :: InferConfig -> Shake.Rules ()
ccORule infer = matchObj "//*.cc.o" ?> \obj -> do
    Shake.askOracle ["fltk"]
    let config = infer obj
    let cc = objToSrc config obj
    includes <- includesOf "ccORule" config cc
    logDeps config "cc" obj (cc:includes)
    Util.cmdline $ compileCc config cc obj

compileCc :: Config -> FilePath -> FilePath -> Util.Cmdline
compileCc config cc obj = ("C++", obj,
    ["g++", "-c"] ++ ccFlags (configFlags config) ++ ["-o", obj, cc])

linkCc :: Config -> FilePath -> [FilePath] -> Util.Cmdline
linkCc config binary objs = ("LD-CC", binary,
    "g++" : objs ++ fltkLd (configFlags config) ++ ["-o", binary])

-- * hsc

hsRule :: Config -> Shake.Rules ()
hsRule config = hscDir config ++ "//*.hs" *> \hs -> do
    let hsc = hsToHsc (hscDir config) hs
    includes <- includesOf "hsRule" config hsc
    logDeps config "hsc" hs (hsc : includes)
    Util.cmdline $ hsc2hs config (hsc `notElem` hsc2hsNeedsC) hs hsc

hsc2hs :: Config -> Bool -> FilePath -> FilePath -> Util.Cmdline
hsc2hs config useCpp hs hsc = ("hsc2hs", hs,
    ["hsc2hs", "-I" ++ ghcLib config </> "include"]
    ++ (if useCpp
        -- Otherwise g++ complains about the offsetof macro hsc2hs uses.
        then words "-c g++ --cflag -Wno-invalid-offsetof"
        else [])
    ++ cInclude flags ++ fltkCc flags ++ define flags
    ++ [hsc, "-o", hs])
    where flags = configFlags config

-- * util

-- | A/B.hs -> build/debug/obj/A/B.hs.o
-- A/B.cc -> build/debug/obj/A/B.cc.o
-- A/B.hsc -> build/debug/obj/A/B.hs.o
-- build/A/B.hs -> build/A/B.hs.o
-- build/hsc/Ui/Key.hs -> build/debug/obj/Ui/Key.hs.o
--
-- Generated .hs files are already in build/ so they shouldn't have build/etc.
-- prepended.  Unless they were .hsc generated files.
srcToObj :: Config -> FilePath -> FilePath
srcToObj config fn = addDir $ case FilePath.takeExtension fn of
    ".hs" -> FilePath.addExtension fn "o"
    ".hsc" -> FilePath.replaceExtension fn "hs.o"
    ".cc" -> FilePath.addExtension fn "o"
    _ -> error $ "unknown haskell extension: " ++ show fn
    where
    addDir
        | hscDir config `List.isPrefixOf` fn =
            (oDir config </>) . dropDir (hscDir config)
        | build `List.isPrefixOf` fn = id
        | otherwise = (oDir config </>)

-- | build/debug/A/B.hs.o -> A/B.hs
objToSrc :: Config -> FilePath -> FilePath
objToSrc config = FilePath.dropExtension . dropDir (oDir config)

-- | build/debug/A/B.o -> build/hsc/A/B.hs
objToHscHs :: Config -> FilePath -> FilePath
objToHscHs config = (hscDir config </>) . objToSrc config

-- | build/hsc/A/B.hs -> A/B.hsc
hsToHsc :: FilePath -> FilePath -> FilePath
hsToHsc hscDir fn = dropDir hscDir $ FilePath.replaceExtension fn "hsc"

-- | A/B.hsc -> build/hsc/A/B.hs
hscToHs :: FilePath -> FilePath -> FilePath
hscToHs hscDir fn = (hscDir </>) $ FilePath.replaceExtension fn "hs"

objToHi :: FilePath -> FilePath
objToHi = (++".hi") . FilePath.dropExtension . FilePath.dropExtension

hiToObj :: FilePath -> FilePath
hiToObj = flip FilePath.replaceExtension "hs.o"

dropDir :: FilePath -> FilePath -> FilePath
dropDir odir fn
    | dir `List.isPrefixOf` fn = drop (length dir) fn
    | otherwise = fn
    where dir = odir ++ "/"

strip :: String -> String
strip = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

errorIO :: (Trans.MonadIO m) => String -> m a
errorIO = Trans.liftIO . Exception.throwIO . Exception.ErrorCall

pathToModule :: FilePath -> String
pathToModule = map (\c -> if c == '/' then '.' else c) . FilePath.dropExtension

logDeps :: Config -> String -> FilePath -> [FilePath] -> Shake.Action ()
logDeps config stage fn deps = do
    need deps
    Shake.putLoud $ "***" ++ stage ++ ": " ++ fn ++ " <- "
        ++ unwords (map (dropDir (oDir config)) deps)

(<>) = Monoid.mappend

includesOf :: String -> Config -> FilePath -> Shake.Action [FilePath]
includesOf caller config fn = do
    let dirs = [dir | '-':'I':dir <- cInclude (configFlags config)]
    (includes, not_found) <- CcDeps.transitiveIncludesOf dirs fn
    when (not (null not_found)) $
        Trans.liftIO $ putStrLn $ caller
            ++ ": WARNING: c includes not found: " ++ show not_found
    return includes

dropPrefix :: String -> String -> Maybe String
dropPrefix pref str
    | pref `List.isPrefixOf` str = Just (drop (length pref) str)
    | otherwise = Nothing

mlast :: [a] -> Maybe a
mlast [] = Nothing
mlast xs = Just (last xs)

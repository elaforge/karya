{-# LANGUAGE FlexibleContexts, ViewPatterns #-}
{- | Shakefile for seq and associated binaries.

    Setting the 'hint' env var will add the -DINTERPRETER_HINT flag.

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

import qualified Data.ByteString.Char8 as B
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import Data.Monoid (mempty)

import qualified Development.Shake as Shake
import Development.Shake ((?==), (?>), (*>), action, system', need)
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
import Shake.Util (Cmdline, system)


-- * config

-- Static constants.
build = "build"
fltkConfig = "/usr/local/src/fltk-1.3/fltk-config"
ghcBinary = "ghc"
hspp = modeToDir Opt </> "hspp"

shakeOptions :: Shake.ShakeOptions
shakeOptions = Shake.shakeOptions
    { Shake.shakeFiles = build </> "shake"
    , Shake.shakeVerbosity = Shake.Normal
    , Shake.shakeThreads = 4
    , Shake.shakeDump = True
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

-- * flags

data Flags = Flags {
    define :: [String]
    , midiLibs :: [String]
    , midiDriver :: [String]
    , cInclude :: [String]
    , ccFlags :: [String]
    , fltkCc :: [String]
    , fltkLd :: [String]
    , hcFlags :: [String]
    , hLinkFlags :: [String]
    } deriving (Show)

-- TODO ghc 7.2.1's GHC.Generics might be able to make this derivable
-- TODO unused?
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
    , plain "make_db" "Instrument/MakeDb.hs"
    , plain "pprint" "App/PPrint.hs"
    -- PrintKeymap wants the global keymap, which winds up importing cmds that
    -- directly call UI level functions.  Even though it doesn't call the
    -- cmds, they're packaged together with the keybindings, so I wind up
    -- having to link in all that stuff anyway.
    , HsBinary "print_keymap" "App/PrintKeymap.hs" ["fltk/fltk.a"] Nothing
    , plain "repl" "App/Repl.hs"
    , plain "send" "App/Send.hs"
    , gui "seq" "App/Main.hs" ["fltk/fltk.a"] (Just "doc/seq.icns")
    , plain "shakefile" "Shake/Shakefile.hs"
    , plain "test_core_midi" "Midi/TestCoreMidi.hs"
    , plain "timer" "LogView/Timer.hs"
    , plain "update" "App/Update.hs"
    ]
    where
    plain name path = HsBinary name path [] Nothing
    gui name path deps icon = HsBinary name path deps (Just icon)

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
    , ("LogView/LogViewC.hsc", ["LogView/interface.cc"])
    , ("Instrument/BrowserC.hsc", ["Instrument/interface.cc"])
    , ("Util/Fltk.hs", ["Util/fltk_interface.cc"])
    ] ++ [(hsc, ["Ui/c_interface.cc"]) | hsc <-
        ["Ui/BlockC.hsc", "Ui/RulerC.hsc", "Ui/StyleC.hsc", "Ui/SymbolC.hsc",
            "Ui/TrackC.hsc", "Ui/UiMsgC.hsc"]]

-- | Rather than trying to figure out which binary needs which packages, I
-- just union all the packages.  TODO can I ask ghc to infer packages
-- automatically like --make?
packages :: [String]
packages = words $ "fixed-list deepseq data-ordlist cereal storablevector "
    ++ "dlist parsec text stm network haskell-src regex-pcre hint "
    ++ "bytestring attoparsec utf8-string "
    ++ "mersenne-random-pure64 hashable random-shuffle "
    ++ "containers filepath transformers vector "
    ++ "QuickCheck "
    ++ "haskeline " -- repl
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
    useHint <- fmap (("hint" `elem`) . map fst) Environment.getEnvironment
    return $ \mode ->
        let flags = osFlags
                { define = define osFlags
                    ++ if mode `elem` [Test, Profile] then ["-DTESTING"] else []
                    ++ if useHint then ["-DINTERPRETER_HINT"] else []
                , cInclude = ["-I.", "-Ifltk"]
                , fltkCc = fltkCs ++ if mode == Opt then ["-O2"] else []
                , fltkLd = fltkLds ++ ["-threaded"]
                , hcFlags = words "-threaded -W -fwarn-tabs -pgml g++"
                    ++ ["-F", "-pgmF", hspp]
                    ++ case mode of
                        Debug -> []
                        Opt -> ["-O"]
                        Test -> ["-fhpc"]
                        -- Omit -auto-all because it slows down the profile
                        -- quite a bit.  Usually when profiling I'm looking
                        -- for overall timing and stats, not individual cost
                        -- centers.  I can turn those on when debugging.
                        Profile -> ["-O", "-prof"]
                , hLinkFlags = ["-rtsopts"]
                    ++ if mode == Profile then ["-prof", "-auto-all"] else []
                }
        in Config (modeToDir mode) (build </> "hsc") (strip ghcLib)
            fltkVersion (setCcFlags flags)
    where
    setCcFlags flags = flags
        { ccFlags = fltkCc flags ++ define flags ++ cInclude flags ++ ["-Wall"]
        }
    osFlags = case System.Info.os of
        "darwin" -> mempty
            -- These apparently control which APIs are visible.  But they
            -- make it slightly more awkward for ghci since it needs the
            -- same flags to load .o files, and things seem to work without
            -- them, so I'll omit them for the time being.
            -- { define = ["-DMAC_OS_X_VERSION_MAX_ALLOWED=1060",
            --     "-DMAC_OS_X_VERSION_MIN_REQUIRED=1050"]
            { define = ["-DCORE_MIDI"]
            , midiLibs = words $ "-framework CoreFoundation "
                ++ "-framework CoreMIDI -framework CoreAudio"
            , midiDriver = ["-DCORE_MIDI"]
            }
        "linux" -> mempty
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
        (GetOpt.OptArg (maybe (Verbosity Shake.Loud) readVerbosity)
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
    writeGhciFlags (modeConfig Debug) (modeConfig Test)
    Shake.shake options $ do
        let infer = inferConfig modeConfig
        setupOracle (modeConfig Debug)
        -- hspp is depended on by all .hs files.  To avoid recursion, I
        -- build hspp itself with --make.
        hspp *> \fn -> do
            -- But I need to mark hspp's deps so it will rebuild.
            need =<< HsDeps.transitiveImportsOf "Util/Hspp.hs"
            system $ makeHs (oDir (modeConfig Opt)) fn "Util/Hspp.hs"
        build </> "tags" *> \fn -> do
            hs <- Util.findHs "*.hs" "."
            hscs <- Util.findHs "*.hs" (hscDir (modeConfig Debug))
            need (hs ++ hscs)
            system' "hasktags" $
                ["--ignore-close-implementation", "--ctags", "-o", fn]
                ++ hs ++ hscs
            -- Let vim know it can use bsearch.
            let magic = B.pack "!_TAG_FILE_SORTED\t1\t ~"
            Trans.liftIO $ B.writeFile fn =<<
                B.unlines . (magic:) . List.sort . B.lines <$> B.readFile fn

        matchObj "fltk/fltk.a" ?> \fn -> do
            let config = infer fn
            need (fltkDeps config)
            system' "ar" $ ["-rs", fn] ++ fltkDeps config
        forM_ ccBinaries $ \binary -> matchBinary (ccName binary) ?> \fn -> do
            let config = infer fn
            let objs = map (oDir config </>) (ccDeps binary)
            need objs
            system $ linkCc config fn objs
            makeBundle fn Nothing
        forM_ hsBinaries $ \binary -> matchBinary (hsName binary) ?> \fn -> do
            let config = infer fn
            hs <- maybe (errorIO $ "no main module for " ++ fn) return
                (Map.lookup (FilePath.takeFileName fn) nameToMain)
            buildHs config (map (oDir config </>) (hsDeps binary)) hs fn
            case hsGui binary of
                Just icon -> makeBundle fn icon
                _ -> return ()
        "doc/keymap.html" *> \fn -> do
            let bin = buildDir (modeConfig Debug) </> "print_keymap"
            need [bin]
            Util.shell $ bin ++ " >" ++ fn
        testRules (modeConfig Test)
        profileRules (modeConfig Profile)
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
    Shake.addOracle ["hint"] $
        return [show $ "-DINTERPRETER_HINT" `elem` define (configFlags config)]

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
        system' "rm" ["-rf", build]
        system' "mkdir" [build]
    "doc" -> action $ do
        hscs <- Util.findHs "*.hs" (hscDir config)
        hs <- filter haddock <$> Util.findHs "*.hs" "."
        need hscs
        system' "haddock" $ ["--html", "-B", ghcLib config,
            "--source-module=\"../%F\"", "-o", build </> "doc"]
            ++ hs ++ hscs
    "checkin" -> do
        let debug = (modeToDir Debug </>)
        Shake.want [debug "browser", debug "logview", debug "make_db",
            debug "seq", debug "update", "doc/keymap.html",
            modeToDir Profile </> "RunProfile"]
        dispatch config "complete-tests"
    "tests" -> action $ do
        need [runTests Nothing]
        system' "test/run_tests" [runTests Nothing]
    (dropPrefix "tests-" -> Just tests) -> action $ do
        need [runTests (Just tests)]
        system' "test/run_tests" [runTests (Just tests)]
    "complete-tests" -> action $ do
        need [runTests Nothing]
        system' "test/run_tests" [runTests Nothing, "normal-", "gui-"]
    "profile" -> action $ do
        need [modeToDir Profile </> "RunProfile"]
        system' "tools/summarize_profile.py" []
    _ -> Shake.want [target]
    where
    runTests tests = modeToDir Test </> ("RunTests" ++ maybe "" ('-':) tests)

-- | Should this module have haddock documentation generated?
haddock :: FilePath -> Bool
haddock hs = not $ hs `elem` map hsMain hsBinaries
    || "_test.hs" `List.isSuffixOf` hs
    || "_profile.hs" `List.isSuffixOf` hs
    -- TODO Actually I would like to haddock these, but they rely on TESTING
    -- being set.  Apparently haddock has no way to set CPP defines, so I
    -- either have to add a way or stop using CPP for conditional exports.
    || "Test.hs" `List.isSuffixOf` hs

makeHs :: FilePath -> FilePath -> FilePath -> Cmdline
makeHs dir out main = ("GHC-MAKE", out, cmdline)
    where
    cmdline = [ghcBinary, "--make", "-outputdir", dir, "-O2", "-o", out,
        "-main-is", pathToModule main, main]

-- | Build a haskell binary.
buildHs :: Config -> [FilePath] -> FilePath -> FilePath -> Shake.Action ()
buildHs config deps hs fn = do
    srcs <- HsDeps.transitiveImportsOf hs
    let ccs = List.nub $
            concat [Map.findWithDefault [] src hsToCc | src <- srcs]
        objs = deps ++ List.nub (map (srcToObj config) (ccs ++ srcs))
    logDeps config "build" fn objs
    system $ linkHs config fn packages objs

makeBundle :: FilePath -> Maybe FilePath -> Shake.Action ()
makeBundle binary icon
    | System.Info.os == "darwin" =
        system' "tools/make_bundle" (binary : maybe [] (:[]) icon)
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
        system' "rm" ["-f",
            FilePath.replaceExtension (FilePath.takeFileName fn) "tix"]
        -- This gets reset on each new test run.
        system' "rm" ["-f", "test.output"]

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
    system' "test/generate_run_tests.py" (fn : tests)

-- * hs

hsORule :: InferConfig -> Shake.Rules ()
hsORule infer = matchObj "//*.hs.o" ?> \obj -> do
    Shake.askOracle ["ghc"]
    let config = infer obj
    isHsc <- Trans.liftIO $
        Directory.doesFileExist (objToSrc config obj ++ "c")
    let hs = if isHsc then objToHscHs config obj else objToSrc config obj
    -- Hardcoded dependency.  If I get more I should come up with a more
    -- general solution.
    when (hs == "Cmd/Lang.hs") $
        void $ Shake.askOracle ["hint"]
    need [hspp]
    imports <- HsDeps.importsOf hs
    let his = map (objToHi . srcToObj config) imports
    logDeps config "hs" obj (hs:his)
    system $ compileHs config hs
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
        system' "ld" ["-r", "-o", obj, obj ++ "2", stub]
        -- Get rid of the stub.o.  Otherwise if this rule fires again but
        -- ghc decides not to recompile, the old .o file will be left alone.
        -- Then the ld -r will try to merge the stub again and will fail with
        -- a duplicate symbol error.
        Trans.liftIO $ Directory.removeFile stub

compileHs :: Config -> FilePath -> Cmdline
compileHs config hs = ("GHC", hs,
    [ghcBinary, "-c"] ++ ghciFlags config ++ hcFlags (configFlags config)
        ++ main_is ++ [hs, "-o", srcToObj config hs])
    where
    main_is = if hs `elem` Map.elems nameToMain
        then ["-main-is", pathToModule hs]
        else []

linkHs :: Config -> FilePath -> [String] -> [FilePath] -> Cmdline
linkHs config output pkgs objs = ("LD-HS", output,
    ghcBinary : fltkLd flags ++ midiLibs flags ++ hLinkFlags flags
        ++ ["-lstdc++"]
        ++ map ("-package="++) pkgs
        ++ objs ++ ["-o", output])
    where flags = configFlags config

-- | ghci has to be called with the same flags that the .o files were compiled
-- with or it won't load them.
writeGhciFlags :: Config -> Config -> IO ()
writeGhciFlags debug test = do
    Directory.createDirectoryIfMissing True (buildDir debug)
    writeFile (buildDir debug </> "ghci-flags") $
        unwords (ghciFlags debug) ++ "\n"
    Directory.createDirectoryIfMissing True (buildDir test)
    writeFile (buildDir test </> "ghci-flags") $
        unwords (ghciFlags test) ++ "\n"

-- | Get the file-independent flags for a haskell compile.
ghciFlags :: Config -> [String]
ghciFlags config =
    -- -osuf is unnecessary because of the -o, but as of 7.4.1 ghci won't
    -- load the .o files if it notices this flag is different.
    [ "-osuf", ".hs.o", "-outputdir", oDir config
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
    system $ compileCc config cc obj

compileCc :: Config -> FilePath -> FilePath -> Cmdline
compileCc config cc obj = ("C++", obj,
    ["g++", "-c"] ++ ccFlags (configFlags config) ++ ["-o", obj, cc])

linkCc :: Config -> FilePath -> [FilePath] -> Cmdline
linkCc config binary objs = ("LD-CC", binary,
    "g++" : fltkLd (configFlags config) ++ ["-o", binary] ++ objs)

-- * hsc

hsRule :: Config -> Shake.Rules ()
hsRule config = hscDir config ++ "//*.hs" *> \hs -> do
    let hsc = hsToHsc (hscDir config) hs
    includes <- includesOf "hsRule" config hsc
    logDeps config "hsc" hs (hsc : includes)
    system $ hsc2hs config hs hsc

hsc2hs :: Config -> FilePath -> FilePath -> Cmdline
hsc2hs config hs hsc = ("hsc2hs", hs,
    ["hsc2hs", "-I" ++ ghcLib config </> "include"]
    -- Otherwise g++ complains about the offsetof macro hsc2hs uses.
    ++ words "-c g++ --cflag -Wno-invalid-offsetof"
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

hsToHsc :: FilePath -> FilePath -> FilePath
hsToHsc hscDir fn = dropDir hscDir $ FilePath.replaceExtension fn "hsc"

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

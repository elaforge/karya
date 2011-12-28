{-# LANGUAGE FlexibleContexts #-}
{- |
    from scratch (191 modules):
    runghc Shake/Shakefile.hs build/debug/seq  128.43s user 20.04s system 178% cpu 1:23.01 total
    no link: runghc Shake/Shakefile.hs build/debug/seq  118.92s user 19.21s system 249% cpu 55.383 total
    make -j3 build/seq  68.81s user 9.98s system 98% cpu 1:19.60 total

    modify nothing:
    runghc Shake/Shakefile.hs build/debug/seq  0.65s user 0.10s system 96% cpu 0.780 total
    make -j3 build/seq  6.05s user 1.21s system 85% cpu 8.492 total

    modify one file:
    runghc Shake/Shakefile.hs build/debug/seq  19.50s user 2.37s system 94% cpu 23.166 total
    make -j3 build/seq  12.81s user 1.85s system 94% cpu 15.586 total

    TODO
    * no rule to build util.h
    * build seq
    * if I do a clean build I get bogus files:
        CoreMidi_stub.c -> CoreMid_stub.hs.o, etc.
        the next build is fine
        Bug in ghc.  7.4 doesn't have it, but it has other problems.
        Problem with my shakefile was looking for _stub.c before needing the
        files that generated it.  Maybe 'XYZ_stub.{c,h}' should be marked as
        an output of 'XYZ.hs -> XYZ.{hi,o}'?
    * make build/hspp
        It's tricky because it should be opt even if nothing else is, and
        because every .hs file depends on it.
    * build other targets, share code with seq
    * automatically make binary targets from binaries
    * some way to automatically get _stub.c?
    * experiment with parallel
    * LogView conflicts with logview, put .os in build/debug/obj
    * put 'need' into *Deps functions
    - If I update build/test/RunTests.hs, it gets regenerated, even though
        it's newer than everything else.  Why?
        Also, if I update generate_run_tests.py it doesn't rebuild anything.
    * post hsc2hs should filter out INCLUDE
    * RunTests
    * individual test targets
    * RunProfile
    * make CcDeps transitive
    - chase #includes from .hsc
    - save .deps files
    - have configure use system' to rebuild if there are config changes?
        Wait, does system' even have that behaviour?
    - run again and it relinks sometimes?
        wait for --lint to look for errors
    - Mark .hi files as generated from .o files and depend on .hi files like
        ghc -M does.  Why do this instead of .o?  ghc will avoid updating the
        timestamp on the .hi file if things dependent on it don't need to be
        recompiled.

    Suggestions:
    - *>, **>, ?>, and the like should have low precedence so
        'dir ++ "*.hs" *> xyz' works as expected.  Maybe same as ($), though
        (<$>) might make sense.
    - If I rm build/debu/obj/Ui/* and then build seq, it locks up after
        printing the ***build line for seq.
    - Would be nice to export ==? from FilePattern.
    - Organized logging for each target and what it needs would be nice.

    - After pulling patches that renamed a file, I got "file does not exist"
    for the old name.  On the second run (after shake.database was corrupted,
    as usual), the error went away, so probably the old name was preserved in
    the db.

    - It would be nice to see which thread each task was run as, to get an
    idea of where parallelism is happening.

    - When I build a binary, the first run after that relinks even though
    nothing changed.  The next run after that does nothing, as I expect.
    I'll look into this some more, but I remember this behaviour from
    openshake too so I'm guessing it's something to do with my shakefile
    and shake's database approach.
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

import Development.Shake
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Info
import qualified System.Process as Process

import qualified Shake.CcDeps as CcDeps
import qualified Shake.HsDeps as HsDeps
import qualified Shake.Util as Util
import Shake.Util (Cmdline, system)


-- * config

-- Static constants.
build = "build"
fltkConfig = "/usr/local/src/fltk-1.3/fltk-config"
ghcBinary = "ghc-7.0.3"
hspp = modeToDir Opt </> "hspp"

options :: ShakeOptions
options = shakeOptions
    { shakeFiles = build </> "shake"
    , shakeVerbosity = 2
    , shakeParallel = 1
    }

data Config = Config {
    buildDir :: FilePath
    , hscDir :: FilePath
    , ghcLib :: FilePath
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
instance Monoid.Monoid Flags where
    mempty = Flags [] [] [] [] [] [] [] [] []
    mappend (Flags a1 b1 c1 d1 e1 f1 g1 h1 i1)
            (Flags a2 b2 c2 d2 e2 f2 g2 h2 i2) =
        Flags (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2) (f1<>f2) (g1<>g2)
            (h1<>h2) (i1<>i2)

-- * binaries

-- This section has project specific hardcoded lists of files.

data HsBinary = HsBinary {
    hsName :: FilePath
    , hsMain :: FilePath -- ^ main module
    , hsDeps :: [FilePath] -- ^ additional deps, relative to obj dir
    , hsGui :: Bool -- ^ GUI apps need make_bundle
    } deriving (Show)

binaries :: [HsBinary]
binaries =
    [ gui "browser" "Instrument/Browser.hs" ["Instrument/browser_ui.cc.o"]
    , plain "dump" "App/Dump.hs"
    , plain "logcat" "LogView/LogCat.hs"
    , gui "logview" "LogView/LogView.hs" ["LogView/logview_ui.cc.o"]
    , plain "make_db" "Instrument/MakeDb.hs"
    , plain "pprint" "App/PPrint.hs"
    , plain "print_keymap" "App/PrintKeymap.hs"
    , plain "repl" "App/Repl.hs"
    , plain "send" "App/Send.hs"
    , gui "seq" "App/Main.hs" ["fltk/fltk.a"]
    , plain "test_core_midi" "Midi/TestCoreMidi.hs"
    , plain "timer" "LogView/Timer.hs"
    , plain "update" "App/Update.hs"
    ]
    where
    plain name path = HsBinary name path [] False
    gui name path deps = HsBinary name path deps True

-- | Module that define 'main' and should get linked to their own binaries,
-- and the names of their eventual binaries.
nameToMain :: Map.Map FilePath FilePath
nameToMain = Map.fromList [(hsName b, hsMain b) | b <- binaries]

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
    ++ "containers filepath transformers "
    ++ "haskeline" -- repl

fltkDeps :: Config -> [FilePath]
fltkDeps config = map (srcToObj config . ("fltk"</>))
    [ "Block.cc", "TrackTile.cc", "Track.cc", "Ruler.cc", "EventTrack.cc"
    , "MoveTile.cc", "P9Scrollbar.cc", "SimpleScroll.cc", "SeqInput.cc"
    , "MsgCollector.cc", "SkeletonDisplay.cc", "StyleTable.cc"
    , "SymbolTable.cc", "SymbolOutput.cc", "f_util.cc", "alpha_draw.cc"
    , "types.cc", "config.cc", "util.cc"
    ]

-- * mode

data Mode = Debug | Opt | Test | Profile deriving (Eq, Enum, Show)

modeToDir :: Mode -> FilePath
modeToDir mode = (build </>) $ case mode of
    Debug -> "debug"
    Opt -> "opt"
    Test -> "test"
    Profile -> "profile"

targetToMode :: FilePath -> Maybe Mode
targetToMode target = snd <$> List.find ((`List.isPrefixOf` target) . fst)
    (zip (map modeToDir [Debug ..]) [Debug ..])

configure :: Mode -> IO Config
configure mode = do
    ghcLib <- run ghcBinary ["--print-libdir"]
    fltkCs <- words <$> run fltkConfig ["--cflags"]
    fltkLds <- words <$> run fltkConfig ["--ldflags"]
    flags <- return $ osFlags
        { define = define osFlags ++ if mode `elem` [Test, Profile]
            then ["-DTESTING"] else []
        , cInclude = ["-I.", "-Ifltk"]
        , fltkCc = fltkCs ++ if mode == Opt then ["-O2"] else []
        , fltkLd = fltkLds ++ ["-threaded"]
        , hcFlags = words "-threaded -W -fwarn-tabs -pgml g++"
            ++ ["-pgmF", hspp]
            ++ case mode of
                Debug -> []
                Opt -> ["-O"]
                Test -> ["-fhpc"]
                Profile -> ["-O", "-prof", "-auto-all"]
        , hLinkFlags = ["-rtsopts"]
            ++ if mode == Profile then ["-prof", "-auto-all"] else []
        }
    flags <- return $ flags
        { ccFlags = fltkCc flags ++ define flags ++ cInclude flags ++ ["-Wall"]
        , hcFlags = hcFlags flags ++ define flags
        }
    return $ Config (modeToDir mode) (build </> "hsc") (strip ghcLib) flags
    where
    osFlags = case System.Info.os of
        "darwin" -> mempty
            { define = ["-DMAC_OS_X_VERSION_MAX_ALLOWED=1060",
                "-DMAC_OS_X_VERSION_MIN_REQUIRED=1050"]
            , midiLibs = words $ "-framework CoreFoundation "
                ++ "-framework CoreMIDI -framework CoreAudio"
            , midiDriver = ["-DCORE_MIDI"]
            }
        "linux" -> mempty
        unknown -> error $ "unknown os: " ++ show unknown
    -- TODO can I put this under system' to rebuild if there are changes?
    run cmd args = Process.readProcess cmd args ""


-- * rules

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    targets <- filter (not . ("-" `List.isPrefixOf`)) <$> Environment.getArgs
    let target = case targets of
            [target] -> target
            _ -> error "expected one argument"
    config <- configure $ Maybe.fromMaybe
        (error $ "no mode for target " ++ target) (targetToMode target)
    let bindir = (buildDir config </>)
        odir = (oDir config </>)
        s2o = srcToObj config
    putStrLn $ "build dir: " ++ buildDir config
    shake options $ do
        -- hspp is depended on by all .hs files.  To avoid recursion, I
        -- build hspp itself with --make.
        hspp *> \fn -> system $ makeHs (modeToDir Opt) fn "Util/Hspp.hs"
        odir "fltk/fltk.a" *> \fn -> do
            need (fltkDeps config)
            system' "ar" $ ["-rs", fn] ++ fltkDeps config
        bindir "test_block" *> \fn -> do
            let objs = [s2o "fltk/test_block.cc", odir "fltk/fltk.a"]
            need objs
            system $ linkCc config fn objs
            makeBundle fn
        forM_ binaries $ \binary -> bindir (hsName binary) *> \fn -> do
            hs <- maybe (errorIO $ "no main module for " ++ fn) return
                (Map.lookup (FilePath.takeFileName fn) nameToMain)
            buildHs config (map odir (hsDeps binary)) hs fn
            when (hsGui binary) $ makeBundle fn
        testRules config
        profileRules config
        hsRule config
        hsORule config
        ccORule config
        want [target]

makeHs :: FilePath -> FilePath -> FilePath -> Cmdline
makeHs dir out main = ("GHC-MAKE", out, cmdline)
    where
    cmdline = [ghcBinary, "--make", "-outputdir", dir, "-O2", "-o", out,
        "-main-is", pathToModule main, main]

-- | Build a haskell binary.
buildHs :: Config -> [FilePath] -> FilePath -> FilePath -> Action ()
buildHs config deps hs fn = do
    srcs <- HsDeps.transitiveImportsOf hs
    let ccs = List.nub $
            concat [Map.findWithDefault [] src hsToCc | src <- srcs]
        objs = deps ++ List.nub (map (srcToObj config) (ccs ++ srcs))
    logDeps config "build" fn objs
    need objs
    -- I could put a dep on .c.o for the stubs, but then I need a separate
    -- .c.o rule that will work in the build dir and the right flags, and
    -- ghc seems happy to compile it for me.
    stubs <- Maybe.catMaybes <$> mapM (HsDeps.findStub (oDir config)) srcs
    system $ linkHs config fn packages (stubs ++ objs)

makeBundle :: FilePath -> Action ()
makeBundle binary
    | System.Info.os == "darwin" = system' "tools/make_bundle" [binary]
    | otherwise = return ()

-- * tests and profiles

-- | Generate RunTests.hs and compile it.
testRules :: Config -> Rules ()
testRules config = do
    let binPrefix = modeToDir Test </> "RunTests"
    (binPrefix ++ "*.hs") *> generateTestHs "_test"
    matchBinary binPrefix ?> \fn -> do
        -- The UI tests use fltk.a.  It would be nicer to have it automatically
        -- added when any .o that uses it is linked in.
        buildHs config [oDir config </> "fltk/fltk.a"] (fn ++ ".hs") fn
        -- This sticks around and breaks hpc.
        system' "rm" ["-f",
            FilePath.replaceExtension "tix" (FilePath.takeFileName fn)]
        -- This gets reset on each new test run.
        system' "rm" ["-f", "test.output"]

profileRules :: Config -> Rules ()
profileRules config = do
    let binPrefix = modeToDir Profile </> "RunProfile"
    (binPrefix ++ "*.hs") *> generateTestHs "_profile"
    matchBinary binPrefix ?> \fn -> do
        buildHs config [oDir config </> "fltk/fltk.a"] (fn ++ ".hs") fn

-- | Match any filename that starts with the given prefix but doesn't have
-- an extension, i.e. binaries.
matchBinary :: FilePath -> FilePath -> Bool
matchBinary prefix fn =
    prefix `List.isPrefixOf` fn && null (FilePath.takeExtension fn)

generateTestHs :: FilePath -> FilePath -> Action ()
generateTestHs hsSuffix fn = do
    let pattern = drop 1 (dropWhile (/='-') (FilePath.dropExtension fn))
        matches = (pattern `List.isInfixOf`) . FilePath.takeFileName
    tests <- Util.findHs
        (\hs -> (hsSuffix ++ ".hs") `List.isSuffixOf` hs && matches hs) "."
    when (null tests) $
        errorIO $ "no tests match pattern: " ++ pattern
    need ["test/generate_run_tests.py"]
    system' "test/generate_run_tests.py" (fn : tests)

-- * hs

hsORule :: Config -> Rules ()
hsORule config = "//*.hs.o" *> \obj -> do
    isHsc <- Trans.liftIO $
        Directory.doesFileExist (objToSrc config obj ++ "c")
    let hs = if isHsc then objToHscHs config obj else objToSrc config obj
    need [hspp]
    imports <- HsDeps.importsOf hs
    let objs = map (srcToObj config) imports
    logDeps config "hs" obj (hs:objs)
    need objs
    system $ compileHs config hs
    -- A bug in ghc 7.0.3 causes the compiled stub .o files to have a
    -- strange name when -o is used:
    -- .../BlockC.hs.o -> .../BlockC.hs_stub.o
    -- should be .../BlockC_stub.o
    -- I don't this makes a difference since buildHs includes the .c on
    -- the ghc cmdline, bypassing the .o, but I might as well put it in
    -- the right place.
    let stub = FilePath.dropExtension (srcToObj config hs) ++ "_stub.o"
        shouldBe = ((++"_stub.o") . reverse . drop (length ".hs_stub.o")
                . reverse) stub
    Util.whenM (Trans.liftIO $ Directory.doesFileExist stub) $
        system' "mv" [stub, shouldBe]

compileHs :: Config -> FilePath -> Cmdline
compileHs config hs = ("GHC", hs,
    [ghcBinary, "-c", "-outputdir", oDir config, "-i" ++ includes]
    ++ main_is ++ hcFlags (configFlags config)
    ++ [hs, "-o", srcToObj config hs])
    where
    includes = oDir config ++ ":" ++ hscDir config ++ ":."
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

-- * cc

ccORule :: Config -> Rules ()
ccORule config = "//*.cc.o" *> \obj -> do
    let cc = objToSrc config obj
    let dirs = [dir | '-':'I':dir <- cInclude (configFlags config)]
    (deps, not_found) <- CcDeps.transitiveIncludesOf dirs cc
    when (not (null not_found)) $
        Trans.liftIO $ putStrLn $
            "WARNING: c includes not found: " ++ show not_found
    logDeps config "cc" obj (cc:deps)
    need deps
    system $ compileCc config cc obj

compileCc :: Config -> FilePath -> FilePath -> Cmdline
compileCc config cc obj = ("C++", obj,
    ["g++", "-c"] ++ ccFlags (configFlags config) ++ ["-o", obj, cc])

linkCc :: Config -> FilePath -> [FilePath] -> Cmdline
linkCc config binary objs = ("LD-CC", binary,
    "g++" : fltkLd (configFlags config) ++ ["-o", binary] ++ objs)

-- * hsc

hsRule :: Config -> Rules ()
hsRule config = (hscDir config ++ "//*.hs") *> \hs -> do
    let hsc = hsToHsc (hscDir config) hs
    logDeps config "hsc" hs [hsc]
    need [hsc]
    system $ hsc2hs config hs hsc

hsc2hs :: Config -> FilePath -> FilePath -> Cmdline
hsc2hs config hs hsc = ("hsc2hs", hs,
    -- My special local version of hsc2hs that doesn't emit INCLUDEs.
    ["/usr/local/bin/hsc2hs", "-I" ++ ghcLib config </> "include"]
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

logDeps :: Config -> String -> FilePath -> [FilePath] -> Action ()
logDeps config stage fn objs = Trans.liftIO $ putStrLn $
    "***" ++ stage ++ ": " ++ fn ++ " <- " ++
        unwords (map (dropDir (oDir config)) objs)

(<>) = Monoid.mappend

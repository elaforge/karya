-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
-- | Shakefile for seq and associated binaries.
module Shake.Shakefile where
import qualified Control.DeepSeq as DeepSeq
import           Control.Monad.Trans (liftIO)
import qualified Data.Binary as Binary
import qualified Data.Char as Char
import qualified Data.Hashable as Hashable
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable

import qualified Development.Shake as Shake
import           Development.Shake (need, (%>), (&?>), (?==), (?>))
import qualified System.Console.Concurrent as Concurrent
import qualified System.Console.Regions as Regions
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error
import qualified System.Posix as Posix
import qualified System.Process as Process

import qualified Util.Exceptions as Exceptions
import qualified Util.PPrint as PPrint
import qualified Util.Seq as Seq
import qualified Util.SourceControl as SourceControl

import           Local.ShakeConfig (localConfig)
import qualified Shake.C as C
import qualified Shake.CcDeps as CcDeps
import qualified Shake.Config as Config
import qualified Shake.HsDeps as HsDeps
import qualified Shake.Progress as Progress
import qualified Shake.Util as Util

import           Control.Monad


-- * config

-- ** packages

-- | Package, with or without version e.g. containers-0.5.5.1
type Package = String

-- | This is the big list of enabled packages.
allPackages :: [Package]
allPackages = map fst enabledPackages

-- NOTE:
-- Remember to run tools/freeze_deps.hs after changing any of these.

-- | This is used to create karya.cabal and supply -package arguments to ghc.
basicPackages :: [(Package, String)]
basicPackages = concat
    -- really basic deps
    [ [("base", ">=4.6"), ("containers", ">=0.5")]
    , w "directory filepath process bytestring time unix array"
    , w "ghc-prim primitive"
    --  basic
    , w "deepseq data-ordlist cereal random text stm network"
    , [("extra", ">=1.3")]
    , [("exceptions", "")] -- only ghc 9
    , w "unordered-containers"
    , [("transformers", ">=0.4"), ("mtl", ">=2.2.1")]
    , w "writer-cps-mtl"
    , w "vector utf8-string"
    , w "c-storable"
    -- shakefile
    , [("shake", ">=0.16")]
    , w "binary hashable concurrent-output"
    -- Util
    , [("Diff", ">=0.4")] -- Util.Test
    , w "QuickCheck" -- Util.Test
    , [("pcre-light", ">=0.4"), ("pcre-heavy", ">=0.2")] -- Util.Regex
    , w "ansi-terminal colour" -- Util.StyledText
    , w "async" -- Util.Processes
    , w "dlist" -- Util.TimeVector
    , w "hedgehog" -- Util.Test
    , w "pretty haskell-src" -- Util.PPrint
    , w "streaming"
    , w "wcwidth" -- Util.Format
    , w "zlib" -- Util.File
    -- karya
    , w "attoparsec" -- Derive: tracklang parsing
    , w "old-locale"
    , w "hlibgit2"
    , [("fclabels", ">=2")]
    , w "ghc ghc-paths haskeline terminfo" -- REPL
    -- Derive: score randomization
    , w "mersenne-random-pure64 random-shuffle"
    -- Has better errors that attoparsec.  TODO: try replacing attoparsec with
    -- this, see if it's fast enough.
    , w "megaparsec parser-combinators"
    , [("zmidi-core", ">=0.6")] -- for Cmd.Load.Midi
    , [("aeson", ">=1.1.0.0")] -- serialize and unserialize log msgs
    , w "med-module" -- for Cmd.Load.Med
    , w "base64-bytestring" -- for hashes in incremental rendering

    , [("ghc-events", ">=0.15")]
    ]
    where w = map (\p -> (p, "")) . words

-- | Packages needed only for targets in Synth.
imPackages :: [(Package, String)]
imPackages = concat
    [ w "hsndfile hsndfile-vector"
    , w "cryptohash-md5" -- Synth.Shared.Note.hash
    , w "resourcet"
    , w "bindings-portaudio"
    , w "vivid-osc vivid-supercollider" -- Perform.Sc
    ]
    where w = map (\p -> (p, "")) . words

-- These have im-specific deps that might not be installed
-- TODO NOTE [no-package]
requiresSynthPackages :: FilePath -> Bool
requiresSynthPackages hs =
    any (`List.isPrefixOf` hs) ["Synth/", "Ness/", "Util/Audio/"]

-- | These are used in the Ness.* hierarchy, which probably only I use, and
-- only from ghci, so I can omit the deps from common use.
nessPackages :: [(Package, String)]
nessPackages = concat
    [ w "conduit-audio conduit-audio-sndfile conduit-audio-samplerate"
    ]
    where w = map (\p -> (p, "")) . words

enabledPackages :: [(Package, String)]
enabledPackages = concat
    [ basicPackages
    , if Config.enableIm localConfig then imPackages else []
    , if Config.enableEkg localConfig then ekgPackages else []
    ]

-- | All packages, not just enabled ones.
reallyAllPackages :: [(Package, String)]
reallyAllPackages = concat
    [ basicPackages
    , imPackages
    , ekgPackages
    -- conduit-audio is heavy, if I re-use ness I can port to Util.Audio.
    -- , nessPackages
    ]

ekgPackages :: [(Package, String)]
ekgPackages = [("ekg", "")]

-- | This is a hack so I can add packages that aren't in 'enabledPackages'.
-- This is for packages with tons of dependencies that I usually don't need.
extraPackagesFor :: FilePath -> [Package]
extraPackagesFor obj
    | (criterionHsSuffix <> ".o") `List.isSuffixOf` obj = ["criterion"]
    | otherwise = []

-- * config implementation

ghcBinary :: FilePath
ghcBinary = "ghc"

build :: FilePath
build = "build"

options :: [String] -> Shake.ShakeOptions
options args = Shake.shakeOptions
    { Shake.shakeFiles = build </> "shake"
    -- I have my own concurrent output, which shake will mess up if it prints
    -- its own output.  Unfortunately this also suppresses the --version flag,
    -- and some other less useful ones.
    , Shake.shakeVerbosity = Shake.Quiet
    , Shake.shakeReport = [build </> "report.html"]
    , Shake.shakeProgress =
        if verbose then const (return ()) else Progress.report
    -- Git branch checkouts change file timestamps, but not contents.
    -- But ghci only understands timestamp changes, not contents.
    , Shake.shakeChange = Shake.ChangeModtime
    }
    where
    -- This is stupid, but shake only lets me set options before parsing flags,
    -- and I only know the verbosity after parsing flags.
    verbose = "-V" `elem` args || "--verbose" `elem` args

data Config = Config {
    buildMode :: Mode
    , hscDir :: FilePath
    , chsDir :: FilePath
    , ghcLib :: FilePath
    , fltkVersion :: String
    , midiConfig :: MidiConfig
    , cLibs :: CLibs
    , configFlags :: Flags
    -- | GHC version as returned by 'parseGhcVersion'.
    , ghcVersion :: (Int, Int, Int)
    , ccVersion :: String
    -- | Absolute path to the root directory for the project.
    , rootDir :: FilePath
    } deriving (Show)

data CLibs = CLibs {
    _libfltk :: C.ExternalLibrary
    } deriving (Show)

buildDir :: Config -> FilePath
buildDir = modeToDir . buildMode

-- | Root of .o and .hi hierarchy.
oDir :: Config -> FilePath
oDir = (</> "obj") . buildDir

-- | Root for generated documentation.
buildDocDir :: FilePath
buildDocDir = build </> "doc"

-- | Root for documentation source.
docDir :: FilePath
docDir = "doc"

cabalDir :: FilePath
cabalDir = "doc/cabal"

-- * flags

type Flag = String

data Flags = Flags {
    -- | -D flags.  This is used by both g++ and ghc.
    define :: [Flag]
    -- | There's one global list of include dirs, for both haskell and C++.
    -- Technically they don't all need the same dirs, but it doesn't hurt to
    -- have unneeded ones.
    , cInclude :: [Flag]
    -- | Analogous to 'cInclude', this has -L flags, used when linking all
    -- C++ binaries.  TODO also use when linking hs?
    , cLibDirs :: [Flag]

    -- | Flags for g++.  This is the complete list and includes the 'define's
    -- and 'cInclude's.  This is global because all CcBinaries get these flags.
    , globalCcFlags :: [Flag]
    -- | Linker flags to link in whatever MIDI driver we are using today.
    -- There should be corresponding flags in 'define' to enable said driver.
    , midiLd :: [Flag]
    -- | GHC-specific flags.  Unlike 'globalCcFlags', this *isn't* the complete
    -- list.
    , hcFlags :: [Flag]
    -- | Flags needed when linking haskell.  Doesn't include the -packages.
    , hLinkFlags :: [Flag]
    -- | Package DB flags to use a cabal sandbox, if there is one.
    , sandboxFlags :: [Flag]
    -- | -package-db flags for ghci-flags.  This comes from GHC_PACKAGE_PATH,
    -- as set by tools/use-stack, and that's enough for ghc, but I need the
    -- explicit flags for the GHC API.
    , packageDbFlags :: [Flag]
    } deriving (Show)

-- TODO surely there is a GHC.Generic way to do this
instance Semigroup Flags where
    (<>)    (Flags a1 b1 c1 d1 e1 f1 g1 h1 i1)
            (Flags a2 b2 c2 d2 e2 f2 g2 h2 i2) =
        Flags (a1<>a2) (b1<>b2) (c1<>c2) (d1<>d2) (e1<>e2) (f1<>f2) (g1<>g2)
            (h1<>h2) (i1<>i2)

instance Monoid Flags where
    mempty = Flags [] [] [] [] [] [] [] [] []
    mappend = (<>)

-- * binaries

-- This section has project specific hardcoded lists of files.

-- ** hs

{- | Describe a single haskell binary.  The dependencies are inferred by
    chasing imports.
-}
data HsBinary = HsBinary {
    hsName :: FilePath
    , hsMain :: FilePath -- ^ main module
    , hsDeps :: [FilePath] -- ^ additional deps, relative to obj dir
    , hsGui :: GuiType
    , hsRtsFlags :: [Flag]
    } deriving (Show)

-- | RTS flags for generated binaries without an explicit target in
-- 'hsBinaries', like tests and profiles.
defaultRtsFlags :: [Flag]
defaultRtsFlags = ["-N", "-T"]

-- | GUI apps require some postprocessing.
data GuiType =
    NoGui -- ^ plain app
    | MakeBundle -- ^ run make_bundle on mac
    | HasIcon -- ^ run make_bundle, and add an icon from build/name.icns
    deriving (Show, Eq)

hsBinaries :: [HsBinary]
hsBinaries =
    [ gui "browser" "Instrument/Browser.hs" ["Instrument/browser_ui.cc.o"]
    , plain "import_query" "Shake/ImportQueryMain.hs"
    , plain "convert_event_log" "App/ConvertEventLog.hs"
    , plain "dump" "App/Dump.hs"
    -- ExtractDoc wants the global keymap, which winds up importing cmds that
    -- directly call UI level functions.  Even though it doesn't call the
    -- cmds, they're packaged together with the keybindings, so I wind up
    -- having to link in all that stuff anyway.
    , (plain "extract_doc" "App/ExtractDoc.hs") { hsDeps = ["fltk/fltk.a"] }
    , plain "extract_korvais" "Solkattu/ExtractKorvais.hs"
    , plain "generate_run_tests" "Util/Test/GenerateRunTests.hs"
    , plain "im_gc" "Synth/ImGcMain.hs"
    , plain "linkify" "Util/Linkify.hs"
    , plain "logcat" "LogView/LogCat.hs"
    , gui "logview" "LogView/LogView.hs" ["LogView/logview_ui.cc.o"]
    , plain "make_db" "Instrument/MakeDb.hs"
    , plain "mixdown" "Synth/MixDown.hs"
    , plain "ness-submit" "Ness/Submit.hs"
    , plain "pprint" "App/PPrint.hs"
    , plain "repl" "App/Repl.hs"
    , plain "resample" "Util/Audio/ResampleMain.hs"
    , (gui "seq" "App/Main.hs" ["fltk/fltk.a"])
        { hsRtsFlags =
            [ "-N"
            -- Increase generation 0 size.  Informal tests with
            -- verify_performance seem to show a significant speed up.
            , "-A8m"
            -- Enable GC stats.  It's pretty cheap and is used by EKG,
            -- MemoryLeak_test, and LDebug.memory.
            , "-T"
            ]
        }
    , plain "send" "App/Send.hs"
    , (plain "shakefile" "Shake/Shakefile.hs")
        -- Turn off idle gc, and parallel gc, as recommended by the shake docs.
        { hsRtsFlags = ["-N", "-I0", "-qg", "-qb"] }
    , plain "show_timers" "LogView/ShowTimers.hs"
    , plain "stream_audio" "Synth/StreamAudioMain.hs"
    , plain "test_midi" "Midi/TestMidi.hs"
    , plain "tscore" "Derive/TScore/TScoreMain.hs"
    , plain "update" "App/Update.hs"
    , (plain "verify_performance" "App/VerifyPerformance.hs")
        { hsRtsFlags = ["-N", "-A8m"] }
    ]
    ++ if not (Config.enableIm localConfig) then [] else
        [ plain "sampler-im" "Synth/Sampler/SamplerIm.hs"
        , plain "faust-im" "Synth/Faust/FaustIm.hs"
        ]
    where
    plain name main = HsBinary
        { hsName = name
        , hsMain = main
        , hsDeps = []
        , hsGui = NoGui
        , hsRtsFlags = ["-N"]
        }
    gui name main deps = HsBinary
        { hsName = name
        , hsMain = main
        , hsDeps = deps
        , hsGui = HasIcon
        , hsRtsFlags = ["-N"]
        }

runProfile :: FilePath
runProfile = modeToDir Profile </> "RunProfile"

-- | This is run as a test, but must be compiled with optimization like
-- a profile.
runProfileTest :: FilePath
runProfileTest = modeToDir Profile </> "RunProfile-Cmd.MemoryLeak"

runTests :: FilePath
runTests = modeToDir Test </> "RunTests"

-- | Hardcoded list of files that should be processed with CPP when chasing
-- deps.
cppFlags :: Config -> FilePath -> Maybe [String]
cppFlags config fn
    | fn `Set.member` cppInImports = Just $
        cInclude (configFlags config) ++ define (configFlags config)
    | otherwise = Nothing

-- | Hardcoded list of modules that use CPP to determine their imports.  This
-- means I need to CPP the file first before tracking the dependencies.
--
-- It would be more robust to always run CPP if the file includes
-- 'LANGUAGE .*CPP' but there aren't many modules with CPP in their import
-- lists so it should be faster to hardcode them.
--
-- TODO this is error-prone, maybe I should have a hack in HsDeps to look for
-- #include in the import block.
-- TODO this is also needed if I use #defines, but why don't I always chase
-- includes?
cppInImports :: Set.Set FilePath
cppInImports = Set.fromList
    [ "App/Main.hs"
    , "Cmd/Repl.hs"
    , "Midi/MidiDriver.hs"
    , "App/LoadInstruments.hs"
    ]

-- | Generated src files.
generatedSrc :: HsDeps.Generated
generatedSrc = HsDeps.Generated
    { _generatedHs = Set.fromList [generatedKorvais, generatedFaustAll]
    , _generatedExtensions = [".hsc", ".chs"]
    }

-- | Module that define 'main' and should get linked to their own binaries,
-- and the names of their eventual binaries.
nameToMain :: Map FilePath FilePath
nameToMain = Map.fromList [(hsName b, hsMain b) | b <- hsBinaries]

-- | Haskell files that use the FFI have dependencies on C++ source.
-- I could figure this out automatically by looking for @foreign import ...@
-- and searching for a neighboring .cc file with those symbols, but it's
-- simpler to give the dependency explicitly.  TODO a somewhat more modular way
-- would be a magic comment that declares a dependency on a C file.
hsToCc :: Map FilePath [FilePath]
hsToCc = Map.fromList $
    [ ("Midi/CoreMidi.hs", ["Midi/core_midi.cc"])
    , ("Midi/JackMidi.hsc", ["Midi/jack.cc"])
    , ("LogView/LogViewC.hsc", ["LogView/interface.cc"])
    , ("Instrument/BrowserC.hsc", ["Instrument/interface.cc"])
    , ("Util/Fltk.hs", ["Util/fltk_interface.cc"])
    , ("Synth/Faust/PatchC.hs", map ("Synth/Faust"</>) ["patch_c.cc"])
    , ("Util/VectorC.hs", ["Util/vectorc.cc"])
    ] ++ map (, ["Ui/c_interface.cc"]) c_interface
    where
    c_interface =
        [ "Ui/BlockC.hsc", "Ui/KeycapsC.hsc", "Ui/RulerC.hsc", "Ui/StyleC.hsc"
        , "Ui/SymbolC.hsc", "Ui/TrackC.hsc", "Ui/UiMsgC.hsc"
        ]

criterionHsSuffix :: FilePath
criterionHsSuffix = "_criterion.hs"

-- ** cc

ccDeps :: Config -> C.Binary config -> [FilePath]
ccDeps config binary = map (oDir config </>) (C.binObjs binary)

libsamplerate :: C.ExternalLibrary
libsamplerate = Config.libsamplerate localConfig

libsndfile :: C.ExternalLibrary
libsndfile = C.library "sndfile"

-- I don't quite like this, because now everything is dependent on Config,
-- when I want minimal dependency.  Return strings with ${var} in them would
-- delay the dependency.
ccBinaries :: [C.Binary Config]
ccBinaries =
    [ (fltk "test_block" ["fltk/test_block.cc.o", "fltk/fltk.a"])
        { C.binLibraries = \c -> [libsamplerate, libsndfile, libfltk c] }
    , fltk "test_browser"
        [ "Instrument/test_browser.cc.o", "Instrument/browser_ui.cc.o"
        , "fltk/f_util.cc.o"
        ]
    , fltk "test_logview"
        [ "LogView/test_logview.cc.o", "LogView/logview_ui.cc.o"
        , "fltk/f_util.cc.o"
        ]
    ] ++ if not (Config.enableIm localConfig) then [] else
    [ playCacheBinary
    , pannerBinary
    , makePlayCacheBinary "test_play_cache" "test_play_cache.cc" []
    ]
    where
    libfltk = _libfltk . cLibs
    fltk name objs = (C.binary name objs)
        { C.binLibraries = \config -> [libfltk config]
        , C.binPostproc = makeBundle False
        }

-- TODO This compiles under linux, but I have no idea if it actually produces
-- a valid vst.
playCacheBinary :: C.Binary Config
playCacheBinary =
    addVstFlags $ makePlayCacheBinary "play_cache" "PlayCache.cc" []

pannerBinary :: C.Binary Config
pannerBinary = addVstFlags $ C.binary "panner" ["Synth/play_cache/Panner.cc.o"]

-- | Add all the gizmos to make a VST.
addVstFlags :: C.Binary Config -> C.Binary Config
addVstFlags binary = binary
    { C.binName = C.binName binary <> soname
    , C.binObjs = "Synth/vst2/interface.cc.o" : C.binObjs binary
    , C.binLink = \c -> C.binLink binary c ++ case Util.platform of
        Util.Mac -> ["-bundle"]
        Util.Linux -> ["-shared", "-Wl,-soname=" <> soname]
    , C.binCompile = \c -> concat
        [ C.binCompile binary c
        -- TODO I need config for VST_BASE_DIR
        -- that means I need to split out rootDir to a independent Config
        -- that could even be static, then I don't need an argument.
        , ["-DVST_BASE_DIR=\"" <> (rootDir c </> "im") <> "\""]
        , case Util.platform of
            Util.Mac -> []
            Util.Linux -> ["-fPIC"]
        ]
    , C.binPostproc = \fn -> do
        makeBundle_ False BNDL False fn
        -- This is weird because the output is thing.vst, but I build with
        -- thing.  Really it should be thing.vst is built from thing via
        -- makeBundle, instead of this postproc thing.
        Util.system "touch" [fn] -- else shake gets upset
    }
    where
    soname = case Util.platform of
        Util.Mac -> ""
        Util.Linux -> ".so"

makePlayCacheBinary :: String -> FilePath -> [FilePath] -> C.Binary config
makePlayCacheBinary name main objs = (C.binary name [])
    { C.binObjs = (objs++) $ map (("Synth/play_cache"</>) . (++".o")) $
        [ main
        , "Thru.cc", "Resample.cc", "Sample.cc", "Streamer.cc", "Tracks.cc"
        , "ringbuffer.cc"
        ]
    , C.binLibraries = const $
        [ libsndfile
        , case Util.platform of
            -- This is the system libsamplerate, not the hacked static one at
            -- 'libsamplerate'.  The reason is that linux doesn't like to put
            -- a .a lib in .so, it wants me to recompile with -fPIC.  In any
            -- case, play_cache doesn't need hacked libsamplerate.
            Util.Linux -> C.library "samplerate"
            -- Meanwhile OS X doesn't seem to care, so just use the same one.
            Util.Mac -> libsamplerate
        ] ++ [C.library "pthread" | Util.platform == Util.Linux]
    }


{- | Since fltk.a is a library, not a binary, I can't just chase includes to
    know all the source files.  I could read fltk/*.cc at runtime, but the fltk
    directory changes so rarely it seems not a great burden to just hardcode
    them all here.

    'ccORule' has a special hack to give these '_libfltk' flags, since I don't
    have a separate CcLibrary target.
-}
fltkDeps :: Config -> [FilePath]
fltkDeps config = map (srcToObj config . ("fltk"</>))
    [ "AbbreviatedInput.cc"
    , "Block.cc"
    , "CachedScroll.cc"
    , "Color.cc"
    , "EventTrack.cc"
    , "FloatingInput.cc"
    , "Keycaps.cc"
    , "MoveTile.cc"
    , "MsgCollector.cc"
    , "PeakCache.cc"
    , "RulerOverlay.cc"
    , "RulerTrack.cc"
    , "Scrollbar.cc"
    , "Selection.cc"
    , "SelectionOverlay.cc"
    , "SimpleScroll.cc"
    , "SkeletonDisplay.cc"
    , "StyleTable.cc"
    , "SymbolOutput.cc"
    , "SymbolTable.cc"
    , "Track.cc"
    , "TrackTile.cc"
    , "WrappedInput.cc"
    , "alpha_draw.cc"
    , "config.cc"
    , "f_util.cc"
    , "input_util.cc"
    , "types.cc"
    , "utf8.cc"
    , "util.cc"
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

data MidiConfig = StubMidi | JackMidi | CoreMidi deriving (Show, Eq)

ghcWarnings :: Config -> [String]
ghcWarnings config = concat
    [ ["-W", "-Wcompat"]
    -- pass -Wundef to CPP for warnings on #if TYPO
    , ["-Wcpp-undef" | ghcVersion config >= (8, 2, 0)]
    , map ("-W"++) warns
    , map ("-Wno-"++) noWarns
    ]
    where
    warns =
        [ "identities"
        , "incomplete-record-updates"
        , "missing-fields"
        -- Check compatibility with
        -- https://ghc.haskell.org/trac/ghc/wiki/Proposal/MonadOfNoReturn
        , "noncanonical-monad-instances"
        -- The 8.2.1 docs claim it's on by default, but it's not.
        , "redundant-constraints"
        , "tabs"
        , "unused-matches"
        , "wrong-do-bind"
        ] ++ ["partial-fields" | ghcVersion config >= (8, 4, 0)]
    noWarns
        -- TEST ifdefs can cause duplicate exports if they add X(..) to the
        -- X export.
        | buildMode config `elem` [Test, Profile] = ["duplicate-exports"]
        | otherwise = []

configure :: IO (Mode -> Config)
configure = do
    env <- Environment.getEnvironment
    let midi = midiFromEnv env
    (fltkVersion, fltkCs, fltkLds) <-
        configureFltk (Config.fltkConfig localConfig)
    ghcLib <- run ghcBinary ["--print-libdir"]
    let ghcVersion = parseGhcVersion ghcLib
    ccVersion <- run "cc" ["--version"]
    sandbox <- Util.sandboxPackageDb
    -- TODO this breaks if you run from a different directory
    rootDir <- Directory.getCurrentDirectory
    return $ \mode -> Config
        { buildMode = mode
        , hscDir = build </> "hsc"
        , chsDir = build </> "chs"
        , ghcLib = ghcLib
        , fltkVersion = fltkVersion
        , midiConfig = midi
        , cLibs = CLibs
            { _libfltk = C.ExternalLibrary
                { libLink = fltkLds
                , libCompile = fltkCs
                }
            }
        , configFlags = setGlobalCcFlags mode $ mconcat
            [ configFlags sandbox mode ghcVersion
                (lookup "GHC_PACKAGE_PATH" env)
            , osFlags midi
            ]
        , ghcVersion = ghcVersion
        , ccVersion = ccVersion
        , rootDir = rootDir
        }
    where
    configFlags sandbox mode ghcVersion ghcPackagePath = mempty
        { define = concat
            [ ["-DTESTING" | mode `elem` [Test, Profile]]
            , ["-DSTUB_OUT_FLTK" | mode == Test]
            , ["-DBUILD_DIR=\"" ++ modeToDir mode ++ "\""]
            , ["-DGHC_VERSION=" ++ ghcVersionMacro ghcVersion]
            , Config.extraDefines localConfig
            ]
        , cInclude = concat
            [ map ("-I"<>) [".", "" ++ modeToDir mode, "fltk"]
            -- Put this first to make sure I don't see the system header.
            -- TODO this breaks the idea of modular libraries, but the haskell
            -- flags really have to be global, because any difference in CPP
            -- flags causes ghc to not want to load .o files.
            , concat [C.libCompile libsamplerate | Config.enableIm localConfig]
            , concat [C.libCompile (Config.rubberband localConfig)
                | Config.enableIm localConfig]
            , map ("-I"<>) (Config.globalIncludes localConfig)
            ]
        , cLibDirs = map ("-L"<>) $ Config.globalLibDirs localConfig
        , hcFlags = concat
            -- This is necessary for ghci loading to work in 7.8.
            -- Except for profiling, where it wants "p_dyn" libraries, which
            -- don't seem to exist.
            [ ["-dynamic" | mode /= Profile]
            , case mode of
                Debug -> []
                Opt -> ["-O"]
                Test -> ["-fhpc"]
                Profile -> ["-O", "-prof"]
                    -- I use manual SCCs for accuracy, but auto ones can be
                    -- useful to figure out where to put manual ones.
                    -- ++ ["-fprof-auto-top"]
                    -- ++ ["-fprof-auto-exported"]
            ]
        , hLinkFlags = concat
            [ ["-rtsopts", "-threaded"]
            -- This has essentially no overhead.  A small initialization
            -- overhead which should go away in 9.4.
            , ["-eventlog" | Config.enableEventLog localConfig && mode == Opt]
            , ["-dynamic" | mode /= Profile]
            , ["-prof" | mode == Profile]
            , map ("-L"<>) (Config.globalLibDirs localConfig)
            , map ("-optl"<>) (Config.extraLinkFlags localConfig)
            ]
        , sandboxFlags = case sandbox of
            Nothing -> []
            Just path -> ["-no-user-package-db", "-package-db", path]
        , packageDbFlags = map ("-package-db="<>) $
            maybe [] (Seq.split ":") ghcPackagePath
        }
    -- This one breaks the monoid pattern because it groups other flags,
    -- which is because the flags are a mess and not in any kind of normal
    -- form.
    setGlobalCcFlags mode flags = flags
        { globalCcFlags = concat
            [ define flags
            , cInclude flags
            , case mode of
                Opt -> ["-O2"]
                _ -> []
            , ["-Wall"]
            -- I'd like to turn on -Wold-style-cast, but faust uses it a lot
            , ["-std=c++11"]
            , ["-fPIC"] -- necessary for ghci loading to work in 7.8
            -- Turn on Effective C++ warnings, which includes uninitialized
            -- variables.  Unfortunately it's very noisy with lots of false
            -- positives.  Also, this is only for g++.
            -- , ["-Weffc++"]
            ]
        }
    osFlags midi = case Util.platform of
        -- In C and C++ programs the OS specific defines like __APPLE__ and
        -- __linux__ are already defined, but ghc doesn't define them.
        Util.Mac -> mempty
            -- These apparently control which APIs are visible.  But they
            -- make it slightly more awkward for ghci since it needs the
            -- same flags to load .o files, and things seem to work without
            -- them, so I'll omit them for the time being.
            -- { define = ["-DMAC_OS_X_VERSION_MAX_ALLOWED=1060",
            --     "-DMAC_OS_X_VERSION_MIN_REQUIRED=1050"]
            { define = ["-D__APPLE__"]
            , midiLd = if midi == CoreMidi
                then frameworks ["CoreFoundation", "CoreMIDI", "CoreAudio"]
                else []
            -- librubberband uses this.  TODO I really need modular libraries!
            , hLinkFlags = if not (Config.enableIm localConfig) then []
                else frameworks ["Accelerate"]
            }
        Util.Linux -> mempty
            { midiLd = if midi == JackMidi || Config.enableIm localConfig
                -- -ljack is needed for PortAudio.initialize, or it will fail
                -- with errors like "Client name conflicts with another running
                -- client".  Why must jack be so unfriendly?
                then ["-ljack"]
                else []
            , define = ["-D__linux__"]
            }
    run cmd args = strip <$> Process.readProcess cmd args ""
    frameworks = concatMap (\f -> ["-framework", f])

configureFltk :: FilePath -> IO (String, [Flag], [Flag])
configureFltk fltkConfig = do
    fltkVersion <- run fltkConfig ["--version"]
    fltkCs <- filter wantCflag . words <$> run fltkConfig ["--cflags"]
    -- The problem is that I have to pass these flags to both ghc and
    -- gcc/clang.  I'm preprocessing these for ghc, but it's probably incorrect
    -- for gcc.  TODO: fix this, mangle for gcc only on the hs link line.
    -- TODO: newer fltk passes -pthread, which apparently just adds -lpthread
    -- and some defines, which are already in there.  This is extra weird
    -- because gcc passes to the linker via -Wl,xyz while ghc uses -optl=xyz.
    -- But ghc's -optl is not actually the linker, but the compiler.
    fltkLds <- map wrapLd . filter (/="-pthread") . words <$>
        run (Config.fltkConfig localConfig) ["--ldflags"]
    return (fltkVersion, fltkCs, fltkLds)
    where
    -- fltk-config --cflags started putting -g and -O2 in the flags, which
    -- messes up hsc2hs, which wants only CPP flags.
    wantCflag w = any (\c -> ('-':c:"") `List.isPrefixOf` w) ['I', 'D']
    -- I get -Wl,-rpath,/nix/store/... stuff from nix.
    -- TODO probably wrong?  See above.
    wrapLd flag
        | "-Wl," `List.isPrefixOf` flag = "-optl=" <> flag
        | otherwise = flag
    run cmd args = strip <$> Process.readProcess cmd args ""

-- | Flags used by both ghc and haddock.  This is unlike 'hcFlags', which is
-- used by ghc only, and vary based on Mode.
ghcGlobalFlags :: [Flag]
ghcGlobalFlags =
    -- There's no particular reason for --nomacro, except I don't use
    -- them, and I don't want to start unless for good reason.
    ["-pgmP", "cpphs --nomacro --cpp"] ++ ghcLanguageFlags
    -- https://gitlab.haskell.org/ghc/ghc/issues/17185

-- | Language extensions which are globally enabled.
ghcLanguageFlags :: [Flag]
ghcLanguageFlags = map ("-X"++)
    -- Pretty conservative, and useful.
    [ "BangPatterns"
    , "DeriveGeneric"
    -- This enables slightly more concise record initialization and doesn't
    -- seem to hurt anything.
    , "DisambiguateRecordFields"
    -- ghc-7.10 adds a new rule where you can't infer a signature you can't
    -- type.  OverloadedStrings combined with local definitions results in
    -- a lot of types like "IsString [a] => [a] -> ...", which results in
    -- "Non type-variable argument in the constraint: IsString [a]".
    , "FlexibleContexts"
    -- Allow instances on nested types
    , "FlexibleInstances"
    -- Just too useful.
    , "GeneralizedNewtypeDeriving"
    , "LambdaCase"
    , "MultiWayIf"
    -- Allow _s in numbers. Harmless, and the _s are nice.
    , "NumericUnderscores"
    -- Without this, it becomes really annoying to use Text everywhere.
    , "OverloadedStrings"
    , "ScopedTypeVariables"
    -- It's nicer than flip (,), but not worth using if you have to put in
    -- a LANGUAGE.
    , "TupleSections"
    -- Allow instances on fully applied type synonyms.
    , "TypeSynonymInstances"
    ]

-- | When using gcc I get these defines automatically, but I need to add them
-- myself for ghci.  But then c2hs complains about duplicate definitions, so
-- filter them back out for that.  Nothing you can't fix by layering on another
-- hack!
platformDefines :: [Flag]
platformDefines = ["-D__APPLE__", "-D__linux__"]

packageFlags :: Flags -> [Package] -> [Flag]
packageFlags flags packages =
    sandboxFlags flags ++ "-hide-all-packages" : map ("-package="++) packages

-- | Parse the GHC version out of the @ghc --print-libdir@ path.  Technically
-- I should probably use ghc --numeric-version, but I already have libdir so
-- let's not run ghc again.
parseGhcVersion :: FilePath -> (Int, Int, Int)
parseGhcVersion path =
    -- take 3 to avoid getting confused by versions like 8.0.1.20161213.
    parse $ take 3 $ Seq.split "." $
        drop 1 $ dropWhile (/='-') $ FilePath.takeFileName path
    where
    parse cs
        | all (all Char.isDigit) cs = case map read cs of
            a : b : c : _ -> (a, b, c)
            _ -> error $ "parseGhcVersion: can't parse " <> show path
        | otherwise = error $ "parseGhcVersion: can't parse " <> show path

-- | Generate a number CPP can compare.
ghcVersionMacro :: (Int, Int, Int) -> String
ghcVersionMacro (a, b, c) =
    dropWhile (=='0') $ concatMap (pad0 . show) [a, b, c]
    where
    pad0 [c] = '0' : c : []
    pad0 cs = cs

type InferConfig = FilePath -> Config

-- | Figure out the Config for a given target by looking at its directory.
inferConfig :: (Mode -> Config) -> InferConfig
inferConfig modeConfig = maybe (modeConfig Debug) modeConfig . targetToMode

-- * rules

main :: IO ()
main = Concurrent.withConcurrentOutput $ Regions.displayConsoleRegions $ do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    env <- Environment.getEnvironment
    modeConfig <- configure
    writeGhciFlags modeConfig
    makeDataLinks
    writeDeps cabalDir [("basic", basicPackages), ("im", imPackages)]
    args <- Environment.getArgs
    Shake.shakeArgsWith (options args) [] $ \[] targets -> return $ Just $ do
        cabalRule basicPackages "karya.cabal"
        cabalRule reallyAllPackages (cabalDir </> "all-deps.cabal")
        when (Config.enableIm localConfig) faustRules
        generateKorvais
        matchBuildDir hsconfigH ?> hsconfigHRule
        let infer = inferConfig modeConfig
        setupOracle env (modeConfig Debug)
        matchObj "fltk/fltk.a" ?> \fn -> do
            let config = infer fn
            need (fltkDeps config)
            Util.system "ar" $ ["-rs", fn] ++ fltkDeps config
        forM_ ccBinaries $ \binary -> matchBuildDir (C.binName binary)
                ?> \fn -> do
            let config = infer fn
            let objs = ccDeps config binary
            need objs
            let flags = cLibDirs (configFlags config)
                    ++ C.binCompileFlags binary config
                    ++ Config.extraLinkFlags localConfig
                    ++ C.binLinkFlags binary config
            Util.cmdline $ linkCc flags fn objs
            C.binPostproc binary fn
        forM_ hsBinaries $ \binary -> matchBuildDir (hsName binary) ?> \fn -> do
            let config = infer fn
            hs <- maybe (Util.errorIO $ "no main module for " ++ fn) return
                (Map.lookup (FilePath.takeFileName fn) nameToMain)
            buildHs config (hsRtsFlags binary) (map (oDir config </>)
                (hsDeps binary)) [] hs fn
            case hsGui binary of
                NoGui -> return ()
                MakeBundle -> makeBundle False fn
                HasIcon -> makeBundle True fn
        (build </> "*.icns") %> \fn -> do
            -- Build OS X .icns file from .iconset dir.
            let iconset = "doc/icon" </> nameExt fn "iconset"
            icons <- Shake.getDirectoryFiles "" [iconset </> "*"]
            need icons
            Util.system "iconutil" ["-c", "icns", "-o", fn, iconset]
        forM_ extractableDocs $ \fn ->
            fn %> extractDoc (modeConfig Debug)
        testRules (modeConfig Test)
        profileRules (modeConfig Profile)
        criterionRules (modeConfig Profile)
        criterionRules (modeConfig Test) -- for typecheck-ci
        markdownRule (buildDir (modeConfig Opt) </> "linkify")
        hsc2hsRule (modeConfig Debug) -- hsc2hs only uses mode-independent flags
        chsRule (modeConfig Debug)
        hsOHiRule infer
        ccORule infer
        dispatch modeConfig targets

-- ** oracle

newtype Question a = Question () deriving
    ( Show, Typeable.Typeable, Eq, Hashable.Hashable, Binary.Binary
    , DeepSeq.NFData
    )

data GhcQ deriving (Typeable.Typeable)
type instance Shake.RuleResult (Question GhcQ) = String

data FltkQ deriving (Typeable.Typeable)
type instance Shake.RuleResult (Question FltkQ) = String

data ReplQ deriving (Typeable.Typeable)
type instance Shake.RuleResult (Question ReplQ) = Bool

data MidiQ deriving (Typeable.Typeable)
type instance Shake.RuleResult (Question MidiQ) = String

setupOracle :: [(String, String)] -> Config -> Shake.Rules ()
setupOracle env config = do
    Shake.addOracle $ \(_ :: Question GhcQ) -> return (ghcLib config)
    Shake.addOracle $ \(_ :: Question FltkQ) -> return (fltkVersion config)
    -- Previously, linking ghc took so long it was worth linking without the
    -- REPL.  But dynamic linking is fast enough that I can reverse it, and
    -- eventually remove norepl if I never wind up using it.
    Shake.addOracle $ \(_ :: Question ReplQ) ->
        return ("norepl" `notElem` map fst env)
    Shake.addOracle $ \(_ :: Question MidiQ) -> return (midiDriver :: String)
    return ()
    where
    midiDriver = case midiFromEnv env of
        StubMidi -> "STUB_MIDI"
        JackMidi -> "JACK_MIDI"
        CoreMidi -> "CORE_MIDI"

midiFromEnv :: [(String, String)] -> MidiConfig
midiFromEnv env = case lookup "midi" env of
      Just "stub" -> StubMidi
      Just "jack" -> JackMidi
      Just "core" -> CoreMidi
      Just unknown -> error $ "midi driver should be stub, jack, or core: "
        ++ show unknown
      Nothing -> case Util.platform of
          Util.Mac -> CoreMidi
          Util.Linux -> JackMidi

-- ** misc rules

-- | Dynamically generated header.
hsconfigH :: FilePath
hsconfigH = "hsconfig.h"

hsconfigPath :: Config -> FilePath
hsconfigPath config = buildDir config </> hsconfigH

-- | Write a header to configure the haskell compilation.
--
-- It's in a separate file so that the relevant haskell files can include it.
-- This way only those files will recompile when the config changes.
hsconfigHRule :: FilePath -> Shake.Action ()
hsconfigHRule fn = do
    -- I probably don't need this because the oracles should notice changes,
    -- but it's cheap to run and writeFileChanged won't cause further
    -- rebuilding, so let's just run it.
    Shake.alwaysRerun
    useRepl <- Shake.askOracle (Question () :: Question ReplQ)
    useRepl <- return $ useRepl && targetToMode fn /= Just Test
    midiDriver <- Shake.askOracle (Question () :: Question MidiQ)
    Shake.writeFileChanged fn $ unlines
        [ "/* Created automatically by the shakefile. */"
        , "#ifndef __HSCONFIG_H"
        , "#define __HSCONFIG_H"
        , define useRepl "INTERPRETER_GHC"
        , define True midiDriver
        , define (Config.enableEkg localConfig) "USE_EKG"
        , define (Config.enableIm localConfig) "ENABLE_IM"
        , "#endif"
        ]
    where
    define b name = (if b then "#define " else "#undef ") ++ name

-- | Match a file in @build/<mode>/obj/@ or @build/<mode>/@.
matchObj :: Shake.FilePattern -> FilePath -> Bool
matchObj pattern fn =
    matchPrefix (map ((</> "obj") . modeToDir) allModes) pattern fn
    || matchPrefix (map modeToDir allModes) pattern fn

-- | Match a file in @build/<mode>/@.
matchBuildDir :: Shake.FilePattern -> FilePath -> Bool
matchBuildDir = matchPrefix (map modeToDir allModes)

matchPrefix :: [Shake.FilePattern] -> Shake.FilePattern -> FilePath -> Bool
matchPrefix prefixes pattern fn =
    case msum $ map (flip dropPrefix fn) prefixes of
        Nothing -> False
        Just rest -> pattern ?== dropWhile (=='/') rest

dispatch :: (Mode -> Config) -> [String] -> Shake.Rules ()
dispatch modeConfig targets
    | null targets = error $ "no mk targets, valid targets are:\n" <> unlines
        [ "- any target in build/{debug,opt,test,profile}/xyz"
        , "- tests-xyz where xyz is a RunTests-xyz target"
        , "- one of: " <> unwords (Map.keys specialTargets)
        ]
    | otherwise = do
        handled <- mapM hardcoded targets
        Shake.want [target | (False, target) <- zip handled targets]
    where
    allBinaries = map hsName hsBinaries ++ map C.binName ccBinaries
    hardcoded target
        | Just run <- Map.lookup target specialTargets = run
        | Just tests <- dropPrefix "tests-" target = action $ do
            need [runTestsTarget (Just tests)]
            Util.system "tools/run_tests" [runTestsTarget (Just tests)]
        | otherwise = return False
    specialTargets = Map.fromList
        -- I should probably run this in keepGoing mode, -k.
        [ ("validate",) $ action $ do
            -- Unfortunately, verify_performance is the only binary in
            -- opt, which causes most of the opt tree to build.  I could build
            -- a debug one, but debug deriving is really slow.
            let opt = (modeToDir Opt </>)
            needEverything [opt "verify_performance", runTests, runProfileTest]
            allTests
            Util.shell $ opt "verify_performance --out=build/verify\
                \ save/complete/*"
        , ("verify",) $ action $ do
            let opt = (modeToDir Opt </>)
            need [opt "verify_performance"]
            Util.shell $ opt "verify_performance --out=build/verify\
                \ save/complete/*"
        -- Compile everything, like validate but when I don't want to test.
        , ("typecheck",) $ action $ needEverything []
        -- Like typecheck, but compile everything as Test, which speeds things
        -- up a lot.  This is for running on CI, so also omit things I know
        -- won't build there.
        , ("typecheck-ci",) $ action needEverythingCI
        , ("binaries",) $ do
            Shake.want $ map (modeToDir Opt </>) allBinaries
            return True
        , ("clean",) $ action $ do
            -- The shake database will remain because shake creates it after the
            -- shakefile runs, but that's probably ok.
            Util.system "rm" ["-rf", build]
            Util.system "mkdir" [build]
        , ("doc",) $ action $ makeAllDocumentation modeConfig
        , ("haddock",) $ action $ makeHaddock modeConfig
        , ("hlint",) $ action $ hlint (modeConfig Debug)
        , ("md",) $ action $ need . map docToHtml =<< getMarkdown
        , ("profile",) $ action $ do
            need [runProfile]
            let with_scc = "-fprof-auto-top"
                    `elem` hcFlags (configFlags (modeConfig Profile))
            Util.system "tools/summarize_profile.py"
                [if with_scc then "scc" else "no-scc"]
        , ("show-debug",) $ action $ liftIO $ PPrint.pprint (modeConfig Debug)
        , ("show-opt",) $ action $ liftIO $ PPrint.pprint (modeConfig Opt)
        , ("tests",) $ action allTests
        -- Run tests with no tags.
        , ("tests-normal",) $ action fastTests
        ]
    action act = Shake.action act >> return True
    runTestsTarget tests = runTests ++ maybe "" ('-':) tests
    needEverything more = do
        criterion <- getCriterionTargets (modeConfig Profile)
        need $ map (modeToDir Debug </>) allBinaries
            ++ criterion ++ [runTests, runProfile] ++ more
    -- See typecheck-ci
    needEverythingCI = do
        -- TODO: include criterion in the deps to re-enable this.
        -- criterion <- getCriterionTargets (modeConfig Test)
        need $ map (modeToDir Test </>)
                (filter (`notElem` cantBuild) allBinaries)
            -- ++ criterion
            ++ [runTests]
            -- This is missing runProfile, but at the moment I can't be
            -- bothered to get that to compile in build/test.
        where
        cantBuild = [C.binName playCacheBinary]

fastTests :: Shake.Action ()
fastTests = do
    need [runTests]
    Util.system "tools/run_tests" [runTests, "^normal-"]

allTests :: Shake.Action ()
allTests = do
    need [runTests, runProfileTest]
    Util.system "tools/run_tests" [runTests, runProfileTest, "^normal-"]

hlint :: Config -> Shake.Action ()
hlint config = do
    hs <- liftIO $ getAllHs (Just config)
    need hs
    Util.systemKeepGoing "hlint" $
        [ "--report=" <> build </> "hlint.html"
        , "--cpp-define=TESTING"
        , "--cpp-include=" <> buildDir config
        ] ++ hs

-- ** doc

-- | Make all documentation.
makeAllDocumentation :: (Mode -> Config) -> Shake.Action ()
makeAllDocumentation modeConfig = do
    docs <- getMarkdown
    need $ extractableDocs ++ map docToHtml docs
    _ <- makeHaddock modeConfig
    return ()

-- | Docs produced by extract_doc.
extractableDocs :: [FilePath]
extractableDocs =
    map (buildDocDir </>) ["keymap.html", "calls.html", "scales.html"]

extractDoc :: Config -> FilePath -> Shake.Action ()
extractDoc config fn = do
    let bin = buildDir config </> "extract_doc"
    need [bin]
    let name = FilePath.takeFileName (FilePath.dropExtension fn)
    Util.shell $ unwords [bin, name, ">", fn]

getMarkdown :: Shake.Action [FilePath]
getMarkdown = map (docDir</>) <$> Shake.getDirectoryFiles docDir ["*.md"]

-- TODO This always generates haddock, even if no input files have changed.
-- I used to use Util.findHs in 'getAllHs', but it still always generated, so
-- using command all_hs.py is not the problem.
makeHaddock :: (Mode -> Config) -> Shake.Action [FilePath]
makeHaddock modeConfig = do
    let config = modeConfig Debug
    -- let packages = map fst (reallyAllPackages ++ nessPackages)
    let packages = map fst enabledPackages
    hs <- filter (wantsHaddock config) <$> liftIO (getAllHs (Just config))
    need $ hsconfigPath config : hs
    let flags = configFlags config
    interfaces <- liftIO $ getHaddockInterfaces packages
    entry <- liftIO $ either Util.errorIO return =<< SourceControl.current "."
    let title = mconcat
            [ "Karya, built on "
            , SourceControl.showDate (SourceControl._date entry)
            , " (patch ", SourceControl._hash entry, ")"
            ]
    let ghcFlags = concat
            [ define flags, cInclude flags
            , ghcGlobalFlags
            , packageFlags flags packages
            ]
    Util.system "haddock" $
        [ "--html"
        , "-B", ghcLib config
        , "--title=" <> Text.unpack title
        , "--hyperlinked-source"
        , "--prologue=doc/prologue"
        , "--package-name=karya" -- otherwise it warns incessantly
        -- Don't report every single function without a doc.
        , "--no-print-missing-docs"
        -- Source references qualified names as written in the doc.
        , "--qual=aliased"
        , "-o", build </> "haddock"
        ] ++ concat
        [ map ("--read-interface="<>) interfaces
        , map ("--optghc="<>) ghcFlags
        , hs
        ]
    return hs

-- | Get paths to haddock interface files for all the packages.
getHaddockInterfaces :: [Package] -> IO [String]
getHaddockInterfaces packages = do
    -- ghc-pkg annoyingly provides no way to get a field from a list of
    -- packages.
    interfaces <- forM packages $ \package -> Process.readProcess "ghc-pkg"
        ["field", package, "haddock-interfaces"] ""
    return $ map extract interfaces
    where extract = drop 1 . dropWhile (/=' ') . takeWhile (/='\n')

-- | Get all hs files in the repo, in their .hs form (so it's the generated
-- output from .hsc or .chs).
getAllHs :: Maybe Config -> IO [FilePath]
getAllHs mbConfig =
    filterHs . lines <$>
        Process.readProcess "git" ["ls-tree", "--name-only", "-r", "HEAD"] ""
    where
    filterHs fnames = concat $ hs : case mbConfig of
        Nothing -> [hsc, chs]
        Just config ->
            [ map (hscToHs (hscDir config)) hsc
            , map (chsToHs (chsDir config)) chs
            ]
        where
        (hs, fnames1) = get ".hs" fnames
        (hsc, fnames2) = get ".hsc" fnames1
        (chs, _fnames3) = get ".chs" fnames2
        get suffix = List.partition (suffix `List.isSuffixOf`)

-- | Should this module have haddock documentation generated?
wantsHaddock :: Config -> FilePath -> Bool
wantsHaddock config hs = not $ or $
    [ "_test.hs" `List.isSuffixOf` hs
    , "_profile.hs" `List.isSuffixOf` hs
    , "_criterion.hs" `List.isSuffixOf` hs
    -- This will crash hsc2hs on OS X since jack.h is likely not present.
    -- TODO NOTE [no-package]
    , midi /= JackMidi && hs == hscToHs (hscDir config) "Midi/JackMidi.hsc"

    -- Omit test util modules as well.  This is because UiTest has
    -- #ifndef TESTING #error in it to prevent imports from non-tests, but
    -- if I run haddock with -DTESTING, the extra module exports cause tons
    -- of duplicate haddock.  Haddock for test utils is not so important, so
    -- let's just omit them.
    , "Test.hs" `List.isSuffixOf` hs
    , "TestInstances.hs" `List.isSuffixOf` hs
    , hs == "Derive/DeriveQuickCheck.hs"
    ] ++ if Config.enableIm localConfig then [] else [requiresSynthPackages hs]
    where midi = midiConfig config

-- ** packages

cabalRule :: [(Package, String)] -> FilePath -> Shake.Rules ()
cabalRule packages fn = (>> Shake.want [fn]) $ fn %> \_ -> do
    Shake.alwaysRerun
    template <- Shake.readFile' (cabalDir </> "karya.cabal.template")
    Shake.writeFileChanged fn $ template ++ buildDepends ++ "\n"
    where
    indent = replicate 8 ' '
    buildDepends = (indent<>) $ List.intercalate (",\n" ++ indent) $
        List.sort $ map mkline packages
    mkline (package, constraint) =
        package ++ if null constraint then "" else " " ++ constraint

-- ** hs

-- | Build a haskell binary.
buildHs :: Config -> [Flag] -> [FilePath] -> [Package] -> FilePath -> FilePath
    -> Shake.Action ()
buildHs config rtsFlags libs extraPackages hs fn = do
    -- Actually I only need it if this binary imports a module that uses
    -- hsconfig.h, but it's cheap to generate so lets always do it.
    need [hsconfigPath config]
    srcs <- HsDeps.transitiveImportsOf generatedSrc (cppFlags config) hs
    let ccs = List.nub $
            concat [Map.findWithDefault [] src hsToCc | src <- srcs]
        objs = List.nub (map (srcToObj config) (ccs ++ srcs)) ++ libs
    logDeps config "build" fn objs
    Util.cmdline $ linkHs config rtsFlags fn (extraPackages ++ allPackages) objs

data BundleType = APPL | BNDL
    deriving (Show)

makeBundle :: Bool -> FilePath -> Shake.Action ()
makeBundle = makeBundle_ True APPL

makeBundle_ :: Bool -> BundleType -> Bool -> FilePath -> Shake.Action ()
makeBundle_ makeWrapper bundleType hasIcon binary = case Util.platform of
    Util.Mac -> do
        let icon = build </> nameExt binary "icns"
        when hasIcon $ need [icon]
        Util.system "tools/make_bundle.py" $
            ["--icon=" <> icon | hasIcon] ++
            ["--make-wrapper" | makeWrapper] ++
            [ "--type=" <> show bundleType
            , binary
            ]
    _ -> return ()

-- * tests and profiles

-- | Generate RunTests.hs and compile it.
testRules :: Config -> Shake.Rules ()
testRules config = do
    runTests ++ "*.hs" %> generateTestHs "_test"
    runTestsBinary runTests ?> \fn -> do
        -- The UI tests use fltk.a.  It would be nicer to have it
        -- automatically added when any .o that uses it is linked in.
        buildHs config defaultRtsFlags [oDir config </> "fltk/fltk.a"] []
            (fn ++ ".hs") fn
        -- A stale .tix file from a previous compile will cause any binary to
        -- instantly crash, and there's no way to turn off .tix generation.
        Util.system "rm" ["-f", FilePath.takeFileName fn ++ ".tix"]

profileRules :: Config -> Shake.Rules ()
profileRules config = do
    runProfile ++ "*.hs" %> generateTestHs "_profile"
    runTestsBinary runProfile ?> \fn ->
        buildHs config defaultRtsFlags [oDir config </> "fltk/fltk.a"] []
            (fn ++ ".hs") fn

-- | Match Run(Tests|Profile)(-A.B)?.hs
--
-- TODO This is hacky because I need to match the binary, but not the generated
-- output.  It's because this is the one place where the source file and
-- outputs live in the same directory.  It would be better to put the generated
-- source in build/generated or something as I do with hsc and chs.
runTestsBinary :: FilePath -> FilePath -> Bool
runTestsBinary prefix fn = prefix `List.isPrefixOf` fn
    && FilePath.takeExtension fn `notElem` [".hs", ".o", ".hi"]

generateTestHs :: FilePath -> FilePath -> Shake.Action ()
generateTestHs suffix fn = do
    -- build/test/RunTests-A.B.Xyz.hs -> A/B/Xyz_test.hs
    let testName = drop 1 $ dropWhile (/='-') $ FilePath.dropExtension fn
    tests <- if null testName
        then filter wantsTest <$> Util.findHs ('*' : suffix ++ ".hs") "."
        else return [moduleToPath testName ++ suffix ++ ".hs"]
    let generate = modeToDir Opt </> "generate_run_tests"
    need $ generate : tests
    Util.system generate (fn : tests)

wantsTest :: FilePath -> Bool
wantsTest hs = if Config.enableIm localConfig then True
    else not (requiresSynthPackages hs)
    -- TODO NOTE [no-package]

-- | Build build/(mode)/RunCriterion-A.B.C from A/B/C_criterion.hs
criterionRules :: Config -> Shake.Rules ()
criterionRules config = buildDir config </> "RunCriterion-*" %> \fn -> do
    let hs = runCriterionToSrc config fn
    need [hs]
    buildHs config defaultRtsFlags [] ["criterion"] hs fn

-- | build/(mode)/RunCriterion-Derive.Derive -> Derive/Derive_criterion.hs
runCriterionToSrc :: Config -> FilePath -> FilePath
runCriterionToSrc config bin = moduleToPath name ++ criterionHsSuffix
    where
    -- build/(mode)/RunCriterion-Derive.Derive -> Derive.Derive
    name = drop 1 $ dropWhile (/='-') $ dropDir (buildDir config) bin

-- | Derive/Derive_criterion.hs -> build/(mode)/RunCriterion-Derive.Derive
srcToRunCriterion :: Config -> FilePath -> FilePath
srcToRunCriterion config src =
    case dropSuffix (pathToModule src) suffix of
        Just m -> buildDir config </> "RunCriterion-" <> m
        Nothing -> error $
            "srcToRunCriterion: expected " <> suffix <> " suffix: " <> show src
    where suffix = dropExtension criterionHsSuffix

-- | Find targets for all criterion benchmarks.
getCriterionTargets :: Config -> Shake.Action [FilePath]
getCriterionTargets config =
    map (srcToRunCriterion config) <$> Util.findHs ('*' : criterionHsSuffix) "."

-- * generated haskell

generateKorvais :: Shake.Rules ()
generateKorvais = generatedKorvais %> \_ -> do
    inputs <- Shake.getDirectoryFiles "" ["Solkattu/Score/*.hs"]
    let generate = modeToDir Opt </> "extract_korvais"
    need $ generate : inputs
    Util.system generate (generatedKorvais : inputs)

generatedKorvais :: FilePath
generatedKorvais = "Solkattu/All.hs"

-- * faust

faustDspDir :: FilePath
faustDspDir = "Synth/Faust/dsp"

faustSrcDir :: FilePath
faustSrcDir = build </> "faust"

faustCppDir :: FilePath
faustCppDir = build </> "faust-cpp"

faustRules :: Shake.Rules ()
faustRules = faustRule *> faustCppRule *> faustAllRule

faustCppRule :: Shake.Rules ()
faustCppRule = faustCppDir </> "*.dsp" %> \output -> do
    -- build/faust-cpp/x.dsp -> Synth/Faust/dsp/x.dsp
    let input = faustDspDir </> FilePath.takeFileName output
    (includes, notFound) <- CcDeps.transitiveIncludesOf mempty [] input
    unless (null notFound) $
        Util.errorIO $ ".dsp includes not found: " <> unwords notFound
    need $ input : includes
    Util.cmdline
        ( "CPP"
        , output
        , ["cpphs", "--noline", "-O" <> output, input]
        )

faustRule :: Shake.Rules ()
faustRule = faustSrcDir </> "*.cc" %> \output -> do
    -- build/faust/x.cc -> build/faust-cpp/x.dsp
    let input = faustCppDir </> nameExt output ".dsp"
    need [input]
    Util.cmdline $ faustCmdline input output
    Util.system "tools/clear_faust" [dspToName input]

faustCmdline :: FilePath -> FilePath -> Util.Cmdline
faustCmdline input output =
    ( "FAUST"
    , output
    , ["faust", input
      , "--class-name", "__faust_" <> dspToName input
      , "-lang", "c"
      , "-o", output
      ]
    )

dspToName :: FilePath -> String
dspToName = FilePath.dropExtension . FilePath.takeFileName

-- | Synth/Faust/dsp/x.dsp -> build/faust/x.cc
dspToSrc :: FilePath -> FilePath
dspToSrc dsp = faustSrcDir </> nameExt dsp ".cc"

faustAllRule :: Shake.Rules ()
faustAllRule = generatedFaustAll %> \output -> do
    dsps <- Shake.getDirectoryFiles "" [faustDspDir </> "*.dsp"]
    let include = "Synth/Faust/Patch.h"
    logDepsGeneric "faust-all" output $ include : map dspToSrc dsps
    Shake.writeFileChanged output $ faustAll dsps [include]

-- | This is in build instead of build/faust because that makes it simpler to
-- just say build/faust/*.cc is generated by faust.
generatedFaustAll :: FilePath
generatedFaustAll = build </> "faust_all.cc"

faustAll :: [FilePath] -> [FilePath] -> String
faustAll dsps extraIncludes = unlines
    -- For some reason faust assumes these are global.
    [ "#include <algorithm>"
    -- Even though it's a GCC pragma, clang seems to understand it too:
    -- https://clang.llvm.org/docs/UsersManual.html#pragma-gcc-diagnostic
    , "#pragma GCC diagnostic ignored \"-Wunused-variable\""
    , ""
    , "// faust expects these to be in scope for whatever reason"
    , "using std::min;"
    , "using std::max;"
    , ""
    , unlines (map ("#include "<>) includes)
    , "static const int all_patches_count = " <> show (length names) <> ";"
    , ""
    , "static const Patch *all_patches[] ="
    , "    { " <> Seq.join "\n    , "
        [ "new Patch(" <> Seq.join ",\n        "
            [ show name
            , "sizeof(" <> struct name <> ")"
            , "getNumInputs" <> struct name <> "(nullptr)"
            , "getNumOutputs" <> struct name <> "(nullptr)"
            -- The casts make it unsafe, but the function takes some dsp
            -- struct, while Patch.h declares it as State *.
            , "(Patch::Initialize) init" <> struct name
            , "metadata" <> struct name
            , "(Patch::UiMetadata) buildUserInterface" <> struct name
            , "(Patch::Compute) compute" <> struct name
            ]
            <> ")"
        | name <- names
        ]

    , "    };"
    ]
    where
    struct = ("__faust_"<>)
    names = map dspToName dsps
    includes = map (show . dspToSrc) dsps ++ map show extraIncludes

-- * markdown

markdownRule :: FilePath -> Shake.Rules ()
markdownRule linkifyBin = buildDocDir </> "*.md.html" %> \html -> do
    let doc = htmlToDoc html
    need [linkifyBin, doc]
    Util.system "tools/convert_doc" [doc, html] -- wrapper around pandoc

-- | build/doc/xyz.md.html -> doc/xyz.md
htmlToDoc :: FilePath -> FilePath
htmlToDoc = (docDir </>) . FilePath.takeFileName . FilePath.dropExtension

-- | doc/xyz.md -> build/doc/xyz.md.html
docToHtml :: FilePath -> FilePath
docToHtml = (buildDocDir </>) . FilePath.takeFileName . (++".html")

-- * hs

-- hsORule hsHiRule
hsOHiRule :: InferConfig -> Shake.Rules ()
hsOHiRule infer = matchHsObjHi &?> \fns -> do
    let Just obj = List.find (".hs.o" `List.isSuffixOf`) fns
    Shake.askOracle (Question () :: Question GhcQ)
    let config = infer obj
    isHsc <- liftIO $ Directory.doesFileExist (objToHsc config obj)
    isChs <- if isHsc then return False else liftIO $
        Directory.doesFileExist (objToChs config obj)
    let hs  | isHsc = objToHscHs config obj
            | isChs = objToChsHs config obj
            | otherwise = objToSrc config obj
    imports <- HsDeps.importsOf generatedSrc (cppFlags config hs) hs
    -- TODO no config.h?  what about hsconfig.h?

    includes <- if Maybe.isJust (cppFlags config hs)
        then includesOf "hsOHiRule" config [] hs else return []
    let his = map (objToHi . srcToObj config) imports
    -- I depend on the .hi files instead of the .hs.o files.  GHC avoids
    -- updaing the timestamp on the .hi file if its .o didn't need to be
    -- recompiled, so hopefully this will avoid some work.
    logDeps config "*.hs.o *.hi" obj (hs : includes ++ his)
    Util.cmdline $ compileHs (extraPackagesFor obj ++ allPackages) config hs

objToHsc :: Config -> FilePath -> FilePath
objToHsc config obj = objToSrc config obj ++ "c"

objToChs :: Config -> FilePath -> FilePath
objToChs config obj = FilePath.replaceExtension (objToSrc config obj) "chs"

-- | Generate both .hs.o and .hi from a .hs file.
matchHsObjHi :: FilePath -> Maybe [FilePath]
matchHsObjHi fn
    | any (`List.isSuffixOf` fn) [".hs.o", ".hi"]
            && "build/" `List.isPrefixOf` fn =
        if isMain then Just [suffixless ++ ".hs.o"]
            else Just [suffixless ++ ".hs.o", suffixless ++ ".hi"]
    | otherwise = Nothing
    where
    suffixless = dropExtension fn
    hs = suffixless ++ ".hs"
    -- Hack: main modules are sometimes called Main, so their .hi file doesn't
    -- have the same name as the module.  But no one should be importing them,
    -- so I don't need to track the .hi.
    isMain = Map.member hs nameToMain
        || runProfile `List.isPrefixOf` hs || runTests `List.isPrefixOf` hs
        || criterionHsSuffix `List.isSuffixOf` hs

compileHs :: [Package] -> Config -> FilePath -> Util.Cmdline
compileHs packages config hs =
    ( "GHC " <> show (buildMode config)
    , hs
    -- color=always since I'll be reading the output via pipe.
    , ghcBinary : "-c" : "-fdiagnostics-color=always" : concat
        [ ghcFlags config, hcFlags (configFlags config)
        , packageFlags (configFlags config) packages, mainIs
        , [hs, "-o", srcToObj config hs]
        ]
    )
    where
    mainIs
        | hs `elem` Map.elems nameToMain
                || criterionHsSuffix `List.isSuffixOf` hs =
            ["-main-is", pathToModule hs]
        | otherwise = []

linkHs :: Config -> [Flag] -> FilePath -> [Package] -> [FilePath]
    -> Util.Cmdline
linkHs config rtsFlags output packages objs =
    ( "LD-HS"
    , output
    , ghcBinary : concat
        [ midiLd flags
        , hLinkFlags flags
        , ["-with-rtsopts=" <> unwords rtsFlags | not (null rtsFlags)]
        , ["-lstdc++"], packageFlags flags packages, objs
        -- Suppress "warning: text-based stub file" after OSX command line
        -- tools update.
        , concat
            [ ["-optl", "-w"]
            | "clang-1000.10.44" `List.isInfixOf` ccVersion config
            ]
        -- Libs have to go last, or traditional unix ld can't see them.
        -- TODO: this means all binaries link fltk, not just who use it.
        -- In fact all the binaries link all the C libs.  I need the shakefile
        -- refactor to fix this.
        , C.libLink (_libfltk (cLibs config))
        , concat [C.libLink libsamplerate | Config.enableIm localConfig]
        , concat [C.libLink (Config.rubberband localConfig)
            | Config.enableIm localConfig]
        , ["-o", output]
        ]
    )
    where
    flags = configFlags config

-- TODO: add a writeIfChanged for these.  Not that it matters for builds, but
-- it seems silly to keep overwriting the file with the same contents.

-- | ghci has to be called with the same flags that the .o files were compiled
-- with or it won't load them.
writeGhciFlags :: (Mode -> Config) -> IO ()
writeGhciFlags modeConfig =
    forM_ (map modeConfig allModes) $ \config -> do
        Directory.createDirectoryIfMissing True (buildDir config)
        writeFile (buildDir config </> "ghci-flags") $
            unlines (ghciFlags config)

-- | Write the deps files, which are like cabal files but easier to parse.
-- Used by the nix build.
writeDeps :: FilePath -> [(String, [(Package, String)])] -> IO ()
writeDeps dir namePackages = forM_ namePackages $ \(name, pkgs) ->
    writeFile (dir </> name) $ unlines $ List.sort $ map fst pkgs

-- | Make links to large binary files I don't want to put into source control.
makeDataLinks :: IO ()
makeDataLinks = do
    Directory.createDirectoryIfMissing True buildDocDir
    run $ Posix.createSymbolicLink "../../../data/www" (buildDocDir </> "data")
    run $ Posix.createSymbolicLink "../../doc/img" (buildDocDir </> "img")
    return ()
    where run = Exceptions.ignoreError IO.Error.isAlreadyExistsError

-- | Get the file-independent flags for a haskell compile.  This is disjunct
-- from 'hcFlags', which is the per-file compile-specific ones.
ghcFlags :: Config -> [Flag]
ghcFlags config = concat $
    [ "-outputdir", oDir config, "-osuf", ".hs.o"
    , "-fwrite-ide-info", "-hiedir", buildDir config </> "hie"
    , "-i" ++ List.intercalate ":" [oDir config, hscDir config, chsDir config]
    ] :
    [ ghcGlobalFlags
    , define (configFlags config)
    , cInclude (configFlags config)
    , ghcWarnings config
    ]

-- | Blend the delicate mix of flags needed to convince ghci to load .o files
-- that ghc just produced.
ghciFlags :: Config -> [Flag]
ghciFlags config = concat
    [ filter wanted $ hcFlags (configFlags config)
    , ghcFlags config
    , sandboxFlags (configFlags config)
    -- Without this, GHC API won't load compiled modules.
    -- See https://ghc.haskell.org/trac/ghc/ticket/13604
    , if | version <= (8, 0, 2) -> []
         -- This is unpleasant, but better than having a broken REPL.
         | version < (8, 4, 1) -> error
            "ghc 8.2 doesn't support the flags needed to make the REPL work,\
            \ use 8.0 or 8.4, see doc/INSTALL.md for details"
         | otherwise -> ["-fignore-optim-changes", "-fignore-hpc-changes"]
    , packageDbFlags (configFlags config)
    ]
    where
    version = ghcVersion config
    wanted flag = not $ or
        -- Otherwise GHC API warns "-O conflicts with --interactive; -O ignored"
        [ "-O" `List.isPrefixOf` flag
        -- Otherwise ghci warns "Hpc can't be used with byte-code interpreter."
        , flag == "-fhpc"
        ]

-- * cc

ccORule :: InferConfig -> Shake.Rules ()
ccORule infer = matchObj "**/*.cc.o" ?> \obj -> do
    Shake.askOracle (Question () :: Question FltkQ)
    let config = infer obj
    let cc = objToSrc config obj
    -- The contents of 'fltkDeps' won't be in CcBinaries, so they use only the
    -- global flags.  This is a hack that only works because I only have
    -- one C++ library.  If I ever have another one I'll need a CcLibrary
    -- target.
    let flags = Maybe.fromMaybe (C.libCompile (_libfltk (cLibs config))) $
            findFlags config obj
        localIncludes = filter ("-I" `List.isPrefixOf`) flags
    includes <- includesOf "ccORule" config localIncludes cc
    logDeps config "*.cc.o" obj (cc:includes)
    Util.cmdline $ compileCc config flags cc obj

-- | Find which 'C.Binary' has the obj file in its 'C.binObjs' and get its
-- 'C.binCompileFlags'.  This assumes that each obj file only occurs in one
-- 'C.Binary'.  Another way to do this would be to create explicit rules for
-- each Mode for each source file, but I wonder if that would add to startup
-- overhead.
findFlags :: Config -> FilePath -> Maybe [Flag]
findFlags config obj =
    ($config) . C.binCompileFlags <$> List.find find ccBinaries
    where find binary = obj `elem` ccDeps config binary

compileCc :: Config -> [Flag] -> FilePath -> FilePath -> Util.Cmdline
compileCc config flags cc obj =
    ( "C++ " <> show (buildMode config)
    , obj
    , concat
        -- color=always since I'll be reading the output via pipe.
        -- This is the gcc flag, but clang understands it too.
        [ ["g++", "-c", "-fdiagnostics-color=always"]
        , globalCcFlags (configFlags config)
        , flags
        , ["-o", obj, cc]
        ]
    )

linkCc :: [Flag] -> FilePath -> [FilePath] -> Util.Cmdline
linkCc flags binary objs =
    ( "LD-CC"
    , binary
    , "g++" : objs ++ flags ++ ["-o", binary]
    )

-- * hsc

hsc2hsRule :: Config -> Shake.Rules ()
hsc2hsRule config = hscDir config </> "**/*.hs" %> \hs -> do
    let hsc = hsToHsc (hscDir config) hs
    includes <- includesOf "hsc2hsRule" config [] hsc
    logDeps config "*.hsc" hs (hsc : includes)
    Util.cmdline $ hsc2hs config hs hsc

hsc2hs :: Config -> FilePath -> FilePath -> Util.Cmdline
hsc2hs config hs hsc =
    ( "hsc2hs"
    , hs
    , concat
        [ ["hsc2hs", "-I" ++ ghcLib config </> "include"]
        -- Otherwise g++ complains about the offsetof macro hsc2hs uses.
        , words "-c g++ --cflag -Wno-invalid-offsetof --cflag -std=c++11"
        , cInclude flags ++ C.libCompile (_libfltk (cLibs config))
        , define flags
        , [hsc, "-o", hs]
        ]
    )
    where flags = configFlags config

-- * c2hs

chsRule :: Config -> Shake.Rules ()
chsRule config = chsDir config </> "**/*.hs" %> \hs -> do
    -- TODO also produces .chi, .chs.h
    let chs = hsToChs (chsDir config) hs
    includes <- includesOf "chsRule" config [] chs
    logDeps config "*.chs" hs (chs : includes)
    Util.cmdline $ c2hs config hs chs

c2hs :: Config -> FilePath -> FilePath -> Util.Cmdline
c2hs config hs chs =
    ( "c2hs"
    , hs
    , concat
        [ ["c2hs"]
        , map ("--cppopts="<>) $
            filter (`notElem` platformDefines) (define flags)
          ++ cInclude flags
        , [ "--output-dir=" <> chsDir config
          , chs
          ]
        ]
    )
    where flags = configFlags config

-- * util

-- |
-- A/B.{hs,hsc,chs} -> build/debug/obj/A/B.hs.o
-- A/B.cc -> build/debug/obj/A/B.cc.o
-- build/A/B.hs -> build/A/B.hs.o
-- build/{hsc,chs}/Ui/Key.hs -> build/debug/obj/Ui/Key.hs.o
--
-- Generated .hs files are already in build/ so they shouldn't have build/etc.
-- prepended.  Unless they were .hsc or .chs generated files.
srcToObj :: Config -> FilePath -> FilePath
srcToObj config fn = addDir $ if
    | ext `elem` [".hsc", ".chs"] -> FilePath.replaceExtension fn "hs.o"
    | ext `elem` [".hs", ".cc"] -> FilePath.addExtension fn "o"
    | otherwise -> error $ "unknown src extension: " ++ show fn
    where
    ext = FilePath.takeExtension fn
    addDir
        | hscDir config `List.isPrefixOf` fn =
            (oDir config </>) . dropDir (hscDir config)
        | chsDir config `List.isPrefixOf` fn =
            (oDir config </>) . dropDir (chsDir config)
        | build `List.isPrefixOf` fn = id
        | otherwise = (oDir config </>)

-- | build/debug/obj/A/B.$ext.o -> A/B.$ext
objToSrc :: Config -> FilePath -> FilePath
objToSrc config = FilePath.dropExtension . dropDir (oDir config)

-- | build/debug/obj/A/B.o -> build/hsc/A/B.hs
objToHscHs :: Config -> FilePath -> FilePath
objToHscHs config = (hscDir config </>) . objToSrc config

-- | build/debug/obj/A/B.o -> build/chs/A/B.hs
objToChsHs :: Config -> FilePath -> FilePath
objToChsHs config = (chsDir config </>) . objToSrc config

-- | build/hsc/A/B.hs -> A/B.hsc
hsToHsc :: FilePath -> FilePath -> FilePath
hsToHsc hscDir fn = dropDir hscDir $ FilePath.replaceExtension fn "hsc"

-- | A/B.hsc -> build/hsc/A/B.hs
hscToHs :: FilePath -> FilePath -> FilePath
hscToHs hscDir fn = (hscDir </>) $ FilePath.replaceExtension fn "hs"

-- | A/B.chs -> build/chs/A/B.hs
chsToHs :: FilePath -> FilePath -> FilePath
chsToHs chsDir fn = (chsDir </>) $ FilePath.replaceExtension fn "hs"

-- | build/chs/A/B.hs -> A/B.chs
hsToChs :: FilePath -> FilePath -> FilePath
hsToChs chsDir fn = dropDir chsDir $ FilePath.replaceExtension fn "chs"

objToHi :: FilePath -> FilePath
objToHi = (++".hi") . dropExtension

hiToObj :: FilePath -> FilePath
hiToObj = flip FilePath.replaceExtension "hs.o"

dropExtension :: FilePath -> FilePath
dropExtension fn
    | ".hs.o" `List.isSuffixOf` fn = take (length fn - 5) fn
    | otherwise = FilePath.dropExtension fn

dropDir :: FilePath -> FilePath -> FilePath
dropDir odir fn
    | dir `List.isPrefixOf` fn = drop (length dir) fn
    | otherwise = fn
    where dir = odir ++ "/"

strip :: String -> String
strip = reverse . dropWhile Char.isSpace . reverse . dropWhile Char.isSpace

-- | Foor/Bar.hs -> Foo.Bar
pathToModule :: FilePath -> String
pathToModule = map (\c -> if c == '/' then '.' else c) . FilePath.dropExtension

-- | Foo.Bar -> Foo/Bar
moduleToPath :: String -> FilePath
moduleToPath = map $ \c -> if c == '.' then '/' else c

logDeps :: Config -> String -> FilePath -> [FilePath] -> Shake.Action ()
logDeps config stage fn deps
    | null deps = return ()
    | otherwise = do
        need deps
        Shake.putLoud $ ">>> " ++ stage ++ ": " ++ fn ++ " <- "
            ++ unwords (map (dropDir (oDir config)) deps)

-- | logDeps for Mode-independent build products.
logDepsGeneric :: String -> FilePath -> [FilePath] -> Shake.Action ()
logDepsGeneric stage fn deps
    | null deps = return ()
    | otherwise = do
        need deps
        Shake.putLoud $ ">>> " ++ stage ++ ": " ++ fn ++ " <- " ++ unwords deps

includesOf :: String -> Config -> [Flag] -> FilePath -> Shake.Action [FilePath]
includesOf caller config moreIncludes fn = do
    let dirs =
            [dir | '-':'I':dir <- cInclude (configFlags config) ++ moreIncludes]
    (includes, notFound) <- hsconfig <$>
        CcDeps.transitiveIncludesOf (HsDeps._generatedHs generatedSrc) dirs fn
    unless (null notFound) $
        liftIO $ putStrLn $ caller
            ++ ": WARNING: c includes not found: " ++ show notFound
            ++ " (looked in " ++ unwords dirs ++ ")"
    return includes
    where
    -- hsconfig.h is the only automatically generated header.  Because the
    -- #include line doesn't give the path (and can't, since each build dir
    -- has its own hsconfig.h), I have to special case it.
    hsconfig (includes, notFound)
        | hsconfigH `elem` notFound =
            (hsconfigPath config : includes, filter (/=hsconfigH) notFound)
        | otherwise = (includes, notFound)

dropPrefix :: String -> String -> Maybe String
dropPrefix pref str
    | pref `List.isPrefixOf` str = Just $ drop (length pref) str
    | otherwise = Nothing

dropSuffix :: String -> String -> Maybe String
dropSuffix str suf
    | suf `List.isSuffixOf` str =
        Just $ reverse $ drop (length suf) (reverse str)
    | otherwise = Nothing

nameExt :: FilePath -> String -> FilePath
nameExt fn = FilePath.replaceExtension (FilePath.takeFileName fn)

-- NOTE [no-package] I don't have a way to declare packages and their
-- dependencies.  I just sort of ad-hoc it by giving most dependencies to
-- everyone, but it's a problem for haddock and tests, which are global.
-- A real generalized reusable package system is complicated, so for the
-- moment I hack it by filtering based on directory prefix.

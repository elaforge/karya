-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Shakefile config which is likely to be different on each system.
-- Customize this in Local.ShakeConfig.
module Shake.Config where
import qualified Shake.C as C


type Flag = String

data Config = Config {
    -- switches

    -- | If True, expect that hackage packages were built and installed via
    -- @cabal v2-build --only-dep@ instead of @cabal v1-install --only-dep@
    useCabalV2 :: Bool
    -- | If true, link in the EKG library for realtime monitoring.  But it
    -- requires the ekg package with a giant set of dependencies so it's
    -- disabled by default.
    , enableEkg :: Bool
    -- | Link with the -eventlog RTS, for threadscope.  Presumably it hurts
    -- performance, so it's off by default.
    , enableEventLog :: Bool
    -- | Extra flags passed to both C++ and Haskell compiles.  I use them
    -- to enable some purely local hacks, e.g. hacked version of libfltk.
    , extraDefines :: [Flag]
    -- | C++ links get these via -F and ghc via -framework-path.
    -- It's just to work around https://github.com/NixOS/nixpkgs/issues/24237
    , extraFrameworkPaths :: [FilePath]

    -- paths

    -- | Path to the fltk installation.  If you @make install@ then it'll
    -- probably be in /usr/local/bin.
    , fltkConfig :: FilePath
    , libsamplerate :: C.ExternalLibrary
    , rubberband :: C.ExternalLibrary
    -- | Extra -I flags that all compiles get, including haskell cpp and
    -- hsc2hs.  Without the -I:  ["/Users/me/homebrew/include"]
    , globalIncludes :: [FilePath]
    -- | Extra -L flags for the C++ link, without the leading -L:
    -- ["/Users/me/homebrew/lib"]
    , globalLibDirs :: [FilePath]
    } deriving (Show)


defaultConfig :: Config
defaultConfig = Config
    { useCabalV2 = False
    , enableEkg = False
    , enableEventLog = False
    , extraDefines = []
    , extraFrameworkPaths = []
    , fltkConfig = "fltk-config"
    , libsamplerate = C.library "samplerate"
    , rubberband = C.library "rubberband"
    , globalIncludes = []
    , globalLibDirs = []
    }

-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Shakefile config which is likely to be different on each system.
module Shake.Config where


type Flag = String

-- * switches

-- | If true, link in the EKG library for realtime monitoring.  But it requires
-- the ekg package with a giant set of dependencies so it's disabled by
-- default.
enableEkg :: Bool
enableEkg = False

-- | Link with the -eventlog RTS, for threadscope.  Presumably it hurts
-- performance, so it's off by default.
enableEventLog :: Bool
enableEventLog = False

-- | If True, compile the im offline synthesizers and PlayCache VST from
-- the Synth hierarchy.  This requires a number of hackage dependencies,
-- including the VST SDK, which has to be downloaded separately.
enableIm :: Bool
enableIm = False


-- * paths

-- | Path to the fltk installation.  If you @make install@ then it'll probably
-- be in /usr/local/bin.
fltkConfig :: FilePath
fltkConfig = "/usr/local/bin/fltk-config"

-- | Extra -I flags that all compiles get, including haskell cpp and hsc2hs.
-- E.g. ["-I/Users/me/homebrew/include"]
globalIncludes :: [Flag]
globalIncludes = []

-- | Extra -L flags for the C++ link.
-- E.g. ["-I/Users/me/homebrew/include"]
globalLibDirs :: [Flag]
globalLibDirs = []

-- | Root of the VST SDK.  Only used if 'enableIm' is true.
vstBase :: FilePath
vstBase = "/usr/local/src/music/vstsdk2.4"

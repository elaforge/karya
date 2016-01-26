-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Shakefile config which is likely to be different on each system.
module Shake.Config where


type Flag = String

-- | Path to the fltk installation.  If you @make install@ then it'll probably
-- be in /usr/local/bin.
fltkConfig :: FilePath
fltkConfig = "/usr/local/src/fltk-1.3/fltk-config"

-- | Extra -I flags that all compiles get, including haskell cpp and hsc2hs.
globalIncludes :: [Flag]
globalIncludes = ["-I/Users/elaforge/homebrew/include"]

-- | Extra -L flags for the C++ link.
globalLibDirs :: [Flag]
globalLibDirs = ["-L/Users/elaforge/homebrew/lib"]

-- | Root of the VST SDK.
vstBase :: FilePath
vstBase = "/usr/local/src/music/vst3-sdk"

-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Shared config to coordinate between the sequencer and im subsystems.
--
-- TODO Currently paths rely on you being in the right directory, but should
-- probably have some more robust configuration at some point.  Of course
-- 'App.Config.app_dir' is just return '.' too.
module Synth.Shared.Config where
import System.FilePath ((</>))

#include "config.h"


data Config = Config {
    -- | Path to the binary.
    binary :: !FilePath
    -- | Write serialized notes to this file.
    , notes :: !FilePath
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { binary = "build/opt/sampler-im"
    , notes = dataDir </> "notes"
    }

-- | All of the data files used by the Im backend are based in this directory.
-- Everything in here should be temporary files, used for communication or
-- caching.
dataDir :: FilePath
dataDir = "im"

cache :: FilePath
cache = dataDir </> "cache"

-- | All im synths render at this sampling rate, and the sequencer sets the
-- start time by it.
samplingRate :: Int
samplingRate = SAMPLING_RATE

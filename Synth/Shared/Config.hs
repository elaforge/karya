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
import qualified Data.Map as Map
import System.FilePath ((</>))

import Global

#include "config.h"


newtype Config = Config { synths :: Map SynthName Synth }
    deriving (Eq, Show)

config :: Config
config = Config $ Map.fromList
    [ (samplerName, sampler)
    , (faustName, faust)
    ]

data Synth = Synth {
    -- | Path to the binary.
    binary :: !FilePath
    -- | Write serialized notes to this file.
    , notesDir :: !FilePath
    } deriving (Eq, Show)

type SynthName = Text

samplerName :: SynthName
samplerName = "sampler"

sampler :: Synth
sampler = Synth
    { binary = "build/opt/sampler-im"
    , notesDir = dataDir </> "sampler-notes"
    }

faustName :: SynthName
faustName = "faust"

faust :: Synth
faust = Synth
    { binary = "build/opt/faust-im"
    , notesDir = dataDir </> "faust-notes"
    }

-- | All of the data files used by the Im backend are based in this directory.
-- Everything in here should be temporary files, used for communication or
-- caching.
dataDir :: FilePath
dataDir = "im"

cacheDir :: FilePath
cacheDir = dataDir </> "cache"

-- | All im synths render at this sampling rate, and the sequencer sets the
-- start time by it.
samplingRate :: Int
samplingRate = SAMPLING_RATE

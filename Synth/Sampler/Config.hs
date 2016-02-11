-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Local config variables.
module Synth.Sampler.Config where
import System.FilePath ((</>))


-- | Root directory for instrument samples.
instrumentDbDir :: FilePath
instrumentDbDir = "Synth/Sampler/instruments"

-- Paths to coordinate with the sampler.
--
-- TODO Currently they rely on you being in the right directory, but should
-- probably have some more robust configuration at some point.  Of course
-- 'App.Config.app_dir' is just return '.' too.

binary :: FilePath
binary = "build/opt/sampler"

-- | All of the data files used by the Im backend are based in this directory.
-- Everything in here should be temporary files, used for communication or
-- caching.
dataDir :: FilePath
dataDir = "im"

-- | Serialize notes to this file to send them to the Im backend.
notes :: FilePath
notes = dataDir </> "notes"

cache :: FilePath
cache = dataDir </> "cache"

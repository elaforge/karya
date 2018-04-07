-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-- | Shared config to coordinate between the sequencer and im subsystems.
--
-- TODO Currently paths rely on you being in the right directory, but should
-- probably have some more robust configuration at some point.  Of course
-- 'App.Config.app_dir' is just return '.' too.
module Synth.Shared.Config where
import qualified Data.Map as Map
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Ui.Id as Id
import Global

#include "config.h"


data Config = Config {
    -- | All of the data files used by the Im backend are based in this
    -- directory.  Everything in here should be temporary files, used for
    -- communication or caching.
    imDir :: FilePath
    , synths :: Map SynthName Synth
    }
    deriving (Eq, Show)

config :: Config
config = Config
    { imDir = "im"
    , synths = Map.fromList
        [ (samplerName, sampler)
        , (faustName, faust)
        , (nessName, ness)
        ]
    }

data Synth = Synth {
    -- | Path to the binary.  Don't run a binary if it's empty.
    binary :: !FilePath
    -- | Write serialized notes to this file.  Relative to 'notesParentDir'.
    , notesDir :: !FilePath
    } deriving (Eq, Show)

type SynthName = Text

nessName :: SynthName
nessName = "ness"

samplerName :: SynthName
samplerName = "sampler"

sampler :: Synth
sampler = Synth
    { binary = "build/opt/sampler-im"
    , notesDir = "sampler"
    }

faustName :: SynthName
faustName = "faust"

faust :: Synth
faust = Synth
    { binary = "build/opt/faust-im"
    , notesDir = "faust"
    }

ness ::Synth
ness = Synth
    { binary = ""
    , notesDir = "ness"
    }

notesParentDir :: FilePath
notesParentDir = "notes"

cacheDir :: FilePath
cacheDir = "cache"

-- | All im synths render at this sampling rate, and the sequencer sets the
-- start time by it.
samplingRate :: Int
samplingRate = SAMPLING_RATE

type SamplingRate = SAMPLING_RATE


-- * cache files

-- | Write serialized notes to this file.
notesFilename :: FilePath -> Synth -> Id.BlockId -> FilePath
notesFilename imDir synth blockId =
    imDir </> notesParentDir </> notesDir synth </> idFilename blockId

-- | Get the filename that should be used for the output of a certain block and
-- instrument.
outputFilename :: FilePath
    -> FilePath -- ^ Names as produced by 'notesFilename'.
    -> Text -- ^ Score.Instrument, but I don't want to import it.
    -> FilePath
outputFilename imDir notesFilename inst = dir </> untxt inst <> ".wav"
    where
    dir = imDir </> cacheDir </> ns </> name
    ns = FilePath.takeFileName $ FilePath.takeDirectory notesFilename
    name = FilePath.takeFileName notesFilename

idFilename :: Id.Ident a => a -> FilePath
idFilename id = untxt $ Id.un_namespace ns <> "/" <> name
    where (ns, name) = Id.un_id $ Id.unpack_id id

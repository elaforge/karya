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
import qualified System.IO.Unsafe as Unsafe
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Seq as Seq
import qualified Ui.Id as Id

import Global

#include "config.h"


-- TODO synchronize this with all the others uses of the data dir
-- I need an absolute path, because this goes to PlayCache, which runs with
-- who knows what CWD.
getDataDir :: FilePath
getDataDir = Unsafe.unsafePerformIO $ Directory.canonicalizePath "../data"
{-# NOINLINE getDataDir #-}

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

-- | Base directory for sampler patches.
samplerRoot :: FilePath
samplerRoot = getDataDir </> "sampler"

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

-- | All serialized notes are in im </> notesParentDir.
notesParentDir :: FilePath
notesParentDir = "notes"

-- | All rendered output is in im </> cacheDir.
cacheDir :: FilePath
cacheDir = "cache"

-- | All im synths render at this sampling rate, and the sequencer sets the
-- start time by it.
samplingRate :: Int
samplingRate = SAMPLING_RATE

type SamplingRate = SAMPLING_RATE

-- | Number of frames in each audio chunk.
chunkSize :: Audio.Frame
chunkSize = Audio.Frame $ samplingRate `div` 4

-- | Save an audio chunk and checkpoint in this many frames.  This should be
-- an integral multiple of 'chunkSize', so checkpoint state lines up with audio
-- output.
checkpointSize :: Audio.Frame
checkpointSize = Audio.Frame $ samplingRate * checkpointSeconds

checkpointSeconds :: Int
checkpointSeconds = CHECKPOINT_SECONDS

-- | play_cache delays play start by this many frames, so MIDI output should
-- also be delayed by this much to match.
--
-- It has to cue up the sample streaming, which means it has to find and
-- seek to the right file.  If playback starts immediately then the first
-- chunk gets cut off, which cuts off note attacks.
startLatency :: Audio.Frame
startLatency = START_LATENCY_FRAMES

oscPort :: Int
oscPort = OSC_PORT

-- * cache files

{- Filenames have to be coordinated between the karya notes output, the
    synth cache output, and the play msg sent to play_cache:

    notes:  im/notes/scorePath/ns/block/synth
    output: im/cache/scorePath/ns/block/inst.wav
    play:   scorePath/ns/block, [inst] in mutes
-}

-- | Write serialized notes to this file.
notesFilename :: FilePath -> Synth -> FilePath
    -- ^ Path to the score, relative to the save dir.  This should uniquely
    -- identify this score.
    -> Id.BlockId -> FilePath
notesFilename imDir synth scorePath blockId =
    imDir </> notesParentDir </> scorePath </> idFilename blockId
    </> notesDir synth

-- | Get the filename that should be used for the output of a certain block and
-- instrument.
outputFilename :: FilePath
    -> FilePath -- ^ Names as produced by 'notesFilename'.
    -> Text -- ^ ScoreTypes.Instrument, but I don't want to import ScoreTypes.
    -> FilePath
outputFilename imDir notesFilename inst =
    imDir </> cacheDir </> scorePathBlock </> untxt inst <> ".wav"
    where
    -- Recover scorePath/ns/block from the path so I don't have to put it in
    -- a file or something.  TODO It's a bit sketchy though.
    scorePathBlock = FilePath.joinPath $ Seq.rdrop 1 $ drop 2 $
        FilePath.splitPath notesFilename

-- |
-- > im/notes/$scorePath/$scoreFname/$namespace/$block/sampler ->
-- > im/cache/$scorePath/$scoreFname/$namespace/$block/$instrument
outputDirectory :: FilePath
    -> FilePath -- ^ Names as produced by 'notesFilename'.
    -> Text -- ^ ScoreTypes.Instrument, but I don't want to import ScoreTypes.
    -> FilePath
outputDirectory imDir notesFilename inst =
    instrumentDirectory imDir notesFilename </> untxt inst

-- | Get the directory which contains each instrument subdir.
--
-- > im/notes/$scorePath/$scoreFname/$namespace/$block/sampler ->
-- > im/cache/$scorePath/$scoreFname/$namespace/$block
instrumentDirectory :: FilePath -> FilePath -> FilePath
instrumentDirectory imDir notesFilename = imDir </> cacheDir </> scorePathBlock
    where
    -- Recover scorePath/ns/block from the path so I don't have to put it in
    -- a file or something.  TODO It's a bit sketchy though.
    scorePathBlock = FilePath.joinPath $ Seq.rdrop 1 $ drop 2 $
        FilePath.splitPath notesFilename

-- | This is text sent over MIDI to tell play_cache which directory to play
-- from.  Relative to imDir/cacheDir.
playFilename :: FilePath -> Id.BlockId -> FilePath
playFilename scorePath blockId = scorePath </> idFilename blockId

idFilename :: Id.Ident a => a -> FilePath
idFilename id = untxt $ Id.un_namespace ns <> "/" <> name
    where (ns, name) = Id.un_id $ Id.unpack_id id

-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-- | Shared config to coordinate between the sequencer and im subsystems.
module Synth.Shared.Config where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Log as Log
import qualified Util.Parse as Parse
import qualified Util.Seq as Seq

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Perform.RealTime as RealTime
import qualified Ui.Id as Id

import           Global
import           Synth.Types

#include "config.h"


data Config = Config {
    -- | All of the data files used by the Im backend are based in this
    -- directory.  Everything in here should be temporary files, used for
    -- communication or caching.
    imDir :: FilePath
    , synths :: Map SynthName Synth
    }
    deriving (Eq, Show)

getConfig :: IO Config
getConfig = config <$> Path.get_app_dir

config :: Path.AppDir -> Config
config appDir = Config
    { imDir = Path.absolute appDir Config.im_dir
    , synths = Map.fromList
        [ (samplerName, sampler)
        , (faustName, faust)
        , (nessName, ness)
        ]
    }

data Synth = Synth {
    -- | This should uniquely determine the synth, since it becomes the notes
    -- filename.
    synthName :: !FilePath
    -- | Path to the binary.  Don't run a binary if it's empty.
    , binary :: !FilePath
    } deriving (Eq, Show)

type SynthName = Text

nessName :: SynthName
nessName = "ness"

samplerName :: SynthName
samplerName = "sampler"

sampler :: Synth
sampler = Synth
    { synthName = "sampler"
    , binary = "build/opt/sampler-im"
    }

-- | Base directory for sampler patches.
samplerRoot :: Path.Relative
samplerRoot = Config.data_dir Path.</> "sampler"

-- | This is samplerRoot, but as an absolute path.
--
-- Technically, Path.get_app_dir is in IO, so I can't get an absolute path
-- without IO.  However, I need to put the absolute path in the OSC play msgs
-- (ultimately since play_cache runs with an unknown CWD), and it gets really
-- annoying to try to get a Path.AppDir into the thru function's closure.  So
-- unsafePerformIO it is.  I could probably just put that on app_dir, but this
-- is the only thing that actually needs it.
unsafeSamplerRoot :: FilePath
unsafeSamplerRoot =
    Path.absolute (Unsafe.unsafePerformIO Path.get_app_dir) samplerRoot
{-# NOINLINE unsafeSamplerRoot #-}

faustName :: SynthName
faustName = "faust"

faust :: Synth
faust = Synth
    { synthName = "faust"
    , binary = "build/opt/faust-im"
    }

ness ::Synth
ness = Synth
    { synthName = "ness"
    , binary = ""
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
    output: im/cache/scorePath/ns/block/inst/###.wav
    play:   scorePath/ns/block, [inst] in mutes
-}

-- | Write serialized notes to this file.
notesFilename :: FilePath -> FilePath
    -- ^ Path to the score, relative to the save dir.  This should uniquely
    -- identify this score.
    -> Id.BlockId -> Synth -> FilePath
notesFilename imDir scorePath blockId synth =
    notesDirectory imDir scorePath blockId </> synthName synth

notesDirectory :: FilePath -> FilePath -> Id.BlockId -> FilePath
notesDirectory imDir scorePath blockId =
    imDir </> notesParentDir </> scorePath </> idFilename blockId

outputDirectory :: FilePath -> FilePath -> Id.BlockId -> FilePath
outputDirectory imDir scorePath blockId =
    imDir </> cacheDir </> scorePath </> idFilename blockId

-- | This is text sent over MIDI to tell play_cache which directory to play
-- from.  Relative to imDir/cacheDir.
playFilename :: FilePath -> Id.BlockId -> FilePath
playFilename scorePath blockId = scorePath </> idFilename blockId

idFilename :: Id.Ident a => a -> FilePath
idFilename id = untxt $ Id.un_namespace ns <> "/" <> name
    where (ns, name) = Id.un_id $ Id.unpack_id id

-- * progress

-- | Emit a progress message.  The sequencer should be able to parse these to
-- show render status.  It shows the end of the chunk being rendered, so it can
-- highlight the time range which is in progress.
emitProgress :: FilePath -> Set Id.TrackId -> RealTime -> RealTime -> Text
    -> IO ()
emitProgress outputDir trackIds start end extra = do
    Log.notice $ Text.unwords $
        "progress" : pathToBlockId outputDir : tracks
        : [pretty start <> "--" <> pretty end, extra]
    emitMessage outputDir trackIds "progress" [time start, time end]
    where
    time = showt . RealTime.to_seconds
    tracks = Text.intercalate "," $ map Id.ident_text $ Set.toList trackIds

-- | Parse the line emitted by 'progress'.
parseProgress :: Text
    -> Maybe (Id.BlockId, Set Id.TrackId, (RealTime, RealTime))
parseProgress line = do
    ("progress", blockId, trackIds, [start, end]) <- parseMessage line
    start <- time start
    end <- time end
    return (blockId, trackIds, (start, end))
    where
    time = fmap RealTime.seconds . Parse.parse_maybe Parse.p_float

emitFailure :: FilePath -> Set Id.TrackId -> Text -> IO ()
emitFailure outputDir trackIds msg =
    emitMessage outputDir trackIds "failure" [msg]

parseFailure :: Text -> Maybe (Id.BlockId, Set Id.TrackId, Text)
parseFailure line = do
    ("failure", blockId, trackIds, msg) <- parseMessage line
    return (blockId, trackIds, Text.unwords msg)

emitMessage :: FilePath -> Set Id.TrackId -> Text -> [Text] -> IO ()
emitMessage outputDir trackIds msg words = Log.with_stdio_lock $ do
    Text.IO.putStrLn $ Text.unwords $
        msg : pathToBlockId outputDir : tracks : words
    IO.hFlush IO.stdout
    where
    tracks = Text.intercalate "," $ map Id.ident_text $ Set.toList trackIds

-- | Infer namespace/block from
-- .../im/cache/$scorePath/$scoreFname/$namespace/$block/$instrument
pathToBlockId :: FilePath -> Text
pathToBlockId =
    Text.intercalate "/" . take 2 . Seq.rtake 3 . Text.splitOn "/" . txt

parseMessage :: Text -> Maybe (Text, Id.BlockId, Set Id.TrackId, [Text])
parseMessage line = case Text.words line of
    msg : block : tracks : args -> Just
        ( msg
        , Id.BlockId (Id.read_id block)
        , Set.fromList $ map (Id.TrackId . Id.read_id) (Text.splitOn "," tracks)
        , args
        )
    _ -> Nothing

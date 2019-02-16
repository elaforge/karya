-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Shared config to coordinate between the sequencer and im subsystems.
module Synth.Shared.Config where
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Generics as Generics
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Ui.Id as Id

import           Global
import           Synth.Types

#include "config.h"


-- | Index into the audio chunks for a single instrument render.
-- This is the same as 'Ui.Types.ChunkNum'.
type ChunkNum = Int
-- | This is the same as 'Synth.Shared.Note.InstrumentName' but I don't want to
-- import that here.
type InstrumentName = Text

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

-- | Number of frames in each audio block.
blockSize :: Audio.Frame
blockSize = Audio.Frame $ samplingRate `div` 4

-- | Save an audio chunk and checkpoint in this many frames.  This should be
-- an integral multiple of 'blockSize', so checkpoint state lines up with audio
-- output.
chunkSize :: Audio.Frame
chunkSize = Audio.Frame $ samplingRate * chunkSeconds

chunkSeconds :: Int
chunkSeconds = CHUNK_SECONDS

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

-- | Get the filename for a particular checkpoint.
chunkPath :: FilePath -> FilePath -> Id.BlockId -> Text -> ChunkNum -> FilePath
chunkPath imDir scorePath blockId instrument chunknum =
    outputDirectory imDir scorePath blockId
        </> untxt instrument
        </> untxt (Num.zeroPad 3 chunknum <> ".wav")

-- | This is text sent over MIDI to tell play_cache which directory to play
-- from.  Relative to imDir/cacheDir.
playFilename :: FilePath -> Id.BlockId -> FilePath
playFilename scorePath blockId = scorePath </> idFilename blockId

idFilename :: Id.Ident a => a -> FilePath
idFilename id = untxt $ Id.un_namespace ns <> "/" <> name
    where (ns, name) = Id.un_id $ Id.unpack_id id

-- * progress

data Message = Message {
    _blockId :: !Id.BlockId
    , _trackIds :: !(Set Id.TrackId)
    , _instrument :: !InstrumentName
    , _payload :: !MessageT
    }
    deriving (Show, Generics.Generic)

instance Aeson.ToJSON Message where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Message

data MessageT =
    ProgressT !Progress
    -- | Cache hit on the first n chunks.
    | SkippedT !ChunkNum
    -- | A failure will cause karya to log the msg and mark the track as
    -- incomplete.  It should be fatal, so don't emit any 'emitProgress'
    -- afterwards.
    | FailureT !Text
    deriving (Show, Generics.Generic)

instance Aeson.ToJSON MessageT where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON MessageT

-- | A progress message.  The sequencer should be able to parse these to show
-- render status.  It shows the end of the chunk being rendered, so it can
-- highlight the time range which is in progress.
data Progress = Progress {
    _renderedPrevChunk :: !Bool
    , _chunknum :: !ChunkNum
    , _range :: !(RealTime, RealTime)
    } deriving (Show, Generics.Generic)

instance Aeson.ToJSON Progress where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Progress

emitMessage :: Text -> Message -> IO ()
emitMessage extra msg = do
    let json = Aeson.encode msg
    let prio = case _payload msg of
            ProgressT {} -> Log.Notice
            SkippedT {} -> Log.Notice
            FailureT {} -> Log.Warn
    Log.log prio $ Text.unwords $
        [ Id.ident_text (_blockId msg)
        , pretty (Set.map Id.ident_text (_trackIds msg))
        , case _payload msg of
            ProgressT progress ->
                showt (_chunknum progress) <> " "
                    <> pretty start <> "--" <> pretty end
                where (start, end) = _range progress
            SkippedT chunknum -> "skipped to " <> showt chunknum
            FailureT err -> err
        ] ++ if Text.null extra then [] else [extra]
    Log.with_stdio_lock $ do
        ByteString.Lazy.Char8.putStrLn json
        IO.hFlush IO.stdout

parseMessage :: Text -> Maybe Message
parseMessage = Aeson.decode . TextUtil.toLazyByteString

-- | Infer namespace/block from
-- .../im/cache/$scorePath/$scoreFname/$namespace/$block/$instrument
pathToBlockId :: FilePath -> Id.BlockId
pathToBlockId = Id.BlockId . Id.read_id
    . Text.intercalate "/" . take 2 . Seq.rtake 3 . Text.splitOn "/" . txt

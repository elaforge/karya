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
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified GHC.Generics as Generics
import qualified GHC.Stack
import qualified Network.Socket as Socket
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe

import qualified Text.Read as Read

import qualified Util.Audio.AudioT as AudioT
import qualified Util.Exceptions as Exceptions
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Util.Texts as Texts

import qualified App.Config
import qualified App.Config as Config
import qualified App.Path as Path

import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Ui.Id as Id

import           Global
import           Synth.Types

#include "config.h"


-- | Index into the audio chunks for a single instrument render.
-- This is the same as 'Ui.Types.ChunkNum'.
type ChunkNum = Int

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

getLogFilename :: String -> IO FilePath
getLogFilename name = do
    dir <- Path.to_absolute <$> Path.get_app_dir <*> pure App.Config.log_dir
    return $ dir </> name

config :: Path.AppDir -> Config
config appDir = Config
    { imDir = Path.to_absolute appDir Config.im_dir
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
-- without IO.  However, I need to put the absolute path in the Thru play msgs
-- (ultimately since play_cache runs with an unknown CWD), and it gets really
-- annoying to try to get a Path.AppDir into the thru function's closure.  So
-- unsafePerformIO it is.  I could probably just put that on app_dir, but this
-- is the only thing that actually needs it.
unsafeSamplerRoot :: FilePath
unsafeSamplerRoot =
    Path.to_absolute (Unsafe.unsafePerformIO Path.get_app_dir) samplerRoot
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

-- | Save an audio chunk and checkpoint in this many frames.
--
-- A smaller size will lead to more checkpoints, which means finer grained
-- caching, but more overhead saving the intermediate states.  So a slower
-- synthesizer with smaller state should use small chunks, fast rendering or
-- large state imply large chunks.  I could in theory adjust this per
-- synthesizer, though currently karya relies on it being constant.
chunkSize :: AudioT.Frames
chunkSize = AudioT.Frames $ samplingRate * chunkSeconds

-- | Number of frames in each audio block.  A chunk corresponds to the output
-- file size, and the block is the internal processing size.
--
-- To make sure checkpoint states line up with the file boundaries, this must
-- divide into 'chunkSize'.
blockSize :: AudioT.Frames
blockSize = chunkSize `Num.assertDiv` 16

chunkSeconds :: Int
chunkSeconds = CHUNK_SECONDS

-- | play_cache delays play start by this many frames, so MIDI output should
-- also be delayed by this much to match.
--
-- It has to cue up the sample streaming, which means it has to find and
-- seek to the right file.  If playback starts immediately then the first
-- chunk gets cut off, which cuts off note attacks.
startLatency :: AudioT.Frames
startLatency = START_LATENCY_FRAMES

thruPort :: Socket.PortNumber
thruPort = THRU_PORT

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

-- | Top level output for for a block render.  It will have directories below
-- it for each instrument.
outputDirectory :: FilePath -> FilePath -> Id.BlockId -> FilePath
outputDirectory imDir scorePath blockId =
    imDir </> cacheDir </> scorePath </> idFilename blockId

-- | Get the filename for a particular checkpoint.
chunkPath :: FilePath -> FilePath -> Id.BlockId -> InstrumentDir -> ChunkNum
    -> FilePath
chunkPath imDir scorePath blockId (InstrumentDir instrument) chunknum =
    outputDirectory imDir scorePath blockId
        </> instrument </> chunkName chunknum

chunkName :: ChunkNum -> FilePath
chunkName chunknum = untxt (Num.zeroPad 3 chunknum <> ".wav")

-- | This relies on the format from 'chunkName'.
isOutputLink :: FilePath -> Maybe ChunkNum
isOutputLink (c1:c2:c3 : ".wav")
    | Just n <- Read.readMaybe [c1, c2, c3] = Just n
    | otherwise = Nothing
isOutputLink _ = Nothing

-- | This is text sent over MIDI to tell play_cache which directory to play
-- from.  Relative to imDir/cacheDir.
playFilename :: FilePath -> Id.BlockId -> FilePath
playFilename scorePath blockId = scorePath </> idFilename blockId

idFilename :: Id.Ident a => a -> FilePath
idFilename id = untxt $ Id.un_namespace ns <> "/" <> name
    where (ns, name) = Id.un_id $ Id.unpack_id id

-- | Delete output links for instruments that have disappeared entirely.
-- This often happens when I disable a track.
clearUnusedInstruments :: FilePath -> HashSet ScoreT.Instrument -> IO ()
clearUnusedInstruments outputDir instruments = do
    dirs <- filterM (Directory.doesDirectoryExist . (outputDir</>))
        =<< listDir outputDir
    let unused = filter
            (not . (`HashSet.member` instruments) . dirInstrument
                . instrumentDir)
            dirs
    forM_ unused $ \dir -> do
        links <- filter (Maybe.isJust . isOutputLink) <$>
            listDir (outputDir </> dir)
        mapM_ (Directory.removeFile . ((outputDir </> dir) </>)) links

-- | There is a subdirectory for each instrument, but it has extra info, so it
-- can't directly be a ScoreT.Instrument.  Instruments never have '_', so I can
-- use that to put extra info on the end.  For faust, I put the patch name, so
-- I can clear obsolete checkpoints when the patch changes.
newtype InstrumentDir = InstrumentDir FilePath
    deriving (Eq, Show, Pretty, Aeson.ToJSON, Aeson.FromJSON)

instrumentDir :: FilePath -> InstrumentDir
instrumentDir = InstrumentDir . FilePath.takeFileName

instrumentDir2 :: ScoreT.Instrument -> Maybe String -> FilePath
instrumentDir2 inst extra =
    untxt (ScoreT.instrument_name inst) <> maybe "" ("_"<>) extra

dirInstrument :: InstrumentDir -> ScoreT.Instrument
dirInstrument (InstrumentDir dir) =
    ScoreT.Instrument $ txt $ takeWhile (/='_') dir

listDir :: FilePath -> IO [FilePath]
listDir = fmap (fromMaybe []) . Exceptions.ignoreEnoent
    . Directory.listDirectory

-- * progress

data Message = Message {
    _blockId :: !Id.BlockId
    , _trackIds :: !(Set Id.TrackId)
    , _instrument :: !InstrumentDir
    , _payload :: !Payload
    }
    deriving (Show, Generics.Generic)

instance Aeson.ToJSON Message where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Message

data Payload =
    RenderingRange !RealTime !RealTime
    -- | Completed waveforms.
    | WaveformsCompleted ![ChunkNum]
    | Warn !Stack.Stack !Text
    -- | A failure will cause karya to log the msg and mark the track as
    -- incomplete.  It should be fatal, so don't do any 'emitMessage'
    -- afterwards.
    | Failure !Text
    deriving (Show, Generics.Generic)

instance Aeson.ToJSON Payload where
    toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance Aeson.FromJSON Payload

emitMessage :: GHC.Stack.HasCallStack => Message -> IO ()
emitMessage msg = do
    let prio = case _payload msg of
            RenderingRange {} -> Log.Debug
            WaveformsCompleted {} -> Log.Debug
            Warn {} -> Log.Warn
            Failure {} -> Log.Warn
    Log.log prio $ Text.unwords $
        [ Id.ident_text (_blockId msg)
        , pretty (Set.map Id.ident_text (_trackIds msg))
        , case _payload msg of
            RenderingRange start end -> pretty start <> "--" <> pretty end
            WaveformsCompleted chunknums -> "completed: " <> pretty chunknums
            Warn stack err -> pretty stack <> ": " <> err
            Failure err -> err
        ]
    Log.with_stdio_lock $ do
        ByteString.Lazy.Char8.putStrLn $ Aeson.encode msg
        IO.hFlush IO.stdout

parseMessage :: Text -> Maybe Message
parseMessage = Aeson.decode . Texts.toLazyByteString

-- | Infer namespace/block from
-- .../im/cache/$scorePath/$scoreFname/$namespace/$block/$instrument
pathToBlockId :: FilePath -> Id.BlockId
pathToBlockId = Id.BlockId . Id.read_id
    . Text.intercalate "/" . take 2 . Seq.rtake 3 . Text.splitOn "/" . txt

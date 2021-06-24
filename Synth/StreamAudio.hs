-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Stream audio from the im cache.  Recreates play_cache's behaviour, but
-- in a standalone way, without a DAW and VST.
module Synth.StreamAudio (
    streamFrom, streamFrom_
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Thread as Thread

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import qualified Ui.Id as Id

import           Global
import           Synth.Types


type Muted = Set Note.InstrumentName

-- | If true, spam stdout even when run from karya.
verbose :: Bool
verbose = True

-- | Stream audio for the give score and block, until done or told to stop.
--
-- This is essentially a haskell version of TrackStreamer (Streamer.h) ->
-- Tracks (Tracks.h)
streamFrom :: IO () -> FilePath -> Id.BlockId -> Set Note.InstrumentName
    -> RealTime -> IO ()
streamFrom waitQuit scorePath blockId muted start = do
    config <- Config.getConfig
    streamFrom_ verbose waitQuit
        (Config.outputDirectory (Config.imDir config) scorePath blockId)
        muted start

streamFrom_ :: Bool -> IO () -> FilePath -> Set Note.InstrumentName
    -> RealTime -> IO ()
streamFrom_ verbose waitQuit dir muted start =
    playSox verbose waitQuit
        =<< streamTracks verbose dir muted (AUtil.toFrames start)

streamTracks :: Bool -> FilePath -> Muted -> Audio.Frames -> IO AUtil.Audio
streamTracks verbose dir muted start = do
    dirnames <- sampleDirs dir muted
    when verbose $
        putStrLn $ "stream " <> show dirnames <> " from " <> show start
    return $ Audio.mix $ map
        (Audio.File.readCheckpointsFrom start
            (if verbose then putStrLn . ("open "<>) else const (pure ()))
            Config.chunkSize . dirChunks)
        dirnames
    where
    dirChunks dir = map (dir</>) chunks
    chunks = map Config.chunkName [0..]

sampleDirs :: FilePath -> Muted -> IO [FilePath]
sampleDirs dir muted = do
    subdirs <- filterM (Directory.doesDirectoryExist . (dir</>))
        . filter (not . ("." `List.isPrefixOf`))
        =<< Directory.listDirectory dir
    when (null subdirs) $
        putStrLn $ "no sample dirs in " <> show dir
    return $ map (dir</>) $ filter (not . instrumentMuted muted) subdirs

instrumentMuted :: Muted -> FilePath -> Bool
instrumentMuted muted fname = Set.member inst muted
    where inst = txt $ takeWhile (/='_') fname
    -- Instruments never have '_', so I can use that to put extra info on the
    -- end.  For faust, I put the patch name, so I can clear obsolete
    -- checkpoints when the patch changes.

-- | Use sox to stream audio to the hardware.  This is easy and portable but
-- high latency.
playSox :: Bool -> IO () -> AUtil.Audio -> IO ()
playSox verbose waitQuit audio = do
    putStrLn $ unwords $ "%" : "sox" : soxArgs
    (Just stdin, Nothing, Nothing, pid) <- Process.createProcess $
        (Process.proc "sox" soxArgs) { Process.std_in = Process.CreatePipe }
    Thread.start $ waitQuit >> Process.terminateProcess pid
    Thread.start $ do
        Resource.runResourceT $ Audio.File.writeHandle stdin audio
        IO.hClose stdin
    code <- Process.waitForProcess pid
    case code of
        Exit.ExitSuccess -> return ()
        Exit.ExitFailure code
            | code == -15 -> when verbose $ putStrLn "sox terminated"
            | otherwise -> errorIO $ "sox failed: " <> showt code

soxArgs :: [String]
soxArgs =
    [ "--no-show-progress"
    , "-V1" -- turn down verbosity, to avoid clipped sample warnings
    , "--type=raw", "--channels=2", "--bits=32", "--encoding=floating-point"
    , "--rate=44100"
    , "-" -- read audio from stdin
    , "--default-device" -- play audio to speaker
    ]

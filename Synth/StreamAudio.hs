-- Copyright 2021 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Stream audio from the im cache.  Recreates play_cache's behaviour, but
-- in a standalone way, without a DAW and VST.
module Synth.StreamAudio (
    play
    , streamDir
    , Device(..)
    , getDevices
) where
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.IO.Unsafe as Unsafe
import qualified System.Process as Process

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.PortAudio as PortAudio
import qualified Util.Lists as Lists
import qualified Util.Thread as Thread

import qualified Derive.ScoreT as ScoreT
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Ui.Id as Id

import           Global
import           Synth.Types


type Muted = Set ScoreT.Instrument

-- | If true, spam stdout even when run from karya.
verbose :: Bool
verbose = True

-- | Stream audio for the give score and block, until done or told to stop.
--
-- The audio backend is hardcoded, but perhaps I should get one from
-- StaticConfig.
--
-- This is essentially a haskell version of TrackStreamer (Streamer.h) ->
-- Tracks (Tracks.h)
play :: Maybe Device -> Thread.Flag -> FilePath -> Id.BlockId
    -> Muted -> RealTime -> IO ()
play mbDevice quit scorePath blockId muted start = do
    config <- Config.getConfig
    let dir = Config.outputDirectory (Config.imDir config) scorePath blockId
    streamDir mbDevice quit muted start dir

streamDir :: Maybe Device -> Thread.Flag -> Muted -> RealTime -> FilePath
    -> IO ()
streamDir mbDevice quit muted start dir = case mbDevice of
    Just Sox -> streamToSox verbose (Thread.wait quit) dir muted start
    Just (PortAudio device) ->
        streamToPortAudio verbose device quit dir muted start
    Nothing -> do
        device <- PortAudio.getDefaultOutput
        streamToPortAudio verbose device quit dir muted start

data Device = Sox | PortAudio PortAudio.Device
    deriving (Show)

getDevices :: IO ([(String, Device)], String)
getDevices = do
    deflt <- PortAudio.getDefaultOutput
    (, PortAudio._name deflt) . Lists.keyOn name_of . (++[Sox]) . map PortAudio
        <$> PortAudio.getOutputDevices
    where
    name_of Sox = "sox"
    name_of (PortAudio dev) = PortAudio._name dev

streamToPortAudio :: Bool -> PortAudio.Device -> Thread.Flag -> FilePath
    -> Muted -> RealTime -> IO ()
streamToPortAudio verbose dev quit dir muted start = do
    MVar.modifyMVar_ currentlyPlaying $ \prevQuit -> do
        whenJust prevQuit Thread.set
        return $ Just quit
    Resource.runResourceT . PortAudio.play (Thread.poll 0 quit) dev
        =<< streamTracks verbose dir muted (AUtil.toFrames start)

-- | Stash the previous quit flag so I can turn it off when I get a new play
-- request.  Even though both CoreAudio and JACK seem to be ok with multiple
-- streams, I don't want them anyway.
--
-- The audio card is a global resource so I feel ok about making this a global
-- variable.
{-# NOINLINE currentlyPlaying #-}
currentlyPlaying :: MVar.MVar (Maybe Thread.Flag)
currentlyPlaying = Unsafe.unsafePerformIO $ MVar.newMVar Nothing

streamToSox :: Bool -> IO () -> FilePath -> Muted -> RealTime -> IO ()
streamToSox verbose waitQuit dir muted start =
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
    return $ map (dir</>) $
        filter ((`Set.notMember` muted) . Config.dirToInstrument) subdirs

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

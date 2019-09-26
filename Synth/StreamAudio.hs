{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Read audio chunks over time, concatenate them, and stream to an audio
-- player.  The reason is that sox, by itself, wants to concatenate all its
-- audio files immediately, but I have to actually stream them if I want to
-- overlap playing with generation.
module Synth.StreamAudio where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Time as Time
import qualified Data.Vector.Storable as Vector
import qualified Foreign.Storable as Storable
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.FilePath ((</>))
import qualified System.IO as IO
import qualified System.Process as Process

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Control as Control
import qualified Util.Num as Num
import qualified Util.Thread as Thread

import qualified Synth.Shared.Config as Config

import           Global


main :: IO ()
main = do
    dir <- Environment.getArgs >>= \case
        [dir] -> return dir
        _ -> error $ "usage: stream-audio dir"
    (Just stdin, Nothing, Nothing, pid) <- Process.createProcess $
        (Process.proc "sox" soxArgs) { Process.std_in = Process.CreatePipe }
    streamDir dir stdin
    IO.hClose stdin
    Process.waitForProcess pid
    return ()

soxArgs :: [String]
soxArgs =
    [ "--no-show-progress"
    , "--type=raw", "--channels=2", "--bits=32", "--encoding=floating-point"
    , "--rate=44100"
    , "-", "--default-device"
    ]

streamDir :: FilePath -> IO.Handle -> IO ()
streamDir dir outHdl = do
    while_ (not <$> Directory.doesFileExist (dir </> head chunks)) $
        Thread.delay 0.25
    streamInTime outHdl $ Audio.File.readCheckpoints Config.chunkSize $
        map (dir</>) chunks
    where
    chunks = map (untxt . (<>".wav") . Num.zeroPad 3) [0..]

-- | Write samples to the handle, stay a short distance ahead of real-time, in
-- case the the stream is still being rendered.
--
-- The wait time always winds up being negative though, probably because sox
-- blocks stdin, so the waiting is probably unnecessary.  Still I might as well
-- keep it since I have it, and sox's behaviour might not be guaranteed anyway.
streamInTime :: IO.Handle -> Audio.AudioIO Config.SamplingRate 2 -> IO ()
streamInTime hdl audio = Resource.runResourceT $ do
    start <- liftIO Time.getCurrentTime
    Control.loop2 audio 0 $ \loop audio played -> do
        (blocks, audio) <- Audio.splitAt leadFrames audio
        if null blocks then return () else do
            liftIO $ mapM_ (writeVector hdl) $
                concatMap Audio.blockSamples blocks
            now <- liftIO Time.getCurrentTime
            let elapsed = now `Time.diffUTCTime` start
            let lead = toSeconds played - elapsed
            -- liftIO $ put $ "elapsed " <> show elapsed <> ", lead "
            --     <> show lead <> " wait " <> show (lead - leadTime)
            liftIO $ Thread.delay $ lead - leadTime
            -- played - elapsed - leadTime
            -- 0 - 0 - 2 = -2
            -- 1 - 0 - 2 = -1
            -- 2 - 0 - 2 = 0
            -- 3 - 0 - 2 = 1 => wait
            -- 3 - 1 - 2 = 0
            loop audio (played + leadFrames)
    where
    leadTime = 1
    leadFrames = Audio.secondsToFrames rate (realToFrac leadTime) `div` 2
    toSeconds :: Audio.Frames -> Time.NominalDiffTime
    toSeconds = realToFrac . Audio.framesToSeconds rate
    rate = Config.samplingRate

writeVector :: IO.Handle -> Vector.Vector Audio.Sample -> IO ()
writeVector hdl vector = Vector.unsafeWith vector $ \ptr ->
    IO.hPutBuf hdl ptr $
        Vector.length vector * Storable.sizeOf (0 :: Audio.Sample)

-- streamFile :: (Vector.Vector Float -> IO ()) -> FilePath -> IO Bool
-- streamFile write fname = open >>= \case
--     Nothing -> return False
--     Just hdl -> do
--         put $ FilePath.takeFileName fname
--         Control.loop0 $ \loop ->
--             Sndfile.hGetBuffer hdl bufferSize >>= \case
--                 Nothing -> Sndfile.hClose hdl >> return True
--                 Just buffer -> do
--                     write $ Sndfile.Buffer.Vector.fromBuffer buffer
--                     loop
--     where
--     open = File.ignoreEnoent $
--         Sndfile.openFile fname Sndfile.ReadMode Sndfile.defaultInfo
--     bufferSize = 512

put :: String -> IO ()
put = IO.hPutStrLn IO.stderr

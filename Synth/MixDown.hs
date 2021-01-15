{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Mix multiple directories of rendered chunks into a single audio file.
module Synth.MixDown where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Synth.Shared.Config as Config

import           Global


main :: IO ()
main = do
    (out, dirs) <- Environment.getArgs >>= \case
        out : dirs
            | ".wav" `List.isSuffixOf` out -> return (out, dirs)
            | otherwise -> usage "out arg should be .wav!"
        [] -> usage ""
    unlessM (allM Directory.doesDirectoryExist dirs) $
        usage "non-directory arg"
    streams <- mapM streamDir dirs
    Resource.runResourceT $ Audio.File.write Audio.File.wavFormat out $
        Audio.mix streams

usage :: String -> IO a
usage msg = do
    unless (null msg) $
        IO.hPutStrLn IO.stderr $ "error: " <> msg
    IO.hPutStrLn IO.stderr $ "usage: mixdown out.wav [ dir1 dir2 ... ]"
    Exit.exitFailure

streamDir :: FilePath -> IO (Audio.AudioIO Config.SamplingRate 2)
streamDir dir = do
    chunks <- List.sort . filter (".wav" `List.isSuffixOf`) <$>
        Directory.listDirectory dir
    when (null chunks) $
        IO.hPutStrLn IO.stderr $ "WARNING: no *.wav in " <> dir
    return $ Audio.File.readCheckpoints Config.chunkSize (map (dir</>) chunks)

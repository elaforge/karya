{-# LANGUAGE DataKinds #-}
module Synth.Lib.Mix (mix) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.List as List
import qualified System.Directory as Directory
import           System.FilePath ((</>))
import qualified System.IO as IO

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Synth.Shared.Config as Config

import           Global


mix :: FilePath -> [FilePath] -> IO ()
mix out dirs = do
    streams <- mapM streamDir dirs
    Resource.runResourceT $ Audio.File.write Audio.File.wavFormat out $
        Audio.mix streams

streamDir :: FilePath -> IO (Audio.AudioIO Config.SamplingRate 2)
streamDir dir = do
    chunks <- List.sort . filter (".wav" `List.isSuffixOf`) <$>
        Directory.listDirectory dir
    when (null chunks) $
        IO.hPutStrLn IO.stderr $ "WARNING: no *.wav in " <> dir
    return $ Audio.File.readCheckpoints Config.chunkSize (map (dir</>) chunks)

-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
-- | Offline sampler.
module Synth.Sampler.Main (main) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Sound.File.Sndfile as Sndfile
import qualified System.Environment as Environment
import System.FilePath ((</>))

import qualified Util.Log as Log
import Global
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Note as Note
import qualified Synth.Sampler.Sample as Sample
import Synth.Sampler.Types


main :: IO ()
main = do
    args <- Environment.getArgs
    case args of
        [notesFn] -> process notesFn "synth_cache"
        _ -> errorIO $ "usage: sampler notes.json"

process :: FilePath -> FilePath -> IO ()
process notesFn outputDir = do
    notes <- loadNotes notesFn
    samples <- either (errorIO . untxt) return $ mapM Convert.noteToSample notes
    mapM_ print samples
    realizeSamples outputDir samples

loadNotes :: FilePath -> IO [Note.Note]
loadNotes notesFn = do
    val <- Aeson.decode <$> ByteString.Lazy.readFile notesFn
    maybe (errorIO "can't parse json") return val

realizeSamples :: FilePath -> [Sample.Sample] -> IO ()
realizeSamples outputDir samples = do
    putStrLn $ "load " <> show (length samples) <> " samples"
    audios <- mapM realizeSample samples
    putStrLn "processing"
    result <- Sample.catchSndfile $ Resource.runResourceT $
        Sndfile.sinkSnd (outputDir </> "out.wav") outputFormat
            (mixAll (Maybe.catMaybes audios))
    case result of
        Left err -> Log.error $ "writing to output: "
            <> showt (outputDir </> "out.wav") <> ": " <> err
        Right () -> return ()
    putStrLn "done"

realizeSample :: Sample.Sample -> IO (Maybe Audio)
realizeSample sample = Sample.catchSndfile (Sample.realize sample) >>= \case
    Left err -> do
        Log.warn $ "sample " <> txt (Sample.filename sample) <> ": " <> err
        return Nothing
    Right audio -> return (Just audio)

mixAll :: [Audio] -> Audio
mixAll [] = Audio.silent (Audio.Frames 0) 44100 2 -- TODO
mixAll audios = List.foldl1' Audio.mix audios

-- I'd write flac, but it's integer only.
outputFormat :: Sndfile.Format
outputFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat
    , endianFormat = Sndfile.EndianFile
    }

-- outputFormat :: Sndfile.Format
-- outputFormat = Sndfile.Format
--     { headerFormat = Sndfile.HeaderFormatWav
--     , sampleFormat = Sndfile.SampleFormatPcm16
--     , endianFormat = Sndfile.EndianFile
--     }

outputInfo :: Sndfile.Info
outputInfo = Sndfile.Info
    { frames = 0
    , samplerate = 44100
    , channels = 2
    , format = outputFormat
    , sections = 1
    , seekable = True
    }

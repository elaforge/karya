-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Log as Log

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global


main :: IO ()
main = do
    args <- Environment.getArgs
    let quality
            | "--fast" `elem` args = Resample.SincFastest
            | otherwise = Resample.SincBestQuality
    case filter (/="--fast") args of
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process quality notesFilename notes
        _ -> errorIO $ "usage: sampler [ --fast ] notes"

process :: Resample.ConverterType -> FilePath -> [Note.Note] -> IO ()
process quality notesFilename notes = do
    samples <- either errorIO return $ mapM Convert.noteToSample notes
    mapM_ print samples
    realizeSamples quality notesFilename samples

realizeSamples :: Resample.ConverterType -> FilePath -> [Sample.Sample] -> IO ()
realizeSamples quality notesFilename samples = do
    put $ "load " <> show (length samples) <> " samples"
    put "processing"
    -- TODO divide up output by instrument instead of mixing them here
    let output = Config.outputFilename (Config.rootDir Config.config)
            notesFilename Nothing
    result <- AUtil.catchSndfile $ Resource.runResourceT $
        Audio.File.write AUtil.outputFormat output $
            AUtil.mix $ map (Sample.realize quality) samples
    case result of
        Left err ->
            Log.error $ "writing to output: " <> showt output <> ": " <> err
        Right () -> return ()
    put $ "wrote to " <> output
    where
    put = putStrLn . ((FilePath.takeFileName notesFilename <> ": ")<>)

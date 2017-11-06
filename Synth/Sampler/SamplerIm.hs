-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE LambdaCase #-}
-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

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
    case args of
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process notesFilename notes
        _ -> errorIO $ "usage: sampler notes"

process :: FilePath -> [Note.Note] -> IO ()
process notesFilename notes = do
    samples <- either errorIO return $ mapM Convert.noteToSample notes
    mapM_ print samples
    realizeSamples notesFilename samples

realizeSamples :: FilePath -> [Sample.Sample] -> IO ()
realizeSamples notesFilename samples = do
    put $ "load " <> show (length samples) <> " samples"
    audios <- mapM realizeSample samples
    put "processing"
    -- TODO divide up output by instrument instead of mixing them here
    let output = Config.outputFilename notesFilename Nothing
    result <- AUtil.catchSndfile $ Resource.runResourceT $
        Sndfile.sinkSnd output AUtil.outputFormat
            (mixAll (Maybe.catMaybes audios))
    case result of
        Left err ->
            Log.error $ "writing to output: " <> showt output <> ": " <> err
        Right () -> return ()
    put "done"
    where
    put = putStrLn . ((FilePath.takeFileName notesFilename <> ": ")<>)

realizeSample :: Sample.Sample -> IO (Maybe AUtil.Audio)
realizeSample sample = AUtil.catchSndfile (Sample.realize sample) >>= \case
    Left err -> do
        Log.warn $ "sample " <> txt (Sample.filename sample) <> ": " <> err
        return Nothing
    Right audio -> return (Just audio)

mixAll :: [AUtil.Audio] -> AUtil.Audio
-- TODO surely there is some better way to do mempty?
mixAll [] = AUtil.empty
mixAll audios = List.foldl1' Audio.mix audios

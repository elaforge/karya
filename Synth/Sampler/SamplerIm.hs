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
import System.FilePath ((</>))

import qualified Util.Log as Log
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global


main :: IO ()
main = do
    let process_ name = process Config.cacheDir name <=< either errorIO return
    args <- Environment.getArgs
    case args of
        [fname] -> process_ name . first pretty =<< Note.unserialize fname
            where name = FilePath.takeFileName fname
        _ -> errorIO $ "usage: sampler notes"

process :: FilePath -> String -> [Note.Note] -> IO ()
process outputDir name notes = do
    samples <- either errorIO return $ mapM Convert.noteToSample notes
    mapM_ print samples
    realizeSamples outputDir name samples

realizeSamples :: FilePath -> String -> [Sample.Sample] -> IO ()
realizeSamples outputDir name samples = do
    put $ "load " <> show (length samples) <> " samples"
    audios <- mapM realizeSample samples
    put "processing"
    result <- AUtil.catchSndfile $ Resource.runResourceT $
        Sndfile.sinkSnd (outputDir </> name <> ".wav") AUtil.outputFormat
            (mixAll (Maybe.catMaybes audios))
    case result of
        Left err -> Log.error $ "writing to output: "
            <> showt (outputDir </> name <> ".wav") <> ": " <> err
        Right () -> return ()
    put "done"
    where
    put = putStrLn . ((name <> ": ")<>)

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

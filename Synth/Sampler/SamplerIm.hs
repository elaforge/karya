-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Trans.Resource as Resource
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global


useCheckpoints :: Bool
useCheckpoints = True

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

process :: Resample.Quality -> FilePath -> [Note.Note] -> IO ()
process quality notesFilename notes = do
    let byInstrument = Seq.keyed_group_sort Note.instrument notes
    Async.forConcurrently_ byInstrument $ \(instrument, notes) -> do
        samples <- either errorIO return $ mapM Convert.noteToSample notes
        realizeSamples quality notesFilename instrument samples

realizeSamples :: Resample.Quality -> FilePath -> Note.InstrumentName
    -> [Sample.Sample] -> IO ()
realizeSamples quality notesFilename instrument samples = do
    notice $ "load " <> showt (length samples) <> " samples"
    let output
            | useCheckpoints = Config.outputDirectory
                (Config.imDir Config.config) notesFilename instrument
            | otherwise = Config.outputFilename (Config.imDir Config.config)
                notesFilename instrument
    Directory.createDirectoryIfMissing True $
            if useCheckpoints then output else FilePath.takeDirectory output
    mbErr <- if useCheckpoints
        then writeCheckpoints quality output samples
        else fmap (either Just (const Nothing)) $ AUtil.catchSndfile $
            Resource.runResourceT $ Audio.File.write AUtil.outputFormat output $
                AUtil.mix $ map (Sample.realize quality) samples
    case mbErr of
        Nothing -> notice $ "done: " <> txt output
        Just err -> Log.error $ "writing " <> txt output <> ": " <> err
    notice $ "wrote to " <> txt output
    where
    notice :: CallStack.Stack => Text -> IO ()
    notice = Log.notice . ((instrument <> ": ")<>)

-- TODO state checkpoints not fully implemented
writeCheckpoints :: Resample.Quality -> FilePath -> [Sample.Sample]
    -> IO (Maybe Text)
writeCheckpoints quality outputDir samples = either Just (const Nothing) <$> do
    AUtil.catchSndfile $ Resource.runResourceT $
        Audio.File.writeCheckpoints size writeState AUtil.outputFormat fnames $
        -- TODO I think AUtil.mix and Sample.realize don't guarantee that chunk
        -- sizes are a factor of checkpointSize
        AUtil.mix $ map (Sample.realize quality) samples
    where
    size = Audio.Frame Config.checkpointSize
    fnames = map (\n -> outputDir </> untxt (Num.zeroPad 3 n) <> ".wav") [0..]
    writeState _fname = return ()

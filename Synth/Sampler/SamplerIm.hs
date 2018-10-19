-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Sampler.Render as Render
import qualified Synth.Sampler.RenderSample as RenderSample
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
        ["--check"] -> do
            let (reference, samples) = Wayang.checkStarts
            mapM_ (renderStarts . (++[reference])) samples
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process PatchDb.db quality notesFilename notes
        _ -> errorIO $ "usage: sampler [ --fast ] notes"

type Error = Text

process :: Patch.Db -> Resample.Quality -> FilePath -> [Note.Note] -> IO ()
process db quality notesFilename notes = do
    Log.notice $ "processing " <> txt notesFilename
    by Note.patch notes $ \(patch, notes) -> case get patch of
        Nothing -> Log.warn $ "patch not found: " <> patch
        Just patch -> by Note.instrument notes $ \(inst, notes) ->
            realize quality notesFilename inst
                =<< mapM makeNote (convert db patch notes)
    where
    by key xs = Async.forConcurrently_ (Seq.keyed_group_sort key xs)
    get patch = Map.lookup patch (Patch._patches db)

convert :: Patch.Db -> Patch.Patch -> [Note.Note]
    -> [(Either Error Sample.Sample, [Log.Msg], Note.Note)]
convert db patch =
    map update . Seq.key_on (Patch.convert patch) . Patch._preprocess patch
    where
    update (Right ((dur, sample), logs), note) =
        ( Right $ Sample.modifyFilename (patchDir</>) sample
        , logs
        , note { Note.duration = dur }
        )
    update (Left err, note) = (Left err, [], note)
    patchDir = Patch._rootDir db </> Patch._dir patch

-- TODO do this incrementally?  A stream?
makeNote :: (Either Error Sample.Sample, [Log.Msg], Note.Note)
    -> IO Sample.Note
makeNote (errSample, logs, note) = do
    mapM_ Log.write logs
    -- It's important to get an accurate duration if I can, because that
    -- determines overlap, which affects caching.
    mbDur <- case errSample of
        Left _ -> return Nothing
        Right sample -> File.ignoreEnoent $
            RenderSample.predictFileDuration (Sample.ratio sample)
                (Sample.filename sample)
    let newDur = maybe id (min . AUtil.toSeconds) mbDur (Note.duration note)
    when (newDur /= Note.duration note) $
        Log.debug $ "sample " <> pretty errSample <> " dur "
            <> pretty (Note.duration note) <> " -> " <> pretty newDur
    return $ Sample.Note
        { start = Note.start note
        , duration = newDur
        , hash = Note.hash note
        , sample = errSample
        }

realize :: Resample.Quality -> FilePath -> Note.InstrumentName
    -> [Sample.Note] -> IO ()
realize quality notesFilename instrument notes = do
    let output = Config.outputDirectory
            (Config.imDir Config.config) notesFilename instrument
    Directory.createDirectoryIfMissing True output
    (result, elapsed) <- Thread.timeActionText $
        Render.write instrument quality output notes
    case result of
        Left err -> Log.error $ instrument <> ": writing " <> txt output
            <> ": " <> err
        Right (rendered, total) ->
            Log.notice $ instrument <> " " <> showt rendered <> "/"
                <> showt total <> " chunks: " <> txt output
                <> " (" <> elapsed <> ")"

-- * one off testing

renderStarts :: [Sample.Sample] -> IO ()
renderStarts samples = do
    putStrLn $ "==> " <> filename
    exist <- mapM (Directory.doesFileExist . (patchDir</>) . Sample.filename)
        samples
    if all id exist
        then renderDirect filename 1 $
            map (Sample.modifyFilename (patchDir</>)) samples
        else putStrLn "*** missing"
    where
    filename = "check" </> replace '/' '-'
            (FilePath.dropExtension (Sample.filename (head samples)))
            ++ ".wav"
    patchDir = "../data/sampler/wayang"
    replace a b = map (\c -> if c == a then b else c)

renderDirect :: FilePath -> Audio.Seconds -> [Sample.Sample] -> IO ()
renderDirect filename dur samples =
    Resource.runResourceT $
    Audio.File.write AUtil.outputFormat filename $
        Audio.take (Audio.Seconds dur) $ Audio.mix $
        map ((Audio.Frames 0,) . RenderSample.render config 0) samples
    where
    config = Resample.Config {
        _quality = Resample.SincFastest
        , _state = Nothing
        , _notifyState = const $ return ()
        , _chunkSize = Audio.Frame Config.checkpointSize
        , _now = 0
        }

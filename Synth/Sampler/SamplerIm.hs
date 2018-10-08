-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Data.Map as Map
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import System.FilePath ((</>))

import qualified Util.Audio.Resample as Resample
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.Patch as Patch
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
        [notesFilename] -> do
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process PatchDb.db quality notesFilename notes
        _ -> errorIO $ "usage: sampler [ --fast ] notes"

type Error = Text

process :: Patch.Db -> Resample.Quality -> FilePath -> [Note.Note] -> IO ()
process db quality notesFilename notes = do
    by Note.patch notes $ \(patch, notes) -> case get patch of
        Nothing -> Log.warn $ "patch not found: " <> patch
        Just patch -> by Note.instrument notes $ \(inst, notes) ->
            realize quality notesFilename inst
                =<< mapM (uncurry makeNote) (convert db patch notes)
    where
    by key xs = Async.forConcurrently_ (Seq.keyed_group_sort key xs)
    get patch = Map.lookup patch (Patch._patches db)

convert :: Patch.Db -> Patch.Patch -> [Note.Note]
    -> [(Either Error Sample.Sample, Note.Note)]
convert db patch notes =
    zip (map (fmap setFilename . Patch._convert patch) notes) notes
    where
    setFilename = Sample.modifyFilename (patchDir</>)
    patchDir = Patch._rootDir db </> untxt (Patch._name patch)

makeNote :: Either Error Sample.Sample -> Note.Note -> IO Sample.Note
makeNote errSample note = do
    -- It's important to get an accurate duration if I can, because that
    -- determines overlap, which affects caching.
    dur <- case errSample of
        Left _ -> return Nothing
        Right sample -> File.ignoreEnoent $
            RenderSample.predictFileDuration (Sample.ratio sample)
                (Sample.filename sample)
    return $ Sample.Note
        { start = Note.start note
        , duration = maybe id (min . AUtil.toSeconds) dur (Note.duration note)
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
        Render.write quality output notes
    case result of
        Left err -> Log.error $ instrument <> ": writing " <> txt output
            <> ": " <> err
        Right (rendered, total) ->
            Log.notice $ instrument <> " " <> showt rendered <> "/"
                <> showt total <> " chunks: " <> txt output
                <> " (" <> elapsed <> ")"

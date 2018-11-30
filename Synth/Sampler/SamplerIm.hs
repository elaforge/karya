-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit
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
import qualified Synth.Lib.Checkpoint as Checkpoint
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
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    let quality = fromMaybe Resample.SincMediumQuality $
            Seq.last [quality | Quality quality <- flags]
    case args of
        ["check"] -> do
            let (reference, samples) = Wayang.checkStarts
            mapM_ (renderStarts . (++[reference])) samples
        [notesFilename, outputDir] -> do
            Log.notice $ Text.unwords
                ["sampler-im", txt notesFilename, txt outputDir]
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process PatchDb.db quality notes outputDir
        _ -> usage ""
    where
    usage msg = do
        unless (null msg) $
            putStrLn $ "ERROR: " ++ msg
        putStr $ GetOpt.usageInfo
            "sampler-im [ flags ] path/to/notes path/to/output/dir" options
        System.Exit.exitFailure

data Flags = Quality Resample.Quality
    deriving (Eq, Show)

readEnum :: (Show a, Enum a, Bounded a) => String -> a
readEnum str =
    fromMaybe (error (show str <> " not in: " <> show (Map.keys toVal))) $
        Map.lookup str toVal
    where
    toVal = Map.fromList $ Seq.key_on show [minBound .. maxBound]

defaultQuality :: Resample.Quality
defaultQuality = Resample.SincMediumQuality

options :: [GetOpt.OptDescr Flags]
options =
    [ GetOpt.Option [] ["quality"]
        (GetOpt.ReqArg (Quality . readEnum) (show defaultQuality))
        ("resample quality: "
            <> show [minBound .. maxBound :: Resample.Quality])
    ]

type Error = Text

process :: Patch.Db -> Resample.Quality -> [Note.Note] -> FilePath -> IO ()
process db quality notes outputDir = do
    clearUnusedInstruments outputDir instruments
    Async.forConcurrently_ byPatchInst $ \(patch, notes) -> case get patch of
        Nothing -> Log.warn $ "patch not found: " <> patch
        Just patch -> Async.forConcurrently_ notes $ \(inst, notes) ->
            realize quality outputDir inst
                =<< mapM makeNote (convert db patch notes)
    where
    instruments = Set.fromList $ concatMap (map fst . snd) byPatchInst
    byPatchInst :: [(Note.PatchName, [(Note.InstrumentName, [Note.Note])])]
    byPatchInst = map (second (Seq.keyed_group_sort Note.instrument)) $
        Seq.keyed_group_sort Note.patch notes
    get patch = Map.lookup patch (Patch._patches db)

-- | Delete output links for instruments that have disappeared entirely.
-- This often happens when I disable a track.
clearUnusedInstruments :: FilePath -> Set Note.InstrumentName -> IO ()
clearUnusedInstruments instDir instruments = do
    unused <- filter ((`Set.notMember` instruments) . txt) <$> listDir instDir
    unless (null unused) $
        Log.notice $ "clearing unused instruments: " <> pretty unused
    forM_ unused $ \dir -> do
        links <- filter (Maybe.isJust . Checkpoint.isOutputLink) <$>
            listDir (instDir </> dir)
        mapM_ (Directory.removeFile . ((instDir </> dir) </>)) links

listDir :: FilePath -> IO [FilePath]
listDir = fmap (fromMaybe []) . File.ignoreEnoent . Directory.listDirectory

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
    -- when (newDur /= Note.duration note) $
    --     Log.debug $ "sample " <> pretty errSample <> " dur "
    --         <> pretty (Note.duration note) <> " -> " <> pretty newDur
    return $ Sample.Note
        { start = Note.start note
        , duration = newDur
        , sample = errSample
        , hash = Sample.makeHash (Note.start note) newDur errSample
        }

realize :: Resample.Quality -> FilePath -> Note.InstrumentName
    -> [Sample.Note] -> IO ()
realize quality outputDir instrument notes = do
    let instDir = outputDir </> untxt instrument
    Directory.createDirectoryIfMissing True instDir
    (result, elapsed) <- Thread.timeActionText $
        Render.write quality instDir notes
    case result of
        Left err -> do
            Log.error $ instrument <> ": writing " <> txt instDir
                <> ": " <> err
            Config.emitFailure instDir err
        Right (rendered, total) ->
            Log.notice $ instrument <> " " <> showt rendered <> "/"
                <> showt total <> " chunks: " <> txt instDir
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
    config = Resample.Config
        { _quality = Resample.SincFastest
        , _state = Nothing
        , _notifyState = const $ return ()
        , _chunkSize = Config.checkpointSize
        , _now = 0
        , _name = txt filename
        }

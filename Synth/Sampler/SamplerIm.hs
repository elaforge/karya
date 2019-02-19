-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.File as File
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
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

import qualified Ui.Id as Id

import           Global
import           Synth.Types


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
        ["dump", notesFilename] ->
            dump PatchDb.db =<< either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
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
            (unlines
                [ "sampler-im [ flags ] path/to/notes path/to/output/dir"
                , "sampler-im [ flags ] dump path/to/notes"
                ])
            options
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

-- | Show the final Sample.Notes, which would have been rendered.
dump :: Patch.Db -> [Note.Note] -> IO ()
dump db notes = forM_ (byPatchInst notes) $ \(patchName, notes) ->
    whenJustM (getPatch db patchName) $ \patch ->
        forM_ notes $ \(inst, notes) -> do
            Text.IO.putStrLn $ Patch._name patch <> ", " <> inst <> ":"
            notes <- mapM makeSampleNote (convert db patch notes)
            let hashes = dumpHashes notes
            mapM_ putHash hashes
            mapM_ putNote notes
    where
    putNote n = Text.IO.putStrLn $ Text.unlines $
        Seq.map_head (annotate n) $ Text.lines $ Pretty.formatted n
    annotate n line =
        Text.unwords [line, pretty s, "+", pretty dur, "=>", pretty (s+dur)]
        where
        s = AUtil.toSeconds $ Sample.start n
        dur = AUtil.toSeconds $ fromMaybe 0 $ Sample.duration n
    putHash (start, end, (hash, hashes)) = Text.IO.putStrLn $
        pretty start <> "--" <> pretty end <> ": " <> pretty hash <> " "
        <> pretty hashes

dumpHashes :: [Sample.Note] -> [(RealTime, RealTime, (Note.Hash, [Note.Hash]))]
dumpHashes notes = zip3 (Seq.range_ 0 size) (drop 1 (Seq.range_ 0 size)) hashes
    where
    size = AUtil.toSeconds Config.chunkSize
    hashes = Seq.key_on mconcat $
        Checkpoint.overlappingHashes 0 size $
        map Render.toSpan notes

process :: Patch.Db -> Resample.Quality -> [Note.Note] -> FilePath -> IO ()
process db quality notes outputDir = do
    clearUnusedInstruments outputDir instruments
    Async.forConcurrently_ grouped $ \(patchName, notes) ->
        whenJustM (getPatch db patchName) $ \patch ->
            Async.forConcurrently_ notes $ \(inst, notes) ->
                realize (trackIds notes) quality outputDir inst
                    =<< mapM makeSampleNote (convert db patch notes)
    where
    trackIds notes = Set.fromList $ mapMaybe Note.trackId notes
    grouped = byPatchInst notes
    instruments = Set.fromList $ concatMap (map fst . snd) grouped

getPatch :: Log.LogMonad m => Patch.Db -> Note.PatchName
    -> m (Maybe Patch.Patch)
getPatch db name = case Map.lookup name (Patch._patches db) of
    Nothing -> do
        Log.warn $ "patch not found: " <> name
        return Nothing
    Just (Patch.DbDummy {}) -> do
        Log.warn $ "dummy patch: " <> name
        return Nothing
    Just (Patch.DbPatch patch) -> return $ Just patch

byPatchInst :: [Note.Note]
    -> [(Note.PatchName, [(Note.InstrumentName, [Note.Note])])]
byPatchInst = map (second (Seq.keyed_group_sort Note.instrument))
    . Seq.keyed_group_sort Note.patch

-- | Delete output links for instruments that have disappeared entirely.
-- This often happens when I disable a track.
clearUnusedInstruments :: FilePath -> Set Note.InstrumentName -> IO ()
clearUnusedInstruments instDir instruments = do
    dirs <- filterM Directory.doesDirectoryExist =<< listDir instDir
    let unused = filter ((`Set.notMember` instruments) . txt) dirs
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
    update (Right (sample, logs), note) =
        ( Right $ Sample.modifyFilename (patchDir</>) sample
        , logs
        , note
        )
    update (Left err, note) = (Left err, [], note)
    patchDir = Patch._rootDir db </> Patch._dir patch

-- TODO do this incrementally?  A stream?
makeSampleNote :: (Either Error Sample.Sample, [Log.Msg], Note.Note)
    -> IO Sample.Note
makeSampleNote (errSample, logs, note) = do
    mapM_ Log.write logs
    errDur <- case errSample of
        Left err -> return $ Left err
        Right sample -> first Audio.exceptionText <$>
            actualDuration (Note.start note) sample
    let start = AUtil.toFrame (Note.start note)
    let mbDur = either (const Nothing) Just errDur
    return $ Sample.Note
        { start = start
        , duration = mbDur
        , sample = either Left (const errSample) errDur
        , hash = Sample.makeHash start mbDur errSample
        }

-- | It's important to get an accurate duration if I can, because that
-- determines overlap, which affects caching.
actualDuration :: RealTime -> Sample.Sample
    -> IO (Either Audio.Exception Audio.Frame)
actualDuration start sample = do
    excFileDur <- Exception.try $
        RenderSample.predictFileDuration (Sample.ratio sample)
            (Sample.filename sample)
    let envDur = AUtil.toFrame <$>
            RenderSample.envelopeDur start (Sample.envelope sample)
    return $ case excFileDur of
        Left exc -> Left exc
        Right fileDur -> case envDur of
            Just dur -> Right $ min fileDur dur
            Nothing -> Right fileDur

realize :: Set Id.TrackId -> Resample.Quality -> FilePath
    -> Note.InstrumentName -> [Sample.Note] -> IO ()
realize trackIds quality outputDir instrument notes = do
    let instDir = outputDir </> untxt instrument
    Directory.createDirectoryIfMissing True instDir
    (result, elapsed) <- Thread.timeActionText $
        Render.write quality instDir trackIds notes
    case result of
        Left err -> do
            Log.error $ instrument <> ": writing " <> txt instDir
                <> ": " <> err
            Config.emitMessage "" $ Config.Message
                { _blockId = Config.pathToBlockId instDir
                , _trackIds = trackIds
                , _instrument = instrument
                , _payload = Config.Failure err
                }
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
        , _blockSize = Config.chunkSize
        , _now = 0
        , _name = txt filename
        }

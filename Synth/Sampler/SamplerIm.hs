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

import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.Thread as Thread

import qualified Perform.RealTime as RealTime
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
import qualified Synth.Shared.Signal as Signal

import qualified Ui.Id as Id

import           Global
import           Synth.Types


main :: IO ()
main = do
    Log.configure $ \st -> st { Log.state_priority = Log.Notice }
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Seq.join ", " errs
    let quality = fromMaybe defaultQuality $
            Seq.last [quality | Quality quality <- flags]
        dumpRange = Seq.head [(start, end) | DumpRange start end <- flags]
        dumpTracks = Seq.head [tracks | DumpTracks tracks <- flags]
    case args of
        ["check"] -> do
            let (reference, samples) = Wayang.checkStarts
            mapM_ (renderStarts . (++[reference])) samples
        ["dump", notesFilename] ->
            dumpNotes False dumpRange dumpTracks notesFilename
        ["dumps", notesFilename] ->
            dumpNotes True dumpRange dumpTracks notesFilename
        [notesFilename, outputDir] -> do
            Log.notice $ Text.unwords
                ["sampler-im", txt notesFilename, txt outputDir]
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process PatchDb.db quality notes outputDir
        _ -> usage ""
    where
    dumpNotes useShow range tracks notesFilename =
        dump useShow range tracks PatchDb.db
            =<< either (errorIO . pretty) return
            =<< Note.unserialize notesFilename
    usage msg = do
        unless (null msg) $
            putStrLn $ "ERROR: " ++ msg
        putStr $ GetOpt.usageInfo
            (unlines
                [ "sampler-im [ flags ] path/to/notes path/to/output/dir"
                , "sampler-im [ flags ] dump[s] path/to/notes"
                ])
            options
        System.Exit.exitFailure

data Flag =
    Quality Resample.Quality
    | DumpRange !RealTime !RealTime
    | DumpTracks !(Set Id.TrackId)
    deriving (Eq, Show)

readEnum :: (Show a, Enum a, Bounded a) => String -> a
readEnum str =
    fromMaybe (error (show str <> " not in: " <> show (Map.keys toVal))) $
        Map.lookup str toVal
    where
    toVal = Map.fromList $ Seq.key_on show [minBound .. maxBound]

defaultQuality :: Resample.Quality
defaultQuality = Resample.SincMediumQuality

options :: [GetOpt.OptDescr Flag]
options =
    [ GetOpt.Option [] ["quality"]
        (GetOpt.ReqArg (Quality . readEnum) (show defaultQuality))
        ("resample quality: "
            <> show [minBound .. maxBound :: Resample.Quality])
    , GetOpt.Option [] ["range"] (GetOpt.ReqArg readDumpRange "start,end")
        "dump events in this time range"
    , GetOpt.Option [] ["tracks"]
        (GetOpt.ReqArg readTracks "track-id,track-id,...")
        "dump events in from these tracks, by stack"
    ]

readDumpRange :: String -> Flag
readDumpRange s = case Seq.split "," s of
    [start, end]
        | Just start <- Read.readMaybe start, Just end <- Read.readMaybe end ->
            DumpRange start end
    _ -> error $ "can't parse: " <> show s

readTracks :: String -> Flag
readTracks s =
    DumpTracks $ Set.fromList $ map readId . Text.splitOn "," $ Text.pack s
    where
    readId w = fromMaybe (error $ "can't parse TrackId: " <> show w) $
        Id.text_ident w

type Error = Text

-- | Show the final Sample.Notes, which would have been rendered.
dump :: Bool -> Maybe (RealTime, RealTime) -> Maybe (Set Id.TrackId)
    -> Patch.Db -> [Note.Note] -> IO ()
dump useShow range tracks db notes =
    forM_ (byPatchInst notes) $ \(patchName, notes) ->
        whenJustM (getPatch db patchName) $ \patch ->
            forM_ notes $ \(inst, notes) -> do
                Text.IO.putStrLn $ Patch._name patch <> ", " <> inst <> ":"
                sampleNotes <- mapM (makeSampleNote emit)
                    (convert db patch notes)
                when False $ -- maybe I'll want this again someday
                    mapM_ putHash $ dumpHashes (Maybe.catMaybes sampleNotes)
                mapM_ putNote $
                    maybe id inTracks tracks $ maybe id inRange range $
                    Seq.map_maybe_snd id $ zip notes sampleNotes
    where
    emit payload = putStrLn (show payload)
    putNote (note, sample)
        | useShow = PPrint.pprint sample
        | otherwise = Text.IO.putStrLn $ Text.unlines $
            Seq.map_tail (Text.drop 4) $ -- dedent
            Seq.map_head (annotate note sample) $ Text.lines $
            Pretty.formatted sample
    annotate note sample line = Text.unwords
        [ line, pretty s, "+", pretty dur, "=>", pretty (s+dur)
        , "[original: ", pretty (Note.start note, Note.duration note)
        , maybe "<no-track>" Id.ident_text (Note.trackId note) <> "]"
        ]
        where
        s = AUtil.toSeconds $ Sample.start sample
        dur = AUtil.toSeconds $ Sample.duration sample
    putHash (start, end, (hash, hashes)) = Text.IO.putStrLn $
        pretty start <> "--" <> pretty end <> ": " <> pretty hash <> " "
        <> pretty hashes

inRange :: (RealTime, RealTime) -> [(Note.Note, a)] -> [(Note.Note, a)]
inRange (start, end) = filter $ \(n, _) ->
    not $ Note.start n <= start || Note.start n > end
    -- This checks if the note start is in the range, not the note extent,
    -- because that's how LPerf works.

inTracks :: Set Id.TrackId -> [(Note.Note, a)] -> [(Note.Note, a)]
inTracks tracks = filter $ \(n, _) ->
    maybe False (`Set.member` tracks) (Note.trackId n)

dumpHashes :: [Sample.Note] -> [(RealTime, RealTime, (Note.Hash, [Note.Hash]))]
dumpHashes notes = zip3 (Seq.range_ 0 size) (drop 1 (Seq.range_ 0 size)) hashes
    where
    size = AUtil.toSeconds Config.chunkSize
    hashes = Seq.key_on mconcat $
        Checkpoint.overlappingHashes 0 size $
        map Render.toSpan notes

process :: Patch.Db -> Resample.Quality -> [Note.Note] -> FilePath -> IO ()
process db quality notes outputDir
    | n : _ <- notes, Note.start n < 0 =
        errorIO $ "notes start <0: " <> pretty n
    | otherwise = do
        Checkpoint.clearUnusedInstruments outputDir instruments
        Async.forConcurrently_ grouped $ \(patchName, notes) ->
            whenJustM (getPatch db patchName) $ \patch ->
                Async.forConcurrently_ notes $ \(inst, notes) ->
                    let trackIds = trackIdsOf notes
                        emit = emitMessage trackIds inst
                    in realize emit trackIds quality outputDir inst
                        =<< mapMaybeM (makeSampleNote emit)
                            (convert db patch notes)
    where
    emitMessage trackIds instrument payload =
        Config.emitMessage "" $ Config.Message
            { _blockId = Config.pathToBlockId (outputDir </> "dummy")
            , _trackIds = trackIds
            , _instrument = instrument
            , _payload = payload
            }

    trackIdsOf notes = Set.fromList $ mapMaybe Note.trackId notes
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
makeSampleNote :: (Config.Payload -> IO ())
    -> (Either Error Sample.Sample, [Log.Msg], Note.Note)
    -> IO (Maybe Sample.Note)
makeSampleNote emitMessage (Left err, logs, note) = do
    mapM_ Log.write logs
    emitMessage $ Config.Warn (Note.stack note) err
    return Nothing
makeSampleNote emitMessage (Right sample, logs, note) = do
    mapM_ Log.write logs
    Exception.try (actualDuration (Note.start note) sample) >>= \case
        Left exc -> do
            emitMessage $
                Config.Warn (Note.stack note) (Audio.exceptionText exc)
            return Nothing
        Right dur | dur <= 0 -> do
            -- Omit samples with 0 duration.  This can happen naturally if they
            -- have 0 volume.
            emitMessage $
                Config.Warn (Note.stack note) "sample with <=0 duration"
            return Nothing
        Right dur -> do
            -- Round the frame up.  Otherwise, since frames are integral, I
            -- might round a note to start before its signal, at which point I
            -- get an extraneous 0.
            let start = Audio.secondsToFrameCeil Config.samplingRate
                    (RealTime.to_seconds (Note.start note))
            return $ Just $ Sample.Note
                { start = start
                , duration = dur
                , sample = sample
                , hash = Sample.makeHash start (Just dur) sample
                }

-- | It's important to get an accurate duration, because that determines
-- overlap, which affects caching.
actualDuration :: RealTime -> Sample.Sample -> IO Audio.Frame
actualDuration start sample = do
    fileDur <- RenderSample.predictFileDuration
        (Signal.shift (-start) (Sample.ratios sample))
        (Sample.filename sample)
    let envDur = AUtil.toFrame <$>
            RenderSample.envelopeDuration start (Sample.envelope sample)
    return $ case envDur of
        Just dur -> min fileDur dur
        Nothing -> fileDur

realize :: (Config.Payload -> IO ()) -> Set Id.TrackId -> Resample.Quality
    -> FilePath -> Note.InstrumentName -> [Sample.Note] -> IO ()
realize emitMessage trackIds quality outputDir instrument notes = do
    let instDir = outputDir </> untxt instrument
    Directory.createDirectoryIfMissing True instDir
    (result, elapsed) <- Thread.timeActionText $
        Render.write quality instDir trackIds notes
    case result of
        Left err -> do
            Log.error $ instrument <> ": writing " <> txt instDir
                <> ": " <> err
            emitMessage $ Config.Failure err
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
renderDirect filename dur samples = do
    audios <- mapM (RenderSample.render config 0) samples
    Resource.runResourceT $
        Audio.File.write AUtil.outputFormat filename $
        Audio.takeS dur $ Audio.mix audios
    where
    config = Resample.Config
        { _quality = Resample.SincFastest
        , _state = Nothing
        , _notifyState = const $ return ()
        , _blockSize = Config.chunkSize
        , _now = 0
        , _name = txt filename
        }

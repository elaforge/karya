-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Offline sampler.
module Synth.Sampler.SamplerIm (main) where
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exception
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import qualified System.Console.GetOpt as GetOpt
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import           System.FilePath ((</>))

import qualified Text.Read as Read

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Resample as Resample
import qualified Util.Limit as Limit
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.PPrint as PPrint
import qualified Util.Pretty as Pretty
import qualified Util.Texts as Texts
import qualified Util.Thread as Thread

import qualified Cmd.Instrument.ImInst as ImInst
import qualified Derive.Attrs as Attrs
import qualified Derive.ScoreT as ScoreT
import qualified Perform.RealTime as RealTime
import qualified Synth.Faust.Effect as Effect
import qualified Synth.Faust.EffectC as EffectC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Lib.Mix as Mix
import qualified Synth.Sampler.Calibrate as Calibrate
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.Patch.Lib.Util as Util
import qualified Synth.Sampler.Patch.Wayang as Wayang
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Sampler.Render as Render
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import qualified Ui.Id as Id

import           Global
import           Synth.Types


main :: IO ()
main = do
    -- There will be one open file per overlapping sample, and instruments
    -- render in parallel.
    Limit.set Limit.ResourceOpenFiles 4096
    args <- Environment.getArgs
    (flags, args) <- case GetOpt.getOpt GetOpt.Permute options args of
        (flags, args, []) -> return (flags, args)
        (_, _, errs) -> usage $ "flag errors:\n" ++ Lists.join ", " errs
    logFname <- Config.getLogFilename "sampler.log"
    logHdl <- Log.rotate logFname
    Log.configure $ const $ Log.State
        { state_write_msg = Log.write_formatted logHdl
        , state_priority = if Debug `elem` flags then Log.Debug else Log.Notice
        }
    let quality = fromMaybe defaultQuality $
            Lists.last [quality | Quality quality <- flags]
        dumpRange = Lists.head [(start, end) | DumpRange start end <- flags]
        dumpTracks = Lists.head [tracks | DumpTracks tracks <- flags]
        emitProgress = Progress `elem` flags
    imDir <- Config.imDir <$> Config.getConfig
    let calibrateDir = imDir </> "calibrate"
    let calibrateWav = calibrateDir </> "calibrate.wav"
    case args of
        -- Listen for samples that have silence at the beginning by rendering
        -- them all to start at the same time.  A delayed attack should stick
        -- out.
        ["calibrate-starts"] -> do
            -- TODO hardcoded to wayang, extend if needed
            let (reference, samples) = Wayang.checkStarts
            mapM_
                (Calibrate.renderStarts "data/sampler/wayang" calibrateDir
                    . (++[reference]))
                samples
        -- Play notes in a dynamic range to calibrate relative dynamics.
        "calibrate-by" : patch : by : attrs : pitches -> do
            let dur = 1
            let vars = 4
            let dyns = 16
            let notes = Calibrate.sequence (parseBy by) (txt patch) dur
                    (parseAttrs (txt attrs)) (map txt pitches) vars dyns
            dumpSamples PatchDb.db notes
            process emitProgress PatchDb.db quality notes calibrateDir
            putStrLn $ "write to " <> calibrateWav
            Mix.mix calibrateWav [calibrateDir </> "inst"]
        ["calibrate-var", patchName, start, len] -> do
            start <- parseInt start
            len <- parseInt len
            calibrateVar calibrateWav (txt patchName) start len
        ["dump", notesFilename] ->
            dumpNotes False dumpRange dumpTracks notesFilename
        ["dumps", notesFilename] ->
            dumpNotes True dumpRange dumpTracks notesFilename
        [notesFilename, outputDir] -> do
            Log.notice $ Text.unwords
                ["sampler-im", txt notesFilename, txt outputDir]
            notes <- either (errorIO . pretty) return
                =<< Note.unserialize notesFilename
            process emitProgress PatchDb.db quality notes outputDir
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
                , "sampler-im calibrate-by (Pitch|Dyn) reyong +open 4e 4u 4a 5i"
                ])
            options
        Exit.exitFailure
    parseInt n = maybe (errorIO $ "expected int: " <> txt n) pure
        (Read.readMaybe n)

-- Edit this along with calibrateVar, then paste into the patch definition.
-- I'd rather use the one in the patch, but then I'd have to go through
-- Note.Note and convert, which means I'd need to knows its internal
-- details to reverse it to cause it to select the sample I want.
dynamicTweaks :: Map Sample.SamplePath Util.Db
dynamicTweaks = Map.fromList
    [
    ]

calibrateVar :: FilePath -> Note.PatchName -> Int -> Int -> IO ()
calibrateVar calibrateWav patchName start len = do
    -- Directly play the underlying samples.  Use to calibrate
    -- variations.
    let dur = 1
    patch <- maybe (errorIO $ "unknown patch: " <> patchName)
        (pure . fst) =<< getPatch PatchDb.db patchName
    -- TODO this winds up being in alphabetical order rather than
    -- dyn.  However, so far I order samples names by general to
    -- specific so it works out.
    let fnames = take len $ drop start $ Set.toList $ Patch._allFilenames patch
    let fnameDyns = Lists.keyOnSnd
            (\n -> maybe 1 ((1+) . Util.dbToDyn) (Map.lookup n dynamicTweaks))
            fnames
    mapM_ Text.IO.putStrLn $ Texts.columns 2 $ ["time", "sample", "dyn"] :
        [ [RealTime.show_units t, txt fn, pretty dyn]
        | (t, (fn, dyn)) <- zip (Lists.range_ 0 dur) fnameDyns
        ]
    Calibrate.renderSequence calibrateWav dur
        (map (first (sampleToPath PatchDb.db patch)) fnameDyns)

sampleToPath :: Patch.Db -> Patch.Patch -> Sample.SamplePath -> FilePath
sampleToPath db patch fn = Patch._rootDir db </> Patch._dir patch </> fn

parseAttrs :: Text -> [Attrs.Attributes]
parseAttrs =
    map (Attrs.attrs . filter (/="") . Text.splitOn "+") . Text.splitOn ","

parseBy :: String -> Calibrate.By
parseBy str = fromMaybe (error ("not a By: " <> str)) (Read.readMaybe str)

data Flag =
    Quality Resample.Quality
    | Debug
    | DumpRange !RealTime !RealTime
    | DumpTracks !(Set Id.TrackId)
    | Progress
    deriving (Eq, Show)

readEnum :: (Show a, Enum a, Bounded a) => String -> a
readEnum str =
    fromMaybe (error (show str <> " not in: " <> show (Map.keys toVal))) $
        Map.lookup str toVal
    where
    toVal = Map.fromList $ Lists.keyOn show [minBound .. maxBound]

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
    , GetOpt.Option [] ["progress"] (GetOpt.NoArg Progress) "emit json progress"
    , GetOpt.Option [] ["debug"] (GetOpt.NoArg Debug) "debug logging"
    ]

readDumpRange :: String -> Flag
readDumpRange s = case Lists.split (==',') s of
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

-- * dump

-- | Show the final Sample.Notes, which would have been rendered.
dump :: Bool -> Maybe (RealTime, RealTime) -> Maybe (Set Id.TrackId)
    -> Patch.Db -> [Note.Note] -> IO ()
dump useShow range tracks db notes = do
    samples <- convertNotes db notes
    forM_ samples $ \(patch, (inst, sampleNotes)) -> do
        Text.IO.putStrLn $ "patch: " <> Patch._name patch
            <> ", inst: " <> pretty inst <> ":"
        when False $ -- maybe I'll want this again someday
            mapM_ putHash $ dumpHashes $ map snd sampleNotes
        mapM_ putNote $
            maybe id inTracks tracks $ maybe id inRange range sampleNotes
    where
    putNote (note, sample)
        | useShow = PPrint.pprint sample
        | otherwise = Text.IO.putStrLn $ Text.unlines $
            Lists.mapTail (Text.drop 4) $ -- dedent
            Lists.mapHead (annotate note sample) $ Text.lines $
            Pretty.formatted sample
    annotate note sample line = Text.unwords
        [ line, pretty s, "+", pretty dur, "=>", pretty (s+dur)
        , "[orig:", pretty (Note.start note, Note.duration note)
        , maybe "<no-track>" Id.ident_text (Note.trackId note)
        , pretty (Note.instrument note) <> "]"
        ]
        where
        s = AUtil.toSeconds $ Sample.start sample
        dur = AUtil.toSeconds $ Sample.duration sample
    putHash (start, end, (hash, hashes)) = Text.IO.putStrLn $
        pretty start <> "--" <> pretty end <> ": " <> pretty hash <> " "
        <> pretty hashes

-- | Version of 'dump' with just start and sample filename.
dumpSamples :: Patch.Db -> [Note.Note] -> IO ()
dumpSamples db notes = do
    samples <- convertNotes db notes
    forM_ samples $ \(_, (_, sampleNotes)) -> Text.IO.putStr $
        if null sampleNotes then "NO SAMPLES\n"
        else Text.unlines $ Texts.columns 2 $
            ["time", "sample", "var", "dyn", "env"]
            : map fmt sampleNotes
    where
    fmt (note, snote) =
        [ RealTime.show_units start
        , txt $ sampleName (Sample.filename sample)
        , pretty $ Note.initial0 Control.variation note
        , pretty $ Note.initial0 Control.dynamic note
        , pretty $ Signal.at (Sample.envelope sample) start
        ]
        where
        start = AUtil.toSeconds (Sample.start snote)
        sample = Sample.sample snote

sampleName :: FilePath -> FilePath
sampleName = FilePath.joinPath . Lists.takeEnd 2 . FilePath.splitPath

-- | Convert notes to samples.  This is duplicated with the one done in
-- 'process'
convertNotes :: Patch.Db -> [Note.Note]
    -> IO [(Patch.Patch, (ScoreT.Instrument, [(Note.Note, Sample.Note)]))]
convertNotes db notes =
    fmap concat $ forM (byPatchInst notes) $ \(patchName, notes) ->
    getPatch db patchName >>= \case
        Nothing -> return []
        Just (patch, mbEffect) -> fmap (map (patch,)) $ forM notes $
            \(inst, notes) -> do
                let converted = convert db patch notes
                sampleNotes <- makeSampleNotes emit mbEffect converted
                return
                    ( inst
                    , [ (note, snote)
                      | ((_, _, note), Just snote) <- zip converted sampleNotes
                      ]
                    )
    where
    emit = \case
        Config.Warn _ err -> put err
        Config.Failure err -> put err
        _ -> return ()
        where put = Text.IO.putStrLn . ("warning while converting: "<>)

inRange :: (RealTime, RealTime) -> [(Note.Note, a)] -> [(Note.Note, a)]
inRange (start, end) = filter $ \(n, _) ->
    not $ Note.start n < start || Note.start n >= end
    -- This checks if the note start is in the range, not the note extent,
    -- because that's how LPerf works.

inTracks :: Set Id.TrackId -> [(Note.Note, a)] -> [(Note.Note, a)]
inTracks tracks = filter $ \(n, _) ->
    maybe False (`Set.member` tracks) (Note.trackId n)

dumpHashes :: [Sample.Note] -> [(RealTime, RealTime, (Note.Hash, [Note.Hash]))]
dumpHashes notes =
    zip3 (Lists.range_ 0 size) (drop 1 (Lists.range_ 0 size)) hashes
    where
    size = AUtil.toSeconds Config.chunkSize
    hashes = Lists.keyOn mconcat $
        Checkpoint.overlappingHashes 0 size $
        map Render.toSpan notes


-- * process

process :: Bool -> Patch.Db -> Resample.Quality -> [Note.Note] -> FilePath
    -> IO ()
process emitProgress db quality allNotes outputDir
    | n : _ <- allNotes, Note.start n < 0 =
        errorIO $ "notes start <0: " <> pretty n
    | otherwise = Async.forConcurrently_ (byPatchInst allNotes) $
        \(patchName, instNotes) ->
            whenJustM (getPatch db patchName) $ \(patch, mbEffect) ->
                Async.forConcurrently_ instNotes $ \(inst, notes) ->
                    processInst patch inst mbEffect notes
    where
    processInst patch inst mbEffect notes =
        realize emit trackIds config outputDir inst mbEffect . Maybe.catMaybes
            =<< makeSampleNotes emit mbEffect (convert db patch notes)
        where
        trackIds = trackIdsOf notes
        emit = emitMessage trackIds inst
    config = (Render.defaultConfig quality)
        { Render._emitProgress = emitProgress }
    emitMessage trackIds inst payload
        | emitProgress = Config.emitMessage $ Config.Message
            { _blockId = Config.pathToBlockId (outputDir </> "dummy")
            , _trackIds = trackIds
            , _instrument = inst
            , _payload = payload
            }
        | otherwise = return ()
    trackIdsOf = Set.fromList . mapMaybe Note.trackId


getPatch :: Patch.Db -> Note.PatchName
    -> IO (Maybe (Patch.Patch, Maybe Render.InstrumentEffect))
getPatch db name = case Map.lookup name (Patch._patches db) of
    Nothing -> do
        Log.warn $ "patch not found: " <> name
        return Nothing
    Just patch | Just {} <- ImInst.patch_dummy (Patch._karyaPatch patch) -> do
        Log.warn $ "dummy patch: " <> name
        return Nothing
    Just patch -> case Patch._effect patch of
        Nothing -> return $ Just (patch, Nothing)
        Just effectConf -> case Map.lookup ename EffectC.patches of
            Nothing -> do
                Log.warn $ name <> ": effect not found: " <> ename
                return Nothing
            Just (Left err) -> do
                Log.warn $ name <> ": effect " <> ename <> " error: " <> err
                return Nothing
            Just (Right effect)
                | not (null warnings) -> do
                    mapM_ (Log.warn . ((name <> ": ") <>)) warnings
                    return Nothing
                | otherwise -> return $ Just
                    ( patch
                    , Just $ Render.InstrumentEffect
                        { _effectPatch = effect
                        , _effectConfig = effectConf
                        }
                    )
                where
                warnings = Patch.checkControls patch
                    (Map.keysSet (Effect._controls effect)) effectConf
            where ename = Patch._effectName effectConf

byPatchInst :: [Note.Note]
    -> [(Note.PatchName, [(ScoreT.Instrument, [Note.Note])])]
byPatchInst = map (second (Lists.keyedGroupSort Note.instrument))
    . Lists.keyedGroupSort Note.patch

convert :: Patch.Db -> Patch.Patch -> [Note.Note]
    -> [(Either Error Sample.Sample, [Log.Msg], Note.Note)]
convert db patch =
    map update . Lists.keyOn (Patch.convert patch) . Patch._preprocess patch
    where
    update (Right (sample, logs), note) =
        ( Right $ Sample.modifyFilename (sampleToPath db patch) sample
        , logs
        , note
        )
    update (Left err, note) = (Left err, [], note)

makeSampleNotes :: (Config.Payload -> IO ())
    -> Maybe Render.InstrumentEffect
    -> [(Either Error Sample.Sample, [Log.Msg], Note.Note)]
    -> IO [Maybe Sample.Note]
makeSampleNotes emitMessage mbEffect converted =
    mapM (uncurry (makeSampleNote emitMessage mbEffect))
        (zip (0 : map start converted) converted)
    where start (_, _, note) = Note.start note

-- TODO do this incrementally?  A stream?
makeSampleNote :: (Config.Payload -> IO ())
    -> Maybe Render.InstrumentEffect
    -> RealTime
    -> (Either Error Sample.Sample, [Log.Msg], Note.Note)
    -> IO (Maybe Sample.Note)
makeSampleNote emitMessage _ _ (Left err, logs, note) = do
    mapM_ Log.write logs
    emitMessage $ Config.Warn (Note.stack note) err
    return Nothing
makeSampleNote emitMessage mbEffect prevStart (Right sample, logs, note) = do
    mapM_ Log.write logs
    Exception.try (actualDuration (Note.start note) sample) >>= \case
        Left exc -> do
            emitMessage $
                Config.Warn (Note.stack note) (Audio.exceptionText exc)
            return Nothing
        Right dur | dur <= 0 -> do
            -- Omit samples with 0 duration.  This can happen naturally if they
            -- have 0 volume.
            emitMessage $ Config.Warn (Note.stack note)
                "sample with <=0 duration"
            return Nothing
        -- The notes should have been sorted prior to serialization.
        _ | Note.start note < prevStart -> do
            emitMessage $ Config.Warn (Note.stack note) $
                "note start " <> pretty (Note.start note)
                <> " < previous " <> pretty prevStart
            return Nothing
        Right dur -> return $ Just $
            Sample.note start dur (effectControls mbEffect note) sample
            where
            -- Round the frame up.  Otherwise, since frames are integral, I
            -- might round a note to start before its signal, at which point I
            -- get an extraneous 0.
            start = Audio.secondsToFramesCeil Config.samplingRate
                (RealTime.to_seconds (Note.start note))

effectControls :: Maybe Render.InstrumentEffect -> Note.Note
    -> Map Control.Control Signal.Signal
effectControls Nothing _ = mempty
effectControls (Just (Render.InstrumentEffect effect config)) note =
    Map.intersection (rename (Note.controls note)) (EffectC._controls effect)
    where
    rename
        | Map.null (Patch._toEffectControl config) = id
        | otherwise = Map.mapKeys $ \c ->
            Map.findWithDefault c c (Patch._toEffectControl config)

-- | It's important to get an accurate duration, because that determines
-- overlap, which affects caching.
actualDuration :: RealTime -> Sample.Sample -> IO Audio.Frames
actualDuration start sample = do
    fileDur <- RenderSample.predictFileDuration
        (Sample.timeRatio (Sample.stretch sample))
        (Signal.shift (-start) (Sample.ratios sample))
        (Sample.filename sample)
    let envDur = AUtil.toFrames <$>
            RenderSample.envelopeDuration start (Sample.envelope sample)
    return $ case envDur of
        Just dur -> min fileDur dur
        Nothing -> fileDur

realize :: (Config.Payload -> IO ()) -> Set Id.TrackId -> Render.Config
    -> FilePath -> ScoreT.Instrument -> Maybe Render.InstrumentEffect
    -> [Sample.Note] -> IO ()
realize emitMessage trackIds config outputDir instrument mbEffect notes = do
    let instDir = outputDir </> Config.instrumentToDir instrument
    Directory.createDirectoryIfMissing True instDir
    (result, elapsed) <- Thread.timeActionText $
        Render.write config instDir trackIds mbEffect notes
    case result of
        Left err -> do
            Log.error $ pretty instrument <> ": writing " <> txt instDir
                <> ": " <> err
            emitMessage $ Config.Failure err
        Right (rendered, total) ->
            Log.notice $ pretty instrument <> " " <> showt rendered <> "/"
                <> showt total <> " chunks: " <> txt instDir
                <> " (" <> elapsed <> ")"

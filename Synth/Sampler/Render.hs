-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeApplications, DataKinds #-}
-- | Render 'Sample.Note's down to audio.
module Synth.Sampler.Render where
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Resource as Resource

import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector.Storable as V

import qualified Sound.File.Sndfile as Sndfile
import qualified Streaming.Prelude as S
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Resample as Resample
import qualified Util.Control as Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize
import qualified Util.Thread as Thread

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import qualified Ui.Id as Id

import           Global


type Error = Text

write :: Resample.Quality -> FilePath -> Set Id.TrackId -> [Sample.Note]
    -> IO (Either Error (Int, Int))
write = write_ Config.chunkSize Config.blockSize

-- TODO lots of this is duplicated with Faust.Render.write, factor out the
-- repeated parts.
write_ :: Audio.Frame -> Audio.Frame -> Resample.Quality -> FilePath
    -> Set Id.TrackId -> [Sample.Note] -> IO (Either Error (Int, Int))
write_ chunkSize blockSize quality outputDir trackIds notes = catch $ do
    -- Debug.tracepM "notes" (map eNote notes)
    -- Debug.tracepM "overlap" $ map (map snd) $
    --     Checkpoint.groupOverlapping 0 (AUtil.toSeconds chunkSize) $
    --     Seq.key_on Checkpoint._hash $ map toSpan notes
    (skipped, hashes, mbState) <- Checkpoint.skipCheckpoints outputDir $
        Checkpoint.noteHashes chunkSize (map toSpan notes)
    let start = AUtil.toSeconds $ fromIntegral (length skipped) * chunkSize
    mapM_ (Checkpoint.linkOutput outputDir) skipped
    unless (null skipped) $ Config.emitMessage "" $ Config.Message
        { _blockId = Config.pathToBlockId outputDir
        , _trackIds = trackIds
        , _instrument = txt $ FilePath.takeFileName outputDir
        , _payload = Config.WaveformsCompleted [0 .. length skipped - 1]
        }

    case maybe (Right []) unserializeStates mbState of
        Left err -> return $ Left $
            "unserializing " <> pretty mbState <> ": " <> err
        Right initialStates -> do
            Log.debug $ txt outputDir <> ": skipped " <> pretty skipped
                <> ", resume at " <> pretty (take 1 hashes)
                <> " states: " <> pretty initialStates
                <> " start: " <> pretty start
            stateRef <- IORef.newIORef $
                fromMaybe (Checkpoint.State mempty) mbState
            let notifyState = IORef.writeIORef stateRef
            result <- Checkpoint.write outputDir trackIds (length skipped)
                    chunkSize hashes stateRef $
                render outputDir blockSize quality initialStates notifyState
                    trackIds notes (AUtil.toFrame start)
            case result of
                Right (_, total) ->
                    Checkpoint.clearRemainingOutput outputDir total
                _ -> return ()
            return result
    where
    catch io = Exception.catches io
        -- Exceptions in haskell are really disorganized.
        [ Exception.Handler $ \(Audio.Exception err) -> return $ Left err
        , Exception.Handler $ \(exc :: IO.Error.IOError) ->
            return $ Left $ txt $ Exception.displayException exc
        , Exception.Handler $ \exc ->
            return $ Left $ txt $ Sndfile.errorString exc
        , Exception.Handler $ \(Resource.ResourceCleanupException origExc
                firstExc _) ->
            return $ Left $ txt $ Exception.displayException
                (fromMaybe firstExc origExc)
        ]

toSpan :: Sample.Note -> Checkpoint.Span
toSpan note = Checkpoint.Span
    { _start = AUtil.toSeconds $ Sample.start note
    , _duration = maybe 0 AUtil.toSeconds $ Sample.duration note
    , _hash = Sample.hash note
    }

-- | A currently playing sample.
data Playing = Playing {
    _noteHash :: !Note.Hash
    , _getState :: IO (Maybe State)
    , _audio :: !AUtil.Audio
    , _noteRange :: !(Audio.Frame, Audio.Frame)
    }

instance Pretty Playing where
    pretty p = "Playing:" <> "(" <> prettyF s <> "--" <> prettyF e <> ")"
        where (s, e) = _noteRange p

failedPlaying :: Sample.Note -> Playing
failedPlaying note = Playing
    { _noteHash = Sample.hash note
    , _getState = return Nothing
    , _audio = mempty
    , _noteRange = (0, 0)
    }

prettyF :: Audio.Frame -> Text
prettyF frame = pretty frame <> "(" <> pretty (AUtil.toSeconds frame) <> ")"

render :: FilePath -> Audio.Frame -> Resample.Quality
    -> [Maybe State] -> (Checkpoint.State -> IO ())
    -> Set Id.TrackId -> [Sample.Note] -> Audio.Frame -> AUtil.Audio
render outputDir blockSize quality initialStates notifyState trackIds notes
        start = Audio.Audio $ do
    -- The first chunk is different because I have to resume already playing
    -- samples.
    let (overlappingStart, overlappingChunk, futureNotes) =
            overlappingNotes start blockSize notes
    -- Debug.tracepM "started, chunk, future"
    --     (map eNote overlappingStart, map eNote overlappingChunk,
    --         map eNote futureNotes)
    playing <- liftIO $
        resumeSamples start quality blockSize initialStates overlappingStart
    (playing, metric) <- renderBlock Nothing start playing overlappingChunk
        (null futureNotes)
    -- Debug.tracepM "renderChunk playing" playing
    Control.loop1 (metric, start + blockSize, playing, futureNotes) $
        \loop (metric, now, playing, notes) ->
            unless (null playing && null notes) $ do
                let (overlappingStart, overlappingChunk, futureNotes) =
                        overlappingNotes now blockSize notes
                -- If notes started in the past, they should already be
                -- 'playing'.
                Audio.assert (null overlappingStart) $
                    "overlappingStart should be []: " <> pretty overlappingStart
                -- Debug.tracepM "playing, starting, future"
                --     (now, playing, overlappingChunk, futureNotes)
                (playing, metric) <- renderBlock (Just metric) now playing
                    overlappingChunk (null futureNotes)
                -- Debug.tracepM "playing, future" (now, playing, futureNotes)
                loop (metric, now + blockSize, playing, futureNotes)
    where
    renderBlock prevMetric now playing overlappingChunk noFuture = do
        starting <- liftIO $
            mapM (startSample now quality blockSize Nothing) overlappingChunk
        metric <- progress prevMetric now playing starting
        -- Debug.tracepM "playing, starting"
        --     (AUtil.toSeconds now, playing, starting)
        (blocks, playing) <- -- Thread.printTimer_ "pull" $
            lift $ pull blockSize (playing ++ starting)
        -- Debug.tracepM "still playing" (AUtil.toSeconds now, playing)
        -- Record playing states for the start of the next chunk.
        -- liftIO $ Debug.tracepM "save states" . (now,) =<<
        --     mapM _getState (Seq.sort_on _noteHash playing)
        let playingTooLong = filter ((<=now) . snd . _noteRange) playing
        -- This means RenderSample.predictFileDuration was wrong.
        Audio.assert (null playingTooLong) $
            "notes still playing at " <> prettyF now <> ": "
            <> pretty playingTooLong
        liftIO $ notifyState . serializeStates
            =<< mapM _getState (Seq.sort_on _noteHash playing)
        Audio.assert (all ((<=blockSize) . AUtil.blockFrames2) blocks) $
            "chunk was >" <> pretty blockSize <> ": "
            <> pretty (map AUtil.blockFrames2 blocks)
            <> " of " <> pretty playing
        -- If there's no output and no chance to be any more output, don't
        -- emit anything.
        unless (null blocks && null playing && noFuture) $ if null blocks
            -- Since I'm inside Audio.Audio, I don't have srate available, so
            -- I have to set it for Audio.silence2.
            then Audio._stream @_ @Config.SamplingRate $
                Audio.take (Audio.Frames blockSize) (Audio.silence @_ @2)
            else S.yield $ Audio.mixV (AUtil.framesCount2 blockSize) blocks
        return (playing, metric)

    progress prevMetric now playing starting = liftIO $ do
        metric <- liftIO Thread.metric
        whenJust prevMetric $ \prev ->
            Log.debug $ "chunk "
                <> pretty (AUtil.toSeconds (now-blockSize)) <> "--"
                <> pretty (AUtil.toSeconds now)
                <> ": elapsed: " <> Thread.showMetric
                (Thread.diffMetric prev metric)
        let msg = "voices:" <> showt (length playing) <> "+"
                <> showt (length starting)
        Config.emitMessage msg $ Config.Message
            { _blockId = Config.pathToBlockId outputDir
            , _trackIds = trackIds
            , _instrument = txt $ FilePath.takeFileName outputDir
            , _payload = Config.RenderingRange
                (AUtil.toSeconds now) (AUtil.toSeconds (now + blockSize))
            }
        return metric

inferChunkNum :: Audio.Frame -> Audio.Frame -> Config.ChunkNum
inferChunkNum chunkSize now = fromIntegral $ now `div` chunkSize

-- | Get chunkSize from each Playing, and remove Playings which no longer are.
pull :: Audio.Frame -> [Playing]
    -> Resource.ResourceT IO ([V.Vector Audio.Sample], [Playing])
pull blockSize = fmap (trim . unzip) . mapM get
    -- TODO this mapM could be concurrent
    where
    trim (chunks, playing) =
        (filter (not . V.null) chunks, Maybe.catMaybes playing)
    get playing = do
        (chunk, audio) <- first mconcat <$>
            Audio.takeFramesGE blockSize (_audio playing)
        return
            ( chunk
            , if AUtil.blockFrames2 chunk < blockSize
                then Nothing else Just (playing { _audio = audio })
            )

resumeSamples :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> [Maybe State] -> [Sample.Note] -> IO [Playing]
resumeSamples now quality blockSize states notes = do
    Audio.assert (length states == length notes) $
        "at " <> pretty now <> ": len states " <> pretty (length states)
        <> " /= len notes " <> pretty (length notes) <> ": "
        <> pretty states <> " /= " <> pretty (map eNote notes)
    mapM (uncurry (startSample now quality blockSize . Just))
        (zip states (Seq.sort_on Sample.hash notes))

-- | Extract from Note for pretty-printing.
eNote :: Sample.Note
    -> (Text, Text, Signal.Signal, Either Text FilePath)
eNote n =
    ( prettyF $ Sample.start n
    , maybe "Nothing" prettyF $ Sample.duration n
    , either mempty Sample.ratio $ Sample.sample n
    , FilePath.takeFileName . Sample.filename <$> Sample.sample n
    )

-- | Convert 'Sample.Note' to a 'Playing'.
startSample :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> Maybe (Maybe State)
    -- ^ If given, resume a playing sample which should have started in the
    -- <= now, otherwise start a new one which should start >= now.
    -- If Just Nothing, this is a resuming sample, but it wasn't resampled, so
    -- there's no resampler state.
    -> Sample.Note -> IO Playing
startSample now quality blockSize mbMbState note = case Sample.sample note of
    -- Just crash on a failed sample.  I used to keep rendering, but then
    -- I need to mark where it failed, and it gets complicated.
    Left err -> Audio.throwIO $
        "note at " <> pretty (Sample.start note)
        <> ", dur: " <> pretty (Sample.duration note)
        <> ": " <> err
    Right sample -> do
        sampleStateRef <- IORef.newIORef Nothing
        let mkConfig mbState = Resample.Config
                { _quality = quality
                , _state = _resampleState <$> mbState
                , _notifyState = IORef.writeIORef sampleStateRef . fmap mkState
                , _blockSize = blockSize
                , _now = now
                , _name = txt $ FilePath.takeFileName $ Sample.filename sample
                }
            mkState (used, rState) = State
                { _filename = Sample.filename sample
                , _offset = used
                , _resampleState = rState
                }
        let start = Sample.start note
        -- Debug.tracepM "startSample" (start, sample, mbMbState)
        case mbMbState of
            Nothing -> Audio.assert (start >= now && now-start < blockSize) $
                "note should have started between " <> showt now <> "--"
                <> showt (now + blockSize) <> " but started at " <> showt start
            Just mbState -> do
                Audio.assert (start < now) $
                    "resume sample should start before " <> showt now
                    <> " but started at " <> showt start
                whenJust mbState $ \state -> do
                    Audio.assert (Sample.filename sample == _filename state) $
                        "starting " <> pretty sample <> " but state was for "
                        <> pretty state
        let offset = case mbMbState of
                Just (Just state) -> _offset state
                -- If Sample.start < now, then this is a resume.  I don't have
                -- the offset because I'm not resampling and that Resample
                -- produces that with State, but I don't need it.  I'm not
                -- resampling so frames are 1:1.
                _ -> max 0 $ now - Sample.start note
        duration <- maybe
            (Audio.throwIO $ "tried to start sample with no duration: "
                <> pretty note) return $
            Sample.duration note
        return $ Playing
            { _noteHash = Sample.hash note
            , _getState = IORef.readIORef sampleStateRef
            , _audio = RenderSample.render
                (mkConfig (Monad.join mbMbState))
                (AUtil.toSeconds (Sample.start note))
                (sample { Sample.offset = offset + Sample.offset sample })
            , _noteRange = (Sample.start note, Sample.start note + duration)
            }

-- | This is similar to 'Note.splitOverlapping', but it differentiates notes
-- that overlap the starting time.
overlappingNotes :: Audio.Frame -> Audio.Frame -> [Sample.Note]
    -> ([Sample.Note], [Sample.Note], [Sample.Note])
overlappingNotes start blockSize notes =
    (overlappingStart, overlappingChunk, rest)
    where
    (overlappingStart, overlappingChunk) =
        List.partition ((<start) . Sample.start) $ filter (not . passed) here
    (here, rest) = span ((<end) . Sample.start) $ dropWhile passed notes
    passed n = Sample.end n <= start && Sample.start n < start
    end = start + blockSize

data State = State {
    -- | I don't actually need this, but it makes the Pretty instance easier to
    -- read.
    _filename :: !Sample.SamplePath
    -- | This is the position in the sample where the state was saved.
    , _offset :: !Audio.Frame
    , _resampleState :: !Resample.SavedState
    } deriving (Eq, Show)

instance Pretty State where
    pretty (State fname offset _) = pretty fname <> ":" <> pretty offset

instance Serialize.Serialize State where
    put (State a b c) = Serialize.put a >> Serialize.put b >> Serialize.put c
    get = State <$> Serialize.get <*> Serialize.get <*> Serialize.get

-- | These will be sorted in order of Note hash.
unserializeStates :: Checkpoint.State -> Either Error [Maybe State]
unserializeStates (Checkpoint.State bytes) = first txt $ Serialize.decode bytes

-- | If there is no resampling, the state will be Nothing.  It's necessary
-- to remember that, so I can still line up notes with states in
-- 'resumeSamples'.
serializeStates :: [Maybe State] -> Checkpoint.State
serializeStates = Checkpoint.State . Serialize.encode

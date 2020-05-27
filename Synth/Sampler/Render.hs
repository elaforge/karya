-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeApplications, DataKinds #-}
-- | Render 'Sample.Note's down to audio.
module Synth.Sampler.Render where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe

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


data Config = Config {
    _quality :: !Resample.Quality
    , _chunkSize :: !Audio.Frames
    , _blockSize :: !Audio.Frames
    -- | Optionally suppress structured progress messages, used by karya.
    , _emitProgress :: !Bool
    }

type Error = Text

defaultConfig :: Resample.Quality -> Config
defaultConfig quality = Config
    { _quality = quality
    , _chunkSize = Config.chunkSize
    , _blockSize = Config.blockSize
    , _emitProgress = True
    }

-- TODO lots of this is duplicated with Faust.Render.write, factor out the
-- repeated parts.
write :: Config -> FilePath -> Set Id.TrackId -> [Sample.Note]
    -> IO (Either Error (Config.ChunkNum, Config.ChunkNum))
    -- ^ (writtenChunks, totalChunks)
write config outputDir trackIds notes = catch $ do
    (skipped, hashes, mbState) <- Checkpoint.skipCheckpoints outputDir $
        Checkpoint.noteHashes (_chunkSize config) (map toSpan notes)
    let startFrame = fromIntegral (length skipped) * _chunkSize config
        start = AUtil.toSeconds startFrame
    mapM_ (Checkpoint.linkOutput outputDir) skipped
    when (_emitProgress config && not (null skipped)) $
        Config.emitMessage "" $ Config.Message
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
            -- See NOTE [audio-state].
            stateRef <- IORef.newIORef $
                fromMaybe (Checkpoint.State mempty) mbState
            -- The streaming should be single threaded so I shouldn't need
            -- atomicWriteIORef, but since I'm relying on write->read ordering,
            -- this makes me feel a bit better anyway.
            let notifyState = IORef.atomicWriteIORef stateRef
                getState = IORef.readIORef stateRef
            result <- Checkpoint.write (_emitProgress config) outputDir
                    trackIds (length skipped) (_chunkSize config) hashes
                    getState $
                render config outputDir initialStates notifyState trackIds
                    notes startFrame
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
    , _duration = AUtil.toSeconds $ Sample.duration note
    , _hash = Sample.hash note
    }

-- | A currently playing sample.
data Playing = Playing {
    _noteHash :: !Note.Hash
    -- | Get the current state of the resample.  NOTE [audio-state]
    , _getState :: IO State
    , _audio :: !AUtil.Audio
    , _noteRange :: !(Audio.Frames, Audio.Frames)
    }

instance Pretty Playing where
    pretty p =
        "Playing:" <> pretty (_noteHash p)
            <> "(" <> prettyF s <> "--" <> prettyF e <> ")"
        where (s, e) = _noteRange p

failedPlaying :: Sample.Note -> Playing
failedPlaying note = Playing
    { _noteHash = Sample.hash note
    , _getState = return Complete
    , _audio = mempty
    , _noteRange = (0, 0)
    }

prettyF :: Audio.Frames -> Text
prettyF frame = pretty frame <> "(" <> pretty (AUtil.toSeconds frame) <> ")"

render :: Config -> FilePath -> [State] -> (Checkpoint.State -> IO ())
    -> Set Id.TrackId -> [Sample.Note] -> Audio.Frames -> AUtil.Audio
render config outputDir initialStates notifyState trackIds notes start =
        Audio.Audio $ do
    -- The first chunk is different because I have to resume already playing
    -- samples.
    let (overlappingStart, overlappingChunk, futureNotes) =
            overlappingNotes start blockSize notes
    playing <- liftIO $
        resumeSamples config start initialStates overlappingStart
    (playing, metric) <- renderBlock Nothing start playing overlappingChunk
        (null futureNotes)
    Control.loop1 (metric, start + blockSize, playing, futureNotes) $
        \loop (metric, now, playing, notes) ->
            unless (null playing && null notes) $ do
                let (overlappingStart, overlappingChunk, futureNotes) =
                        overlappingNotes now blockSize notes
                -- If notes started in the past, they should already be
                -- 'playing'.
                Audio.assert (null overlappingStart) $
                    "overlappingStart should be []: " <> pretty overlappingStart
                (playing, metric) <- renderBlock (Just metric) now playing
                    overlappingChunk (null futureNotes)
                loop (metric, now + blockSize, playing, futureNotes)
    where
    blockSize = _blockSize config
    renderBlock prevMetric now playing overlappingChunk noFuture = do
        starting <- liftIO $
            mapM (startSample config now Nothing) overlappingChunk
        metric <- progress prevMetric now playing starting
        (blocks, playing) <- lift $ pull blockSize now (playing ++ starting)
        liftIO $ notifyState =<< serializeStates playing
        -- Record playing states for the start of the next chunk.
        let playingTooLong = filter ((<now) . snd . _noteRange) playing
        -- This means RenderSample.predictFileDuration was wrong.
        Audio.assert (null playingTooLong) $
            "notes still playing at " <> prettyF now <> ": "
            <> pretty playingTooLong
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
                Audio.take blockSize (Audio.silence @_ @2)
            else S.yield $ Audio.Block $
                Audio.mixV (AUtil.framesCount2 blockSize)
                    (map Audio.blockVector blocks)
                    -- I could use an Audio.mixB in case there are Constants in
                    -- there, but resample will never produce Constants so
                    -- don't bother.
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
        when (_emitProgress config) $ Config.emitMessage msg $ Config.Message
            { _blockId = Config.pathToBlockId outputDir
            , _trackIds = trackIds
            , _instrument = txt $ FilePath.takeFileName outputDir
            , _payload = Config.RenderingRange
                (AUtil.toSeconds now) (AUtil.toSeconds (now + blockSize))
            }
        return metric

inferChunkNum :: Audio.Frames -> Audio.Frames -> Config.ChunkNum
inferChunkNum chunkSize now = fromIntegral $ now `div` chunkSize

-- | Get chunkSize from each Playing, and remove Playings which no longer are.
pull :: Audio.Frames -> Audio.Frames -> [Playing]
    -> Resource.ResourceT IO ([Audio.Block], [Playing])
pull blockSize now = fmap (trim . unzip) . mapM get
    -- TODO This mapM could be concurrent, which would make concurrent notes
    -- evaluate concurrently.
    where
    trim (chunks, playing) =
        ( filter (not . Audio.isEmptyBlock) chunks
        , Maybe.catMaybes playing
        )
    get playing = do
        (chunk, audio) <- first mconcat <$>
            Audio.takeFramesGE blockSize (_audio playing)
        -- Previously I checked if the stream was complete with AUtil.next.
        -- But since I'm using IORefs for the state, this is unsafe, since it
        -- advances the resampler state too far.
        -- TODO But now this approach can be wrong if the sample happens to end
        -- exactly on a chunk boundary.  Or the _noteRange is slightly wrong,
        -- which happens too.  I should make sure it's harmless.
        let end = now + AUtil.blockFrames2 chunk
        when (AUtil.blockFrames2 chunk < blockSize) $ liftIO $ Log.debug $
            let diff = snd (_noteRange playing) - end in
            pretty (_noteHash playing) <> ": expected "
            <> pretty (_noteRange playing)
            <> " diff: " <> pretty diff
            <> " " <> pretty (AUtil.toSeconds diff)
        return
            ( chunk
            , if AUtil.blockFrames2 chunk < blockSize
                then Nothing
                else Just $ playing { _audio = audio }
            )

resumeSamples :: Config -> Audio.Frames -> [State] -> [Sample.Note]
    -> IO [Playing]
resumeSamples config now states notes = do
    Audio.assert (length states == length notes) $
        "at " <> pretty now <> ": len states " <> pretty (length states)
        <> " /= len notes " <> pretty (length notes) <> ": "
        <> pretty states <> " /= " <> pretty (map eNote notes)
    mapM (uncurry (startSample config now . Just))
        (zip states (Seq.sort_on Sample.hash notes))

-- | Extract from Note for pretty-printing.
eNote :: Sample.Note -> (Text, Text, Signal.Signal, FilePath)
eNote n =
    ( prettyF $ Sample.start n
    , prettyF $ Sample.duration n
    , Sample.ratios $ Sample.sample n
    , FilePath.joinPath $ Seq.rtake 3 $ FilePath.splitPath $
        Sample.filename $ Sample.sample n
    )

-- | Convert 'Sample.Note' to a 'Playing'.
startSample :: Config -> Audio.Frames -> Maybe State
    -- ^ If Just Just, resume a playing sample which should have started <=now,
    -- otherwise start a new one which should start >= now.  If Just NoResample,
    -- this is a resuming sample, but it wasn't resampled, so there's no
    -- resampler state.
    -> Sample.Note -> IO Playing
startSample config now mbState note = do
    let start = Sample.start note
    let sample = Sample.sample note
    -- NOTE [audio-state]
    sampleStateRef <- IORef.newIORef NoResample
    let mkConfig mbState = Resample.Config
            { _quality = _quality config
            , _state = _resampleState <$> mbState
            -- The streaming should be single threaded so I shouldn't need
            -- atomicWriteIORef, but since I'm relying on write->read ordering,
            -- this makes me feel a bit better anyway.
            , _notifyState = IORef.atomicWriteIORef sampleStateRef . mkState
            , _blockSize = _blockSize config
            , _now = now
            , _name = FilePath.takeFileName $ Sample.filename sample
            }
        -- This is the offset the state had when I saved it.  Since I'm
        -- resuming at that point, add it to resample's further used samples.
        -- Resample doesn't have a notion of its absolute position in the input
        -- file and always starts at 0.
        initialOffset = case mbState of
            Just (Resample state) -> _offset state
            _ -> 0
        mkState Nothing = Complete
        mkState (Just (used, rState)) = Resample $ ResampleState
            { _filename = Sample.filename sample
            , _offset = initialOffset + used
            , _resampleState = rState
            }
    case mbState of
        Nothing -> assert (start >= now && now - start < _blockSize config) $
            "note should have started between " <> showt now <> "--"
            <> showt (now + _blockSize config) <> " but started at "
            <> showt start
        Just state -> do
            assert (start < now) $
                "resume sample should start before " <> showt now
                <> " but started at " <> showt start
            case state of
                Resample rstate ->
                    assert (Sample.filename sample == _filename rstate) $
                        "starting " <> pretty sample <> " but state was for "
                        <> pretty rstate
                -- 'pull' should have filtered out this Playing.
                Complete -> assert False "Complete sample still Playing"
                NoResample ->
                    assert (maybe False RenderSample.ratioCloseEnough ratio) $
                        "no resample state, but ratios is not 1-ish: "
                        <> pretty (Sample.ratios sample)
                    where
                    ratio = Signal.constant_val_from
                        (AUtil.toSeconds start) (Sample.ratios sample)
    let offset = case mbState of
            Just (Resample state) -> _offset state
            -- If start < now, then this is a resume.  I don't have
            -- the offset because I'm not resampling and that Resample
            -- produces that with ResampleState, but I don't need it.  I'm not
            -- resampling so frames are 1:1.
            _ -> max 0 $ now - start
    audio <- RenderSample.render
        (mkConfig $ case mbState of
            Nothing -> Nothing
            Just NoResample -> Nothing
            Just Complete -> Nothing
            Just (Resample state) -> Just state)
        (AUtil.toSeconds start)
        (sample { Sample.offset = offset + Sample.offset sample })
    return $ Playing
        { _noteHash = Sample.hash note
        , _getState = IORef.readIORef sampleStateRef
        , _audio = audio
        , _noteRange = (start, start + Sample.duration note)
        }
    where
    assert check msg = Audio.assert check $ msg <> ": " <> pretty note

-- | This is similar to 'Checkpoint.splitOverlapping', but it differentiates
-- notes that overlap the starting time.
overlappingNotes :: Audio.Frames -> Audio.Frames -> [Sample.Note]
    -> ([Sample.Note], [Sample.Note], [Sample.Note])
overlappingNotes start blockSize notes =
    (overlappingStart, overlappingChunk, rest)
    where
    (overlappingStart, overlappingChunk) =
        List.partition ((<start) . Sample.start) $ filter (not . passed) here
    (here, rest) = span ((<end) . Sample.start) $ dropWhile passed notes
    -- end < start means that overlappingStart includes notes that end exactly
    -- at the start time, which is inconsistent with the usual half-open rule.
    -- The reason is that I can't tell if an audio stream has completed until
    -- I pull samples and there are none, but if I do that, I've already
    -- advanced the resample state too far.  So I save the state anyway for
    -- samples which are exactly used up, which in turn means I have to
    -- consider those notes "still playing" on resume, which in turn means
    -- that overlappingStart has to be inclusive.
    passed n = Sample.end n < start && Sample.start n < start
    end = start + blockSize

data State =
    NoResample
    | Resample !ResampleState
    -- | There was a resample, but it's complete.  If this shows up, something
    -- is wrong, because 'pull' should have filtered out the Playing when
    -- it turned up short samples.
    | Complete
    deriving (Eq, Show)

instance Pretty State where
    pretty NoResample = "NoResample"
    pretty Complete = "Complete"
    pretty (Resample state) = pretty state

instance Serialize.Serialize State where
    put NoResample = Serialize.put_tag 0
    put (Resample state) = Serialize.put_tag 1 >> Serialize.put state
    put Complete = Serialize.put_tag 2
    get = Serialize.get_tag >>= \case
        0 -> return NoResample
        1 -> Resample <$> Serialize.get
        2 -> return Complete
        n -> Serialize.bad_tag "Render.State" n

-- | The saved state of a note that had to resample.
data ResampleState = ResampleState {
    -- | I don't actually need this, but it makes the Pretty instance easier to
    -- read.
    _filename :: !Sample.SamplePath
    -- | This is the position in the sample where the state was saved.
    , _offset :: !Audio.Frames
    , _resampleState :: !Resample.SavedState
    } deriving (Eq, Show)

instance Pretty ResampleState where
    pretty (ResampleState fname offset state) =
        pretty (FilePath.takeFileName fname) <> ":" <> pretty offset
            <> "(" <> pretty state <> ")"

instance Serialize.Serialize ResampleState where
    put (ResampleState a b c) =
        Serialize.put a >> Serialize.put b >> Serialize.put c
    get = ResampleState <$> Serialize.get <*> Serialize.get <*> Serialize.get

-- | These will be sorted in order of Note hash.
unserializeStates :: Checkpoint.State -> Either Error [State]
unserializeStates (Checkpoint.State bytes) = first txt $ Serialize.decode bytes

-- | If there is no resampling, the state will be NoResample.  It's necessary
-- to save that explicitly, so I can still line up notes with states in
-- 'resumeSamples'.
serializeStates :: [Playing] -> IO Checkpoint.State
serializeStates playing =
    Checkpoint.State . Serialize.encode <$>
        mapM _getState (Seq.sort_on _noteHash playing)
    -- Sort the notes by hash before saving their states, since the resume will
    -- then sort again by Sample.hash to match them back up.  This is because I
    -- don't enforce invariants on the order of simultaneous notes, so they may
    -- vary across renders.

{- NOTE [audio-state]

    Whne I write an audio checkpoint, I have to write the state of the audio
    generators and processors.  I do this via @notifyState@ and @getState@
    functions, which are just wrappers around IORef.

    This is pretty indirect and confusing, but the reason I have to do it is an
    Audio.Audio stream is only an effectful lazy list of audio chunks, with no
    way to query its internal state.  So it would have to either return a
    "query state" callback (a "pull" model), or take an IORef pointer, which it
    will update with new state on each audio chunk (a "push" model).

    However, I think even the "query state" callback will turn into the IORef,
    because it will need to get the current state of the audio processing loop,
    which means someone in there will need to be a mutable pointer, updated
    each time through the loop.

    But IORefs, like all mutable variables, are hard to understand and don't
    compose well.  In the sampler case, Resample gets
    a 'Resample._notifyState', which the loop in 'render' reads via the
    '_getState' fields, and merges to a 'Checkpoint.State' (via
    'serializeStates').  Then 'Checkpoint.write' reads that at the end of
    each chunk, in the callback from 'File.writeCheckpoints'.  Since this is
    the push model, I get a state for each block, but only use it on each
    chunk.  That's probably ok, since state is (dangerously) copy-free, and
    'serializeStates' is lazy.

    A way to get away from these IORefs entirely would be that Audio streams
    (IO (Maybe State), Vector Sample).  It would complicate all of the audio
    processing functions.  Many could just fmap over the vector though.  In
    fact, maybe it's safer, because it's accurately representing that if you
    split the block, you have to put the state on the first one, but not the
    second, or if you drop that part of the block, you no longer have its
    state.  See TODO non-copying state:.
-}

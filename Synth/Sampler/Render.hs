-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE TypeApplications, DataKinds #-}
module Synth.Sampler.Render where
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Vector.Storable as V

import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Patch as Patch
import qualified Synth.Sampler.PatchDb as PatchDb
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global
import Synth.Lib.Global


type Error = Text

write :: Resample.Quality -> FilePath -> [Note.Note]
    -> IO (Either Error (Int, Int))
write = write_ PatchDb.db (Audio.Frame Config.checkpointSize)

write_ :: Patch.Db -> Audio.Frame -> Resample.Quality -> FilePath -> [Note.Note]
    -> IO (Either Error (Int, Int))
write_ db chunkSize quality outputDir notes = do
    let allHashes = Checkpoint.noteHashes chunkSize notes
    (hashes, mbState) <- Checkpoint.skipCheckpoints outputDir allHashes
    -- Debug.tracepM "skipped: hashes, state"
    --     (take (length allHashes - length hashes) allHashes, hashes, mbState)
    stateRef <- IORef.newIORef $ fromMaybe (Checkpoint.State mempty) mbState
    let notifyState = IORef.writeIORef stateRef
    let start = case hashes of
            (i, _) : _ -> AUtil.toSeconds (fromIntegral i * chunkSize)
            _ -> 0
    let skipped = length allHashes - length hashes
    case maybe (Right []) unserializeStates mbState of
        _ | null hashes -> return $ Right (0, length allHashes)
        Left err -> return $ Left $
            "unserializing " <> pretty mbState <> ": " <> err
        Right states -> do
            result <- AUtil.catchSndfile $ Resource.runResourceT $
                Audio.File.writeCheckpoints chunkSize
                    (Checkpoint.getFilename outputDir stateRef)
                    (Checkpoint.writeState outputDir stateRef)
                    AUtil.outputFormat (Checkpoint.extendHashes hashes) $
                render db chunkSize quality states notifyState
                    (dropUntil (\_ n -> Note.end n > start) notes)
                    (AUtil.toFrame start)
            return $ second (\written -> (written, written + skipped)) result

-- | A currently playing sample.
data Playing = Playing {
    _noteHash :: Note.Hash
    , _getState :: IO (Maybe Resample.SavedState)
    , _audio :: Audio
    }

instance Pretty Playing where
    pretty = pretty . _noteHash

failedPlaying :: Note.Note -> Playing
failedPlaying note = Playing
    { _noteHash = Note.hash note
    , _getState = return Nothing
    , _audio = mempty
    }

render :: Patch.Db -> Audio.Frame -> Resample.Quality
    -> [Maybe Resample.SavedState] -> (Checkpoint.State -> IO ())
    -> [Note.Note] -> Audio.Frame -> Audio
render db chunkSize quality states notifyState notes start = Audio.Audio $ do
    -- The first chunk is different because I have to resume alreading playing
    -- samples.
    let (playingNotes, startingNotes, futureNotes) =
            overlappingNotes (AUtil.toSeconds start) chunkSize notes
    -- Debug.tracepM "start, states" (start, states)
    -- Debug.tracepM "playing, starting, future"
    --     (playingNotes, startingNotes, futureNotes)
    playing <- liftIO $
        resumeSamples db start quality chunkSize states playingNotes
    playing <- renderChunk start playing startingNotes (null futureNotes)
    -- Debug.tracepM "renderChunk playing" playing
    Audio.loop1 (start + chunkSize, playing, futureNotes) $
        \loop (now, playing, notes) -> unless (null playing && null notes) $ do
            let (playingNotes, startingNotes, futureNotes) =
                    overlappingNotes (AUtil.toSeconds now) chunkSize notes
            Audio.assert (null playingNotes) $
                "playingNotes: " <> pretty playingNotes
            -- Debug.tracepM "playing, starting, future"
            --     (now, playing, startingNotes, futureNotes)
            playing <- renderChunk now playing startingNotes (null futureNotes)
            -- Debug.tracepM "playing, future" (now, playing, futureNotes)
            loop (now + chunkSize, playing, futureNotes)
    where
    renderChunk now playing startingNotes noFuture = do
        starting <- liftIO $
            mapM (startSample db now quality chunkSize Nothing) startingNotes
        (chunks, playing) <- lift $ pull chunkSize (playing ++ starting)
        liftIO $ notifyState . serializeStates
            =<< mapM _getState (Seq.sort_on _noteHash playing)
        -- Debug.tracepM "chunks" chunks
        Audio.assert (all ((<=chunkSize) . AUtil.chunkFrames2) chunks) $
            ">" <> showt chunkSize <> ": "
            <> showt (map AUtil.chunkFrames2 chunks)
        -- If there's no output and no chance to be any more output, don't
        -- emit anything.
        unless (null chunks && null playing && noFuture) $
            S.yield $ if null chunks
                then silentChunk
                else Audio.zipWithN (+) (map padZero chunks)
        return playing
    silentChunk -- try to reuse existing memory
        | V.length Audio.silentChunk <= size = V.take size Audio.silentChunk
        | otherwise = V.replicate size 0
        where size = Audio.framesCount (Proxy @AUtil.Channels) chunkSize
    padZero chunk
        | delta > 0 = chunk <> V.replicate (AUtil.framesCount2 delta) 0
        | otherwise = chunk
        where delta = chunkSize - AUtil.chunkFrames2 chunk

-- | Get chunkSize from each Playing, and remove Playings which no longer are.
pull :: Audio.Frame -> [Playing]
    -> Resource.ResourceT IO ([V.Vector Audio.Sample], [Playing])
pull chunkSize = fmap (first (filter (not . V.null)) . unzip) . mapMaybeM get
    where
    get playing = do
        (chunks, audio) <- Audio.takeFramesGE chunkSize (_audio playing)
        -- Debug.tracepM "pull" chunks
        return $ if null chunks
            then Nothing
            else Just (mconcat chunks, playing { _audio = audio })

resumeSamples :: Patch.Db -> Audio.Frame -> Resample.Quality -> Audio.Frame
    -> [Maybe Resample.SavedState] -> [Note.Note] -> IO [Playing]
resumeSamples db now quality chunkSize states notes = do
    Audio.assert (length states == length notes) $
        "len states /= len notes: " <> pretty states <> " /= " <> pretty notes
    mapM (uncurry (startSample db now quality chunkSize . Just))
        (zip states (Seq.sort_on Note.hash notes))

-- | Convert Note to Sample
-- Insert empty space if necessary.
--
-- TODO what if Sample.start is different from Note.start?  I think that spoils
-- my resume logic, so I should ensure they are the same, which maybe means
-- remove Sample.start.
startSample :: Patch.Db -> Audio.Frame -> Resample.Quality -> Audio.Frame
    -> Maybe (Maybe Resample.SavedState)
    -- ^ If given, resume a playing sample which should have started in the
    -- <= now, otherwise start a new one which should start >= now.
    -- If Just Nothing, this is a resuming sample, but it wasn't resampled, so
    -- there's no resampler state.
    -> Note.Note -> IO Playing
startSample db now quality chunkSize mbMbState note =
    case Convert.noteToSample db note of
        Left err -> Log.warn err >> return (failedPlaying note)
        Right sample -> do
            stateRef <- IORef.newIORef Nothing
            let config mbState = Resample.Config
                    { _quality = quality
                    , _state = mbState
                    , _notifyState = IORef.writeIORef stateRef . Just
                    , _chunkSize = chunkSize
                    , _now = now
                    }
            let start = AUtil.toFrame (Sample.start sample)
            case mbMbState of
                Just _ -> Audio.assert (start < now) $
                    "resumeSample should start before " <> showt now
                    <> " but started at " <> showt start
                Nothing ->
                    Audio.assert (start >= now && now-start < chunkSize) $
                        "note should have started between " <> showt now
                        <> "--" <> showt (now + chunkSize)
                        <> " but started at " <> showt start
            -- if AUtil.toFrame (Sample.start sample) < now
            --     then Debug.tracepM "resume" (now, sample)
            --     else Debug.tracepM "start" (now, sample)
            let audio = Sample.render (config (Monad.join mbMbState)) sample
            return $ Playing
                { _noteHash = Note.hash note
                , _getState = IORef.readIORef stateRef
                , _audio = audio
                }

-- | This is similar to 'Note.splitOverlapping', but it differentiates notes
-- that overlap the starting time.
overlappingNotes :: RealTime -> Audio.Frame -> [Note.Note]
    -> ([Note.Note], [Note.Note], [Note.Note])
    -- ^ (overlappingStart, overlappingRange, afterEnd)
overlappingNotes start chunkSize notes = (overlapping, starting, rest)
    where
    (starting, overlapping) = List.partition ((>=start) . Note.start) here
    (here, rest) = span ((<end) . Note.start) $ dropWhile passed notes
    passed n = Note.end n <= start && Note.start n < start
    end = start + AUtil.toSeconds chunkSize

instance Serialize.Serialize Resample.SavedState where
    put (Resample.SavedState a b) = Serialize.put a >> Serialize.put b
    get = Resample.SavedState <$> Serialize.get <*> Serialize.get

instance Pretty Resample.SavedState where
    pretty = pretty . serializeStates . (:[]) . Just

-- | These will be sorted in order of Note hash.
unserializeStates :: Checkpoint.State
    -> Either Error [Maybe Resample.SavedState]
unserializeStates (Checkpoint.State bytes) = first txt $ Serialize.decode bytes

-- | If there is no resampling, the state will be Nothing.  It's necessary
-- to remember that, so I can still line up notes with states in
-- 'resumeSample'.
serializeStates :: [Maybe Resample.SavedState] -> Checkpoint.State
serializeStates = Checkpoint.State . Serialize.encode

-- Effectively, the synth is the combination of each sample render, which
-- means the state has to be:
-- I think I don't need this, I can get _filename and _offset from the Sample.
-- In fact, I have to, unless I also want to save the envelope.
data State = State
    { _filename :: !Sample.SamplePath
    , _offset :: !Audio.Frame
    , _resampleState :: !Resample.SavedState
    } deriving (Eq, Show)

-- TODO from Faust.Render
-- | Drop until this element and the next one matches.
dropUntil :: (a -> a -> Bool) -> [a] -> [a]
dropUntil match = go
    where
    go [] = []
    go [x] = [x]
    go (x1 : xs@(x2 : _))
        | match x1 x2 = x1 : xs
        | otherwise = go xs

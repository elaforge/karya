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

import qualified Streaming.Prelude as S
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Resample as Resample
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global
import Synth.Lib.Global


type Error = Text

write :: Note.InstrumentName -> Resample.Quality -> FilePath -> [Sample.Note]
    -> IO (Either Error (Int, Int))
write = write_ Config.checkpointSize

-- TODO lots of this is duplicated with Faust.Render.write, factor out the
-- repeated parts.
write_ :: Audio.Frame -> Note.InstrumentName -> Resample.Quality -> FilePath
    -> [Sample.Note] -> IO (Either Error (Int, Int))
write_ chunkSize inst quality outputDir notes = catch $ do
    -- Debug.tracepM "overlap" $ map (map snd) $
    --     Checkpoint.groupOverlapping 0 (AUtil.toSeconds chunkSize) $
    --     Seq.key_on Checkpoint._hash $ map toSpan notes
    (skipped, hashes, mbState) <- Checkpoint.skipCheckpoints outputDir $
        Checkpoint.noteHashes chunkSize (map toSpan notes)
    let start = AUtil.toSeconds $ fromIntegral (length skipped) * chunkSize
    Log.debug $ inst <> ": skipped " <> pretty skipped
        <> ", resume at " <> pretty (take 1 hashes)
        <> " state: " <> pretty mbState
        <> " start: " <> pretty start
    mapM_ (Checkpoint.linkOutput outputDir) skipped

    case maybe (Right []) unserializeStates mbState of
        Left err -> return $ Left $
            "unserializing " <> pretty mbState <> ": " <> err
        Right states -> do
            stateRef <- IORef.newIORef $
                fromMaybe (Checkpoint.State mempty) mbState
            let notifyState = IORef.writeIORef stateRef
            Checkpoint.write outputDir (length skipped) chunkSize hashes
                    stateRef $
                render inst chunkSize quality states notifyState
                    (dropUntil (\_ n -> Sample.end n > start) notes)
                    (AUtil.toFrame start)
    where
    catch io = Exception.catches io
        [ Exception.Handler $ \(Audio.Exception err) -> return $ Left err
        , Exception.Handler $ \(exc :: IO.Error.IOError) ->
            return $ Left $ txt $ Exception.displayException exc
        ]

toSpan :: Sample.Note -> Checkpoint.Span
toSpan note = Checkpoint.Span
    { _start = Sample.start note
    , _duration = Sample.duration note
    , _hash = Sample.hash note
    }

-- | A currently playing sample.
data Playing = Playing {
    _noteHash :: !Note.Hash
    , _getState :: IO (Maybe Resample.SavedState)
    , _audio :: !Audio
    }

instance Pretty Playing where
    pretty p = "Playing:" <> "(" <> pretty (_noteHash p) <> ")"

failedPlaying :: Sample.Note -> Playing
failedPlaying note = Playing
    { _noteHash = Sample.hash note
    , _getState = return Nothing
    , _audio = mempty
    }

render :: Note.InstrumentName -> Audio.Frame -> Resample.Quality
    -> [Maybe Resample.SavedState] -> (Checkpoint.State -> IO ())
    -> [Sample.Note] -> Audio.Frame -> Audio
render inst chunkSize quality states notifyState notes start = Audio.Audio $ do
    -- The first chunk is different because I have to resume already playing
    -- samples.
    let (playingNotes, startingNotes, futureNotes) =
            overlappingNotes (AUtil.toSeconds start) chunkSize notes
    -- Debug.tracepM "start, states" (start, states)
    -- Debug.tracepM "playing, starting, future"
    --     (playingNotes, startingNotes, futureNotes)
    playing <- liftIO $
        resumeSamples start quality chunkSize states playingNotes
    playing <- renderChunk start playing startingNotes (null futureNotes)
    -- Debug.tracepM "renderChunk playing" playing
    Audio.loop1 (start + chunkSize, playing, futureNotes) $
        \loop (now, playing, notes) -> unless (null playing && null notes) $ do
            liftIO $ Log.debug $ inst <> ": voices: " <> showt (length playing)
                <> " time: " <> pretty (AUtil.toSeconds now)
            let (playingNotes, startingNotes, futureNotes) =
                    overlappingNotes (AUtil.toSeconds now) chunkSize notes
            -- If notes started in the past, they should already be 'playing'.
            Audio.assert (null playingNotes) $
                "playingNotes should be []: " <> pretty playingNotes
            -- Debug.tracepM "playing, starting, future"
            --     (now, playing, startingNotes, futureNotes)
            playing <- renderChunk now playing startingNotes (null futureNotes)
            -- Debug.tracepM "playing, future" (now, playing, futureNotes)
            loop (now + chunkSize, playing, futureNotes)
    where
    renderChunk now playing startingNotes noFuture = do
        starting <- liftIO $
            mapM (startSample now quality chunkSize Nothing) startingNotes
        -- Debug.tracepM "playing, starting"
        --     (AUtil.toSeconds now, playing, starting)
        (chunks, playing) <- lift $ pull chunkSize (playing ++ starting)
        -- Debug.tracepM "still playing" (AUtil.toSeconds now, playing)
        -- Record playing states for the start of the next chunk.
        liftIO $ notifyState . serializeStates
            =<< mapM _getState (Seq.sort_on _noteHash playing)
        Audio.assert (all ((<=chunkSize) . AUtil.chunkFrames2) chunks) $
            "chunk was >" <> pretty chunkSize <> ": "
            <> pretty (map AUtil.chunkFrames2 chunks)
            <> " of " <> pretty playing
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
pull chunkSize = fmap (trim . unzip) . mapM get
    where
    trim (chunks, playing) =
        (filter (not . V.null) chunks, Maybe.catMaybes playing)
    get playing = do
        (chunk, audio) <- first mconcat <$>
            Audio.takeFramesGE chunkSize (_audio playing)
        return
            ( chunk
            , if AUtil.chunkFrames2 chunk < chunkSize
                then Nothing else Just (playing { _audio = audio })
            )

resumeSamples :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> [Maybe Resample.SavedState] -> [Sample.Note] -> IO [Playing]
resumeSamples now quality chunkSize states notes = do
    Audio.assert (length states == length notes) $
        "at " <> pretty now <> ": len states /= len notes: "
        <> pretty states <> " /= " <> pretty notes
    mapM (uncurry (startSample now quality chunkSize . Just))
        (zip states (Seq.sort_on Sample.hash notes))

-- | Convert 'Sample.Note' to a 'Playing'.
startSample :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> Maybe (Maybe Resample.SavedState)
    -- ^ If given, resume a playing sample which should have started in the
    -- <= now, otherwise start a new one which should start >= now.
    -- If Just Nothing, this is a resuming sample, but it wasn't resampled, so
    -- there's no resampler state.
    -> Sample.Note -> IO Playing
startSample now quality chunkSize mbMbState note = case Sample.sample note of
    Left err -> do
        Log.warn $ pretty note <> ": " <> err
        return (failedPlaying note)
    Right sample -> do
        sampleStateRef <- IORef.newIORef Nothing
        let config mbState = Resample.Config
                { _quality = quality
                , _state = mbState
                , _notifyState = IORef.writeIORef sampleStateRef
                , _chunkSize = chunkSize
                , _now = now
                }
        let start = AUtil.toFrame (Sample.start note)
        case mbMbState of
            Just _ -> Audio.assert (start < now) $
                "resumeSample should start before " <> showt now
                <> " but started at " <> showt start
            Nothing -> Audio.assert (start >= now && now-start < chunkSize) $
                "note should have started between " <> showt now <> "--"
                <> showt (now + chunkSize) <> " but started at " <> showt start
        -- if AUtil.toFrame (Sample.start sample) < now
        --     then Debug.tracepM "resume" (now, sample)
        --     else Debug.tracepM "start" (now, sample)
        return $ Playing
            { _noteHash = Sample.hash note
            , _getState = IORef.readIORef sampleStateRef
            , _audio = RenderSample.render
                (config (Monad.join mbMbState)) (Sample.start note) sample
            }

-- | This is similar to 'Note.splitOverlapping', but it differentiates notes
-- that overlap the starting time.
overlappingNotes :: RealTime -> Audio.Frame -> [Sample.Note]
    -> ([Sample.Note], [Sample.Note], [Sample.Note])
    -- ^ (overlappingStart, overlappingRange, afterEnd)
overlappingNotes start chunkSize notes = (overlapping, starting, rest)
    where
    (starting, overlapping) = List.partition ((>=start) . Sample.start) here
    (here, rest) = span ((<end) . Sample.start) $ dropWhile passed notes
    passed n = Sample.end n <= start && Sample.start n < start
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

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
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified Streaming.Prelude as S
import qualified System.FilePath as FilePath
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

write :: Resample.Quality -> FilePath -> [Sample.Note]
    -> IO (Either Error (Int, Int))
write = write_ Config.checkpointSize

-- TODO lots of this is duplicated with Faust.Render.write, factor out the
-- repeated parts.
write_ :: Audio.Frame -> Resample.Quality -> FilePath -> [Sample.Note]
    -> IO (Either Error (Int, Int))
write_ chunkSize quality outputDir notes = catch $ do
    -- Debug.tracepM "notes" (map eNote notes)
    -- Debug.tracepM "overlap" $ map (map snd) $
    --     Checkpoint.groupOverlapping 0 (AUtil.toSeconds chunkSize) $
    --     Seq.key_on Checkpoint._hash $ map toSpan notes
    (skipped, hashes, mbState) <- Checkpoint.skipCheckpoints outputDir $
        Checkpoint.noteHashes chunkSize (map toSpan notes)
    let start = AUtil.toSeconds $ fromIntegral (length skipped) * chunkSize
    Log.debug $ txt outputDir <> ": skipped " <> pretty skipped
        <> ", resume at " <> pretty (take 1 hashes)
        <> " state: " <> pretty mbState
        <> " start: " <> pretty start
    when (start > 0) $
        progress outputDir start "skipped"
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
                render outputDir chunkSize quality states notifyState notes
                    (AUtil.toFrame start)
    where
    catch io = Exception.catches io
        [ Exception.Handler $ \(Audio.Exception err) -> return $ Left err
        , Exception.Handler $ \(exc :: IO.Error.IOError) ->
            return $ Left $ txt $ Exception.displayException exc
        ]

-- | Emit a progress message.  The sequencer should be able to parse these to
-- show render status.  It shows the end of the chunk being rendered, so it can
-- highlight the time range which is in progress.
progress :: FilePath -> RealTime -> Text -> IO ()
progress outputDir endTime extra =
    Log.notice $ Text.unwords
        ["progress", txt (dropDir (dropDir outputDir)), pretty endTime, extra]
    where
    -- The leading im/cache is clutter, so I can drop it.
    dropDir = drop 1 . dropWhile (/='/')

toSpan :: Sample.Note -> Checkpoint.Span
toSpan note = Checkpoint.Span
    { _start = Sample.start note
    , _duration = Sample.duration note
    , _hash = Sample.hash note
    }

-- | A currently playing sample.
data Playing = Playing {
    _noteHash :: !Note.Hash
    , _getState :: IO (Maybe State)
    , _audio :: !Audio
    , _noteRange :: !(RealTime, RealTime)
    }

instance Pretty Playing where
    pretty p = "Playing:" <> "(" <> pretty (_noteRange p) <> ")"

failedPlaying :: Sample.Note -> Playing
failedPlaying note = Playing
    { _noteHash = Sample.hash note
    , _getState = return Nothing
    , _audio = mempty
    , _noteRange = (0, 0)
    }

render :: FilePath -> Audio.Frame -> Resample.Quality
    -> [Maybe State] -> (Checkpoint.State -> IO ())
    -> [Sample.Note] -> Audio.Frame -> Audio
render outputDir chunkSize quality states notifyState notes start =
        Audio.Audio $ do
    -- The first chunk is different because I have to resume already playing
    -- samples.
    let (overlappingStart, overlappingChunk, futureNotes) =
            overlappingNotes (AUtil.toSeconds start) chunkSize notes
    -- Debug.tracepM "started, chunk, future"
    --     (map eNote overlappingStart, map eNote overlappingChunk, map eNote futureNotes)
    playing <- liftIO $
        resumeSamples start quality chunkSize states overlappingStart
    playing <- renderChunk start playing overlappingChunk (null futureNotes)
    -- Debug.tracepM "renderChunk playing" playing
    Audio.loop1 (start + chunkSize, playing, futureNotes) $
        \loop (now, playing, notes) -> unless (null playing && null notes) $ do
            liftIO $ progress outputDir (AUtil.toSeconds (now + chunkSize))
                ("voices:" <> showt (length playing))
            let (overlappingStart, overlappingChunk, futureNotes) =
                    overlappingNotes (AUtil.toSeconds now) chunkSize notes
            -- If notes started in the past, they should already be 'playing'.
            Audio.assert (null overlappingStart) $
                "overlappingStart should be []: " <> pretty overlappingStart
            -- Debug.tracepM "playing, starting, future"
            --     (now, playing, overlappingChunk, futureNotes)
            playing <- renderChunk now playing overlappingChunk
                (null futureNotes)
            -- Debug.tracepM "playing, future" (now, playing, futureNotes)
            loop (now + chunkSize, playing, futureNotes)
    where
    renderChunk now playing overlappingChunk noFuture = do
        starting <- liftIO $
            mapM (startSample now quality chunkSize Nothing) overlappingChunk
        -- Debug.tracepM "playing, starting"
        --     (AUtil.toSeconds now, playing, starting)
        (chunks, playing) <- lift $ pull chunkSize (playing ++ starting)
        -- Debug.tracepM "still playing" (AUtil.toSeconds now, playing)
        -- Record playing states for the start of the next chunk.
        -- liftIO $ Debug.tracepM "save states" . (now,) =<<
        --     mapM _getState (Seq.sort_on _noteHash playing)
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
    -- TODO this mapM could be concurrent
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
    -> [Maybe State] -> [Sample.Note] -> IO [Playing]
resumeSamples now quality chunkSize states notes = do
    Audio.assert (length states == length notes) $
        "at " <> pretty now <> ": len states " <> pretty (length states)
        <> " /= len notes " <> pretty (length notes) <> ": "
        <> pretty states <> " /= " <> pretty (map eNote notes)
    -- Debug.tracepM "resume" $ map eNote notes
    mapM (uncurry (startSample now quality chunkSize . Just))
        (zip states (Seq.sort_on Sample.hash notes))

-- | Extract from Note for pretty-printing.
eNote :: Sample.Note -> (RealTime, RealTime, Either Text FilePath)
eNote n = (Sample.start n, Sample.duration n,
    FilePath.takeFileName . Sample.filename <$> Sample.sample n)

-- | Convert 'Sample.Note' to a 'Playing'.
startSample :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> Maybe (Maybe State)
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
        let mkConfig mbState = Resample.Config
                { _quality = quality
                , _state = _resampleState <$> mbState
                , _notifyState = IORef.writeIORef sampleStateRef . fmap mkState
                , _chunkSize = chunkSize
                , _now = now
                }
            mkState (used, rState) = State
                { _filename = Sample.filename sample
                , _offset = used
                , _resampleState = rState
                }
        let start = AUtil.toFrame (Sample.start note)
        -- Debug.tracepM "startSample" (start, sample, mbMbState)
        case mbMbState of
            Nothing -> Audio.assert (start >= now && now-start < chunkSize) $
                "note should have started between " <> showt now <> "--"
                <> showt (now + chunkSize) <> " but started at " <> showt start
            Just mbState -> do
                Audio.assert (start < now) $
                    "resumeSample should start before " <> showt now
                    <> " but started at " <> showt start
                whenJust mbState $ \state -> do
                    Audio.assert (Sample.filename sample == _filename state) $
                        "starting " <> pretty sample <> " but state was for "
                        <> pretty state
        let offset = case mbMbState of
                Just (Just state) -> _offset state
                -- If Sample.start < now, then this is a resume.  I don't have
                -- the offset because Resample produces that with State, but
                -- I don't need it, since there is no resample then frames are
                -- 1:1.
                _ -> max 0 $ now - AUtil.toFrame (Sample.start note)
        return $ Playing
            { _noteHash = Sample.hash note
            , _getState = IORef.readIORef sampleStateRef
            , _audio = RenderSample.render (mkConfig (Monad.join mbMbState))
                (Sample.start note) $ sample
                    { Sample.offset = offset + Sample.offset sample
                    }
            , _noteRange = (Sample.start note, Sample.duration note)
            }

-- | This is similar to 'Note.splitOverlapping', but it differentiates notes
-- that overlap the starting time.
overlappingNotes :: RealTime -> Audio.Frame -> [Sample.Note]
    -> ([Sample.Note], [Sample.Note], [Sample.Note])
overlappingNotes start chunkSize notes =
    (overlappingStart, overlappingChunk, rest)
    where
    (overlappingStart, overlappingChunk) =
        List.partition ((<start) . Sample.start) $ filter (not . passed) here
    (here, rest) = span ((<end) . Sample.start) $ dropWhile passed notes
    passed n = Sample.end n <= start && Sample.start n < start
    end = start + AUtil.toSeconds chunkSize

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
-- 'resumeSample'.
serializeStates :: [Maybe State] -> Checkpoint.State
serializeStates = Checkpoint.State . Serialize.encode

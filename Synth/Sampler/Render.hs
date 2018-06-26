-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Vector.Storable as V

import qualified Streaming.Prelude as S
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Util.Serialize as Serialize

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Sampler.Convert as Convert
import qualified Synth.Sampler.Sample as Sample
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Note as Note

import Global
import Synth.Lib.Global


type Error = Text

write :: Resample.Quality -> FilePath -> [Note.Note]
    -> IO (Either Error (Int, Int))
write = write_ (Audio.Frame Config.checkpointSize)

write_ :: Audio.Frame -> Resample.Quality -> FilePath -> [Note.Note]
    -> IO (Either Error (Int, Int))
write_ chunkSize quality outputDir notes = do
    let allHashes = Checkpoint.noteHashes chunkSize notes
    (hashes, mbState) <- Checkpoint.skipCheckpoints outputDir allHashes
    stateRef <- IORef.newIORef $ fromMaybe (Checkpoint.State mempty) mbState
    let notifyState = IORef.writeIORef stateRef
    let start = case hashes of
            (i, _) : _ -> AUtil.toSeconds (fromIntegral i * chunkSize)
            _ -> 0
    let total = length allHashes
    case maybe (Right []) unserializeStates mbState of
        _ | null hashes -> return $ Right (0, total)
        Left err -> return $ Left err
        Right states -> do
            result <- AUtil.catchSndfile $ Resource.runResourceT $
                Audio.File.writeCheckpoints chunkSize
                    (Checkpoint.getFilename outputDir stateRef)
                    (Checkpoint.writeState outputDir stateRef)
                    AUtil.outputFormat (Checkpoint.extendHashes hashes) $
                render chunkSize quality states notifyState
                    (dropUntil (\_ n -> Note.end n > start) notes)
                    (AUtil.toFrame start)
            return $ second (\() -> (length hashes, total)) result

data Running = Running {
    _noteHash :: Note.Hash
    , _getState :: IO (Maybe Resample.SavedState)
    , _audio :: Audio
    }

failedRunning :: Note.Note -> Running
failedRunning note = Running
    { _noteHash = Note.hash note
    , _getState = return Nothing
    , _audio = mempty
    }

render :: Audio.Frame -> Resample.Quality -> [Resample.SavedState]
    -> (Checkpoint.State -> IO ()) -> [Note.Note] -> Audio.Frame -> Audio
render chunkSize quality states notifyState notes now = Audio.Audio $ do
    -- The first chunk is different because I have to resume alreading running
    -- samples.
    let (runningNotes, startingNotes, futureNotes) =
            overlappingNotes (AUtil.toSeconds now) chunkSize notes
    running <- liftIO $ resumeSamples now quality chunkSize states runningNotes
    running <- renderChunk running startingNotes
    Audio.loop1 (now + chunkSize, running, futureNotes) $
        \loop (now, running, notes) -> do
            let (runningNotes, startingNotes, futureNotes) =
                    overlappingNotes (AUtil.toSeconds now) chunkSize notes
            Audio.assert (null runningNotes) $
                "runningNotes: " <> pretty runningNotes
            running <- renderChunk running startingNotes
            loop (now + chunkSize, running, futureNotes)
    where
    renderChunk running startingNotes = do
        starting <- liftIO $
            mapM (startSample now quality chunkSize Nothing) startingNotes
        (chunks, running) <- pull (running ++ starting)
        liftIO $ notifyState . serializeStates
            =<< mapM _getState (Seq.sort_on _noteHash running)
        Audio.assert (all ((==chunkSize) . AUtil.chunkFrames2) chunks) $
            "/= " <> showt chunkSize <> ": "
            <> showt (map AUtil.chunkFrames2 chunks)
        -- Mix concurrent samples and emit them.
        S.yield $ Audio.zipWithN (+) chunks
        return $ running ++ starting

pull :: [Running] -> m ([V.Vector Audio.Sample], [Running])
pull = undefined
-- takeFrames :: forall m rate chan. (Monad m, KnownNat chan)
--     => Frame -> Audio m rate chan -> m ([V.Vector Sample], Audio m rate chan)

resumeSamples :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> [Resample.SavedState] -> [Note.Note] -> IO [Running]
resumeSamples now quality chunkSize states notes = do
    Audio.assert (length states /= length notes) $
        "states /= notes: " <> showt (length states) <> " /= "
            <> showt (length notes)
    mapM (uncurry (startSample now quality chunkSize . Just))
        (zip states (Seq.sort_on Note.hash notes))

-- | Convert Note to Sample
-- Insert empty space if necessary.
--
-- TODO what if Sample.start is different from Note.start?  I think that spoils
-- my resume logic, so I should ensure they are the same, which maybe means
-- remove Sample.start.
startSample :: Audio.Frame -> Resample.Quality -> Audio.Frame
    -> Maybe Resample.SavedState
    -- ^ If given, resume a playing sample which should have started in the
    -- <= now, otherwise start a new one which should start >= now.
    -> Note.Note -> IO Running
startSample now quality chunkSize mbState note =
    case Convert.noteToSample note of
        Left err -> Log.warn err >> return (failedRunning note)
        Right sample -> do
            stateRef <- IORef.newIORef Nothing
            let config state = Resample.Config
                    { _quality = quality
                    , _state = state
                    , _notifyState = IORef.writeIORef stateRef . Just
                    , _chunkSize = chunkSize
                    }
            audio <- case mbState of
                Just state -> do
                    let startOffset = AUtil.toFrame (Note.start note) - now
                    Audio.assert (startOffset >= 0) $
                        "resumeSample should start before " <> showt now
                        <> " but: " <> showt (Note.start note)
                    return $ Sample.render (config (Just state)) startOffset
                        sample
                Nothing -> do
                    let silence = now - AUtil.toFrame (Note.start note)
                    Audio.assert (0 <= silence && silence < chunkSize) $
                        "note should have started between " <> showt now
                        <> "--" <> showt (now + chunkSize) <> " but started at "
                        <> showt (Note.start note)
                    return $ Audio.take (Audio.Frames silence) Audio.silence2
                        <> Sample.render (config Nothing) 0 sample
            return $ Running
                { _noteHash = Note.hash note
                , _getState = IORef.readIORef stateRef
                , _audio = audio
                }

overlappingNotes :: RealTime -> Audio.Frame -> [Note.Note]
    -> ([Note.Note], [Note.Note], [Note.Note])
    -- ^ (overlappingStart, overlappingRange, afterEnd)
overlappingNotes start chunkSize notes = (overlapping, starting, rest)
    where
    (starting, overlapping) = List.partition ((>start) . Note.start) here
    (here, rest) = span ((<end) . Note.start) $
        dropWhile ((<=start) . Note.end) notes
    end = start + AUtil.toSeconds chunkSize

-- splitOverlapping :: RealTime -> RealTime -> [(a, Note.Note)]
--     -> ([(a, Note.Note)], [(a, Note.Note)])
-- splitOverlapping start end notes = (overlapping, overlapping ++ rest)
--     where
--     overlapping = filter (not . (<=start) . Note.end . snd) here
--     (here, rest) = span ((<end) . Note.start . snd) $
--         dropWhile ((<=start) . Note.end . snd) notes

instance Serialize.Serialize Resample.SavedState where
    put (Resample.SavedState a b) = Serialize.put a >> Serialize.put b
    get = Resample.SavedState <$> Serialize.get <*> Serialize.get

-- | These will be sorted in order of Note hash.
unserializeStates :: Checkpoint.State -> Either Error [Resample.SavedState]
unserializeStates (Checkpoint.State bytes) = first txt $ Serialize.decode bytes

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

{-
    noteToSample :: Note.Note -> Either Text Sample.Sample

    convert Notes to Samples
    initialize a Sample.realize for each state

    -- | Evaluating the Audio could probably produce more exceptions...
    realize :: Resample.Quality -> Sample -> (RealTime, Audio)
        -- ^ sample start time, and audio to render
    realize quality (Sample start filename offset env ratio) = (start,) $
        resample quality ratio start $
        applyEnvelope start env $
        Audio.File.readFrom (Audio.Seconds (RealTime.to_seconds offset))
            (Config.instrumentDbDir </> filename)
-}

{-
renderPatch :: DriverC.Patch -> Config -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -> [Note.Note] -> RealTime -> Audio
renderPatch patch config mbState notifyState notes_ start =
    maybe id AUtil.volume vol $ interleave $
        render patch mbState notifyState inputs
            (AUtil.toFrame start) (AUtil.toFrame final) config
    where
    inputs = renderControls (_chunkSize config)
        (filter (/=Control.volume) controls) notes start
    controls = DriverC.getControls patch
    vol = renderControl (_chunkSize config) notes start Control.volume
    final = maybe 0 Note.end (Seq.last notes)
    notes = dropUntil (\_ n -> Note.end n > start) notes_

render :: DriverC.Patch -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -- ^ notify new state after each audio chunk
    -> NAudio -> Audio.Frame -> Audio.Frame -- ^ logical end time
    -> Config -> NAudio
render patch mbState notifyState inputs start end config =
    Audio.NAudio (DriverC.patchOutputs patch) $ do
        (key, inst) <- lift $
            Resource.allocate (DriverC.initialize patch) DriverC.destroy
-}

{-
To restart, initialize a Sample for each State, and then mix them.
I have to get the signals from the Notes though.
Some signals, like envelope, might come from the Note -> Sample conversion.
Unlike faust, I don't flatten each control into a single signal.
I think I redo conversion for the affected Notes, then chop the control signals
accordingly.

All faust does is set the state, and then chop the merged signal breakpoints.
That's because by the time I get to render, the notes are already gone.
But since Note -> Sample is 1:1, I just need to restart any current samples,
and then play the rest, and subtract start from their starting times.

Could I hash with Samples?  No, because I don't want to convert what I don't
have to.

I have to match each state with its corresponding current Sample, so keep the
Note hashes, and sort by hash when saving and restoring.

-}

-- can fail with: patch not found, sample not found
-- noteToSample :: Note.Note -> Either Text Sample.Sample

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

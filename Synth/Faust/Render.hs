-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Render FAUST instruments.
module Synth.Faust.Render where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Storable as V

import qualified GHC.TypeLits as TypeLits
import qualified Streaming.Prelude as S
import qualified System.FilePath as FilePath
import qualified System.IO.Error as IO.Error

import qualified Util.Audio.Audio as Audio
import qualified Util.CallStack as CallStack
import qualified Util.Control
import qualified Util.Log as Log
import qualified Util.Map
import qualified Util.Num as Num
import qualified Util.Seq as Seq

import qualified Perform.RealTime as RealTime
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import qualified Ui.Id as Id

import           Global
import           Synth.Types


type Error = Text

-- * write

write :: FilePath -> Set Id.TrackId -> DriverC.Patch -> [Note.Note]
    -> IO (Either Error (Int, Int))
write = write_ defaultConfig

write_ :: Config -> FilePath -> Set Id.TrackId -> DriverC.Patch -> [Note.Note]
    -> IO (Either Error (Int, Int)) -- ^ (renderedChunks, totalChunks)
write_ config outputDir trackIds patch notes = catch $ do
    (skipped, hashes, mbState) <- Checkpoint.skipCheckpoints outputDir $
        Checkpoint.noteHashes chunkSize (map toSpan notes)
    stateRef <- IORef.newIORef $ fromMaybe (Checkpoint.State mempty) mbState
    let startFrame = fromIntegral (length skipped) * _chunkSize config
        start = AUtil.toSeconds startFrame
    Log.debug $ "skipped " <> pretty skipped
        <> ", resume at " <> pretty (take 1 hashes)
        <> " state: " <> pretty mbState
        <> " start: " <> pretty start
    mapM_ (Checkpoint.linkOutput outputDir) skipped
    unless (null skipped) $ emitMessage $
        Config.WaveformsCompleted [0 .. length skipped - 1]
    let notifyState = IORef.writeIORef stateRef
        getState = IORef.readIORef stateRef
    checkElements emitMessage patch notes
    result <- Checkpoint.write outputDir trackIds (length skipped) chunkSize
            hashes getState $
        renderPatch emitMessage patch config mbState notifyState notes start
    case result of
        Right (_, total) -> Checkpoint.clearRemainingOutput outputDir total
        _ -> return ()
    return result
    where
    chunkSize = _chunkSize config
    catch io = Exception.catches io
        [ Exception.Handler $ \(Audio.Exception err) -> return $ Left err
        , Exception.Handler $ \(exc :: IO.Error.IOError) ->
            return $ Left $ txt $ Exception.displayException exc
        ]
    emitMessage payload = Config.emitMessage "" $ Config.Message
        { _blockId = Config.pathToBlockId outputDir
        , _trackIds = trackIds
        , _instrument = txt $ FilePath.takeFileName outputDir
        , _payload = payload
        }

-- | Emit a warning if the patch expects element-address controls and a note
-- doesn't have an element, or vice versa.
checkElements :: (Config.Payload -> IO ()) -> DriverC.Patch -> [Note.Note]
    -> IO ()
checkElements emitMessage patch = mapM_ check
    where
    check note
        | Set.null elements = when (elt /= "") $
            warn $ "expected no element but got: " <> elt
        | elt == "" = warn "expected element but didn't have one"
        | elt `Set.notMember` elements = warn $ "element " <> elt <> " not in "
            <> pretty elements
        | otherwise = return ()
        where
        elt = Note.element note
        warn msg = emitMessage $ Config.Warn (Note.stack note) msg
    elements = Set.fromList $ map fst $ Map.keys $ DriverC._controls patch

toSpan :: Note.Note -> Checkpoint.Span
toSpan note = Checkpoint.Span
    { _start = Note.start note
    , _duration = Note.duration note
    , _hash = Note.hash note
    }

-- * render

data Config = Config {
    _chunkSize :: !Audio.Frame
    , _blockSize :: !Audio.Frame
    , _controlSize :: !Audio.Frame
    -- | This is _chunkSize / _controlSize.
    , _controlsPerBlock :: !Audio.Frame
    -- | Force an end if the signal hasn't gone to zero before this.
    , _maxDecay :: !RealTime
    } deriving (Show)

{-
    Here are the various constants and derived values:

    SamplingRate = 44100
    chunkSeconds = 4
    blocksPerChunk = 16
    controlsPerBlock = 25

    chunkSize = chunkSeconds * SamplingRate
    blockSize = chunkSize / blocksPerChunk -- 11025
    controlSize = blockSize / controlsPerBlock -- 441

    blocksPerSecond = blocksPerChunk / chunkSeconds -- 4
    controlRate = controlsPerBlock * blocksPerSecond -- 100
-}
defaultConfig :: Config
defaultConfig = Config
    { _chunkSize = Config.chunkSize
    , _blockSize = Config.blockSize
    -- 441 or 147
    , _controlSize = Config.blockSize `Num.assertDiv` controlsPerBlock
    , _controlsPerBlock = controlsPerBlock
    -- TODO it should be longer, but since 'isBasicallySilent' is
    -- unimplemented every decay lasts this long.
    , _maxDecay = 2
    }
    where controlsPerBlock = 75
    -- if c-rate is 100, then 10ms
    -- if c-rate is 300, then 3ms

-- | Control signals run at this rate.
--
-- This should divide into Config.blockSize, which in turn divides into
-- Config.SamplingRate.
_controlRate :: Config -> Int
_controlRate config =
    Num.assertIntegral $ fromIntegral (_controlsPerBlock config)
        * blocksPerSecond
    where
    blocksPerSecond =
        fromIntegral Config.samplingRate / fromIntegral (_blockSize config)

-- | Render notes belonging to a single FAUST patch.  Since they render on
-- a single element, they should either not overlap, or be ok if overlaps
-- cut each other off.
renderPatch :: (Config.Payload -> IO ()) -> DriverC.Patch -> Config
    -> Maybe Checkpoint.State -> (Checkpoint.State -> IO ()) -> [Note.Note]
    -> RealTime -> AUtil.Audio
renderPatch emitMessage patch config mbState notifyState notes_ start =
    maybe id AUtil.volume vol $ interleave $
        render emitMessage patch mbState notifyState
            controls inputs (AUtil.toFrame start) (AUtil.toFrame final) config
    where
    controls = renderControls (_controlRate config)
        (Map.keysSet (DriverC._controls patch)) notes start
    inputs = renderInputs (_blockSize config) inputControls notes start
    inputControls = map fst $ DriverC._inputControls patch
    vol = renderInput (_blockSize config) notes start Control.volume
    final = maybe 0 Note.end (Seq.last notes)
    notes = dropUntil (\_ n -> Note.end n > start) notes_

interleave :: AUtil.NAudio -> AUtil.Audio
interleave naudio = case Audio.interleaved naudio of
    Right audio -> audio
    -- All faust instruments are required to have 1 or 2 outputs.  This should
    -- have been verified by DriverC.getPatch.
    Left err -> Audio.throw $ "expected 1 or 2 outputs: " <> err

-- | Render a FAUST instrument incrementally.
--
-- Chunk size is determined by the size of the @inputs@ chunks, or
-- Audio.blockSize if they're empty or run out.  The inputs will go to zero
-- if they end before the given time.
render :: (Config.Payload -> IO ()) -> DriverC.Patch -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -- ^ notify new state after each audio chunk
    -> Map DriverC.Control AUtil.Audio1
    -> AUtil.NAudio -> Audio.Frame -> Audio.Frame -- ^ logical end time
    -> Config -> AUtil.NAudio
render emitMessage patch mbState notifyState controls inputs start end config =
    Audio.NAudio (DriverC._outputs patch) $ do
        (key, inst) <- lift $
            Resource.allocate (DriverC.allocate patch) DriverC.destroy
        liftIO $ whenJust mbState $ \state -> DriverC.putState state inst
        inputs <- return $ Audio._nstream $
            Audio.zeroPadN (_blockSize config) inputs
        Util.Control.loop1 (start, controls, inputs) $
            \loop (start, controls, inputs) -> do
                -- Audio.zeroPadN should have made this infinite.
                (inputSamples, nextInputs) <-
                    maybe (CallStack.errorIO "end of endless stream") return
                        =<< lift (S.uncons inputs)
                -- For inputs I try to create the right block size, and then
                -- DriverC.render will assert that they are the expected block
                -- size.  This is more finicky but should be more efficient.
                -- For controls, I take the correct number of frames so
                -- upstream doesn't have to synchronize.  Maybe controls should
                -- do the efficient thing too.
                (controls, nextControls) <- lift $
                    takeControls (_controlsPerBlock config) controls
                result <- renderBlock inst controls inputSamples start
                case result of
                    Nothing -> Resource.release key
                    Just nextStart -> loop (nextStart, nextControls, nextInputs)
    where
    renderBlock inst controls inputSamples start
        | start >= end + maxDecay = return Nothing
        | otherwise = do
            liftIO $ emitMessage $ Config.RenderingRange
                (AUtil.toSeconds start)
                (AUtil.toSeconds (start + _blockSize config))
            let controlVals = findControls (DriverC._controls inst) controls
            outputs <- liftIO $ DriverC.render
                (_controlSize config) (_controlsPerBlock config) inst
                controlVals inputSamples
            -- XXX Since this uses unsafeGetState, readers of notifyState
            -- have to entirely use the state before returning.  See
            -- Checkpoint.getFilename and Checkpoint.writeBs.
            liftIO $ notifyState =<< DriverC.unsafeGetState inst
            S.yield outputs
            case outputs of
                -- This should have already been checked by DriverC.getPatches.
                [] -> CallStack.errorIO "patch with 0 outputs"
                output : _
                    | frames == 0 || chunkEnd >= end + maxDecay
                            || chunkEnd >= end && isBasicallySilent output ->
                        return Nothing
                    | otherwise -> return $ Just chunkEnd
                    where
                    chunkEnd = start + frames
                    frames = Audio.Frame $ V.length output
    maxDecay = AUtil.toFrame $ _maxDecay config

findControls :: Map DriverC.Control (ptr, config)
    -> Map DriverC.Control (V.Vector Float) -> [(ptr, V.Vector Float)]
findControls controls vals = map get $ Util.Map.zip_intersection controls vals
    where get (_, (ptr, _), val) = (ptr, val)

-- | Pull a chunk from each of the controls.  Omit the control if its signal
-- has run out.  This is ok because controls naturally retain their last value.
takeControls :: Audio.Frame -> Map DriverC.Control AUtil.Audio1
    -> Resource.ResourceT IO
        ( Map DriverC.Control (V.Vector Audio.Sample)
        , Map DriverC.Control AUtil.Audio1
        )
takeControls frames controlStreams = do
    nexts <- mapM (takeExtend frames) streams
    return
        ( Map.fromList
            [(c, block) | (c, Just (block, _)) <- zip controls nexts]
        , Map.fromList
            [(c, stream) | (c, Just (_, stream)) <- zip controls nexts]
        )
    where
    (controls, streams) = unzip $ Map.toList controlStreams

-- | 'Audio.splitAt', but extend the final sample.  I need this because
-- DriverC.render relies on all control blocks being the same length, for
-- simplicity.
takeExtend :: Monad m => Audio.Frame -> Audio.Audio m rate 1
    -> m (Maybe (V.Vector Audio.Sample, Audio.Audio m rate 1))
takeExtend frames audio = do
    (blocks_, audio) <- Audio.splitAt frames audio
    let blocks = filter (not . V.null) blocks_
    let missing = Audio.framesCount (Proxy @1) $
            frames - Num.sum (map (Audio.blockFrames (Proxy @1)) blocks)
    return $ if null blocks then Nothing
        else if missing == 0 then Just (mconcat blocks, audio)
        else let final = V.last (last blocks)
            in Just (mconcat (blocks ++ [V.replicate missing final]), audio)

isBasicallySilent :: V.Vector Audio.Sample -> Bool
isBasicallySilent _samples = False -- TODO RMS < -n dB

renderControls :: Int -> Set DriverC.Control
    -> [Note.Note] -> RealTime -> Map DriverC.Control AUtil.Audio1
renderControls controlRate controls notes start =
    render <$> extractControls controls notes
    where
    -- Audio.linear gets its breakpoints in seconds, so I have to do this
    -- little dance.  Maybe it could use frames?
    render = case Audio.someNat controlRate of
        TypeLits.SomeNat (_ :: Proxy crate) ->
            Audio.castRate . Audio.linear @_ @crate False . shiftBack
    shiftBack = map $ first (subtract (RealTime.to_seconds start))

extractControls :: Set DriverC.Control -> [Note.Note]
    -> Map DriverC.Control [(Double, Double)]
extractControls controls allNotes =
    Map.fromList $ filter (not . null . snd) $
        map (get "" allNotes) withoutElement ++ mapMaybe getE withElement
    where
    (withoutElement, withElement) = first (map snd) $
        List.partition (Text.null . fst) $ Set.toList controls
    get element notes control =
        ((element, control), controlBreakpoints control notes)
    byElement = Seq.keyed_group_stable Note.element allNotes
    getE (element, control) =
        flip (get element) control <$> lookup element byElement

-- | Render the supported controls down to audio rate signals.  This causes the
-- stream to be synchronized by 'Config.chunkSize', which should determine
-- 'render' chunk sizes, which should be a factor of Config.checkpointSize.
-- TODO checkpointSize -> chunkSize, chunkSize -> blockSize
renderInputs :: Audio.Frame -> [Control.Control]
    -- ^ inputs expected by the instrument, in the expected order
    -> [Note.Note] -> RealTime -> AUtil.NAudio
renderInputs chunkSize controls notes start =
    Audio.nonInterleaved now chunkSize $
        map (fromMaybe Audio.silence . renderInput chunkSize notes start)
            controls
    where
    -- This is used for chunk alignment, and rendering starts on a chunk
    -- boundary, so it can be 0.
    now = 0

renderInput :: (Monad m, TypeLits.KnownNat rate)
    => Audio.Frame -> [Note.Note] -> RealTime -> Control.Control
    -> Maybe (Audio.Audio m rate 1)
renderInput chunkSize notes start control
    | null bps = Nothing
    | otherwise = Just $ Audio.synchronizeToSize now chunkSize $
        Audio.linear True $ shiftBack bps
    where
    shiftBack = map $ first (subtract (RealTime.to_seconds start))
    bps = controlBreakpoints control notes
    -- This is used for chunk alignment, and rendering starts on a chunk
    -- boundary, so it can be 0.
    now = 0

controlBreakpoints :: Control.Control -> [Note.Note] -> [(Double, Double)]
controlBreakpoints control
    | control == Control.gate = gateBreakpoints
    | otherwise = concat . mapMaybe get . Seq.zip_next
    where
    get (note, next) = do
        signal <- Map.lookup control (Note.controls note)
        let bps = Signal.to_pairs $
                maybe id (Signal.clip_after_keep_last . Note.start) next $
                Signal.clip_before (Note.start note) signal
        -- Add an explicit 0 if there's no signal.  This is consistent with
        -- the usual signal treatment, which is that if it is present but
        -- empty, then it's 0.
        return $ map (first RealTime.to_seconds) $
            if null bps then [(Note.start note, 0)] else bps

-- | Make a signal which goes to 1 for the duration of the note.
--
-- Disabled for now: It won't go to 0 for touching or overlapping notes.  If
-- a gate transition is required to trigger an attack, presumably the notes
-- should be shorter duration, such as 0 if it's percussion-like.
gateBreakpoints :: [Note.Note] -> [(Double, Double)]
gateBreakpoints = map (first RealTime.to_seconds) . go
    where
    go [] = []
    go (n : ns) =
        (Note.start n, 0) : (Note.start n, 1) : (Note.end end, 0) : go rest
        where (end, rest) = (n, ns)

    -- TODO this combines touching notes as documented above, but it turns out
    -- I rely on not doing that.  Either I should make gate always do that and
    -- be explicitly percussive, or have karya set percussive events to
    -- dur = 0.
    -- go (n : ns) =
    --     (Note.start n, 0) : (Note.start n, 1)
    --     : (Note.end end, 1) : (Note.end end, 0)
    --     : go rest
    --     where
    --     (end : rest) = dropUntil (\n1 n2 -> Note.end n1 < Note.start n2)
    --         (n:ns)

-- | Drop until this element and the next one matches.
dropUntil :: (a -> a -> Bool) -> [a] -> [a]
dropUntil match = go
    where
    go [] = []
    go [x] = [x]
    go (x1 : xs@(x2 : _))
        | match x1 x2 = x1 : xs
        | otherwise = go xs

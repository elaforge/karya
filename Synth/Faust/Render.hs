-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
-- | Render FAUST instruments.
module Synth.Faust.Render where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.IORef as IORef
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V

import qualified GHC.TypeLits as TypeLits
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq

import qualified Perform.RealTime as RealTime
import qualified Synth.Faust.DriverC as DriverC
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Lib.Global


type Error = Text

-- * write

write :: FilePath -> DriverC.Patch -> [Note.Note]
    -> IO (Either Error (Int, Int))
write = write_ defaultConfig

write_ :: Config -> FilePath -> DriverC.Patch -> [Note.Note]
    -> IO (Either Error (Int, Int)) -- ^ (renderedChunks, totalChunks)
write_ config outputDir patch notes = do
    let allHashes = Checkpoint.noteHashes chunkSize notes
    (hashes, mbState) <- Checkpoint.skipCheckpoints outputDir allHashes
    stateRef <- IORef.newIORef $ fromMaybe (Checkpoint.State mempty) mbState
    let notifyState = IORef.writeIORef stateRef
    let start = case hashes of
            (i, _) : _ -> AUtil.toSeconds (fromIntegral i * chunkSize)
            _ -> 0
    let total = length allHashes
    if null hashes
        then return (Right (0, total))
        else do
            result <- AUtil.catchSndfile $ Resource.runResourceT $
                Audio.File.writeCheckpoints chunkSize
                    (Checkpoint.getFilename outputDir stateRef)
                    (Checkpoint.writeState outputDir stateRef)
                    AUtil.outputFormat (Checkpoint.extendHashes hashes) $
                renderPatch patch config mbState notifyState notes start
            return $ second (\() -> (length hashes, total)) result
    where
    chunkSize = _chunkSize config

-- * render

data Config = Config {
    _chunkSize :: Audio.Frame
    -- | Force an end if the signal hasn't gone to zero before this.
    , _maxDecay :: RealTime
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { _chunkSize = Audio.Frame Config.checkpointSize
    -- TODO it should be longer, but since 'isBasicallySilent' is
    -- unimplemented every decay lasts this long.
    , _maxDecay = 2
    }

-- | Render notes belonging to a single FAUST patch.  Since they render on
-- a single element, they should either not overlap, or be ok if overlaps
-- cut each other off.
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

interleave :: NAudio -> Audio
interleave naudio = case Audio.interleaved naudio of
    Right audio -> audio
    -- All faust instruments are required to have 1 or 2 outputs.  This should
    -- have been verified by DriverC.getParsedMetadata.
    Left err -> Audio.throw $ "expected 1 or 2 outputs: " <> err

-- | Render a FAUST instrument incrementally.
--
-- Chunk size is determined by the size of the @inputs@ chunks, or
-- Audio.chunkSize if they're empty or run out.  The inputs will go to zero
-- if they end before the given time.
render :: DriverC.Patch -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -- ^ notify new state after each audio chunk
    -> NAudio -> Audio.Frame -> Audio.Frame -- ^ logical end time
    -> Config -> NAudio
render patch mbState notifyState inputs start end config =
    Audio.NAudio (DriverC.patchOutputs patch) $ do
        (key, inst) <- lift $
            Resource.allocate (DriverC.initialize patch) DriverC.destroy
        liftIO $ whenJust mbState $ \state -> DriverC.putState state inst
        let nstream = Audio._nstream (Audio.zeroPadN (_chunkSize config) inputs)
        Audio.loop1 (start, nstream) $ \loop (start, inputs) -> do
            -- Audio.zeroPadN should have made this infinite.
            (controls, nextInputs) <-
                maybe (CallStack.errorIO "end of endless stream") return
                    =<< lift (S.uncons inputs)
            result <- render1 inst controls start
            case result of
                Nothing -> Resource.release key
                Just nextStart -> loop (nextStart, nextInputs)
        where
        render1 inst controls start
            | start >= end + maxDecay = return Nothing
            | otherwise = do
                outputs <- liftIO $ DriverC.render inst controls
                -- XXX Since this uses unsafeGetState, readers of notifyState
                -- have to entirely use the state before returning.  See
                -- Checkpoint.getFilename and Checkpoint.writeBs.
                liftIO $ notifyState =<< DriverC.unsafeGetState inst
                S.yield outputs
                case outputs of
                    [] -> CallStack.errorIO "dsp with 0 outputs"
                    output : _
                        | frames == 0 || chunkEnd >= end + maxDecay
                                || chunkEnd >= end
                                    && isBasicallySilent output ->
                            return Nothing
                        | otherwise -> return $ Just chunkEnd
                        where
                        chunkEnd = start + frames
                        frames = Audio.Frame $ V.length output
        maxDecay = AUtil.toFrame $ _maxDecay config

isBasicallySilent :: V.Vector Audio.Sample -> Bool
isBasicallySilent _samples = False -- TODO RMS < -n dB

-- | Render the supported controls down to audio rate signals.  This causes the
-- stream to be synchronized by 'Config.chunkSize', which should determine
-- 'render' chunk sizes, which should be a factor of Config.checkpointSize.
renderControls :: Audio.Frame -> [Control.Control]
    -- ^ controls expected by the instrument, in the expected order
    -> [Note.Note] -> RealTime -> NAudio
renderControls chunkSize controls notes start =
    Audio.nonInterleaved now chunkSize $
        map (fromMaybe Audio.silence1 . renderControl chunkSize notes start)
            controls
    where now = 0 -- for the moment, faust always starts at 0

renderControl :: (Monad m, TypeLits.KnownNat rate)
    => Audio.Frame -> [Note.Note] -> RealTime -> Control.Control
    -> Maybe (Audio.Audio m rate 1)
renderControl chunkSize notes start control
    | control == Control.gate =
        Just $ sync $ Audio.linear $ shiftBack $ gateBreakpoints notes
    | null bps = Nothing
    | otherwise = Just $ sync $ Audio.linear $ shiftBack bps
    where
    shiftBack = map $ first (subtract (RealTime.to_seconds start))
    bps = controlBreakpoints control notes
    sync = Audio.synchronizeToSize now chunkSize
    now = 0 -- for the moment, faust always starts at 0

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

controlBreakpoints :: Control.Control -> [Note.Note] -> [(Double, Double)]
controlBreakpoints control = concat . mapMaybe get . Seq.zip_next
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

-- | Drop until this element and the next one matches.
dropUntil :: (a -> a -> Bool) -> [a] -> [a]
dropUntil match = go
    where
    go [] = []
    go [x] = [x]
    go (x1 : xs@(x2 : _))
        | match x1 x2 = x1 : xs
        | otherwise = go xs

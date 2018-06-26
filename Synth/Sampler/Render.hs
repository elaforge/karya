-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Render where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as ByteString
import qualified Data.IORef as IORef
import System.FilePath ((</>))

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as Audio.File
import qualified Util.Audio.Resample as Resample

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
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
    -- TODO put quality into the hashes, actually no need, the state will
    -- surely be different
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
                render chunkSize quality mbState notifyState
                    (dropUntil (\_ n -> Note.end n > start) notes) start
                -- TODO I think AUtil.mix and Sample.realize don't guarantee
                -- that chunk sizes are a factor of checkpointSize
                -- AUtil.mix $ map (Sample.realize quality) samples
            return $ second (\() -> (length hashes, total)) result

render :: Audio.Frame -> Resample.Quality -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -> [Note.Note] -> RealTime -> Audio
render chunkSize quality mbState notifyState notes start = Audio.Audio $ do
    whenJust mbState $
    undefined

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

-- Effectively, the synth is the combination of each sample render, which
-- means the state has to be:
data State = State
    { _filename :: !Sample.SamplePath
    , _offset :: !Audio.Frame
    , _resampleState :: !ResampleState
    } deriving (Eq, Show)

data ResampleState =
    ResampleState !ByteString.ByteString !ByteString.ByteString
    -- main and private state
    deriving (Eq, Show)

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

-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | High level binding to faust dsps, treated as audio effect processors.
module Synth.Faust.Effect (
    Patch, EffectT(..)
    , Config(..), controlRate
    , config
    , process
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Vector.Storable as V
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Control
import qualified Util.Num as Num

import qualified Synth.Faust.EffectC as EffectC
import           Synth.Faust.EffectC (Patch, EffectT(..))
import qualified Synth.Faust.RenderUtil as RenderUtil
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           Global


-- TODO this has to be initialized to be consistent with the sampler
data Config = Config {
    _blockSize :: !Audio.Frames
    -- | This is _blockSize / _controlsPerBlock
    , _controlSize :: !Audio.Frames
    -- | This is _blockSize / _controlSize
    , _controlsPerBlock :: !Audio.Frames
    -- | Force an end if the signal hasn't gone to zero before this.
    , _maxDecay :: !Audio.Frames
    } deriving (Show)

-- TODO duplicated with Faust.Render.Config

-- | Control signals run at this rate.
--
-- This should divide into Config.blockSize, which in turn divides into
-- Config.SamplingRate.
controlRate :: Config -> Int
controlRate config = Num.assertIntegral $
    fromIntegral (_controlsPerBlock config) * blocksPerSecond
    where
    blocksPerSecond =
        fromIntegral Config.samplingRate / fromIntegral (_blockSize config)

config :: Audio.Frames -> Audio.Frames -> Config
config blockSize controlsPerBlock = Config
    { _blockSize = blockSize
    , _controlSize = blockSize `Num.assertDiv` controlsPerBlock
    , _controlsPerBlock = controlsPerBlock
    , _maxDecay = AUtil.toFrames 32
    }

process :: Config
    -> Patch
    -> Maybe EffectC.State
    -> (EffectC.State -> IO ()) -- ^ notify new state after each audio chunk
    -> Map Control.Control AUtil.Audio1
    -> AUtil.Audio -> AUtil.Audio
process config patch mbState notifyState controls input = Audio.Audio $ do
    -- This never exits on its own, so the effect is only destroyed when the
    -- audio as a whole is complete.
    (key, effect) <- lift $
        Resource.allocate (EffectC.allocate patch) EffectC.destroy
    whenJust mbState $ liftIO . EffectC.putState effect
    input <- return $ Audio._nstream $ -- Audio.zeroPadN (_blockSize config) $
        Audio.splitChannels input
    Util.Control.loop1 (controls, input) $ \loop (controls, input) ->
        lift (S.uncons input) >>= \case
            Just (inputBlocks, input) -> do
                (controls, nextControls) <- lift $
                    RenderUtil.takeControls (_controlsPerBlock config) controls
                -- Debug.tracepM "inputs" (map trim inputBlocks)
                output <- liftIO $
                    renderBlock config notifyState effect controls inputBlocks
                S.yield $ Audio.Block output
                loop (nextControls, input)
            Nothing -> decay effect key controls
    where
    decay effect key controls =
        Util.Control.loop2 0 controls $ \loop now controls ->
            if now >= _maxDecay config then Resource.release key
            else do
                (controls, nextControls) <- lift $
                    RenderUtil.takeControls (_controlsPerBlock config) controls
                output <- liftIO $
                    renderBlock config notifyState effect controls
                        [silence, silence]
                if RenderUtil.isBasicallySilent output
                    then Resource.release key
                    else do
                        S.yield $ Audio.Block output
                        loop (now + _blockSize config) nextControls
        where
        silence = Audio.Constant (fromIntegral (_blockSize config)) 0

-- trim :: Audio.Block -> Audio.Block
-- trim (Audio.Block v) = Audio.Block (V.take 4 v)
-- trim b = b

renderBlock :: Config -> (EffectC.State -> IO ())
    -> EffectC.Effect -> Map Control.Control Audio.Block
    -> [Audio.Block] -> IO (V.Vector Audio.Sample)
renderBlock config notifyState effect controls inputBlocks = do
    let controlVals = RenderUtil.findControls (EffectC._controls effect)
            controls
    -- Debug.tracepM "controls"
    --     ( map (\(c, _, val) -> (c, val)) $
    --       Maps.zip_intersection (EffectC._controls effect) controls
    --     )
    outputs <- EffectC.render
        (_controlSize config) (_controlsPerBlock config) effect
        controlVals (map Audio.blockVector inputBlocks)
    -- XXX Since this uses unsafeGetState, readers of notifyState have to
    -- entirely use the state before returning.  See Checkpoint.getFilename and
    -- Checkpoint.writeBs.
    notifyState =<< EffectC.unsafeGetState effect
    case outputs of
        [_, _] -> return $ Audio.interleaveV outputs
        -- This should have already been checked by EffectC.makePatch.
        _ -> Audio.throwIO "patch with outputs /= 2"

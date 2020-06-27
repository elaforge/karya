-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | High level binding to faust dsps, treated as audio effect processors.
module Synth.Faust.Effect (
    Patch, EffectT(..)
    , get
    , Config(..), controlRate
    , config
    , process
) where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Streaming.Prelude as S

import qualified Util.Audio.Audio as Audio
import qualified Util.Control
import qualified Util.Num as Num

import qualified Synth.Faust.EffectC as EffectC
import           Synth.Faust.EffectC (Patch, EffectT(..))
import qualified Synth.Faust.RenderUtil as RenderUtil
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           Global



get :: Text -> IO (Maybe Patch)
get name = EffectC.getPatch name >>= \case
    Nothing -> return Nothing
    Just (Left err) -> Audio.throwIO $ "patch " <> name <> ": " <> err
    Just (Right patch) -> return $ Just patch

-- TODO this has to be initialized to be consistent with the sampler
data Config = Config {
    -- _chunkSize :: !Audio.Frames -- TODO unused
    _blockSize :: !Audio.Frames
    -- | This is _blockSize / _controlsPerBlock
    , _controlSize :: !Audio.Frames
    -- | This is _blockSize / _controlSize
    , _controlsPerBlock :: !Audio.Frames
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
    }

process :: Config
    -> Patch
    -> Maybe Checkpoint.State
    -> (Checkpoint.State -> IO ()) -- ^ notify new state after each audio chunk
    -> Map Control.Control AUtil.Audio1
    -> AUtil.Audio -> AUtil.Audio
process config patch mbState notifyState controls input = Audio.Audio $ do
    -- This never exits on its own, so the effect is only destroyed when the
    -- audio as a whole is complete.
    (_key, effect) <- lift $
        Resource.allocate (EffectC.allocate patch) EffectC.destroy
    whenJust mbState $ liftIO . EffectC.putState effect
    input <- return $ Audio._nstream $ -- Audio.zeroPadN (_blockSize config) $
        Audio.splitChannels input
    Util.Control.loop1 (controls, input) $ \loop (controls, input) ->
        lift (S.uncons input) >>= \case
            Just (inputSamples, input) -> do
                (controls, nextControls) <- lift $
                    RenderUtil.takeControls (_controlsPerBlock config) controls
                renderBlock config notifyState effect controls inputSamples
                loop (nextControls, input)
            Nothing -> return ()
            -- TODO keep processing until isBasicallySilent or > maxDecay

renderBlock :: Config -> (Checkpoint.State -> IO ())
    -> EffectC.Effect -> Map Control.Control Audio.Block
    -> [Audio.Block] -> S.Stream (S.Of Audio.Block) (Resource.ResourceT IO) ()
renderBlock config notifyState effect controls input = do
    let controlVals = RenderUtil.findControls (EffectC._controls effect)
            controls
    outputs <- liftIO $ EffectC.render
        (_controlSize config) (_controlsPerBlock config) effect
        controlVals (map Audio.blockVector input)
    -- XXX Since this uses unsafeGetState, readers of notifyState have to
    -- entirely use the state before returning.  See Checkpoint.getFilename and
    -- Checkpoint.writeBs.
    liftIO $ notifyState =<< EffectC.unsafeGetState effect
    case outputs of
        [_, _] -> S.yield $ Audio.Block $ Audio.interleaveV outputs
        -- This should have already been checked by EffectC.getPatches.
        _ -> Audio.throwIO "patch with outputs /= 2"

-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Functions shared between instrument and effect faust patches.
module Synth.Faust.RenderUtil where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import qualified GHC.TypeLits as TypeLits

import qualified Util.Audio.Audio as Audio
import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified Util.Segment as Segment
import qualified Util.Seq as Seq

import qualified Perform.RealTime as RealTime
import           Perform.RealTime (RealTime)
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Shared.Control as Control
import qualified Synth.Shared.Signal as Signal

import           Global


isBasicallySilent :: V.Vector Audio.Sample -> Bool
isBasicallySilent samples = rms samples < Audio.dbToLinear (-82)
    -- I arrived at the dB by just trying it and seeing how it sounds.

rms :: V.Vector Float -> Float
rms block =
    sqrt $ V.sum (V.map (\n -> n*n) block) / fromIntegral (V.length block)

-- * controls

findControls :: Ord control => Map control (ptr, config)
    -> Map control block -> [(ptr, block)]
findControls controls vals = map get $ Maps.zipIntersection controls vals
    where get (_, (ptr, _), block) = (ptr, block)

-- | Pull a chunk from each of the controls.  Omit the control if its signal
-- has run out.  This is ok because controls naturally retain their last value.
takeControls :: Ord control => Audio.Frames -> Map control AUtil.Audio1
    -> Resource.ResourceT IO (Map control Audio.Block, Map control AUtil.Audio1)
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
-- PatchC.render relies on all control blocks being the same length, for
-- simplicity.
takeExtend :: Monad m => Audio.Frames -> Audio.Audio m rate 1
    -> m (Maybe (Audio.Block, Audio.Audio m rate 1))
takeExtend frames audio = do
    (blocks_, audio) <- Audio.splitAt frames audio
    let blocks = filter (not . Audio.isEmptyBlock) blocks_
    let missing = Audio.framesCount (Proxy @1) $
            frames - Num.sum (map (Audio.blockFrames (Proxy @1)) blocks)
    let final = case last blocks of
            Audio.Constant _ val -> val
            Audio.Block v -> V.last v
    return $ if null blocks then Nothing
        else if missing == 0 then Just (mconcat blocks, audio)
        else Just (mconcat (blocks ++ [Audio.Constant missing final]), audio)

renderControl :: Monad m => Int -> RealTime -> [(Double, Double)]
    -> Audio.Audio m rate 1
renderControl controlRate start = case Audio.someNat controlRate of
    TypeLits.SomeNat (_ :: Proxy cRate) ->
        -- Audio.linear gets its breakpoints in seconds, so I have to do this
        -- little dance.  Maybe it could use frames?
        Audio.castRate . Audio.linear @_ @cRate False . shiftBack
    where shiftBack = map $ first $ subtract $ RealTime.to_seconds start

controlBreakpoints :: Audio.Frames -> Control.Control
    -> [(RealTime, Map Control.Control Signal.Signal)]
    -> [(Double, Double)]
controlBreakpoints controlSize control =
    Segment.simplify . concat . mapMaybe get . Seq.zip_next
    where
    get ((start, controls), next) = do
        signal <- Map.lookup control controls
        return $ (if controlSize == 1 then id else tweak) $
            roundBreakpoints controlSize $ Signal.to_pairs $
            maybe id (Signal.clip_after_keep_last . fst) next $
            Signal.clip_before start signal
    controlSizeS = RealTime.to_seconds $ AUtil.toSeconds controlSize
    -- See NOTE [faust-controls].
    tweak = map $ first $ subtract controlSizeS

-- | Round controls to controlSize boundaries.  See NOTE [faust-controls].
roundBreakpoints :: Audio.Frames -> [(RealTime, Signal.Y)] -> [(Double, Double)]
roundBreakpoints controlSize
    | controlSize == 1 = map (first RealTime.to_seconds)
    | otherwise = map (first (RealTime.to_seconds . roundTo size))
    where
    size = AUtil.toSeconds controlSize

roundTo :: RealTime -> RealTime -> RealTime
roundTo factor = RealTime.seconds
    . Num.roundToD (RealTime.to_seconds factor) . RealTime.to_seconds

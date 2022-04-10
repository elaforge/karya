-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Synth.Sampler.RenderSample_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Vector.Storable as Storable
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.Resample as Resample
import qualified Util.Num as Num

import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Shared.Signal as Signal

import           Global
import           Util.Test


test_predictDuration :: Test
test_predictDuration = do
    let f = verify Resample.ZeroOrderHold . signal
    -- up octave, ratio = 0.5, duration *= 0.5
    -- down octave, ratio = 2, duration *= 2
    uncurry (equalf 1) (f [(0, 1)] 42)
    uncurry (equalf 1) (f [(0, 2)] 42)
    uncurry (equalf 1) (f [(0, 0.5)] 42)

    -- 21 consumes 21, next 21 consumes 42, so 63
    equalf 1 (f [(0, 1), (21, 1), (21, 2)] 42) (63, 63)
    uncurry (equalf 2) (f [(0, 1), (21, 2)] 42)

test_actualDuration :: Test
test_actualDuration = do
    let f quality = actualDuration (mkConfig quality)
        low = Resample.ZeroOrderHold
        high = Resample.SincBestQuality
    equal (f low (Signal.constant 1) 42) (42 + 1)
    equal (f low (Signal.constant 2) 42) (84 + 1)
    equal (f low (Signal.constant 0.5) 42) (21 + 1)
    equal (f high (Signal.constant 1) 42) 42
    equal (f high (Signal.constant 2) 42) 84
    equal (f high (Signal.constant 0.5) 42) 21

test_envelopeDur :: Test
test_envelopeDur = do
    let f start = RenderSample.envelopeDuration start . Signal.from_pairs
    equal (f 0 [(0, 0), (1, 0), (2, 0)]) (Just 0)
    equal (f 0 [(0, 0), (1, 1)]) Nothing
    equal (f 0 [(0, 1), (1, 0), (2, 0)]) (Just 1)
    equal (f 0 [(0, 0), (1, 1), (2, 0)]) (Just 2)
    equal (f 5 [(0, 0), (1, 1), (2, 0)]) (Just 0)
    equal (f 1 [(0, 0), (1, 1), (2, 0)]) (Just 1)
    equal (f 1 []) Nothing

checkDuration = do
    let ratios = Signal.shift (-42.5) $ Signal.from_pairs
            [ (42.5, 0.9438743126816925)
            , (42.75, 0.9438743126816925)
            , (42.75, 0.9438743126816925)
            , (42.85, 0.9438743126816925)
            , (48.225, 0.9438743126816925)
            , (48.225, 0.9438743126816925)
            , (48.375, 0.8389993890503937)
            ]
    let dur = 643495
    pprint (verify Resample.SincMediumQuality ratios dur)
    -- predict = 568802.0
    -- actual =  568327.0

-- | Signal.from_pairs but on Frames instead of RealTime.
signal :: [(Audio.Frames, Signal.Y)] -> Signal.Signal
signal = Signal.from_pairs . map (first AUtil.toSeconds)

verify :: Resample.Quality -> Signal.Signal
    -> Audio.Frames -- ^ sample duration
    -> (Double, Double) -- ^ (predicted, actual)
verify quality ratios dur =
    ( realToFrac $ RenderSample.predictDuration ratios dur
    , realToFrac $ actualDuration (mkConfig quality) ratios dur
    )

actualDuration :: Resample.Config -> Signal.Signal -> Audio.Frames
    -> Audio.Frames
actualDuration config ratios dur =
    Num.sum . map (Audio.vectorFrames (Proxy @1))
    . Unsafe.unsafePerformIO
    . resample config ratios
    . Audio.take dur $ Audio.silence

resample :: Resample.Config -> Signal.Signal -> AUtil.Audio
    -> IO [Storable.Vector Audio.Sample]
resample config ratios = Resource.runResourceT . Audio.toSamples
    . Audio.extractChannel 0 . Resample.resampleBy config ratios

triangle :: [Audio.Sample]
triangle = [1, 2, 3, 4, 3, 2, 1, 0]

triangleA :: Int -> AUtil.Audio
triangleA n =
    Audio.expandChannels $ Audio.fromSampleLists [take n (cycle triangle)]

mkConfig :: Resample.Quality -> Resample.Config
mkConfig quality = Resample.Config
    { _quality = quality
    , _state = Nothing
    , _notifyState = const $ return ()
    , _blockSize = 8
    , _now = 0
    , _name = "test"
    }

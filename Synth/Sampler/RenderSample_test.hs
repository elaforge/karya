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
import qualified Synth.Lib.AUtil as AUtil
import qualified Synth.Sampler.RenderSample as RenderSample
import qualified Synth.Shared.Signal as Signal

import Global
import Util.Test


test_predictDuration = do
    let f = verify Resample.ZeroOrderHold
    -- up octave, ratio = 0.5, duration *= 0.5
    -- down octave, ratio = 2, duration *= 2
    uncurry (equalf 1) (f [(0, 1)] 42)
    uncurry (equalf 1) (f [(0, 2)] 42)
    uncurry (equalf 1) (f [(0, 0.5)] 42)

    -- 21 consumes 21, next 21 consumes 42, so 63
    equalf 1 (f [(0, 1), (21, 1), (21, 2)] 42) (63, 63)
    uncurry (equalf 2) (f [(0, 1), (21, 2)] 42)

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

-- | Signal.from_pairs but on Frame instead of RealTime.
signal :: [(Audio.Frame, Signal.Y)] -> Signal.Signal
signal = Signal.from_pairs . map (first AUtil.toSeconds)

verify :: Resample.Quality -> [(Audio.Frame, Signal.Y)]
    -> Audio.Frame -- ^ sample duration
    -> (Double, Double) -- ^ (predicted, actual)
verify quality ratio dur =
    ( realToFrac $ RenderSample.predictDuration sig dur
    , realToFrac $ actualDuration (mkConfig quality) sig dur
    )
    where sig = signal ratio

actualDuration :: Resample.Config -> Signal.Signal -> Audio.Frame
    -> Audio.Frame
actualDuration config ratio dur =
    sum . map (Audio.chunkFrames (Proxy @1))
    . Unsafe.unsafePerformIO
    . resample config ratio
    . Audio.take (Audio.Frames dur) $ Audio.silence2

resample :: Resample.Config -> Signal.Signal -> AUtil.Audio
    -> IO [Storable.Vector Audio.Sample]
resample config ratio = Resource.runResourceT . Audio.toSamples
    . Audio.extractChannel 0 . Resample.resampleBy config ratio

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
    , _chunkSize = 8
    , _now = 0
    }

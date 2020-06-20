-- Copyright 2020 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Util.Audio.Rubberband_test where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Vector.Storable as V

import qualified Util.Audio.Audio as Audio
import qualified Util.Audio.File as File
import qualified Util.Audio.Rubberband as Rubberband
import qualified Util.Num as Num

import           Util.Test


test_offline_time = do
    let f convert ratio frames =
            fmap (Num.sum . map V.length) $ convert $
            Rubberband.offline (config 1 ratio) $
            Audio.expandChannels $ Audio.take frames $ Audio.sine 440
    io_equal (f (toSamples @44100 @1) 1 600) 600
    io_equal (f (toSamples @44100 @1) 2 600) 1200
    io_equal (f (toSamples @44100 @1) 0.5 600) 300
    io_equal (f (toSamples @44100 @2) 2 600) 2400
    io_equal (f (toSamples @22050 @2) 2 600) 2400
    io_equal (f (toSamples @22050 @2) 2 0) 0

    -- This emits warnings like:
    --
    -- WARNING: draining: shiftIncrement == 0, can't handle that in this
    -- context: setting to 128
    --
    -- I don't know why and the interaction seems correct.
    -- io_equal (f (toSamples @22050 @2) 0.5 600) 600

manual_test :: IO ()
manual_test = write "out.wav" $ Rubberband.offline (config 0.5 2) $
    Audio.takeS 0.5 $ Audio.sine 440

toSamples :: forall rate chan. Audio.AudioIO rate chan
    -> IO [V.Vector Audio.Sample]
toSamples = Resource.runResourceT . Audio.toSamples

config :: Double -> Double -> Rubberband.Config
config pitch time = Rubberband.config
    { Rubberband._pitchRatio = pitch, Rubberband._timeRatio = time }

write :: FilePath -> Audio.AudioIO 44100 1 -> IO ()
write fname = Resource.runResourceT . File.write File.wavFormat fname

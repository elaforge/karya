-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-- | Audio utilities.  This is named AUtil instead of the more obvious
-- Audio to avoid clashing with Util.Audio.Audio.
module Synth.Lib.AUtil where
import qualified Control.Exception as Exception
import qualified Sound.File.Sndfile as Sndfile

import qualified Util.Audio.Audio as Audio
import qualified Util.Num as Num
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control
import Global


type Audio = Audio.AudioIO Config.SamplingRate 2
type Audio1 = Audio.AudioIO Config.SamplingRate 1
type NAudio = Audio.NAudioIO Config.SamplingRate

toFrame :: RealTime.RealTime -> Audio.Frame
toFrame = Audio.secondsToFrame Config.samplingRate . RealTime.to_seconds

toSeconds :: Audio.Frame -> RealTime.RealTime
toSeconds = RealTime.seconds . Audio.frameToSeconds Config.samplingRate

outputFormat :: Sndfile.Format
outputFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat
    , endianFormat = Sndfile.EndianFile
    }

catchSndfile :: IO a -> IO (Either Text a)
catchSndfile = fmap try . Exception.try
    where try = either (Left . txt . Sndfile.errorString) Right

mix :: [(RealTime.RealTime, Audio)] -> Audio
mix = Audio.mix . map (first (Audio.Seconds . RealTime.to_seconds))

-- | Convert a volume in dB to linear.
dbToLinear :: Float -> Float
dbToLinear = Audio.dbToLinear . Num.scale (Num.d2f Control.minimumDb) 0

volume :: Audio1 -> Audio -> Audio
volume = Audio.multiply . Audio.expandChannels . Audio.mapSamples dbToLinear

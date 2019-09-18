-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
-- | Audio utilities.  This is named AUtil instead of the more obvious
-- Audio to avoid clashing with Util.Audio.Audio.
module Synth.Lib.AUtil where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Sound.File.Sndfile as Sndfile
import qualified System.IO.Unsafe as Unsafe

import qualified Util.Audio.Audio as Audio
import qualified Util.Num as Num
import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Config as Config
import qualified Synth.Shared.Control as Control

import           Global


type Audio = Audio.AudioIO Config.SamplingRate Channels
type Audio1 = Audio.AudioIO Config.SamplingRate 1
type NAudio = Audio.NAudioIO Config.SamplingRate

-- | Synth output is pretty tied to 2 channels, but I may as well at least
-- document the things that depend on that.
type Channels = 2

toFrames :: RealTime.RealTime -> Audio.Frames
toFrames = Audio.secondsToFrames Config.samplingRate . RealTime.to_seconds

toSeconds :: Audio.Frames -> RealTime.RealTime
toSeconds = RealTime.seconds . Audio.framesToSeconds Config.samplingRate

blockFrames2 :: Audio.Block -> Audio.Frames
blockFrames2 = Audio.blockFrames (Proxy @2)

framesCount2 :: Audio.Frames -> Audio.Count
framesCount2 = Audio.framesCount (Proxy @2)

outputFormat :: Sndfile.Format
outputFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat
    , endianFormat = Sndfile.EndianFile
    }

catchSndfile :: IO a -> IO (Either Text a)
catchSndfile = fmap try . Exception.try
    where try = either (Left . txt . Sndfile.errorString) Right

-- | Convert a volume in dB to linear.
dbToLinear :: Float -> Float
dbToLinear = Audio.dbToLinear . Num.scale (Num.d2f Control.minimumDb) 0

volume :: Audio1 -> Audio -> Audio
volume = Audio.multiply . Audio.expandChannels . Audio.mapSamples dbToLinear

-- * debug utils

debugAudio :: Audio.Audio (Resource.ResourceT IO) rate chan -> [Audio.Block]
debugAudio = Unsafe.unsafePerformIO . Resource.runResourceT . Audio.toBlocks

debugAudioN :: Audio.NAudio (Resource.ResourceT IO) rate -> [[Audio.Block]]
debugAudioN = Unsafe.unsafePerformIO . Resource.runResourceT . Audio.toBlocksN

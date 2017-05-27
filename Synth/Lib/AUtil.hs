-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Audio utilities.  This is named AUtil instead of the more obvious
-- Audio to avoid clashing with Data.Conduit.Audio.
module Synth.Lib.AUtil where
import qualified Control.Exception as Exception
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio
import qualified Data.List as List
import qualified Sound.File.Sndfile as Sndfile

import qualified Perform.RealTime as RealTime
import qualified Synth.Shared.Config as Config
import Global


type Audio = Audio.AudioSource (Resource.ResourceT IO) Float
type Frames = Int

toAudioTime :: RealTime.RealTime -> Audio.Duration
toAudioTime = Audio.Seconds . RealTime.to_seconds

toFrames :: RealTime.RealTime -> Frames
toFrames = round . (* fromIntegral Config.samplingRate) . RealTime.to_seconds

empty :: Audio
empty = Audio.silent (Audio.Frames 0) (fromIntegral Config.samplingRate) 2

outputFormat :: Sndfile.Format
outputFormat = Sndfile.Format
    { headerFormat = Sndfile.HeaderFormatWav
    , sampleFormat = Sndfile.SampleFormatFloat -- Sndfile.SampleFormatPcm16
    , endianFormat = Sndfile.EndianFile
    }

catchSndfile :: IO a -> IO (Either Text a)
catchSndfile = fmap try . Exception.try
    where try = either (Left . txt . Sndfile.errorString) Right

mix :: [(RealTime.RealTime, Audio)] -> Audio
-- TODO surely there is some better way to do mempty?
-- TODO also, Data.Conduit.Audio is inefficient in that it pads both before
-- start and after end with zeroes so they're all the same length.
mix [] = empty
mix audios = List.foldl1' Audio.mix (map place audios)
    where place (start, audio) = Audio.padStart (toAudioTime start) audio

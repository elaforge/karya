module Ness.SplitSound where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.Int as Int

import qualified Sound.File.Sndfile as File.Sndfile

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import Global


type Audio = Audio.AudioSource (Resource.ResourceT IO) Int.Int16

source = "ness-data/guitar/NdNPrA"

split :: Double
split = 6

-- goes to 128, should go to 127

load :: FilePath -> IO ()
load fname = Resource.runResourceT $ do
    audio :: Audio <- liftIO $ Sndfile.sourceSnd fname
    let dur = fromIntegral (Audio.frames audio) / Audio.rate audio
    forM_ (zip names (Seq.range' 0 dur split)) $ \(name, t) ->
        Sndfile.sinkSnd ("ness-data/split/" <> name <> ".wav") format $
            Audio.takeStart (Audio.Seconds split) $
            Audio.dropStart (Audio.Seconds t) audio

names =
    [ Seq.join "-"
        ["str" <> show str, fmt (Midi.from_key key), fmt low, fmt high]
    | (str, key) <- zip [1..10] keys, (low, high) <- rangesFrom vels
    ]

vels = [8, 16 .. 128]
rangesFrom xs =
    map (Num.clamp 1 127 *** Num.clamp 1 127) $ zip (1 : map (+1) xs) xs

fmt :: Int -> String
fmt = show -- Util.zeroPad 3

keys = oct ++ map (+12) oct
    where oct = [Key2.c3, Key2.d3, Key2.e3, Key2.g3, Key2.a3]

format :: File.Sndfile.Format
format = File.Sndfile.Format
    { headerFormat = File.Sndfile.HeaderFormatWav
    , sampleFormat = File.Sndfile.SampleFormatPcm16
    , endianFormat = File.Sndfile.EndianFile
    }

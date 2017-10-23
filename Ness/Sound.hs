module Ness.Sound where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.Sndfile as Sndfile
import qualified Data.Int as Int
import qualified Data.List as List

import qualified Sound.File.Sndfile as File.Sndfile
import qualified System.Directory as Directory

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Midi.Key2 as Key2
import qualified Midi.Midi as Midi
import Global


type Audio = Audio.AudioSource (Resource.ResourceT IO) Int.Int16
type AudioF = Audio.AudioSource (Resource.ResourceT IO) Float

mix :: [FilePath] -> FilePath -> IO ()
mix [input] output = Directory.copyFile input output
mix inputs output = Resource.runResourceT $ do
    audios :: [AudioF] <- mapM (liftIO . Sndfile.sourceSnd) inputs
    Sndfile.sinkSnd output format16 $
        Audio.gain (1 / fromIntegral (length audios)) $
        List.foldl1' Audio.mix audios

split :: FilePath -> Double -> [FilePath] -> IO ()
split fname dur names = Resource.runResourceT $ do
    audio :: Audio <- liftIO $ Sndfile.sourceSnd fname
    let totalDur = fromIntegral (Audio.frames audio) / Audio.rate audio
    forM_ (zip names (Seq.range' 0 totalDur dur)) $ \(name, t) ->
        Sndfile.sinkSnd ("ness-data/split/" <> name <> ".wav") format16 $
            Audio.takeStart (Audio.Seconds dur) $
            Audio.dropStart (Audio.Seconds t) audio

velocityNames =
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

format16 :: File.Sndfile.Format
format16 = File.Sndfile.Format
    { headerFormat = File.Sndfile.HeaderFormatWav
    , sampleFormat = File.Sndfile.SampleFormatPcm16
    , endianFormat = File.Sndfile.EndianFile
    }

formatFloat :: File.Sndfile.Format
formatFloat = File.Sndfile.Format
    { headerFormat = File.Sndfile.HeaderFormatWav
    , sampleFormat = File.Sndfile.SampleFormatFloat
    , endianFormat = File.Sndfile.EndianFile
    }

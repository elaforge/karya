-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveGeneric #-}
-- | The 'Sample' type and support.
module Synth.Sampler.Sample where
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.Audio as Audio
import qualified Data.Conduit.Audio.SampleRate as SampleRate
import qualified Data.Conduit.Audio.Sndfile as Sndfile

import qualified GHC.Generics as Generics

import qualified Util.ApproxEq as ApproxEq
import qualified Util.Num as Num
import Global
import qualified Synth.Sampler.Signal as Signal
import Synth.Sampler.Types


-- | Path to a sample, relative to the instrument db root.
type SamplePath = FilePath

-- | Low level representation of a note.  This corresponds to a single sample
-- played.
data Sample = Sample {
    start :: !Time
    -- | Relative to the sampler root.  TODO define this better?
    , filename :: !SamplePath
    -- | Sample start offset.
    , offset :: !Time
    -- | The sample ends when it runs out of samples, or when envelope ends
    -- on 0.
    , envelope :: !Signal.Signal
    -- | Sample rate conversion ratio.  This controls the pitch.
    , ratio :: !Signal.Signal
    } deriving (Show, Generics.Generic)

instance Aeson.ToJSON Sample
instance Aeson.FromJSON Sample

-- | Evaluating the Audio could probably produce more exceptions...
realize :: Sample -> IO (Either Text Audio)
realize (Sample start filename offset env ratio) = try $ do
    -- open filename, error if can't be found
    audio <- Sndfile.sourceSndFrom (Audio.Seconds offset) filename
    return $ Audio.padStart (Audio.Seconds start) $
        resample (Signal.at start ratio) $ applyEnvelope start env audio
    where
    -- TODO if the input file is not found, this throws a SystemError, not an
    -- IOError.  Who is doing that?
    try io = either (Left . annotate) Right <$> Exception.try io
    -- annotate :: IO.Error.IOError -> Text
    annotate :: Exception.SomeException -> Text
    annotate = showt
    -- annotate exc = txt filename <> ": " <> showt exc

resample :: Double -> Audio -> Audio
resample ratio audio
    -- Don't do any work if it's close enough to 1.
    | ApproxEq.eq closeEnough ratio 1 = audio
    | otherwise = (SampleRate.resample ratio SampleRate.SincBestQuality audio)
        { Audio.rate = Audio.rate audio }
        -- Since I am changing the pitch I actually do want to retain the old
        -- sample rate.
    where
    -- More or less a semitone / 100 cents / 10.  Anything narrower than this
    -- probably isn't perceptible.
    closeEnough = 1.05 / 1000

applyEnvelope :: Time -> Signal.Signal -> Audio -> Audio
applyEnvelope start sig
    | ApproxEq.eq 0.01 val 1 = id
    | otherwise = Audio.gain val
    where val = Num.d2f (Signal.at start sig)
    -- TODO scale by envelope, and shorten the audio if the 'sig' ends on 0

empty :: Audio
empty = Audio.silent (Audio.Frames 0) 44100 2

mix :: [(Time, Audio)] -> Audio
mix [] = empty
    -- emit silence while there are no Audios in scope
    -- otherwise, keep track of frame for each Audio and emit a chunk with it
    -- mixed.

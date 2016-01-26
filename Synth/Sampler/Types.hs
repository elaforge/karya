module Synth.Sampler.Types where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio


-- | Time in absolute seconds since the start of the score.
type Time = Double

type Audio = Audio.AudioSource (Resource.ResourceT IO) Float

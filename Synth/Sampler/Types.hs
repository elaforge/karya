-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Local global types, meant for unqualified import.
module Synth.Sampler.Types where
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.Conduit.Audio as Audio


-- | Time in absolute seconds since the start of the score.
type Time = Double

type Audio = Audio.AudioSource (Resource.ResourceT IO) Float

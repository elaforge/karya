-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Types where
import qualified Util.Pretty as Pretty
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Lib.Global


-- | Path to a sample, relative to the instrument db root.
type SamplePath = FilePath

-- | Low level representation of a note.  This corresponds to a single sample
-- played.
data Sample = Sample {
    start :: !RealTime
    -- | Relative to 'Config.instrumentDbDir'.
    , filename :: !SamplePath
    -- | Sample start offset.
    , offset :: !RealTime
    -- | The sample ends when it runs out of samples, or when envelope ends
    -- on 0.
    , envelope :: !Signal.Signal
    -- | Sample rate conversion ratio.  This controls the pitch.
    , ratio :: !Signal.Signal
    } deriving (Show)

instance Pretty Sample where
    format (Sample start filename offset envelope ratio) =
        Pretty.record "Sample"
            [ ("start", Pretty.format start)
            , ("filename", Pretty.format filename)
            , ("offset", Pretty.format offset)
            , ("envelope", Pretty.format envelope)
            , ("ratio", Pretty.format ratio)
            ]

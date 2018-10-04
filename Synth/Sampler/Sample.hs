-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Sampler.Sample where
import qualified Util.Pretty as Pretty
import qualified Perform.Pitch as Pitch
import qualified Synth.Shared.Note as Note
import qualified Synth.Shared.Signal as Signal

import Global
import Synth.Lib.Global


-- | Path to a sample, relative to the instrument db root.
type SamplePath = FilePath

-- | Low level representation of a note.  This corresponds to a single sample
-- played.
data Note = Note {
    start :: !RealTime
    -- | This is the actual duration of the sample at the given 'ratio', not
    -- the requested 'Note.duration'.
    , duration :: !RealTime
    -- | The hash of the source note.
    , hash :: !Note.Hash
    -- | This is Left Error if the converter failed to find a sample.
    , sample :: Either Text Sample
    }

end :: Note -> RealTime
end note = start note + duration note

-- | The actual sample played by a 'Note'.
data Sample = Sample {
    -- | Relative to 'Config.instrumentDbDir'.
    filename :: !SamplePath
    -- | Sample start offset.
    , offset :: !RealTime
    -- | The sample ends when it runs out of samples, or when envelope ends
    -- on 0.
    , envelope :: !Signal.Signal
    -- | Sample rate conversion ratio.  This controls the pitch.
    , ratio :: !Signal.Signal
    } deriving (Show)

instance Pretty Note where
    format (Note start dur hash sample) = Pretty.record "Note"
        [ ("start", Pretty.format start)
        , ("duration", Pretty.format dur)
        , ("hash", Pretty.format hash)
        , ("sample", Pretty.format sample)
        ]

instance Pretty Sample where
    format (Sample filename offset envelope ratio) = Pretty.record "Sample"
        [ ("filename", Pretty.format filename)
        , ("offset", Pretty.format offset)
        , ("envelope", Pretty.format envelope)
        , ("ratio", Pretty.format ratio)
        ]

-- * util

pitchToRatio :: Pitch.Hz -> Pitch.NoteNumber -> Signal.Y
pitchToRatio sampleHz nn = sampleHz / Pitch.nn_to_hz nn

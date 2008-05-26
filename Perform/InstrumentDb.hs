module Perform.InstrumentDb where
import Prelude hiding (lookup)

import qualified Perform.Midi.Instrument as Midi.Instrument
import qualified Perform.Midi.InstrumentDb as Midi.InstrumentDb

import qualified Derive.Score as Score


data Backend = Midi Midi.Instrument.Instrument
    | Csound
    -- csound, osc, etc.
    deriving (Show)

-- | Convert a Score.Instrument into a full-fledged Midi.Instrument by looking
-- it up in the midi instrument db.
--
-- TODO For now there's only the MIDI backend, but later this should search
-- the various backends.
lookup :: Score.Instrument -> Maybe Backend
lookup score_inst = fmap Midi $ Midi.InstrumentDb.lookup score_inst

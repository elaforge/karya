{- | Convert a Score.Instrument into a full-fledged Midi.Instrument by looking
it up in the midi instrument db.

TODO: this should also include local instruments loaded at runtime, or compiled
in from a local directory

TODO: when I support multiple backends, this will be called from
Perform.InstrumentDb
-}
module Perform.Midi.InstrumentDb where
import Prelude hiding (lookup)

import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Perform.Midi.Instrument (Instrument(..), InitializeInstrument(..))


lookup :: Score.Instrument -> Maybe Instrument.Instrument
lookup (Score.Instrument name)
    | length (filter (=='/') name) == 1 =
        let [synth, patch] = Seq.split "/" name in Just (generic synth patch)
    | otherwise = Nothing

default_pb_range :: Instrument.PbRange
default_pb_range = (-12, 12)

generic synth patch =
    Instrument patch synth NoInitialization default_pb_range Nothing

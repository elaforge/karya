module Perform.Midi.InstrumentDb where
import Prelude hiding (lookup)

import qualified Util.Seq as Seq

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import Perform.Midi.Instrument (Instrument(..), InitializeInstrument(..))


lookup :: Score.Instrument -> Maybe Instrument.Instrument
lookup (Score.Instrument name Score.Midi)
    | length (filter (=='/') name) == 1 =
        let [synth, patch] = Seq.split "/" name in Just (generic synth patch)
    | otherwise = Nothing
-- lookup _ = Nothing

default_pb_range :: Instrument.PbRange
default_pb_range = (-12, 12)

generic synth patch =
    Instrument patch synth NoInitialization default_pb_range Nothing

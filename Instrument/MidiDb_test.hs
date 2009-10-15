module Instrument.MidiDb_test where
import qualified Data.Set as Set

import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Controller as Controller
import qualified Instrument.MidiDb as MidiDb
import qualified Local.Instrument.Kontakt as Kontakt


test_lookup_instrument = do
    synth_descs <- mapM ($"") [Kontakt.load]
    let midi_db = MidiDb.midi_db synth_descs
    let f inst attrs =
            MidiDb.lookup_midi midi_db
                (Set.fromList attrs) (Score.Instrument inst)

    let ks name key = Just (Instrument.Keyswitch name key)
        kkt_inst name = Instrument.instrument "kkt" name Nothing
            Controller.empty_map (-96, 96)
        hang = kkt_inst "hang1"
    equal (f "kkt/hang1" ["slap"]) $ Just $
        hang { Instrument.inst_keyswitch = ks "slap" 38 }
    equal (f "kkt/hang1" []) $ Just $
        hang { Instrument.inst_keyswitch = ks "" 36 }

    -- wildcard allows any other name, but ks is not allowed
    equal (f "kkt/none" []) $ Just (kkt_inst "none")
    equal (f "kkt/none" ["slap"]) $ Just (kkt_inst "none")

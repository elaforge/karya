module Instrument.MidiDb_test where

import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Local.Instrument.Kontakt as Kontakt


test_lookup_midi = do
    synth_desc <- Kontakt.load ""
    let midi_db = MidiDb.midi_db [synth_desc]
    let f inst attrs = MidiDb.lookup_midi midi_db
            (Score.attributes attrs) (Score.Instrument inst)

    let ks = Just . Instrument.Keyswitch
    let kkt_inst name = (Instrument.instrument name [] (-96, 96))
            { Instrument.inst_score = Score.Instrument ("kkt/" ++ name)
            , Instrument.inst_synth = "kkt"
            }
        hang = kkt_inst "hang1"
    equal (f "kkt/hang1" ["slap"]) $
        Just (hang { Instrument.inst_keyswitch = ks 38 },
            (Score.attributes ["slap"]))
    equal (f "kkt/hang1" []) $
        Just (hang { Instrument.inst_keyswitch = ks 36 },
            Score.no_attrs)

    -- wildcard allows any other name, but ks is not allowed
    equal (f "kkt/none" []) $ Just (kkt_inst "none", Score.no_attrs)
    equal (f "kkt/none" ["slap"]) $ Just (kkt_inst "none", Score.no_attrs)

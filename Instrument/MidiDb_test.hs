module Instrument.MidiDb_test where

import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Local.Instrument.Kontakt as Kontakt


test_lookup_instrument = do
    synth_descs <- mapM ($"") [Kontakt.load]
    let midi_db = MidiDb.midi_db synth_descs
    let f = fmap Instrument.inst_keyswitch
            . MidiDb.lookup_midi midi_db . Score.Instrument
    -- explicit ks and implicit ks
    equal (f "kkt/hang1/slap") (Just (Just (Instrument.Keyswitch "slap" 38)))
    equal (f "kkt/hang1") (Just (Just (Instrument.Keyswitch "" 36)))
    -- wildcard picks up other names, but a ks is not allowed
    equal (f "kkt/none") (Just Nothing)
    equal (f "kkt/none/none") Nothing

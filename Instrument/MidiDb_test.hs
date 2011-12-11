module Instrument.MidiDb_test where
import qualified Data.ByteString as B
import qualified Data.Char as Char
import qualified Data.Map as Map

import Util.Control
import Util.Test
import qualified Midi.Midi as Midi
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Local.Instrument.Kontakt as Kontakt


test_lookup_midi = do
    synth_descs <- Kontakt.load ""
    let midi_db = fst $ MidiDb.midi_db synth_descs
    let f inst attrs = MidiDb.lookup_midi midi_db
            (Score.attributes attrs) (Score.Instrument inst)

    let ks = Just . Instrument.Keyswitch
    let kkt_inst name = (Instrument.instrument name [] (-12, 12))
            { Instrument.inst_score = Score.Instrument ("kkt/" ++ name)
            , Instrument.inst_synth = "kkt"
            }
        hang = kkt_inst "hang1"
    equal (f "kkt/hang1" ["slap"]) $
        Just (hang { Instrument.inst_keyswitch = ks 38 },
            Score.attributes ["slap"])
    equal (f "kkt/hang1" []) $
        Just (hang { Instrument.inst_keyswitch = ks 36 },
            Score.no_attrs)

    -- wildcard allows any other name, but ks is not allowed
    equal (f "kkt/none" []) $ Just (kkt_inst "none", Score.no_attrs)
    equal (f "kkt/none" ["slap"]) $ Just (kkt_inst "none", Score.no_attrs)

test_patch_map = do
    let f = first extract . MidiDb.patch_map . mkpatches
        extract (MidiDb.PatchMap ps) = map (second name) (Map.toList ps)
        name = Instrument.inst_name . Instrument.patch_instrument . fst
    -- different initialization gets split
    equal (f [("a", pgm_change 1), ("*a", pgm_change 2)])
        ([("a1", "*a"), ("a2", "a")], ["split into a1, a2: *a, a"])

    equal (f [("a", pgm_change 1), ("*a", pgm_change 1)])
        ([("a", "*a")], ["dropped patches with identical initialization: a"])
    -- no dropping needed if the names are different
    equal (f [("a", pgm_change 1), ("b", pgm_change 1)])
        ([("a", "a"), ("b", "b")], [])

    -- Uses the pgm_change from the first, and the sysex from the second.
    -- I don't actually verify that but it's probably right.
    equal (f [("a", pgm_change 1), ("a", sysex "abc")])
        ([("a", "a")], ["merging program-change patch into sysex patch: a, a"])
    -- but not if the names differ
    equal (f [("a", pgm_change 1), ("*a", sysex "abc")])
        ([("a1", "*a"), ("a2", "a")], ["split into a1, a2: *a, a"])

mkpatches ps = map (uncurry mkpatch) ps

mkpatch :: Instrument.InstrumentName -> Instrument.InitializePatch
    -> MidiDb.PatchCode ()
mkpatch name init = (patch, ())
    where
    inst = Instrument.instrument name [] (-2, 2)
    patch = (Instrument.patch inst) { Instrument.patch_initialize = init }

pgm_change :: Midi.Program -> Instrument.InitializePatch
pgm_change pgm = Instrument.InitializeMidi $ map (Midi.ChannelMessage 0) $
    Midi.program_change 0 pgm

sysex :: String -> Instrument.InitializePatch
sysex bs = Instrument.InitializeMidi
    [Midi.CommonMessage $ Midi.SystemExclusive 0 $
        B.pack (map (fromIntegral . Char.ord) bs)]

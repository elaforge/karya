module Instrument.Search_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


test_search = do
    let f = map Score.inst_name . Search.search index . Search.parse
    equal (f "") t_all_insts
    equal (f "synth=") t_all_insts
    equal (f "category=key control=comb") ["z1/comb-clav"]
    equal (f "category=key control=") ["z1/comb-clav", "z1/pulse-clav"]
    equal (f "name=delg name=comb") []
    equal (f "synth=fm8") ["fm8/*"]

    equal (f "!synth=z1") ["fm8/*"]
    equal (f "!category=epiano !category=synth")
        ["fm8/*", "z1/comb-clav", "z1/pulse-clav"]
    equal (f "!category=epiano !category=synth name=clav")
        ["z1/comb-clav", "z1/pulse-clav"]

index :: Search.Index
index = Search.make_index midi_db

midi_db :: MidiDb.MidiDb Cmd.InstrumentCode
midi_db = fst $ MidiDb.midi_db
    [(z1_synth, z1_patches), (fm8_synth, fm8_patches)]

t_all_insts :: [String]
t_all_insts = map Score.inst_name (Map.keys (Search.idx_instrument_tags index))

z1_synth :: Instrument.Synth
z1_synth = Instrument.synth "z1" [(13, "pe 1")]

z1_patches :: MidiDb.PatchMap Cmd.InstrumentCode
z1_patches = fst $ MidiDb.patch_map $ map mkpatch
    [ ("Mr. Delgado", "synth-lead", [(14, "delgado")])
    , ("Studio E.P.", "epiano", [])
    , ("Square Hollow", "synth-soft", [])
    , ("Dyna Expressive", "epiano", [])
    , ("Digi-Clear E.P.", "epiano", [])
    , ("Comb Clav", "keyboard", [(15, "comb")])
    , ("Pulse Clav", "keyboard", [])
    ]

fm8_synth :: Instrument.Synth
fm8_synth = Instrument.synth "fm8" []

fm8_patches :: MidiDb.PatchMap Cmd.InstrumentCode
fm8_patches = MidiDb.wildcard_patch_map (mkpatch ("none", "fm", []))

mkpatch :: (String, String, [(Midi.Control, String)])
    -> MidiDb.PatchCode Cmd.InstrumentCode
mkpatch (name, cat, conts) = (patch, Cmd.empty_code)
    where
    tags = map (uncurry Instrument.tag) [("category", cat)]
    inst = Instrument.instrument name conts (-2, 2)
    patch = (Instrument.patch inst) { Instrument.patch_tags = tags }

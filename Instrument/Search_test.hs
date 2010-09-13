module Instrument.Search_test where
import qualified Data.Map as Map

import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument

import qualified Instrument.Search as Search
import qualified Instrument.MidiDb as MidiDb


test_search = do
    let f = map Score.inst_name . Search.search index
    equal (f []) t_all_insts
    equal (f [("synth", "")]) t_all_insts
    equal (f [("category", "key"), ("control", "comb")])
        ["z1/comb_clav"]
    equal (f [("category", "key"), ("control", "")])
        ["z1/comb_clav", "z1/pulse_clav"]
    equal (f [("name", "delg"), ("name", "comb")])
        []
    equal (f [("synth", "fm8")])
        ["fm8/*"]

index = Search.make_index midi_db
midi_db = MidiDb.midi_db
    [(t_synth, t_patches), (t_synth2, t_patches2)]
t_all_insts = map Score.inst_name (Map.keys (Search.idx_inverted index))

t_synth = Instrument.set_device "z1 dev" $ Instrument.synth "z1" [(13, "pe 1")]
t_patches = fst $ MidiDb.patch_map $ map mkpatch
    [ ("Mr. Delgado", "Synth-Lead", [(14, "delgado")])
    , ("Studio E.P.", "E.Piano", [])
    , ("Square Hollow", "Synth-Soft", [])
    , ("Dyna Expressive", "E.Piano", [])
    , ("Digi-Clear E.P.", "E.Piano", [])
    , ("Comb Clav", "Keyboard", [(15, "comb")])
    , ("Pulse Clav", "Keyboard", [])
    ]

t_synth2 = Instrument.set_device "fm8 dev" $ Instrument.synth "fm8" []
t_patches2 = MidiDb.wildcard_patch_map (mkpatch ("none", "fm", []))

t_tags = Search.patch_tags t_patches

mkpatch (name, cat, conts) = (Instrument.patch inst)
    { Instrument.patch_tags = tags }
    where
    tags = map (uncurry Instrument.tag) [("category", cat)]
    inst = Instrument.instrument name conts (-2, 2)

module Instrument.Search_test where
import qualified Data.Map as Map

import Util.Test

import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Control as Control

import qualified Instrument.Search as Search
import qualified Instrument.MidiDb as MidiDb

import qualified Local.Instrument.Z1


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

t_synth = Instrument.synth "z1" "Z1 dev" [(13, "pe 1")]
t_patches = fst $ MidiDb.patch_map $ map mkpatch
    [ ("Mr. Delgado", "Synth-Lead", [(14, "delgado")])
    , ("Studio E.P.", "E.Piano", [])
    , ("Square Hollow", "Synth-Soft", [])
    , ("Dyna Expressive", "E.Piano", [])
    , ("Digi-Clear E.P.", "E.Piano", [])
    , ("Comb Clav", "Keyboard", [(15, "comb")])
    , ("Pulse Clav", "Keyboard", [])
    ]

t_synth2 = Instrument.synth "fm8" "fm8 dev" []
t_patches2 = MidiDb.wildcard_patch_map (mkpatch ("none", "fm", []))

t_tags = Search.patch_tags t_patches

mkpatch (name, cat, conts) = (Instrument.patch inst)
    { Instrument.patch_tags = tags }
    where
    tags = map (uncurry Instrument.tag) [("category", cat)]
    inst = Instrument.instrument (Instrument.synth_name t_synth) name Nothing
        (Control.control_map conts) (-2, 2)

get_z1 = do
    (_synth, (MidiDb.PatchMap patches)) <-
        Local.Instrument.Z1.load "Local/Instrument"
    return $ take 10 (Map.assocs patches)

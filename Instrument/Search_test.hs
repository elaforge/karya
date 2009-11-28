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
    let idx = Search.make_index midi_db
    equal (Search.search idx []) []
    equal (Search.search idx [("category", "key"), ("control", "comb")])
        (insts ["z1/comb_clav"])
    equal (Search.search idx [("category", "key"), ("control", "")])
        (insts ["z1/comb_clav", "z1/pulse_clav"])
    equal (Search.search idx [("name", "delg"), ("name", "comb")])
        []
    equal (Search.search idx [("synth", "fm8")])
        (insts ["fm8/*"])

insts = map Score.Instrument

midi_db = MidiDb.midi_db
    [(t_synth, t_patches), (t_synth2, t_patches2)]

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

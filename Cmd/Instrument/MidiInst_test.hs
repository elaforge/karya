-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.MidiInst_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Midi as Midi
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Perform.Midi.Instrument as Instrument
import Global


test_generate_names = do
    let f = first extract . MidiInst.generate_names . map (uncurry mkpatch)
        extract = map (second name) . Map.toList
        name = Instrument.inst_name . Instrument.patch_instrument . fst
    -- different initialization gets split
    equal (f [("a", pgm_change 1), (" a", pgm_change 2)])
        ([("a1", "a"), ("a2", " a")],
            ["split into a1, a2: a (a.vc),  a ( a.vc)"])

    equal (f [("a", pgm_change 1), (" a", pgm_change 1)])
        ([("a", "a")], ["dropped patches with the same initialization as "
            <> "a (a.vc):  a ( a.vc)"])
    -- no dropping needed if the names are different
    equal (f [("a", pgm_change 1), ("b", pgm_change 1)])
        ([("a", "a"), ("b", "b")], [])

mkpatch :: Instrument.InstrumentName -> Instrument.InitializePatch
    -> (Instrument.Patch, ())
mkpatch name init = (patch, ())
    where
    inst = Instrument.instrument (-2, 2) name []
    patch = (Instrument.patch inst)
        { Instrument.patch_initialize = init
        , Instrument.patch_file = "path/" ++ untxt name ++ ".vc"
        }

pgm_change :: Midi.Program -> Instrument.InitializePatch
pgm_change pgm = Instrument.InitializeMidi $ map (Midi.ChannelMessage 0) $
    Midi.program_change 0 pgm

-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.MidiInstDb_test where
import qualified Data.Map as Map

import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Instrument.MidiInstDb as MidiInstDb
import qualified Instrument.Common as Common
import qualified Instrument.InstT as InstT
import qualified Instrument.Tag as Tag

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch

import           Global
import           Util.Test


test_generate_names :: Test
test_generate_names = do
    let f = first extract . MidiInstDb.generateNames . map (uncurry mkpatch)
        extract = map (second name) . Map.toList
        name = Patch.patchName .  MidiInst.patchPatch
    -- different initialization gets split
    equal (f [("a", pgmChange 1), (" a", pgmChange 2)])
        ([("a1", "a"), ("a2", " a")],
            ["split into a1, a2: a (a.vc),  a ( a.vc)"])

    equal (f [("a", pgmChange 1), (" a", pgmChange 1)])
        ([("a", "a")], ["dropped patches with the same initialization as "
            <> "a (a.vc):  a ( a.vc)"])
    -- no dropping needed if the names are different
    equal (f [("a", pgmChange 1), ("b", pgmChange 1)])
        ([("a", "a"), ("b", "b")], [])

mkpatch :: InstT.Name -> Patch.InitializePatch -> MidiInst.Patch
mkpatch name init =
    MidiInst.common#Common.tags #= [(Tag.file, name <> ".vc")] $
    MidiInst.patch#Patch.initialize #= init $
    MidiInst.namedPatch (-2, 2) name []

pgmChange :: Midi.Program -> Patch.InitializePatch
pgmChange = Patch.initializeMidi . Midi.programChange 0

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Search_test where
import qualified Data.Map as Map

import Util.Test
import qualified Midi.Midi as Midi
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Common as Common
import qualified Instrument.Inst as Inst
import qualified Instrument.InstT as InstT
import qualified Instrument.Search as Search
import qualified Instrument.Tag as Tag

import Global


test_search :: Test
test_search = do
    let f = map InstT.showQualified . Search.search index . Search.parse
    equal (f "") tAllInsts
    equal (f "synth=") tAllInsts
    equal (f "category=key control=comb") ["z1/comb-clav"]
    equal (f "category=key control=") ["z1/comb-clav", "z1/pulse-clav"]
    equal (f "name=delg name=comb") []
    equal (f "synth=fm8") ["fm8/"]
    equal (f "!synth=z1") ["fm8/"]
    equal (f "!category=epiano !category=synth")
        ["fm8/", "z1/comb-clav", "z1/pulse-clav"]
    equal (f "!category=epiano !category=synth name=clav")
        ["z1/comb-clav", "z1/pulse-clav"]
    equal (f "fm") ["fm8/"]
    equal (f "fm8/") ["fm8/"]
    equal (f "fm8/x") []
    equal (f "z1/s") ["z1/square-hollow", "z1/studio-ep"]
    equal (f "z1/sq") ["z1/square-hollow"]

index :: Search.Index
index = Search.makeIndex db

db :: Cmd.InstrumentDb
db = makeDb [("fm8", fm8Patches), ("z1", z1Patches)]

tAllInsts :: [Text]
tAllInsts =
    map InstT.showQualified (Map.keys (Search.idxInstrumentTags index))

z1Patches :: [(Patch.Patch, Text)]
z1Patches =
    [ (mkpatch "mr-delgado" [(14, "delgado")], "synth-lead")
    , (mkpatch "studio-ep" [], "epiano")
    , (mkpatch "square-hollow" [], "synth-soft")
    , (mkpatch "dyna-expressive" [], "epiano")
    , (mkpatch "digi-clear" [], "epiano")
    , (mkpatch "comb-clav" [(15, "comb")], "keyboard")
    , (mkpatch "pulse-clav" [(16, "pe1")], "keyboard")
    ]

fm8Patches :: [(Patch.Patch, Text)]
fm8Patches = [(mkpatch "" [], "fm")]

mkpatch :: InstT.Name -> [(Midi.Control, ScoreT.Control)] -> Patch.Patch
mkpatch name controls = (Patch.patch (-2, 2) name)
    { Patch.patchControlMap = Control.controlMap controls }

makeDb :: [(InstT.SynthName, [(Patch.Patch, Text)])] -> Cmd.InstrumentDb
makeDb synthPatches = fst $ Inst.db $ map make synthPatches
    where make (name, patches) = makeSynth name patches

makeSynth :: InstT.SynthName -> [(Patch.Patch, Text)] -> MidiInst.Synth
makeSynth name = MidiInst.synth name "Test Synth" . map make
    where
    make (patch, category) =
        MidiInst.common#Common.tags #= [(Tag.category, category)] $
        MidiInst.makePatch patch

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    let f inst = MidiDb.lookup_midi midi_db (Score.Instrument inst)

    let kkt_inst name = (Instrument.instrument name [] Kontakt.pb_range)
            { Instrument.inst_score = Score.Instrument ("kkt/" <> name)
            , Instrument.inst_synth = "kkt"
            }
        hang = kkt_inst "hang"
    equal (f "kkt/hang") (Just hang)
    -- wildcard allows any other name
    equal (f "kkt/none") $ Just (kkt_inst "none")

test_patch_map = do
    let f = first extract . MidiDb.patch_map . mkpatches
        extract (MidiDb.PatchMap ps) = map (second name) (Map.toList ps)
        name = Instrument.inst_name . Instrument.patch_instrument . fst
    -- different initialization gets split
    equal (f [("a", pgm_change 1), ("*a", pgm_change 2)])
        ([("a1", "a"), ("a2", "*a")],
            ["split into a1, a2: a (a.vc), *a (*a.vc)"])

    equal (f [("a", pgm_change 1), ("*a", pgm_change 1)])
        ([("a", "a")], ["dropped patches with the same initialization as "
            ++ "a (a.vc): *a (*a.vc)"])
    -- no dropping needed if the names are different
    equal (f [("a", pgm_change 1), ("b", pgm_change 1)])
        ([("a", "a"), ("b", "b")], [])

mkpatches ps = map (uncurry mkpatch) ps

mkpatch :: Instrument.InstrumentName -> Instrument.InitializePatch
    -> MidiDb.PatchCode ()
mkpatch name init = (patch, ())
    where
    inst = Instrument.instrument name [] (-2, 2)
    patch = (Instrument.patch inst)
        { Instrument.patch_initialize = init
        , Instrument.patch_file = "path/" ++ untxt name ++ ".vc"
        }

pgm_change :: Midi.Program -> Instrument.InitializePatch
pgm_change pgm = Instrument.InitializeMidi $ map (Midi.ChannelMessage 0) $
    Midi.program_change 0 pgm

sysex :: String -> Instrument.InitializePatch
sysex bs = Instrument.InitializeMidi
    [Midi.CommonMessage $ Midi.SystemExclusive 0 $
        B.pack (map (fromIntegral . Char.ord) bs)]

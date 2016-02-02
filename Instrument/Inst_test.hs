-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Inst_test where
import Util.Test
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Inst as Inst
import qualified Local.Instrument.Kontakt as Kontakt


test_lookup = do
    let db = fst $ Inst.db [Kontakt.synth]
    let f synth name = Instrument.patch_instrument <$>
            (Inst.inst_midi =<< Inst.lookup (Inst.Qualified synth name) db)

    let kontakt_inst name = Instrument.instrument Kontakt.pb_range name []
    equal (f "kontakt" "hang") $ Just (kontakt_inst "hang")
    -- Has default inst.
    equal (f "kontakt" "") $ Just (kontakt_inst "")

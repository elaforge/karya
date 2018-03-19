-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Instrument.Inst_test where
import Util.Test
import qualified Perform.Midi.Patch as Patch
import qualified Instrument.Inst as Inst
import qualified Instrument.InstTypes as InstTypes
import qualified User.Elaforge.Instrument.Kontakt as Kontakt


test_lookup = do
    let db = fst $ Inst.db [Kontakt.synth]
    let f synth name =
            Inst.inst_midi =<< Inst.lookup (InstTypes.Qualified synth name) db

    let kontakt_inst name = Patch.patch Kontakt.pb_range name
    equal (Patch.patch_name <$> f "kontakt" "hang") $ Just "hang"
    -- Has default inst.
    equal (f "kontakt" "") $ Just (kontakt_inst "")

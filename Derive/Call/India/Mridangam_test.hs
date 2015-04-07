-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Mridangam_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Local.Instrument.Kontakt as Kontakt


test_pattern = do
    let run = DeriveTest.extract extract
            . derive_tracks "import india.mridangam"
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 5, "p1 ktkno")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+nam"), (6, "+thom")], [])
    equal (run [(2, 3, "p1 k_D")])
        ([(2, "+ki"), (4, "+thom"), (4, "+din")], [])

    equal (run [(2, 4, "pn kt")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+ta")], [])
    equal (run [(2, 3, "pn kt")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki")], [])
    equal (run [(2, 2, "Pn kt")])
        ([(2, "+ta"), (3, "+ki"), (4, "+ta")], [])

derive_tracks :: String -> [UiTest.EventSpec] -> Derive.Result
derive_tracks title notes = DeriveTest.derive_tracks_with with_synth title
    [(">kontakt/mridangam", notes)]

with_synth :: Derive.Deriver a -> Derive.Deriver a
with_synth = DeriveTest.with_inst_db Kontakt.synth_descs

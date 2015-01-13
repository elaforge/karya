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


test_farans = do
    let run = DeriveTest.extract extract
            . derive_tracks "import india.mridangam.faran"
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 4, "ktkn 1")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+nam")], [])
    -- ki ta ki nam | tha ki ta ki |
    equal (run [(2, 2, "ktkn 2 4/32")])
        ([(2, "+tha"), (2.5, "+ki"), (3, "+ta"), (3.5, "+ki")], [])
    equal (run [(2, 4, "ou-k 1")])
        ([(2, "+thom"), (3, "+arai"), (5, "+ki")], [])

test_kandam = do
    let run = DeriveTest.extract extract
            . derive_tracks "import india.mridangam.kandam"
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 5, "ktkn5")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+nam"), (6, "+thom")], [])

derive_tracks :: String -> [UiTest.EventSpec] -> Derive.Result
derive_tracks title notes = DeriveTest.derive_tracks_with with_synth title
    [(">kontakt/mridangam", notes)]

with_synth :: Derive.Deriver a -> Derive.Deriver a
with_synth = DeriveTest.with_inst_db Kontakt.synth_descs

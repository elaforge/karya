-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Bali.Gong_test where
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Ui.UiTest as UiTest

import           Util.Test


test_jegog :: Test
test_jegog = do
    let title = "import bali.gong | jegog-insts = (list i1) | scale=legong"
            <> " | cancel"
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks title . UiTest.note_track
        extract e = (s, d, p, DeriveTest.e_instrument e)
            where (s, d, p) = DeriveTest.e_note e
        jegog = "i1"
    equal (run [(0, 1, "J | -- 4i")])
        ([(0, 1, "4i", ""), (0, 1, "3i", jegog)], [])
    -- Duration is until the next jegog note.
    equal (run [(0, 1, "J | -- 4o"), (1, 1, "4e"), (2, 1, "J | -- 4u")])
        ([ (0, 1, "4o", ""), (0, 2, "3o", jegog)
         , (1, 1, "4e", "")
         , (2, 1, "4u", ""), (2, 1, "3u", jegog)
         ], [])

test_nruk :: Test
test_nruk = do
    let run = DeriveTest.extract Score.event_start
            . DeriveTest.derive_tracks "import bali.gong" . UiTest.note_track
    equal (run [(0, 2, "nruk 2 2 | --")]) ([0, 0.5, 1, 1.5], [])
    -- It only emits full duration notes.
    equal (run [(0, 2.15, "nruk 2 2 | --")]) ([0, 0.5, 1, 1.5], [])

test_cycle :: Test
test_cycle = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks "import bali.gong" . UiTest.note_track
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(0, 4, "cycle (list '+a' '+b' '+c') 1 --")])
        ([(0, "+a"), (1, "+b"), (2, "+c"), (3, "+a")], [])
    equal (run [(4, -4, "cycle (list '+a' '+b' '+c') 1 --")])
        ([(1, "+c"), (2, "+a"), (3, "+b"), (4, "+c")], [])

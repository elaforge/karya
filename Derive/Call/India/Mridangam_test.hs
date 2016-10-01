-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Mridangam_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Local.Instrument.Kontakt as Kontakt
import Global


test_sequence = do
    let run = DeriveTest.extract extract . derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 5, "seq ktkno")])
        (zip (Seq.range_ 2 1) ktkno, [])
    -- D is two strokes together, and _ or space are rest.
    equal (run [(2, 5, "seq 'k_D t'")])
        ([(2, "+ki"), (4, "+thom"), (4, "+din"), (6, "+ta")], [])

    -- Positive means clip the end.
    equal (run [(2, 3, "seq ktkno")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki")], [])
    -- Negative means clip the beginning.
    equal (run [(5, -3, "seq ktkno")])
        ([(2, "+ta"), (3, "+ki"), (4, "+nam"), (5, "+thom")], [])
    -- Cycle if longer than needed.
    equal (run [(2, 10, "seq ktkno")])
        (zip (Seq.range_ 2 1) (ktkno ++ ktkno), [])
    -- Otherwise dur 0 means stretch.
    equal (run [(2, 10, "dur=0 | seq ktkno")])
        (zip (Seq.range_ 2 2) ktkno, [])

    -- hardcoded pattern
    equal (run [(2, 2, "tk")]) ([(2, "+ki"), (3, "+tha")], [])

test_pattern = do
    let run = DeriveTest.extract extract . derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 5, "p 5")]) (zip (Seq.range_ 2 1) ktkno, [])
    equal (run [(2, 5, "var=f567-1 | p 5")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+ki"), (5.5, "+ta"),
            (6, "+thom")], [])

test_infer_pattern = do
    let run title = DeriveTest.extract extract . derive_tracks title
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run " | pattern = \"(infer _ 1)" [(1, 5, "seq (infer _ 1)")])
        (zip [1, 2, 3, 4, 5] ktkno, [])
    equal (run " | pattern = \"(infer _ 1)" [(1, 6, "seq")])
        (zip [1, 2, 4, 5, 6] ktkno, [])
    equal (run " | pattern = \"(infer _ 1)" [(1, 7, "seq")])
        (zip [1, 3, 5, 6, 7] ktkno, [])

ktkno :: [String]
ktkno = ["+ki", "+ta", "+ki", "+nam", "+thom"]

derive_tracks :: String -> [UiTest.EventSpec] -> Derive.Result
derive_tracks title notes = DeriveTest.derive_tracks_setup with_synth
    ("import india.mridangam" <> title) [(">m", notes)]

with_synth :: DeriveTest.Setup
with_synth =
    DeriveTest.with_synths_simple [("m", "kontakt/mridangam")] [Kontakt.synth]

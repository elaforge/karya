-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.India.Mridangam_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.India.Mridangam as Mridangam
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

    -- -- TODO align negative 0 dur to end as well
    -- equal (run [(7, -5, "seq ktkno")])
    --     (zip (Seq.range_ 3 1) ktkno, [])

    -- Positive means clip the end.
    equal (run [(2, 3, "seq ktkno 1")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki")], [])
    -- Negative means clip the beginning.
    equal (run [(5, -3, "seq ktkno 1")])
        ([(2, "+ta"), (3, "+ki"), (4, "+nam"), (5, "+thom")], [])
    -- Cycle if longer than needed.
    equal (run [(2, 10, "seq ktkno 1")])
        (zip (Seq.range_ 2 1) (ktkno ++ ktkno), [])
    -- Otherwise dur 0 means stretch.
    equal (run [(2, 10, "seq ktkno 0")])
        (zip (Seq.range_ 2 2) ktkno, [])

    -- hardcoded pattern
    equal (run [(2, 2, "tk")]) ([(2, "+ki"), (3, "+tha")], [])

test_tirmanam = do
    let run = DeriveTest.extract extract . derive_tracks " | cancel"
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    let tathom = ["+ta", "+thom", "+ta", "+thom", "+ta"]
    strings_like (snd $ run [(0, 4, "dur=1 | tir t o")])
        ["event dur 4t: karvai would have to be .5t matras"]
    strings_like (snd $ run [(0, 2, "dur=1 | tir t o")])
        ["would have to stretch karvai to -.5t"]
    equal (run [(0, 5, "dur=1 | tir t o")])
        (zip (Seq.range_ 0 1) tathom, [])
    strings_like (snd $run [(0, 10, "dur=1 | tir t o")])
        ["karvai would have to be 3.5t matras"]
    equal (run [(0, 9, "dur=1 | tir t o")])
        (zip [0, 1, 4, 5, 8] tathom, [])
    equal (run [(5, -5, "dur=1 | tir t o")])
        (zip (Seq.range_ 0 1) (tathom ++ ["+thom"]), [])

    strings_like (snd $ run [(0, 10, "dur=1 | tir t o_p")]) ["expected 9\\*1t"]
    equal (run [(0, 9, "dur=1 | tir t o_+")])
        ( [ (0, "+ta"), (1, "+thom"), (3, "+tha"), (4, "+ta"), (5, "+thom")
          , (7, "+tha"), (8, "+ta")
          ]
        , []
        )

    equal (run [(0, 10, "dur=0 | tir t o")]) (zip [0, 2, 4, 6, 8] tathom, [])
    equal (run [(10, -10, "dur=0 | tir t o")])
        (zip [0, 2, 4, 6, 8, 10] (tathom ++ ["+thom"]), [])

    -- nam is cancelled by the final thom.
    equal (run [(5, -5, "dur=1 | tir t o"), (5, 0, "n"), (6, 0, "d")])
        (zip (Seq.range_ 0 1) (tathom ++ ["+thom", "+din"]), [])

test_stretch_karvai = do
    let f seq karvai matra event =
            Mridangam.stretch_karvai (Mridangam.parse_sequence seq)
            (Mridangam.parse_sequence karvai) matra event
    -- sequence is 10, karvai is 1
    left_like (f "+kn+k_+u_k" "D" 1 16) "would have to stretch karvai"
    equal (f "+kn+k_+u_k" "D" 1 32) (Right [(1, Mridangam.Stroke 'D')])
    left_like (f "+kn+k_+u_k" "D_" 1 32) "expected 34\\*1"
    equal (f "+kn+k_+u_k" "D_" 1 34)
        (Right [(1, Mridangam.Stroke 'D'), (1, Mridangam.Rest)])

test_pattern = do
    let run = DeriveTest.extract extract . derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run [(2, 5, "p 5")]) (zip (Seq.range_ 2 1) ktkno, [])
    equal (run [(2, 5, "var=f567-1 | p 5")])
        ([(2, "+ki"), (3, "+ta"), (4, "+ki"), (5, "+ki"), (5.5, "+ta"),
            (6, "+thom")], [])

    -- infer
    strings_like (snd $ run [(0, 5, "p _")]) ["can't infer"]
    equal (run [(0, 5, "dur=1 | p _")]) (zip (Seq.range_ 0 1) ktkno, [])
    equal (run [(0, 6, "dur=1 | p _")]) (zip [0, 1, 3, 4, 5] ktkno, [])

ktkno :: [String]
ktkno = ["+ki", "+ta", "+ki", "+nam", "+thom"]

derive_tracks :: Text -> [UiTest.EventSpec] -> Derive.Result
derive_tracks title notes = DeriveTest.derive_tracks_setup with_synth
    ("import india.mridangam" <> title) [(">m", notes)]

with_synth :: DeriveTest.Setup
with_synth =
    DeriveTest.with_synths_simple [("m", "kontakt/mridangam")] [Kontakt.synth]

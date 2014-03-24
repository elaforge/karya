-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.KontaktKendang_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.KontaktKendang as KontaktKendang


test_kendang = do
    let run extract inst notes = DeriveTest.extract extract $ derive
            [(">" <> inst <> inst_title, mknotes notes)]
        e_inst e = (DeriveTest.e_inst e, DeriveTest.e_attributes e)
        mknotes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
        k = "kontakt/kendang"
    equal (run e_inst "kontakt/kendang" ["PL", "P", "o"])
        ([(k, "+plak"), (k, "+pak"), (k, "+tut")], [])
    equal (run e_inst "kontakt/kendang-pasang"
            ["PL", "k", "P", "t", "T", "u", "U"])
        ([("w", "+plak"), ("w", "+pak"), ("l", "+pak"),
            ("w", "+pang"), ("l", "+pang"), ("w", "+tut"), ("l", "+tut")], [])

    -- Soft attributes.
    let e_dyn e = (DeriveTest.e_attributes e, Score.initial_dynamic e)
    equal (run e_dyn "kontakt/kendang" [".", "..", "-", "+"])
        ([("+ka+soft", 0.3), ("+ka", 1), ("+de+soft", 0.3), ("+de", 1)], [])

test_pasang_calls = do
    -- every pasang call should be a tunggal call
    let extract (_, _, _, call) =
            [sym | (_, sym, _) <- KontaktKendang.tunggal_calls, sym == call]
    equal (map extract KontaktKendang.pasang_calls)
        [[call] | (_, _, _, call) <- KontaktKendang.pasang_calls]

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_with
    (DeriveTest.with_inst_db_aliases aliases Kontakt.synth_descs) ""
    where aliases = [("w", "kontakt/kendang"), ("l", "kontakt/kendang")]

inst_title :: String
inst_title = " | wadon = >w | lanang = >l"

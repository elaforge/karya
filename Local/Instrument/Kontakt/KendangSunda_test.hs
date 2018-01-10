-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.Kontakt.KendangSunda_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Local.Instrument.Kontakt as Kontakt
import qualified Local.Instrument.Kontakt.KendangSunda as KendangSunda


test_kendang = do
    let run control notes = DeriveTest.extract extract $ derive $
            control ++ [(">k", mknotes notes)]
        extract e =
            ( DeriveTest.e_attributes e
            , DeriveTest.e_control_vals KendangSunda.pitch_control e
            )
        mknotes ns = [(t, 0, n) | (t, n) <- zip (Seq.range_ 0 1) ns]
    equal (run [] ["o", "e_", "e-", "e^"])
        ([("+dong", [0]), ("+det+low", [0.25]),
            ("+det+middle", [0.5]), ("+det+high", [0.75])], [])
    equal (run [("pitch", [(0, 0, ".45")])] ["o", "e"])
        ([("+dong", [0]), ("+det", [0.45])], [])

test_resolve = do
    equal KendangSunda.resolve_errors []

derive :: [UiTest.TrackSpec] -> Derive.Result
derive = DeriveTest.derive_tracks_setup with_synths ""
    where
    with_synths = DeriveTest.with_synths_simple
        [("k", "kontakt/kendang-sunda")] [Kontakt.synth]

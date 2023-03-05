-- Copyright 2019 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Swam_test where
import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Ui.UiTest as UiTest
import qualified User.Elaforge.Instrument.Swam as Swam

import           Util.Test


test_bow :: Test
test_bow = do
    let run title = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup setup
                ("inst=i | dyn=.75" <> title)
            . UiTest.note_track1
        extract e = (Score.event_start e,
            DeriveTest.e_initial_control Controls.dynamic e)
    equal (run "" ["4c"]) ([(0, Just 0.75)], [])
    equal (run "" ["bow down | -- 4c"]) ([(0, Just (-0.75))], [])
    equal (run "" ["bow up | -- 4c"]) ([(0, Just 0.75)], [])
    equal (run "| bow" ["4c", "4d"]) ([(0, Just (-0.75)), (1, Just 0.75)], [])
    equal (run "| bow" ["+ -- 4c", "4d", "4e"])
        ([(0, Just (-0.75)), (1, Just (-0.75)), (2, Just 0.75)], [])

    equal (run "| `downbow`" ["4c", "4d"])
        ([(0, Just (-0.75)), (1, Just (-0.75))], [])

setup :: DeriveTest.Setup
setup = DeriveTest.with_synths_simple allocs [Swam.synth]

allocs :: DeriveTest.SimpleAllocations
allocs = [("i", "swam/violin")]

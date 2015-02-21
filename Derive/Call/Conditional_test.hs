-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Conditional_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_multiple = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks ""
        extract e = (Score.event_start e, DeriveTest.e_inst e)
    equal (run
            [("> | multiple \"(inst = >s/1) \"(inst = >s/2)", [(0, 1, "")])])
        ([(0, "s/1"), (0, "s/2")], [])
    equal (run [("> | multiple >s/1 >s/2", [(0, 1, "")])])
        ([(0, "s/1"), (0, "s/2")], [])

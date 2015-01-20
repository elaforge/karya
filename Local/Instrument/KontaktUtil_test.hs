-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Local.Instrument.KontaktUtil_test where
import Util.Test
import qualified Local.Instrument.KontaktUtil as KontaktUtil


test_make_stop_groups = do
    let f stops = KontaktUtil.make_stop_groups stops ["g1", "g2"]
    equal (f []) (Right [-1, -1, -1, -1])
    equal (f [("g1", ["g2"])]) (Right [1, -1, -1, -1])
    equal (f [("g1", ["g2"])]) (Right [1, -1, -1, -1])
    equal (f [("g1", ["g1", "g2"]), ("g2", ["g1"])]) (Right [0, 1, 0, -1])
    equal (f [("g3", ["g1", "g2"]), ("g2", ["g1"])]) (Left "no group: \"g3\"")

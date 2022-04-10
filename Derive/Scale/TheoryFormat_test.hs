-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.TheoryFormat_test where
import Util.Test
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch


test_show_degree :: Test
test_show_degree = do
    let f fmt key = TheoryFormat.show_degree fmt (Just (Pitch.Key key))
        rel = TheoryFormat.sargam $ ChromaticScales.relative_fmt
            Twelve.default_theory_key Twelve.all_keys
    equal (f rel "f#-min" (Pitch.Degree 3 1)) "s"
    equal (f rel "f#-min" (Pitch.Degree 4 0)) "rb"
    equal (f rel "f#-min" (Pitch.Degree 5 0)) "g"

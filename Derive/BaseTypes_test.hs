-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.BaseTypes_test where
import Util.Test
import qualified Derive.Parse as Parse
import qualified Derive.ShowVal as ShowVal
import qualified Derive.BaseTypes as BaseTypes


test_map_symbol = do
    let f modify = fmap (ShowVal.show_val . BaseTypes.map_symbol modify)
            . Parse.parse_expr
    -- Mostly this is testing that show_val is a proper inverse of
    -- Parse.parse_expr.
    right_equal (f (const "1") "23 23 '23' | 42") "1 23 '1' | 1"

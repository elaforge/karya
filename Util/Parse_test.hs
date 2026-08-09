-- Copyright 2022 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.Parse_test where
import qualified Util.Parse as Parse
import qualified Util.ParseText as ParseText

import           Util.Test


test_attoparse :: Test
test_attoparse = do
    let nat = Parse.p_nat
    let anat = Parse.attoparse "anat" ParseText.p_nat
    let parse = Parse.parse
    let comma p1 p2 = parse ((,) <$> p1 <*> ("," *> p2))
    right_equal (parse nat "42") 42
    right_equal (parse anat "42") 42
    right_equal (comma nat nat "1,2") (1, 2)
    right_equal (comma anat nat "1,2") (1, 2)
    right_equal (comma nat anat "1,2") (1, 2)
    right_equal (comma anat anat "1,2") (1, 2)
    left_like (comma nat nat "a,2") "expecting nat"
    left_like (comma anat nat "a,2") "expected anat"
    left_like (comma nat anat "1,a") "expected anat"

-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Util.ExtractHs_test where
import qualified Util.ExtractHs as ExtractHs

import Util.Test


test_stripComments :: Test
test_stripComments = do
    let f = ExtractHs.stripComments
    equal (f "abc") "abc"
    -- Newlines inside the comments remain, otherwise line numbers get off.
    equal (f "a\nq {-\n bc\n-} d") "a\nq \n\n d"
    equal (f "a {-b-} c {-d-} e") "a  c  e"

test_typeDeclarations :: Test
test_typeDeclarations = do
    let f = ExtractHs.typeDeclarations
    equal (f "x :: Y") [(1, ("x", "Y"))]
    equal (f "-- x :: Y") []
    equal (f "x, y :: Z") [(1, ("x", "Z")), (1, ("y", "Z"))]

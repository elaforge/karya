-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.DeriveQuickCheck_test where
import Util.Test
import qualified Derive.DeriveQuickCheck as DeriveQuickCheck


-- TODO placeholder for now, just to force DeriveQuickCheck to compile.
test_derive :: Test
test_derive = do
    equal (length (DeriveQuickCheck.simple_derive [])) 0

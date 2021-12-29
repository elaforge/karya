-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler_test where
import qualified Ui.Ruler as Ruler

import           Util.Test


test_get_set_bounds :: Test
test_get_set_bounds = do
    let f s e = Ruler.get_bounds $ Ruler.set_bounds s e Ruler.empty_ruler
    equal (f Nothing Nothing) (Nothing, Nothing)
    equal (f (Just 1) Nothing) (Just 1, Nothing)
    equal (f Nothing (Just 1)) (Nothing, Just 1)
    equal (f (Just 1) (Just 2)) (Just 1, Just 2)
    -- Start is always <= end.
    equal (f (Just 2) (Just 1)) (Just 1, Just 2)

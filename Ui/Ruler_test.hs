-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ruler_test where
import qualified Ui.Ruler as Ruler
import qualified Ui.UiTest as UiTest

import           Util.Test


test_get_set_bounds :: Test
test_get_set_bounds = do
    let f s e = Ruler.getBounds $ Ruler.setBounds s e Ruler.empty
    equal (f Nothing Nothing) (Nothing, Nothing)
    equal (f (Just 1) Nothing) (Just 1, Nothing)
    equal (f Nothing (Just 1)) (Nothing, Just 1)
    equal (f (Just 1) (Just 2)) (Just 1, Just 2)
    -- Start is always <= end.
    equal (f (Just 2) (Just 1)) (Just 1, Just 2)

test_bounds_of :: Test
test_bounds_of = do
    let f = Ruler.boundsOf
    equal (f $ UiTest.mkruler44 8 0.5) (0, Just 4)
    -- Old scores load with marklist but no Meter, make sure bounds still work!
    let mlist = Ruler.getMarklist Ruler.meterName $ UiTest.mkruler44 8 0.5
    equal (f $ Ruler.setMarklist Ruler.meterName mlist Ruler.empty)
        (0, Just 4)

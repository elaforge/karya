-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Z1_test where
import qualified Data.ByteString as B

import Util.Test
import qualified User.Elaforge.Instrument.Z1 as Z1


test_enkorg :: Test
test_enkorg = do
    let f bs = (Z1.dekorg (Z1.enkorg (B.pack bs)), B.pack bs)
    uncurry equal (f (0x7f : replicate 7 0))
    uncurry equal (f (0 : replicate 7 1))
    uncurry equal (f (1 : replicate 7 0))

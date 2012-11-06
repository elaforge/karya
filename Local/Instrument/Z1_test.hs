module Local.Instrument.Z1_test where
import qualified Data.ByteString as B

import Util.Test
import qualified Local.Instrument.Z1 as Z1


test_enkorg = do
    let f bs = (Z1.dekorg (Z1.enkorg (B.pack bs)), B.pack bs)
    uncurry equal (f (0x7f : replicate 7 0))
    uncurry equal (f (0 : replicate 7 1))
    uncurry equal (f (1 : replicate 7 0))

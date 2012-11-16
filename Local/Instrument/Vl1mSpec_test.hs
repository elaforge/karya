module Local.Instrument.Vl1mSpec_test where
import qualified Data.ByteString as ByteString

import Util.Test
import qualified Local.Instrument.Vl1mSpec as Vl1mSpec


-- 0-127 => 00 - 7f
-- 0-127, 128-16383      => 0000 - 007f, 0100 - 7f7f
-- -64 - -1, 0, 1 - 63   => 40 - 7f, 00, 01 - 3f (2s complement)
-- -128 - -1, 0, 1 - 127 => 0100 - 017f, 0000, 0001 - 007f

test_decode_num = do
    let f range = Vl1mSpec.decode_num range . ByteString.pack
    equal (map (f (0, 127)) [[0], [0x7f]]) [0, 0x7f]
    equal (map (f (0, 16383)) [[0, 0], [0, 0x7f], [1, 0], [0x7f, 0x7f]])
        [0, 127, 128, 16383]
    equal (map (f (-64, 63)) [[0x40], [0x7f], [0], [1], [0x3f]])
        [-64, -1, 0, 1, 63]
    equal (map (f (-127, 127)) [[1, 0], [1, 0x7f], [0], [1], [0x7f]])
        [-128, -1, 0, 1, 127]

test_encode_num = do
    let f range = ByteString.unpack . Vl1mSpec.encode_num range
    equal (map (f (0, 127)) [0, 0x7f]) [[0], [0x7f]]
    equal (map (f (0, 16383)) [0, 127, 128, 16383])
        [[0, 0], [0, 0x7f], [1, 0], [0x7f, 0x7f]]
    equal (map (f (-64, 63)) [-64, -1, 0, 1, 63])
        [[0x40], [0x7f], [0], [1], [0x3f]]
    equal (map (f (-128, 127)) [-128, -1, 0, 1, 127])
        [[1, 0], [1, 0x7f], [0, 0], [0, 1], [0, 0x7f]]

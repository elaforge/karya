-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module User.Elaforge.Instrument.Vl1Spec_test where
import qualified Data.ByteString as ByteString

import Util.Test
import qualified User.Elaforge.Instrument.Vl1Spec as Vl1Spec


-- 0-127 => 00 - 7f
-- 0-127, 128-16383      => 0000 - 007f, 0100 - 7f7f
-- -64 - -1, 0, 1 - 63   => 40 - 7f, 00, 01 - 3f (2s complement)
-- -128 - -1, 0, 1 - 127 => 0100 - 017f, 0000, 0001 - 007f

test_decode_num = do
    let f range = Vl1Spec.decode_num range . ByteString.pack
    equal (map (f (0, 127)) [[0], [0x7f]]) [0, 0x7f]
    equal (map (f (0, 16383)) [[0, 0], [0, 0x7f], [1, 0], [0x7f, 0x7f]])
        [0, 127, 128, 16383]
    equal (map (f (-64, 63)) [[0x40], [0x7f], [0], [1], [0x3f]])
        [-64, -1, 0, 1, 63]
    equal (map (f (-127, 127)) [[1, 0], [1, 0x7f], [0], [1], [0x7f]])
        [-128, -1, 0, 1, 127]

test_encode_num = do
    let f range = ByteString.unpack . Vl1Spec.encode_num range
    equal (map (f (0, 127)) [0, 0x7f]) [[0], [0x7f]]
    equal (map (f (0, 16383)) [0, 127, 128, 16383])
        [[0, 0], [0, 0x7f], [1, 0], [0x7f, 0x7f]]
    equal (map (f (-64, 63)) [-64, -1, 0, 1, 63])
        [[0x40], [0x7f], [0], [1], [0x3f]]
    equal (map (f (-128, 127)) [-128, -1, 0, 1, 127])
        [[1, 0], [1, 0x7f], [0, 0], [0, 1], [0, 0x7f]]

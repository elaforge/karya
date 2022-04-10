-- Copyright 2017 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Encode_test where
import Util.Test
import Data.ByteString (pack)
import Midi.Encode (encode, decode)
import qualified Midi.Midi as Midi


test_encode :: Test
test_encode = do
    let roundtrip = decode . encode
        check msg = (roundtrip msg, msg)
    let chan = Midi.ChannelMessage 0
    equal (roundtrip (chan (Midi.PitchBendInt 127 127)))
        (chan (Midi.PitchBend 1))
    uncurry equal $ check $ chan $ Midi.PitchBend 0
    uncurry equal $ check $ chan $ Midi.PitchBend 1
    -- Unknown msgs still round-trip.
    let unknown = pack [0xb0, 0x7d, 0x42]
    equal (decode unknown) (chan (Midi.UndefinedChannelMode 0x7d 0x42))
    equal (encode (decode unknown)) unknown
    -- TODO QuickCheck

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Midi.Midi_test where
import qualified Data.ByteString as ByteString
import Data.Word (Word8)

import Util.Test
import qualified Midi.Encode as Encode
import qualified Midi.Midi as Midi
import Global


test_generate_mtc :: Test
test_generate_mtc = do
    let f rate = Midi.generate_mtc rate . Midi.seconds_to_frame rate
        one_sec fps = floor $ fps * 4
    let msgs = take (one_sec 30 + 1) (f Midi.Frame30 2)
    check ("sorted: " <> pretty msgs) $ is_sorted (map fst msgs)
    equal (fst (last msgs)) 3
    -- pprint msgs

is_sorted :: (Ord a) => [a] -> Bool
is_sorted xs = all (uncurry (<=)) (zip xs (drop 1 xs))

test_seconds_to_smpte :: Test
test_seconds_to_smpte = do
    let f rate = e_smpte . Midi.seconds_to_smpte rate
        rate = Midi.Frame29_97df
    equal (f rate 0) (0, 0, 0, 0)
    equal (f rate 1) (0, 0, 0, 29)
    -- 29.97 fps so it's running behind 30fps
    equal (f rate 60) (0, 0, 59, 28)
    -- 2 frames were dropped, so it's now ahead
    equal (f rate 60.08) (0, 1, 0, 2)
    -- At 10m there's no drop.
    equal (f rate 600) (0, 10, 0, 0)

    -- Convert back to seconds to make sure it stays more or less accurate.
    let trip = to_sec . f rate
        to_sec (h, m, s, f) =
            (fi h * 60*60*30 + fi m * 60*30 + fi s * 30 + fi f) / 30
        fi = fromIntegral
        hr = 60 * 60
    equal (trip 600) 600
    equalf 0.1 (trip 601) 601
    equal (trip (12*hr)) (12*hr)

test_frame_to_smpte :: Test
test_frame_to_smpte = do
    let f rate = e_smpte . Midi.frame_to_smpte rate
    equal (f Midi.Frame29_97df 107892) (1, 0, 0, 0)
    equal (f Midi.Frame30 (30 * 60 * 60)) (1, 0, 0, 0)

e_smpte :: Midi.Smpte -> (Word8, Word8, Word8, Word8)
e_smpte (Midi.Smpte h m s f) = (h, m, s, f)

test_realtime_tuning :: Test
test_realtime_tuning = do
    let f = ByteString.unpack . Encode.encode . Midi.realtime_tuning
    equal (f [(1, 1.5), (2, 2)])
        [ Encode.sox_byte, 0x7f, 0x7f, 8, 2, 0
        , 0x02 -- 2 keys
        , 0x01, 0x01, 0x40, 0x00
        , 0x02, 0x02, 0x00, 0x00
        , Encode.eox_byte
        ]

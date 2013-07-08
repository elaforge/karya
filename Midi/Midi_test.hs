module Midi.Midi_test where
import Util.Test
import qualified Midi.Midi as Midi


test_generate_mtc = do
    let f rate = Midi.generate_mtc rate . Midi.seconds_to_frame rate
        one_sec fps = floor $ fps * 4
    let msgs = take (one_sec 30 + 1) (f f30 2)
    check (is_sorted $ map fst msgs)
    equal (fst (last msgs)) 3
    pprint msgs

is_sorted :: (Ord a) => [a] -> Bool
is_sorted xs = all (uncurry (<=)) (zip xs (drop 1 xs))

test_seconds_to_smpte = do
    let f rate = e_smpte . Midi.seconds_to_smpte rate
    equal (f df 0) (0, 0, 0, 0)
    equal (f df 1) (0, 0, 0, 29)
    -- 29.97 fps so it's running behind 30fps
    equal (f df 60) (0, 0, 59, 28)
    -- 2 frames were dropped, so it's now ahead
    equal (f df 60.08) (0, 1, 0, 2)
    -- At 10m there's no drop.
    equal (f df 600) (0, 10, 0, 0)

    -- Convert back to seconds to make sure it stays more or less accurate.
    let trip = to_sec . f df
        to_sec (h, m, s, f) =
            (fi h * 60*60*30 + fi m * 60*30 + fi s * 30 + fi f) / 30
        fi = fromIntegral
        hr = 60 * 60
    equal (trip 600) 600
    equalf 0.1 (trip 601) 601
    equal (trip (12*hr)) (12*hr)

test_frame_to_smpte = do
    let f rate = e_smpte . Midi.frame_to_smpte rate
    equal (f df 107892) (1, 0, 0, 0)
    equal (f Midi.Frame30 (30 * 60 * 60)) (1, 0, 0, 0)

df = Midi.Frame29_97df
f30 = Midi.Frame30
e_smpte (Midi.Smpte h m s f) = (h, m, s, f)

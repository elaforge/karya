module Derive.Scale.Twelve_test where
import qualified Data.Map as Map

import Util.Test

import qualified Perform.Pitch as Pitch
import qualified Derive.Scale.Twelve as Twelve
import Derive.Scale.Theory (PitchClass(..))


test_note_to_nn = do
    let f = fmap snd . flip Map.lookup Twelve.note_to_degree . Pitch.Note
    equal (f "4c") (Just 60)
    equal (f "-1c") Nothing
    equal (f "-1c#") (Just 1)
    equal (f "-2b") Nothing
    equal (f "0c") (Just 12)
    equal (f "9g") (Just 127)
    equal (f "9g#") Nothing
    equal (f "10g") Nothing

test_transpose_diatonic = do
    let intervals n offset start_on = map fromIntegral $
            take n $ map (subtract offset) $ drop start_on $
                scanl (+) 0 $ cycle Twelve.standard_intervals
    let Just cmaj = Twelve.parse_key (Pitch.Key "c-maj")
    let f = Twelve.transpose_diatonic

    equal (map (f cmaj C) [0..7]) (intervals 8 0 0)
    equal (map (f cmaj D) [0..7]) (intervals 8 2 1)
    equal (map (f cmaj E) [0..7]) (intervals 8 4 2)
    equal (map (f cmaj F) [0..7]) (intervals 8 5 3)
    equal (map (f cmaj G) [0..7]) (intervals 8 7 4)
    equal (map (f cmaj A) [0..7]) (intervals 8 9 5)
    equal (map (f cmaj B) [0..7]) (intervals 8 11 6)

    equal (f cmaj C 1.5) 3
    let Just cmin = Twelve.parse_key (Pitch.Key "c-min")
    equal (map (f cmin C) [0..3]) [0, 2, 3, 5]
    let Just dmin = Twelve.parse_key (Pitch.Key "d-min")
    equal (map (f dmin D) [0..3]) [0, 2, 3, 5]

    equal (map (f cmaj C) [0, -1, -2]) [0, -1, -3]
    equal (f cmaj C (-0.5)) (-0.5)
    equal (f cmaj C (-1.5)) (-2)

test_key_transpose = do
    let Just cmaj = Twelve.parse_key (Pitch.Key "c-maj")
    let f = Twelve.key_transpose cmaj

    equal (map (f C) [0..55])
        (take 56 (scanl (+) 0 (cycle Twelve.standard_intervals)))
    equal (map (f C) [0, -1 .. -55])
        (take 56 (scanl (-) 0 (cycle (reverse Twelve.standard_intervals))))
    equal (map (f D) [0, -1, -2]) [0, -2, -3]
    equal (map (f A) [0, -1, -2]) [0, -2, -4]

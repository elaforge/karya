module Derive.Scale.Just_test where
import qualified Data.Vector.Unboxed as Vector

import Util.Control
import Util.Test
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch


test_input_to_note = do
    let f = Just.input_to_note Nothing
        n = Just . Pitch.Note
    equal (map f (map Pitch.InputKey [0..5])) $
        map n ["-1c", "-1c 50", "-1d", "-1d 50", "-1e", "-1f"]
    equal (f Pitch.middle_c) (n "4c")
    equal (f (Pitch.middle_c + Pitch.InputKey 0.5)) (n "4c 25")

test_transpose = do
    let f = Just.transpose Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "4b", "5c"]
    equal [f n (Pitch.Chromatic 0) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "5a", "6a"]

test_degree_to_hz = do
    let f base_hz tonic pitch = Just.degree_to_hz Just.pc_per_octave
            Just.ratios_major base_hz tonic (p pitch)

    -- tonic A, nice minor triad is in tune
    equal (f Nothing 0 "4a") 440
    equal (f Nothing 0 "5c") 550
    equal (f Nothing 0 "5e") 660

    -- just major scale from A to A
    let hzs = map (f Nothing 0) ["3a", "3b", "4c", "4d", "4e", "4f", "4g", "4a"]
    equal (ratios hzs) (take (length hzs) (make_ratios Just.ratios_major))

    -- Base should be 3b, ratios should be major.
    let hzs = map (f Nothing 1) ["3b", "4c", "4d", "4e", "4f", "4g", "4a", "4b"]
    equal (ratios hzs) (take (length hzs) (make_ratios Just.ratios_major))

    -- Key is B and B is tuned to 10hz.
    let hzs = map (f (Just 10) 1) ["-1a", "-1b", "0c"]
    equal hzs [9.375, 10, 11.25]


-- * implementation

make_ratios :: Just.Ratios -> [Pitch.Hz]
make_ratios ratios = concat [map (+ oct) (Vector.toList ratios) | oct <- [0..]]

ratios :: [Pitch.Hz] -> [Pitch.Hz]
ratios [] = []
ratios (x:xs) = map (/x) (x:xs)

p :: String -> Theory.Pitch
p s = fromMaybe (error $ "can't parse pitch: " ++ show s) $
    Theory.read_pitch Twelve.layout s

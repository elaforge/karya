module Derive.Scale.Just_test where
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector

import Util.Control
import Util.Test
import qualified Ui.State as State
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Theory as Theory

import qualified Perform.Pitch as Pitch


test_input_to_note = do
    let f = Just.input_to_note Nothing
        n = Just . Pitch.Note
    equal (map f (map Pitch.InputKey [0..5])) $
        map n ["-1c", "-1c 50", "-1d", "-1d 50", "-1e", "-1f"]
    equal (f Pitch.middle_c) (n "4c")
    equal (f (Pitch.middle_c + Pitch.InputKey 0.5)) (n "4c 25")

test_input_to_nn = do
    let f = DeriveTest.with_key "c" . Scale.scale_input_to_nn Just.just_major 0
    equalf 0.01 (DeriveTest.eval State.empty $ f Pitch.middle_c) $
        Right (Just (Pitch.nn Pitch.middle_c))
    equalf 0.01 (DeriveTest.eval State.empty $ f (Pitch.middle_c + 2)) $
        Right $ Just $
            Pitch.hz_to_nn (Pitch.nn_to_hz (Pitch.nn Pitch.middle_c) * 9/8)

test_transpose = do
    let f = Just.transpose Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "4b", "5c"]
    equal [f n (Pitch.Chromatic 0) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "5a", "6a"]

test_degree_to_hz = do
    let f base_hz tonic pitch = Just.degree_to_hz Just.pc_per_octave
            Just.major_ratios base_hz tonic (p pitch)
        Just key_a = Map.lookup 'a' Just.all_keys
        Just key_b = Map.lookup 'b' Just.all_keys

    -- tonic A, nice minor triad is in tune
    equal (f Nothing key_a "4a") 440
    equal (f Nothing key_a "5c") 550
    equal (f Nothing key_a "5e") 660

    -- just major scale from A to A
    let hzs = map (f Nothing key_a)
            ["3a", "3b", "4c", "4d", "4e", "4f", "4g", "4a"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios Just.major_ratios))

    -- Base should be 3b, ratios should be major.
    let hzs = map (f Nothing key_b)
            ["3b", "4c", "4d", "4e", "4f", "4g", "4a", "4b"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios Just.major_ratios))

    -- Key is B and B is tuned to 10hz.
    let hzs = map (f (Just 10) key_b) ["-2a", "-2b", "-1c"]
    equalf 0.01 hzs [9.375, 10, 11.25]


-- * implementation

make_ratios :: Just.Ratios -> [Pitch.Hz]
make_ratios ratios = concat [map (+ oct) (Vector.toList ratios) | oct <- [0..]]

ratios :: [Pitch.Hz] -> [Pitch.Hz]
ratios [] = []
ratios (x:xs) = map (/x) (x:xs)

p :: String -> Theory.Pitch
p s = fromMaybe (error $ "can't parse pitch: " ++ show s) $
    Theory.parse_pitch s

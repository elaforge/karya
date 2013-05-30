module Derive.Scale.Just_test where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Ratio as Ratio
import qualified Data.Vector as Vector

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run ps = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("*just-maj | key = a", [(t, 0, p) | (t, p) <- times ps])
            , (">", [(t, 1, "") | (t, _) <- times ps])
            ]
            where times = zip (Seq.range_ 0 1)
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    equalf 0.001 (run ["4a"]) ([Just 440], [])
    equalf 0.001 (run ["4a 3/2"]) ([Just 660], [])
    equalf 0.001 (run ["4a P5"]) ([Just 660], [])
    equalf 0.001 (run ["5e"]) ([Just 660], [])
    equalf 0.001 (run ["5e -3/2"]) ([Just 440], [])
    equalf 0.001 (run ["4a 3/2 -3/2"]) ([Just 440], [])

    -- relative pitch
    equalf 0.001 (run ["4a", "3/2"]) ([Just 440, Just 660], [])
    equalf 0.001 (run ["4a", "3/2 3/2"]) ([Just 440, Just 990], [])
    equalf 0.001 (run ["5e", "-3/2"]) ([Just 660, Just 440], [])
    equalf 0.001 (run ["5e", "-P5"]) ([Just 660, Just 440], [])
    equalf 0.001 (run ["4a", "3/2", "3/2"]) ([Just 440, Just 660, Just 990], [])
    equalf 0.001 (run ["4a", "P5", "-P5"]) ([Just 440, Just 660, Just 440], [])

    let acc = Just.accidental_interval
    equalf 0.001 (run ["4a", "4a#", "4ab"])
        ([Just 440, Just $ 440 * acc, Just $ 440 / acc], [])

test_input_to_note = do
    let f = Just.input_to_note show_absolute Nothing
        n = Just . Pitch.Note
        (show_absolute, _, _) = Just.absolute_format
    equal (map f (map Pitch.InputKey [0..5])) $
        map n ["-1c", "-1c#", "-1d", "-1d#", "-1e", "-1f"]
    equal (f Pitch.middle_c) (n "4c")

    let f2 = Just.input_to_note show_relative Nothing
        (show_relative, _, _) = Just.relative_format
    equal (map f2 [0, 1, 2, 11, 12, 14]) $
        map n ["-1s", "-1s#", "-1r", "-1n", "0s", "0r"]
    equal (f2 Pitch.middle_c) (Just (Pitch.Note "4s"))

test_input_to_nn = do
    let f = DeriveTest.with_key "c" . Scale.scale_input_to_nn just_major 0
    equalf 0.01 (DeriveTest.eval State.empty $ f Pitch.middle_c) $
        Right (Just (Pitch.nn Pitch.middle_c))
    equalf 0.01 (DeriveTest.eval State.empty $ f (Pitch.middle_c + 2)) $
        Right $ Just $
            Pitch.hz_to_nn (Pitch.nn_to_hz (Pitch.nn Pitch.middle_c) * 9/8)

test_transpose = do
    let f = Just.transpose Just.absolute_format Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "4b", "5c"]
    equal [f n (Pitch.Chromatic 0) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "5a", "6a"]

test_degree_to_hz = do
    let f base_hz tonic pitch = Just.degree_to_hz Just.pc_per_octave
            major base_hz tonic (p pitch)
        major = convert Just.major_ratios
        Just key_a = Map.lookup "a" Just.all_keys
        Just key_b = Map.lookup "b" Just.all_keys

    -- tonic A, nice minor triad is in tune
    equal (f Nothing key_a "4a") 440
    equal (f Nothing key_a "5c") 550
    equal (f Nothing key_a "5e") 660

    -- just major scale from A to A
    let hzs = map (f Nothing key_a)
            ["3a", "3b", "4c", "4d", "4e", "4f", "4g", "4a"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios major))

    -- Base should be 3b, ratios should be major.
    let hzs = map (f Nothing key_b)
            ["3b", "4c", "4d", "4e", "4f", "4g", "4a", "4b"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios major))

    -- Key is B and B is tuned to 10hz.
    let hzs = map (f (Just 10) key_b) ["-2a", "-2b", "-1c"]
    equalf 0.01 hzs [9.375, 10, 11.25]

convert :: Vector.Vector Ratio.Rational -> Just.Ratios
convert = Vector.map realToFrac


-- * implementation

just_major :: Scale.Scale
Just just_major =
    List.find ((== Pitch.ScaleId "just-maj") . Scale.scale_id) Just.scales

make_ratios :: Just.Ratios -> [Pitch.Hz]
make_ratios ratios = concat [map (+ oct) (Vector.toList ratios) | oct <- [0..]]

ratios :: [Pitch.Hz] -> [Pitch.Hz]
ratios [] = []
ratios (x:xs) = map (/x) (x:xs)

p :: Text -> Theory.Pitch
p s = fromMaybe (error $ "can't parse pitch: " ++ show s) $
    TheoryFormat.parse_pitch (Pitch.Note s)

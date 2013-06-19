-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Just_test where
import qualified Data.List as List
import qualified Data.Map as Map
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

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run key ps = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("*just | key = " <> key, [(t, 0, p) | (t, p) <- times ps])
            , (">", [(t, 1, "") | (t, _) <- times ps])
            ]
            where times = zip (Seq.range_ 0 1)
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    -- Ensure that the octave wraps at C.
    equalf 0.001 (run "c-maj" ["4c"]) ([Just c_hz], [])
    equalf 0.001 (run "a-maj" ["4a"]) ([Just 440], [])
    equalf 0.001 (run "b-maj" ["4b"])
        ([Just $ Pitch.nn_to_hz (NN.middle_c + 11)], [])
    equalf 0.001 (run "c-min" ["4e"]) ([Just (c_hz * 6/5)], [])

    let runa = run "a-maj"
    equalf 0.001 (runa ["4a 3/2"]) ([Just 660], [])
    equalf 0.001 (runa ["4a P5"]) ([Just 660], [])
    equalf 0.001 (runa ["5e"]) ([Just 660], [])
    equalf 0.001 (runa ["5e -3/2"]) ([Just 440], [])
    equalf 0.001 (runa ["4a 3/2 -3/2"]) ([Just 440], [])

    -- relative intervals
    equalf 0.001 (runa ["4a", "3/2"]) ([Just 440, Just 660], [])
    equalf 0.001 (runa ["4a", "3/2 3/2"]) ([Just 440, Just 990], [])
    equalf 0.001 (runa ["5e", "-3/2"]) ([Just 660, Just 440], [])
    equalf 0.001 (runa ["5e", "-P5"]) ([Just 660, Just 440], [])
    equalf 0.001 (runa ["4a", "3/2", "3/2"])
        ([Just 440, Just 660, Just 990], [])
    equalf 0.001 (runa ["4a", "P5", "-P5"]) ([Just 440, Just 660, Just 440], [])

    let acc = Just.accidental_interval
    equalf 0.001 (runa ["4a", "4a#", "4ab"])
        ([Just 440, Just $ 440 * acc, Just $ 440 / acc], [])

test_note_to_call_relative = do
    let run scale key p = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("*" <> scale <> " | key = " <> key, [(0, 0, p)])
            , (">", [(0, 1, "")])
            ]
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    equalf 0.001 (run "just-r" "c-maj" "4s") ([Just c_hz], [])
    equalf 0.001 (run "just-r" "a-maj" "4s") ([Just 440], [])
    equalf 0.001 (run "just-r" "c-maj" "3s") ([Just (c_hz / 2)], [])

c_hz :: Pitch.Hz
c_hz = Pitch.nn_to_hz NN.middle_c

show_abs = TheoryFormat.show_pitch Just.absolute_format
show_rel = TheoryFormat.show_pitch Just.relative_format

test_input_to_note = do
    let f = Just.input_to_note show_abs Nothing
        n = Just . Pitch.Note

    equal (map f (map Pitch.InputKey [0..5])) $
        map n ["-1c", "-1c#", "-1d", "-1d#", "-1e", "-1f"]
    equal (f Pitch.middle_c) (n "4c")

    let f2 = Just.input_to_note show_rel Nothing
    equal (map f2 [0, 1, 2, 11, 12, 14]) $
        map n ["-1s", "-1s#", "-1r", "-1n", "0s", "0r"]
    equal (f2 Pitch.middle_c) (Just (Pitch.Note "4s"))

test_input_to_note_relative = do
    let f key = Just.input_to_note show_rel (Just (Pitch.Key key))
        n = Just . Pitch.Note
    equal (f "c" Pitch.middle_c) (n "4s")
    equal (f "c" (Pitch.middle_c - 1)) (n "3n")
    equal (f "d" Pitch.middle_c) (n "4s")

test_input_to_nn = do
    let f = DeriveTest.with_key "c-maj" . Scale.scale_input_to_nn just_scale 0
    equalf 0.01 (DeriveTest.eval State.empty $ f Pitch.middle_c) $
        Right (Just (Pitch.nn Pitch.middle_c))
    equalf 0.01 (DeriveTest.eval State.empty $ f (Pitch.middle_c + 2)) $
        Right $ Just $ Pitch.modify_hz (* (9/8)) (Pitch.nn Pitch.middle_c)

test_input_to_nn_relative = do
    let to_nn key = DeriveTest.with_key key
            . Scale.scale_input_to_nn just_scale_rel 0
        run key input = DeriveTest.eval State.empty (to_nn key input)
    equalf 0.01 (run "c-maj" Pitch.middle_c) $ Right (Just 60)
    equalf 0.01 (run "a-maj" Pitch.middle_c) $ Right (Just 69)

test_transpose = do
    let f = Just.transpose Just.absolute_format Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "4b", "5c"]
    equal [f n (Pitch.Chromatic 0) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "5a", "6a"]

test_transpose_relative = do
    let f = Just.transpose Just.relative_format Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4s") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4s", "4r", "4g"]

test_degree_to_hz = do
    let f base_hz tonic pitch = Just.degree_to_hz Just.pc_per_octave
            major base_hz tonic (read_note pitch)
        Just major = lookup "maj" Just.all_key_ratios
        Just key_a = Map.lookup (Pitch.Key "a-maj") Just.all_keys
        Just key_b = Map.lookup (Pitch.Key "b-maj") Just.all_keys

    -- tonic A, nice minor triad is in tune
    equalf 0.01 (f Nothing key_a "4a") 440
    equalf 0.01 (f Nothing key_a "5c") 550
    equalf 0.01 (f Nothing key_a "5e") 660

    -- just major scale from A to A
    let hzs = map (f Nothing key_a)
            ["3a", "3b", "4c", "4d", "4e", "4f", "4g", "4a"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios major))

    -- Base should be 3b, ratios should be major.
    let hzs = map (f Nothing key_b)
            ["3b", "4c", "4d", "4e", "4f", "4g", "4a", "4b"]
    equalf 0.01 (ratios hzs) (take (length hzs) (make_ratios major))

    -- Key is B and B is tuned to 10hz.
    let hzs = map (f (Just 10) key_b) ["-1a", "-1b", "0c"]
    equalf 0.01 hzs [9.375, 10, 11.25]


-- * implementation

just_scale :: Scale.Scale
Just just_scale =
    List.find ((== Pitch.ScaleId "just") . Scale.scale_id) Just.scales

just_scale_rel :: Scale.Scale
Just just_scale_rel =
    List.find ((== Pitch.ScaleId "just-r") . Scale.scale_id) Just.scales

make_ratios :: Just.Ratios -> [Pitch.Hz]
make_ratios ratios = concat [map (+ oct) (Vector.toList ratios) | oct <- [0..]]

ratios :: [Pitch.Hz] -> [Pitch.Hz]
ratios [] = []
ratios (x:xs) = map (/x) (x:xs)

read_note :: Text -> Theory.Pitch
read_note s = either (const $ error $ "can't parse pitch: " ++ show s) id $
    Just.read_note Just.absolute_format Nothing (Pitch.Note s)

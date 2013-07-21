-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JustScales_test where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run key base ps = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("> | %just-base = " <> base, [(t, 1, "") | (t, _) <- times ps])
            , ("*just | key = " <> key,
                [(t, 0, p) | (t, p) <- times ps])
            ]
            where times = zip (Seq.range_ 0 1)
        extract = fmap Pitch.nn_to_hz . Score.initial_nn

    -- Scale starts at 4c by default, and tunes to %just-base.
    equalf 0.001 (run "c-maj" "440" ["4c"]) ([Just 440], [])
    equalf 0.001 (run "a-maj" "440" ["4a"]) ([Just 440], [])

    -- Intervals work.
    let runa = run "c-maj" "440"
    equalf 0.001 (runa ["4c 3/2"]) ([Just 660], [])
    equalf 0.001 (runa ["4c P5"]) ([Just 660], [])
    equalf 0.001 (runa ["4g"]) ([Just 660], [])
    equalf 0.001 (runa ["4g -3/2"]) ([Just 440], [])
    equalf 0.001 (runa ["4c 3/2 -3/2"]) ([Just 440], [])

    -- relative intervals
    equalf 0.001 (runa ["4c", "3/2"]) ([Just 440, Just 660], [])
    equalf 0.001 (runa ["4c", "3/2 3/2"]) ([Just 440, Just 990], [])
    equalf 0.001 (runa ["4g", "-3/2"]) ([Just 660, Just 440], [])
    equalf 0.001 (runa ["4g", "-P5"]) ([Just 660, Just 440], [])
    equalf 0.001 (runa ["4c", "3/2", "3/2"])
        ([Just 440, Just 660, Just 990], [])
    equalf 0.001 (runa ["4c", "P5", "-P5"]) ([Just 440, Just 660, Just 440], [])

    let acc = JustScales.smap_accidental_interval
            (Just.scale_map TheoryFormat.absolute_c)
    equalf 0.001 (runa ["4c", "4c#", "4cb"])
        ([Just 440, Just $ 440 * acc, Just $ 440 / acc], [])

test_note_to_call_relative = do
    let run key p = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ ("*just-r | key = " <> key, [(0, 0, p)])
            , ("> | %just-base = 440", [(0, 1, "")])
            ]
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    equalf 0.001 (run "a-maj" "4s") ([Just 440], [])
    equalf 0.001 (run "a-maj" "4p") ([Just (440 * 3/2)], [])

show_abs = TheoryFormat.show_pitch TheoryFormat.absolute_c
show_rel = TheoryFormat.show_pitch (TheoryFormat.sargam Just.relative_fmt)

test_input_to_note = do
    let f = JustScales.input_to_note show_abs Nothing
        n = Just . Pitch.Note

    equal (map f (map Pitch.InputKey [0..5])) $
        map n ["-1c", "-1c#", "-1d", "-1d#", "-1e", "-1f"]
    equal (f Pitch.middle_c) (n "4c")

    let f2 = JustScales.input_to_note show_rel Nothing
    equal (map f2 [0, 1, 2, 11, 12, 14]) $
        map n ["-1s", "-1s#", "-1r", "-1n", "0s", "0r"]
    equal (f2 Pitch.middle_c) (Just (Pitch.Note "4s"))

test_input_to_note_relative = do
    let f = JustScales.input_to_note show_rel Nothing
        n = Just . Pitch.Note
    equal (f Pitch.middle_c) (n "4s")
    equal (f (Pitch.middle_c - 1)) (n "3n")

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
    -- TODO for now, input is always relative, so middle_c is always sa
    -- equalf 0.01 (run "a-maj" Pitch.middle_c) $ Right (Just 69)

test_transpose = do
    let f = JustScales.transpose TheoryFormat.absolute_c Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "4b", "5c"]
    equal [f n (Pitch.Chromatic 0) (Pitch.Note "4a") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4a", "5a", "6a"]

test_transpose_relative = do
    let f = JustScales.transpose (TheoryFormat.sargam Just.relative_fmt) Nothing
    equal [f 0 (Pitch.Chromatic n) (Pitch.Note "4s") | n <- [0..2]] $
        map (Right . Pitch.Note) ["4s", "4r", "4g"]


-- * implementation

just_scale :: Scale.Scale
Just just_scale =
    List.find ((== Pitch.ScaleId "just") . Scale.scale_id) Just.scales

just_scale_rel :: Scale.Scale
Just just_scale_rel =
    List.find ((== Pitch.ScaleId "just-r") . Scale.scale_id) Just.scales

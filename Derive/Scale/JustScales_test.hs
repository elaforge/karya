-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.JustScales_test where
import qualified Control.Arrow as Arrow
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Just as Just
import qualified Derive.Scale.JustScales as JustScales
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch


test_note_to_call = do
    let run key base ps = DeriveTest.extract extract $
            DeriveTest.derive_tracks ""
            [ (title, [(t, 1, "") | (t, _) <- times ps])
            , ("*just", [(t, 0, p) | (t, p) <- times ps])
            ]
            where
            times = zip (Seq.range_ 0 1)
            title = "> | %just-base = " <> base <> " | key = " <> key
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

test_transpose_smooth = do
    let run = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks "scale=raga | key=kharaharapriya"
            [ ("*", [(0, 0, "4g")])
            , ("t-diatonic",
                [(0, 0, "-1"), (1, 0, "-.7"), (2, 0, "-.4"), (3, 0, "0")])
            , (">", [(0, 8, "")])
            ]
    let [nns] = map (map snd) (fst run)
        diffs = zipWith (-) (drop 1 nns) nns
    -- Diatonic transpose changes pitch smoothly.  This tests the bug fixed
    -- by split_fraction.
    check $ all (>0) diffs

test_note_to_call_relative = do
    let run key p = DeriveTest.extract extract $ DeriveTest.derive_tracks ""
            [ ("*just-r | key = " <> key, [(0, 0, p)])
            , ("> | %just-base = 440", [(0, 1, "")])
            ]
        extract = fmap Pitch.nn_to_hz . Score.initial_nn
    equalf 0.001 (run "a-maj" "4s") ([Just 440], [])
    equalf 0.001 (run "a-maj" "4p") ([Just (440 * 3/2)], [])

test_input_to_note = do
    let f smap key = either (const "") Pitch.note_text <$>
            JustScales.input_to_note smap (Just (Pitch.Key key))
        rel = make_scale_map True
        abs = make_scale_map False
        ascii oct = CmdTest.ascii_kbd . CmdTest.oct_pc oct
        piano oct = CmdTest.piano_kbd . CmdTest.oct_pc oct
    let notes empty n = map (("0-"<>) . showt) [1..n] ++ replicate empty ""
            ++ map (("1-"<>) . showt) [1..6] ++ replicate empty ""

    -- The key doesn't matter for ascii.
    equal (f (rel 7) "a" (ascii 0 0)) "0-1"
    equal (f (rel 7) "a" (ascii 1 1)) "1-2"
    equal (f (rel 7) "b" (ascii 0 0)) "0-1"
    equal (f (rel 7) "b" (ascii 1 1)) "1-2"
    equal (f (abs 7) "a" (ascii 0 0)) "0a"
    equal (f (abs 7) "a" (ascii 1 1)) "1b"
    equal (f (abs 7) "b" (ascii 0 0)) "0a"
    equal (f (abs 7) "b" (ascii 1 1)) "1b"

    -- A short scale will wrap early.
    equal [f (abs 6) "a" (ascii 0 pc) | pc <- [0..6]]
        ["0a", "0b", "0c", "0d", "0e", "0f", "1a"]
    equal [f (rel 6) "a" (ascii 0 pc) | pc <- [0..6]] (take 7 (notes 0 6))
    equal [f (rel 9) "a" (ascii 0 pc) | pc <- [0..9]] (take 10 (notes 0 9))

    -- PianoKbd is absolute, so degree 1 depends on the key.
    equal (f (rel 7) "a" (piano 0 0)) "0-1"
    equal (f (rel 7) "a" (piano 1 1)) "1-2"
    equal (f (rel 7) "b" (piano 0 0)) "-1-7"
    equal (f (rel 7) "b" (piano 1 1)) "1-1"
    equal (f (rel 7) "b" (piano 1 2)) "1-2"

    -- If the scale has less than 7 notes, it still starts at the tonic:
    equal (f (rel 6) "a" (piano 0 0)) "0-1"
    equal (f (rel 6) "a" (piano 0 6)) ""
    equal (f (rel 6) "b" (piano 0 0)) ""

    equal [f (rel 6) "a" (piano 0 pc) | pc <- [0..8]] (take 9 (notes 1 6))
    equal [f (rel 6) "b" (piano 0 pc) | pc <- [0..8]] ("" : take 8 (notes 1 6))
    equal [f (rel 6) "c" (piano 0 pc) | pc <- [0..8]]
        ("-1-6" : "" : take 7 (notes 1 6))

    equal [f (rel 9) "a" (piano 0 pc) | pc <- [0..14]]
        (take 15 (notes 5 9))
    equal [f (rel 9) "b" (piano 0 pc) | pc <- [0..14]]
        ("" : take 14 (notes 5 9))
    equal [f (rel 9) "c" (piano 0 pc) | pc <- [0..14]]
        ("" : "" : take 13 (notes 5 9))
    equal [f (rel 9) "h" (piano 0 pc) | pc <- [0..14]]
        (["-1-8", "-1-9", "", "", "", "", ""] ++ take 8 (notes 5 9))

make_scale_map :: Bool -> Int -> JustScales.ScaleMap
make_scale_map relative per_oct =
    JustScales.scale_map keys default_key fmt
    where
    fmt = if relative
        then TheoryFormat.make_relative_format "" degrees
            (JustScales.make_relative_fmt keys default_key)
        else TheoryFormat.letters per_oct
    degrees =
        TheoryFormat.make_degrees ["-" <> showt pc | pc <- [1..per_oct]]
    default_key = JustScales.Key 0 mempty
    keys = Map.fromList $ take per_oct
        [ (Pitch.Key (Text.singleton c), JustScales.Key pc mempty)
        | (c, pc) <- zip "abcdefghijklmnopq" [0..]
        ]

test_input_to_nn = do
    let Just scale = List.find ((== "just") . Scale.scale_id) Just.scales
    let f = DeriveTest.with_key "c-maj" . Scale.scale_input_to_nn scale 0
        input = CmdTest.ascii_kbd . CmdTest.oct_pc Pitch.middle_octave
        run = Arrow.right (Arrow.left pretty)
            . DeriveTest.eval State.empty . f . input
    equalf 0.01 (run 0) $ Right (Right NN.middle_c)
    equalf 0.01 (run 1) $ Right $ Right $ Pitch.modify_hz (* (9/8)) NN.middle_c

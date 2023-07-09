-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Twelve_test where
import qualified Data.Text as Text

import qualified Cmd.CmdTest as CmdTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import           Global
import           Util.Test


test_note_to_nn :: Test
test_note_to_nn = do
    let f = DeriveTest.extract Score.initial_nn . derive_pitch "twelve" ""
    equal (f "3b") ([Just NN.b3], [])
    equal (f "4c") ([Just NN.c4], [])
    equal (f "-2c") ([Nothing], [])
    equal (f "-1c#") ([Just NN.cs_1], [])
    equal (f "-2b") ([Nothing], [])
    equal (f "0c") ([Just NN.c0], [])
    equal (f "9f#") ([Just NN.fs9], [])
    equal (f "9g") ([Just NN.g9], [])
    equal (f "9g#") ([Nothing], [])

test_read_relative :: Test
test_read_relative = do
    let f = read_scale (ScaleTest.get_scale Twelve.scales "twelve-r")
    print (f "d-maj" "5s")
    pprint (map (f "d-maj") ["5s", "5r", "5g", "5m", "5p", "5d", "5n", "6s"])

read_scale :: Scale.Scale -> Text -> Pitch.Note -> Either Text Text
read_scale scale key note = bimap pretty pretty $
    Scale.scale_read scale (ScaleTest.key_environ key) note

test_note_to_call_relative :: Test
test_note_to_call_relative = do
    let f key = DeriveTest.extract Score.initial_nn
            . derive_pitch "twelve-r" key
    equal (f "" "4s") ([Just NN.c4], [])
    equal (f "" "4g") ([Just NN.e4], [])
    equal (f "c-min" "4g") ([Just NN.ds4], [])
    equal (f "d-maj" "4s") ([Just NN.d4], [])
    equal (f "d#-maj" "4s") ([Just NN.ds4], [])

    equal (f "c-min" "4p") ([Just NN.g4], [])
    equal (f "c-min" "4p#") ([Just NN.gs4], [])
    equal (f "c-min" "4d") ([Just NN.gs4], [])
    equal (f "c-min" "4db") ([Just NN.g4], [])

test_pitch_note_relative :: Test
test_pitch_note_relative = do
    let f key = DeriveTest.extract Score.initial_note
            . derive_pitch "twelve-r" key
    equal (f "c-min" "4s") ([Just "4s"], [])
    -- Symbolic Pitch.Note is still relative.
    equal (f "b-min" "4s") ([Just "4s"], [])

test_keyed_to_nn :: Test
test_keyed_to_nn = do
    let run = DeriveTest.extract Score.initial_nn
            . derive_pitch "twelve-k" "d-min"
    equal (run "4c") ([Just NN.c4], [])
    equal (run "4b") ([Just NN.as4], [])
    equal (run "4b`n`") ([Just NN.b4], [])
    equal (run "4b`#`") ([Just NN.c5], [])

    -- TODO should preserve the natural
    -- let runp = DeriveTest.extract Score.initial_note
    --         . derive_pitch "twelve-k" "d-min"
    -- equal (runp "4b`n`") ([Just "4b`n`"], [])

test_keyed_input_to_note :: Test
test_keyed_input_to_note = do
    let f key = either pretty Pitch.note_text <$>
            ChromaticScales.input_to_note Twelve.keyed_scale_map
                (ScaleTest.key_environ key)
        ascii = CmdTest.ascii_kbd . (\(a, b, c) -> CmdTest.pitch a b c)
        invalid = "invalid input"
    equal (map (f "d-min" . ascii) [(4, pc, acc) | pc <- [0..6], acc <- [0, 1]])
        [ "4c", "4d`b`", "4d", "4e`b`", "4e", invalid, "4f", "4g`b`"
        , "4g", "4a`b`", "4a", "4b", "4b`n`", invalid
        ]
    equal (f "d-min" (ascii (4, 1, 1))) "4e`b`"

-- * util

derive_pitch :: Text -> Text -> Text -> Derive.Result
derive_pitch scale key pitch =
    DeriveTest.derive_tracks (if Text.null key then "" else "key = " <> key)
        [(">", [(0, 1, "")]), ("*" <> scale, [(0, 0, pitch)])]

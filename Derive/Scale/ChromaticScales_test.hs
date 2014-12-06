-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.ChromaticScales_test where
import Util.Test
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Pitch as Pitch
import Global


test_input_to_note = do
    let f smap key = either prettyt Pitch.note_text <$>
            ChromaticScales.input_to_note smap (ScaleTest.key_environ key)
        abs = Twelve.absolute_scale_map
        rel = Twelve.relative_scale_map
        ascii = CmdTest.ascii_kbd . (\(a, b, c) -> CmdTest.pitch a b c)
        invalid = "invalid input"
    equal (map (f abs "c-maj" . ascii)
            [(4, pc, acc) | pc <- [0..7], acc <- [0, 1]])
        ["4c", "4c#", "4d", "4d#", "4e", invalid, "4f", "4f#", "4g", "4g#"
        , "4a", "4a#", "4b", invalid, "5c", "5c#"
        ]
    equal (map (f rel "d-min" . ascii)
            [(4, pc, acc) | pc <- [0..7], acc <- [0, 1]])
        ["4s", "4s#", "4r", invalid, "4g", "4g#", "4m", "4m#", "4p", invalid
        , "4d", "4d#", "4n", "4n#", "5s", "5s#"
        ]

test_transpose = do
    let f smap key_ trans steps =
            ChromaticScales.show_pitch smap key
                <=< ChromaticScales.transpose smap trans
                    (ScaleTest.key_environ key_) steps
                <=< ChromaticScales.read_pitch smap key
            where key = Just (Pitch.Key key_)
        rel = Twelve.relative_scale_map
        abs = Twelve.absolute_scale_map
    equal [f abs "f#-min" Scale.Diatonic n "4f#" | n <- [0..4]] $
        map Right ["4f#", "4g#", "4a", "4b", "5c#"]
    equal [f rel "f#-min" Scale.Diatonic n "4s" | n <- [0..4]] $
        map Right ["4s", "4r", "4g", "4m", "4p"]
    equal (f rel "f#-min" Scale.Diatonic 2 "4s") (Right "4g")

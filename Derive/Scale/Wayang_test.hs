-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Wayang_test where
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.CmdTest as CmdTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.ScaleTest as ScaleTest
import qualified Derive.Scale.Wayang as Wayang
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Global


test_read = do
    let f scale_id pitch = read_scale
            (ScaleTest.get_scale Wayang.scales scale_id) pitch
    -- The same pitch also winds up with the same Pitch and same frequency.
    equal (f "wayang" "5i") (Right "5-0")
    equal (f "wayang-pemade" "i^") (Right "5-0")
    equal (f "wayang-kantilan" "i-") (Right "5-0")
    let run scale pitch = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks "" $ scale_track scale [pitch]
    equal (run "wayang" "5i") (run "wayang-pemade" "i^")
    equal (run "wayang" "5i") (run "wayang-kantilan" "i-")

test_note_to_call = do
    let run key ps = DeriveTest.extract Score.initial_nn $
            DeriveTest.derive_tracks ("scale=" ++ key) $
            UiTest.note_track [(t, 1, p) | (t, p) <- zip (Seq.range_ 0 1) ps]
        umbang = Vector.toList $ BaliScales.nn_umbang Wayang.note_numbers
    equal (run "wayang" ["1i"]) ([Just (head umbang)], [])
    equal (run "wayang" ["4i"]) ([Just (umbang !! (3*5))], [])

read_scale :: Scale.Scale -> Pitch.Note -> Either String String
read_scale scale note = (prettys *** prettys) $
    Scale.scale_read scale mempty note

scale_track :: String -> [String] -> [UiTest.TrackSpec]
scale_track scale_id pitches =
    [ (">", [(n, 1, "") | n <- map fst events])
    , ('*' : scale_id, [(n, 0, p) | (n, p) <- events])
    ]
    where events = zip (Seq.range_ 0 1) pitches


test_input_to_note = do
    let f scale = either pretty Pitch.note_text
            . Scale.scale_input_to_note scale mempty
            . CmdTest.ascii_kbd . (\(a, b, c) -> CmdTest.pitch a b c)
        wayang = ScaleTest.get_scale Wayang.scales "wayang"
        wayang_pemade = ScaleTest.get_scale Wayang.scales "wayang-pemade"
        invalid = "invalid input"
    equal (f wayang (1, 0, 0)) "1i"
    equal (f wayang_pemade (3, 1, 0)) "o_"
    equal (f wayang_pemade (4, 0, 0)) "i-"
    equal (map (f wayang) [(4, pc, 0) | pc <- [0..5]])
        ["4i", "4o", "4e", "4u", "4a", "5i"]
    equal (map (f wayang_pemade) $
            take 12 [(oct, pc, 0) | oct <- [3..6], pc <- [0..4]])
        [ invalid, "o_", "e_", "u_", "a_", "i-", "o-", "e-", "u-", "a-"
        , "i^", invalid
        ]

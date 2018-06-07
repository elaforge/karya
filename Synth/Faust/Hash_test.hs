-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Faust.Hash_test where
import Util.Test
import qualified Synth.Faust.Hash as Hash
import Synth.Lib.Global
import qualified Synth.Shared.Note as Note


test_overlapping = do
    let f start size = map (\(Note.Hash h) -> h)
            . Hash.overlapping start size . map note
    equal (f 0 1 []) []
    check_val (f 0 1 [(1, 2)]) $ \case
        [0, x1, x2] -> x1 == x2
        _ -> False
    check_val (f 0 2 [(1, 2)]) $ \case
        [x1, x2] -> x1 /= 0 && x1 == x2
        _ -> False
    check_val (f 0 1 [(0, 3), (1, 1)]) $ \case
        [x1, y1, x2] -> y1 /= 0 && x1 == x2
        _ -> False

test_groupOverlapping = do
    let f start size = map (map (e_note . snd))
            . Hash.groupOverlapping start size
            . map ((),) . map note
    equal (f 0 1 []) []
    equal (f 0 1 [(1, 2)]) [[], [(1, 2)], [(1, 2)]]

    equal (f 0 2 [(1, 2)]) [[(1, 2)], [(1, 2)]]
    equal (f 1 2 [(1, 2)]) [[(1, 2)]]
    equal (f 0 1 [(0, 3), (1, 1)]) [[(0, 3)], [(0, 3), (1, 1)], [(0, 3)]]

note :: (RealTime, RealTime) -> Note.Note
note (s, d) = Note.note "patch" "inst" s d

e_note :: Note.Note -> (RealTime, RealTime)
e_note n = (Note.start n, Note.duration n)

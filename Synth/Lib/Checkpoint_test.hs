-- Copyright 2018 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Synth.Lib.Checkpoint_test where
import qualified Data.Digest.CRC32 as CRC32

import qualified Synth.Lib.Checkpoint as Checkpoint
import qualified Synth.Shared.Note as Note

import Synth.Lib.Global
import Util.Test


-- Many functions in Checkpoint are tested by caller tests, e.g.
-- 'Synth.Faust.Render_test'.

test_hashOverlapping = do
    let f start size = Checkpoint.hashOverlapping start size . map mkSpan
    equal (f 0 1 []) []
    -- 0 dur notes are also counted.
    equal (f 0 1 [(0, 0)]) [Checkpoint._hash $ mkSpan (0, 0)]

    check_val (f 0 1 [(1, 2)]) $ \case
        [Note.Hash 0, x1, x2] -> x1 == x2
        _ -> False
    check_val (f 0 2 [(1, 2)]) $ \case
        [x1, x2] -> x1 /= Note.Hash 0 && x1 == x2
        _ -> False
    check_val (f 0 1 [(0, 3), (1, 1)]) $ \case
        [x1, y1, x2] -> y1 /= Note.Hash 0 && x1 == x2
        _ -> False

test_groupOverlapping = do
    let f start size = map (map (eSpan . snd))
            . Checkpoint.groupOverlapping start size
            . map (((),) . mkSpan)
    equal (f 0 1 []) []

    -- 0 dur notes also included.
    equal (f 0 1 [(0, 0)]) [[(0, 0)]]
    equal (f 0 1 [(0, 0), (1, 0)]) [[(0, 0)], [(1, 0)]]
    equal (f 0 1 [(0, 0), (0.5, 0), (1, 0)]) [[(0, 0), (0.5, 0)], [(1, 0)]]
    equal (f 0 1 [(0, 0), (0.5, 1), (1, 0)])
        [[(0, 0), (0.5, 1)], [(0.5, 1), (1, 0)]]

    equal (f 0 1 [(1, 2)]) [[], [(1, 2)], [(1, 2)]]
    equal (f 0 2 [(1, 2)]) [[(1, 2)], [(1, 2)]]
    equal (f 1 2 [(1, 2)]) [[(1, 2)]]
    equal (f 0 1 [(0, 3), (1, 1)]) [[(0, 3)], [(0, 3), (1, 1)], [(0, 3)]]

mkSpan :: (RealTime, RealTime) -> Checkpoint.Span
mkSpan (s, d) = Checkpoint.Span
    { _start = s
    , _duration = d
    , _hash = Note.Hash $ CRC32.crc32 (s, d)
    }

eSpan :: Checkpoint.Span -> (RealTime, RealTime)
eSpan n = (Checkpoint._start n, Checkpoint._duration n)

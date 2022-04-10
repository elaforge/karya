-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.Scales_test where
import Util.Test
import qualified Derive.Scale.Scales as Scales
import qualified Perform.Pitch as Pitch


test_ascii_kbd_pitch :: Test
test_ascii_kbd_pitch = do
    let f per_oct oct pc = extract $ Scales.ascii_kbd_pitch per_oct $
            Pitch.Pitch oct (Pitch.Degree pc 0)
        extract (Pitch.Pitch oct (Pitch.Degree pc _)) = (oct, pc)
    -- Octave smaller than the row.
    equal [f 5 0 pc | pc <- [0..9]]
        [ (0, 0), (0, 1), (0, 2), (0, 3), (0, 4)
        , (1, 0), (1, 1), (1, 2), (1, 3), (1, 4)
        ]
    equal [f 5 1 pc | pc <- [0..9]]
        [ (1, 0), (1, 1), (1, 2), (1, 3), (1, 4)
        , (2, 0), (2, 1), (2, 2), (2, 3), (2, 4)
        ]

    -- Octave larger than the row.
    equal [f 12 0 pc | pc <- [0..9]]
        [ (0, 0), (0, 1), (0, 2), (0, 3), (0, 4)
        , (0, 5), (0, 6), (0, 7), (0, 8), (0, 9)
        ]
    equal [f 12 1 pc | pc <- [0..9]]
        [ (0, 10), (0, 11), (1, 0), (1, 1), (1, 2)
        , (1, 3), (1, 4), (1, 5), (1, 6), (1, 7)
        ]

test_piano_kbd_pitch :: Test
test_piano_kbd_pitch = do
    let f per_oct oct pc = extract <$> Scales.piano_kbd_pitch 0 per_oct
            (Pitch.Pitch oct (Pitch.Degree pc 0))
        extract (Pitch.Pitch oct (Pitch.Degree pc _)) = (oct, pc)
    -- Octave smaller than the row.
    equal (map (f 5 0) [0..6])
        [ Just (0, 0), Just (0, 1), Just (0, 2), Just (0, 3), Just (0, 4)
        , Nothing, Nothing
        ]
    equal (map (f 5 1) [0..6])
        [ Just (1, 0), Just (1, 1), Just (1, 2), Just (1, 3), Just (1, 4)
        , Nothing, Nothing
        ]

    -- Octave larger than the row.
    equal (map (f 8 0) [0..6])
        [ Just (0, 0), Just (0, 1), Just (0, 2), Just (0, 3), Just (0, 4)
        , Just (0, 5), Just (0, 6)
        ]
    equal (map (f 8 1) [0..6])
        [Just (0, 7), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]


--    C D E F G A B|C D E F G A B
-- C  1 2 3 4 5 / / 1 2 3 4 5 / /
-- D  / 1 2 3 4 5 / / 1 2 3 4 5 /
--    5 1 2 3 4 / /
-- E / / 1 2 3 4 5 / / 1 2 3 4 5
--

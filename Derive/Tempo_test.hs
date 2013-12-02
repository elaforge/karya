-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Tempo_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.RealTime as RealTime


-- * with_tempo

test_tempo = do
    let extract = e_floor . DeriveTest.e_event
        e_floor (start, dur, text) =
            (floor (secs start), floor (secs dur), text)
        secs = RealTime.to_seconds
    let f tempo_track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks
                [ ("tempo", tempo_track)
                , ("*", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
                , (">", [(0, 10, "n --1"), (10, 10, "n --2"),
                    (20, 10, "n --3")])
                ]

    equal (f [(0, 0, "2")]) $
        ([(0, 5, "n --1"), (5, 5, "n --2"), (10, 5, "n --3")], [])

    -- Slow down.
    equal (f [(0, 0, "2"), (20, 0, "i 1")]) $
        ([(0, 5, "n --1"), (5, 7, "n --2"), (13, 10, "n --3")], [])
    equal (f [(0, 0, "2"), (20, 0, "i 0")]) $
        ([(0, 6, "n --1"), (6, 29, "n --2"), (35, 10000, "n --3")], [])
    -- Speed up.
    equal (f [(0, 0, "1"), (20, 0, "i 2")]) $
        ([(0, 8, "n --1"), (8, 5, "n --2"), (14, 5, "n --3")], [])
    equal (f [(0, 0, "0"), (20, 0, "i 2")]) $
        ([(0, 1028, "n --1"), (1028, 7, "n --2"), (1035, 5, "n --3")], [])

    -- Change tempo.
    equal (f [(0, 0, "1"), (10, 0, "2")]) $
        ([(0, 10, "n --1"), (10, 5, "n --2"), (15, 5, "n --3")], [])

-- * with_hybrid

test_with_hybrid = do
    let run start dur tempo events = DeriveTest.extract extent $
            DeriveTest.derive_blocks
                [ ("top", [(">", [(start, dur, "sub")])])
                , ("sub=ruler", [("tempo hybrid", tempo),
                    (">", [(n, 1, "") | n <- Seq.range' 0 events 1])])
                ]
        extent e = (Score.event_start e, Score.event_end e)
    -- Tempo is cancelled out by stretch_to_1 as usual.
    equal (run 0 2 [(0, 0, "1")] 4)
        ([(0, 0.5), (0.5, 1), (1, 1.5), (1.5, 2)], [])
    equal (run 0 2 [(0, 0, "2")] 4)
        ([(0, 0.25), (0.25, 0.5), (0.5, 0.75), (0.75, 1)], [])

    -- Absolute tempo, goes over event bounds.
    equal (run 0 2 [(0, 0, "0")] 4)
        ([(0, 1), (1, 2), (2, 3), (3, 4)], [])

    let tempo = [(0, 0, "0"), (2, 0, "1")]
    equalf 0.001 (run 0 3 tempo 4) ([(0, 1), (1, 2), (2, 2.5), (2.5, 3)], [])
    equalf 0.001 (run 0 4 tempo 4) ([(0, 1), (1, 2), (2, 3), (3, 4)], [])
    equalf 0.001 (run 0 6 tempo 4) ([(0, 1), (1, 2), (2, 4), (4, 6)], [])
    equalf 0.001 (run 2 4 tempo 4) ([(2, 3), (3, 4), (4, 5), (5, 6)], [])
    equalf 0.001 (run 2 6 tempo 4) ([(2, 3), (3, 4), (4, 6), (6, 8)], [])

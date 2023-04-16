-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Tempo_test where
import qualified Util.Lists as Lists
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.RealTime as RealTime
import qualified Ui.UiTest as UiTest

import           Util.Test


test_tempo :: Test
test_tempo = do
    let extract = e_floor . DeriveTest.e_event
        e_floor (start, dur, text) =
            (floor (secs start), floor (secs dur), text)
        secs = RealTime.to_seconds
    let run tempo_track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks ""
                [ ("tempo", tempo_track)
                , ("*", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
                , (">", [(0, 10, "--1"), (10, 10, "--2"), (20, 10, "--3")])
                ]

    equal (run [(0, 0, "2")]) $
        ([(0, 5, "--1"), (5, 5, "--2"), (10, 5, "--3")], [])

    -- Slow down.
    equal (run [(0, 0, "1"), (10, 0, "2")])
        ([(0, 10, "--1"), (10, 5, "--2"), (15, 5, "--3")], [])
    equal (run [(0, 0, "2"), (20, 0, "i 1")]) $
        ([(0, 5, "--1"), (5, 8, "--2"), (13, 10, "--3")], [])
    equal (run [(0, 0, "2"), (20, 0, "i 0.001")]) $
        ([(0, 6, "--1"), (6, 100, "--2"), (107, 10000, "--3")], [])
    -- Speed up.
    equal (run [(0, 0, "1"), (20, 0, "i 2")]) $
        ([(0, 8, "--1"), (8, 5, "--2"), (13, 5, "--3")], [])
    equal (run [(0, 0, ".001"), (20, 0, "i 2")]) $
        ([(0, 100, "--1"), (100, 6, "--2"), (107, 5, "--3")], [])

    -- Change tempo.
    equal (run [(0, 0, "1"), (10, 0, "2")]) $
        ([(0, 10, "--1"), (10, 5, "--2"), (15, 5, "--3")], [])

test_tempo_zero :: Test
test_tempo_zero = do
    let run tempo = DeriveTest.extract DeriveTest.e_start_dur $
            DeriveTest.derive_tracks "" $
                ("tempo", tempo) : UiTest.regular_notes 4
    equal (run [(0, 0, "1")]) (map (,1) [0, 1, 2, 3], [])
    equal (run [(0, 0, "1"), (1, 0, "i 0"), (2, 0, "i 1")])
        ([], ["tempo signal crosses zero"])
    equal (run [(0, 0, "1"), (2, 0, "i -1")])
        ([], ["tempo signal crosses zero"])
    equal (run [(1, 0, "1")]) ([], ["tempo signal crosses zero"])

test_multiple_tempo_tracks :: Test
test_multiple_tempo_tracks = do
    let run = DeriveTest.extract DeriveTest.e_start_dur
            . DeriveTest.derive_blocks_setup
                (DeriveTest.with_linear_block (UiTest.bid "sub"))
    let mktempos tempos =
            [ ("top", [(">", [(0, 8, "sub")])])
            , ("sub=ruler",
                ("tempo", [(0, 0, "2"), (2, 0, "1")])
                : tempos
                ++ [(">", [(0, 2, ""), (2, 1, "")])])
            ]
    let mkblocks tempo =
            [ ("top", [(">", [(0, 8, "sub")])])
            , ("sub=ruler",
                [ ("tempo", [(0, 0, "2"), (2, 0, "1")])
                , (">", [(0, 3, "sub2")])
                ])
            , ("sub2=ruler",
                [ ("tempo", tempo)
                , (">", [(0, 2, ""), (2, 1, "")])
                ])
            ]

    equal (run $ mktempos []) ([(0, 4), (4, 4)], [])
    -- Adding a constant tempo should have no effect.
    equal (run $ mktempos [("tempo", [(0, 0, "1")])]) ([(0, 4), (4, 4)], [])
    equal (run $ mktempos [("tempo", [(0, 0, "2")])]) ([(0, 4), (4, 4)], [])
    -- Complicated nesting.
    equalf 0.01 (run $ mktempos [("tempo", [(0, 0, "1"), (2, 0, "2")])])
        ([(0, 5.6), (5.6, 2.4)], [])
    -- Nested tempo tracks should be equivalent to nested blocks.
    equalf 0.01 (run $ mkblocks [(0, 0, "1"), (2, 0, "2")])
        ([(0, 5.6), (5.6, 2.4)], [])
    -- TODO with a logical start time

test_with_absolute :: Test
test_with_absolute = do
    let run start dur tempo events = DeriveTest.extract Score.event_start $
            DeriveTest.derive_blocks
                [ ("top", [(">", [(start, dur, "sub")])])
                , ("sub=ruler",
                    [ ("tempo abs", tempo)
                    , (">", [(n, 1, "") | n <- Lists.range' 0 events 1])
                    ])
                ]
    equal (run 0 4 [(0, 0, "1")] 4) ([0, 1, 2, 3], [])
    equal (run 0 8 [(0, 0, "1")] 4) ([0, 2, 4, 6], [])
    equal (run 1 4 [(0, 0, "1")] 4) ([1, 2, 3, 4], [])
    -- Stretches to 1 regardless of the tempo.
    equal (run 1 4 [(0, 0, "2")] 4) ([1, 2, 3, 4], [])
    equal (run 1 3 [(0, 0, "1"), (2, 0, "2")] 4) ([1, 2, 3, 3.5], [])

_test_with_hybrid :: Test
_test_with_hybrid = do
    let run start dur tempo events = DeriveTest.extract extent $
            DeriveTest.derive_blocks
                [ ("top", [(">", [(start, dur, "sub")])])
                , ("sub=ruler", [("tempo hybrid", tempo),
                    (">", [(n, 1, "") | n <- Lists.range' 0 events 1])])
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

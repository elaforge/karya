-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.India.Gamakam2_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


test_sequence :: Test
test_sequence = do
    -- TODO: these are kind of messed up because I should be using nns_literal.
    -- Can't be bothered at the moment though, because I probably won't keep
    -- this module.
    let run = run_note_track ""
    strings_like (snd (run [(0, 8, "@ ; no-call ; -- 4c")]))
        ["generator not found"]

    -- Implicit hold.
    equal (run [(0, 8, "@ -- 4c")]) ([[(0, 60)]], [])
    equal (run [(0, 8, "@ ; ; -- 4c")]) ([[(0, 60)]], [])

    -- Implicit set pitch if you don't supply a begin call.
    equal (run [(0, 1, "4c"), (1, 1, "@ -- 4d")])
        ([[(0, 60)], [(1, 62)]], [])

    -- From call.
    equal (run [(0, 1, "4c"), (1, 4, "@ prev 2 -- 4d")])
        ([[(0, 60)], [(1, 60), (3, 62)]], [])
    equal (run [(0, 4, "@ cur 1 1 -- 4c")])
        ([[(0, 61), (1, 60)]], [])

    -- To call.
    equal (run [(0, 4, "@ ; - ; to 2 2 -- 4c")])
        ([[(0, 60), (4, 62)]], [])
    equal (run [(0, 4, "@ ; ; to 2 2 -- 4c")])
        ([[(0, 60), (4, 62)]], [])

    -- Enough room.
    equal (run [(0, 8, "@ ; ; to 2 2 -- 4c")])
        ([[(0, 60), (8, 62)]], [])
    equal (run [(0, 2, "@ ; ; to 2 2 -- 4c")])
        ([[(0, 60), (2, 62)]], [])
    -- Not enough room.
    equal (run [(0, 1, "@ ; ; to 2 2 -- 4c")])
        ([[(0, 60), (1, 62)]], [])

    -- Middle is divided evenly.
    equal (run [(0, 8, "@ ; - 1 ; - 0 ; -- 4c")])
        ([[(0, 61), (4, 60)]], [])

    -- Begin, middle, end.
    equal (run [(0, 8, "@ cur 1 1 ; ; to 1 1 -- 4c")])
        ([[(0, 61), (1, 60), (8, 61)]], [])
    -- Middle divided equally between 59 and 60.
    equal (run [(0, 8, "@ cur 1 1 ; - -1 ; - 0 ; to 1 1 -- 4c")])
        ([[(0, 61), (1, 59), (4, 60), (8, 61)]], [])

test_flat_start_end :: Test
test_flat_start_end = do
    let run = run_note_track ""
    equal (run [(0, 4, "@ - 0 1 ; - 1 ; - -1 1 -- 4c")])
        ([[(0, 60), (1, 61), (3, 60)]], [])

test_fade :: Test
test_fade = do
    let run = run_note_track_dyn ""
    equal (run [(0, 4, "@ ; ; -> 2 -- 4c")])
        ([( [(0, 60), (4, 60)]
          , [(-RealTime.larger, 1), (2, 1), (4, 0), (4, 1)]
          )], [])
    equal (run [(0, 4, "@ -< 2 ; ; -- 4c")])
        ([([(0, 60), (4, 60)], [(0, 1), (2, 0), (2, 1)])], [])

test_jaru :: Test
test_jaru = do
    let run = run_note_track "| jaru-time=1 | jaru-transition=1"
    equal (run [(0, 4, "@ J 1 -1 -- 4c")])
        ([[(0, 62), (1, 59)]], [])
    equal (run [(0, 2, "@ J 1 -1 1 -- 4c")])
        ([[(0, 62), (1, 59), (2, 62)]], [])
    equal (run [(0, 1, "@ J 1 -1 1 -- 4c")])
        ([[(0, 62), (0.5, 59), (1, 62)]], [])

test_kampita :: Test
test_kampita = do
    let run = run_note_track "| kam-transition=0 | kam-speed=1"
    equal (run [(0, 2.5, "@ ; k 1 ; -- 4c")])
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run [(0, 2.5, "@ ; k^ 1 ; -- 4c")])
        ([[(0, 60), (1, 61)]], [])
    equal (run [(0, 2.5, "@ ; k_ 1 ; -- 4c")])
        ([[(0, 60), (1, 61), (2, 60)]], [])
    -- Starts from the previous pitch.
    equal (run [(0, 4, "@ ; flat -1 ; k 1 ; -- 4c")])
        ([[(0, 59), (3, 60), (4, 59)]], [])
    -- Adjust.
    equal (run [(0, 2.5, "kam-adjust=stretch | @ ; k^ 1 ; -- 4c")])
        ([[(0, 60), (2.5, 61)]], [])

test_nkampita :: Test
test_nkampita = do
    let run = run_note_track "| nkam-transition=0"
    strings_like (snd (run [(0, 2, "@  ; nk 0 ; -- 4c")]))
        ["cycles: expected Signal (>0)"]
    equal (run [(0, 2, "@ ; nk 1 ; -- 4c")])
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run [(0, 2, "@ ; nk_ 1 ; -- 4c")])
        ([[(0, 60), (1, 61), (2, 60)]], [])
    equal (run [(0, 2, "@ ; nk 2 ; -- 4c")])
        ([[(0, 60), (0.5, 61), (1, 60), (1.5, 61), (2, 60)]], [])
    equal (run [(0, 2, "@ ; nk^ 1 ; -- 4c")])
        ([[(0, 60), (2, 61)]], [])
    equal (run [(0, 2, "@ ; nk_ 1 -1 ; -- 4c")])
        ([[(0, 60), (2, 59)]], [])
    equal (run [(0, 4, "nkam-transition=2 | @ ; nk^ 1 ; -- 4c")])
        ([[(0, 60), (4, 61)]], [])

run_note_track_dyn :: Text -> [UiTest.EventSpec]
    -> ([([(RealTime, Pitch.NoteNumber)], [(RealTime, Signal.Y)])], [Text])
run_note_track_dyn = run_ $ \e -> (DeriveTest.e_nns e, DeriveTest.e_dyn e)

run_note_track :: Text -> [UiTest.EventSpec]
    -> ([[(RealTime, Pitch.NoteNumber)]], [Text])
run_note_track = run_ DeriveTest.e_nns_old

run_ :: (Score.Event -> a) -> Text -> [UiTest.EventSpec] -> ([a], [Text])
run_ extract transform = DeriveTest.extract extract
    . DeriveTest.derive_tracks ("import india.gamakam2 " <> transform)
    . UiTest.note_track

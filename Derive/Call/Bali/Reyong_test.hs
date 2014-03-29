-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt
module Derive.Call.Bali.Reyong_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Bali.Reyong as Reyong
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import Types


test_kilitan = do
    let run notes = e_voice 1 $ DeriveTest.derive_tracks "import bali.reyong" $
            UiTest.note_spec (" | scale = legong", notes, [])
    equal (run [(0, 0, "X --"), (1, 0, "O --"), (2, 0, "+ --")])
        (Just [(0, "3u"), (1, "3e"), (1, "3a"), (2, "3e"), (2, "3a")], [])
    equal (run [(4, -4, ">kilit -- 3u"), (8, -4, "kilit -- 3u")])
        (Just
            [ (1, "3u"), (2, "3u"), (3, "3a"), (4, "3u")
            , (5, "3a"), (6, "3u"), (7, "3a"), (8, "3u")
            ], [])

test_kotekan = do
    let run notes = e_voice 1 $ DeriveTest.derive_tracks "import bali.reyong" $
            UiTest.note_spec (" | scale = legong", notes, [])
    equal (run [(16, -16, "k\\/ -- 3o")])
        (Just
            [ (2, "3a"), (3, "4i"), (5, "3a"), (6, "4i"), (8, "4i")
            , (9, "3a"), (11, "4i"), (12, "3a"), (14, "3a"), (15, "4i")
            ], [])
    equal (run [(8, -8, "k// -- 3a")])
        (Just
            [ (1, "3u"), (2, "3a"), (4, "3u"), (5, "3a"), (7, "3u"), (8, "3a")
            ], [])

e_voice :: Int -> Derive.Result -> (Maybe [(RealTime, String)], [String])
e_voice voice = group_voices . DeriveTest.extract extract
    where
    extract e = (DeriveTest.e_environ_val Environ.voice e :: Maybe Int,
        (Score.event_start e, DeriveTest.e_pitch e))
    group_voices = first (lookup (Just voice) . Seq.group_fst)

test_positions = do
    -- Force all the positions because of partial functions in there.
    forM_ Reyong.reyong_positions $ \pos -> equal pos pos
    forM_ Reyong.reyong_patterns $ \p -> equal p p

test_assign_positions = do
    let f pattern dest = extract $ Reyong.assign_positions pattern 5 dest
            (map Reyong.pos_cek Reyong.reyong_positions)
        extract = map (unwords . map show_pitch)
        p1 = Reyong.parse_kotekan "4-3" "12-"
    equal (f p1 0) ["3u 3a -", "4o - 4i", "4u 4a -", "5o - 5i"]
    equal (f p1 1) ["3a 4i -", "4e - 4o", "4a 5i -", "5e - 5o"]
    equal (f p1 2) ["3u - 3e", "4i 4o -", "4u - 4e", "5i 5o -"]
    equal (f p1 3) ["3a - 3u", "4o 4e -", "4a - 4u", "5o 5e -"]
    equal (f p1 4) ["3e 3u -", "4i - 3a", "4e 4u -", "5i - 4a"]

show_pitch :: Maybe Pitch.Pitch -> String
show_pitch Nothing = "-"
show_pitch (Just (Pitch.Pitch oct (Pitch.Degree d _))) =
    show oct ++ ("ioeua" !! d) : ""

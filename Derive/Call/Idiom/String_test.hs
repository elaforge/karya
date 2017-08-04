-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Idiom.String_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import Global
import Types


test_string = do
    let track p1 p2 p3 = [(0, 5, p1), (5, 5, p2), (10, 5, p3)]
    let (res, logs) = run $ track "4c" "2d" "2e"
    equal res [(0, [(0, 60)])]
    strings_like logs ["38nn below lowest string", "40nn below lowest string"]
    -- All separate strings doesn't do anything interesting.
    equal (run $ track "4c" "4d" "4e")
        ([ (0, [(0, 60)])
        , (5, [(5, 62)])
        , (10, [(10, 64)])
        ], [])
    equal (run $ track "4c" "4c#" "4d")
        ([ (0, [(0, 60), (4, 60.5), (5, 61)]) -- bend up for attack
        , (5, [(5, 61), (12, 60.5), (13, 60)]) -- bend back down for release
        , (10, [(10, 62)])
        ], [])
    strings_like (snd $ DeriveTest.extract id $ DeriveTest.derive_tracks
            "import idiom.string | bent-string" [(">", [(0, 1, "")])])
        ["open-strings required"]

test_close_notes = do
    -- Notes closer than the transition time still play properly.
    equal (run [(1, 1, "4d"), (2, 1, "4d")])
        ([(1, [(1, 62)]), (2, [(2, 62)])], [])

run :: [UiTest.EventSpec]
    -> ([(RealTime, [(RealTime, Pitch.NoteNumber)])], [Text])
run = DeriveTest.extract e_nns . DeriveTest.derive_tracks title
    . UiTest.note_track
    where
    title = "import idiom.string\
        \ | open-strings = (list (4c) (4d) (4e) (4g) (4a))\
        \ | bent-string 2 2 1"

e_nns :: Score.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
e_nns e = (Score.event_start e, Seq.drop_dups snd $ DeriveTest.e_nns e)
    -- 'merge_curve' can't see that pitches are the same, so it can produce
    -- duplicate pitches.

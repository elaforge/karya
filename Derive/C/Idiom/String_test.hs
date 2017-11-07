-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Idiom.String_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Score as Score

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import Global
import Types


test_attack_release = do
    let run = run_string e_nns "bent-string 2 2 1" . UiTest.note_track
    let track p1 p2 p3 = [(0, 5, p1), (5, 5, p2), (10, 5, p3)]

    let (res, logs) = run $ track "4c" "2d" "2e"
    equal res [(0, [(0, 60)])]
    strings_like logs ["below lowest string: 38nn", "below lowest string: 40nn"]
    -- All separate strings doesn't do anything interesting.
    equal (run $ track "4c" "4d" "4e")
        ([ (0, [(0, 60)])
        , (5, [(5, 62)])
        , (10, [(10, 64)])
        ], [])
    equal (run $ track "4c" "4c#" "4d")
        ( [ (0, [(0, 60), (4, 60.5), (5, 61)]) -- bend up for attack
          , (5, [(5, 61), (12, 60.5), (13, 60)]) -- bend back down for release
          , (10, [(10, NN.d4)])
          ]
        , []
        )
    strings_like (snd $ DeriveTest.extract id $ DeriveTest.derive_tracks
            "import idiom.string | bent-string" [(">", [(0, 1, "")])])
        ["open-strings required"]

    -- Notes closer than the transition time still play properly.
    equal (run [(1, 1, "4d"), (2, 1, "4d")])
        ([(1, [(1, 62)]), (2, [(2, 62)])], [])

test_string_select = do
    let run = run_string extract "bent-string 0 0 1"
        extract e = (DeriveTest.e_environ EnvKey.string e, e_nns e)
    equal (run $ UiTest.note_track [(0, 1, "4c")])
        ([(Just "'60nn'", (0, [(0, NN.c4)]))], [])
    equal (run $ UiTest.note_track [(0, 1, "4d")])
        ([(Just "'62nn'", (0, [(0, NN.d4)]))], [])
    -- The string is assigned based on the lowest pitch.
    equal (run [(">", [(0, 2, "")]), ("*", [(0, 0, "4d"), (1, 0, "4c")])])
        ([(Just "'60nn'", (0, [(0, NN.d4), (1, NN.c4)]))], [])


-- TODO I have to test this ad-hoc per call until I figure out a better
-- solution, see NOTE [signal-discontinuity]
test_signal_discontinuity = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks (title <> " | bent-string 0 0 1")
        extract = fst . DeriveTest.e_nns_errors
    equal (run [(">", [(0, 2, "")]), ("*", [(0, 0, "4c"), (1, 0, "4d")])])
        ([[(0, NN.c4), (1, NN.c4), (1, NN.d4), (3, NN.d4), (3, NN.c4)]], [])

test_mute_end = do
    let run args = run_string extract ("mute-end " <> args) . UiTest.note_track
        extract e = (DeriveTest.e_start_dur e, Score.initial_dynamic e,
            DeriveTest.e_attributes e)
    equal (run "1 1 .5" [(0, 1, "3c")])
        ([((0, 1), 1, "+"), ((1, 1), 0.5, "+"), ((2, 0), 1, "+mute")], [])
    equal (run "1 1 .5" [(0, 1, "+ring -- 3c")])
        ([((0, 1), 1, "+ring")], [])
    equal (run "1 0 .5" [(0, 1, "3c")])
        ([((0, 1), 1, "+"), ((1, 0), 1, "+mute")], [])


run_string :: (Score.Event -> a) -> Text -> [UiTest.TrackSpec] -> ([a], [Text])
run_string extract call =
    DeriveTest.extract extract
        . DeriveTest.derive_tracks (title <> " | " <> call)

title :: Text
title = "import idiom.string | open-strings = (list (4c) (4d) (4e) (4g) (4a))"

e_nns :: Score.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
e_nns e = (Score.event_start e, Seq.drop_dups snd $ DeriveTest.e_nns e)
    -- 'merge_curve' can't see that pitches are the same, so it can produce
    -- duplicate pitches.

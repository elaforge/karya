-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Idiom.String_test where
import qualified Util.Num as Num
import qualified Derive.DeriveT as DeriveT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_attack_release :: Test
test_attack_release = do
    let run = runString eNns "bent-string 2 2 1" . UiTest.noteTrack
    let track p1 p2 p3 = [(0, 5, p1), (5, 5, p2), (10, 5, p3)]

    let (res, logs) = run $ track "4c" "2d" "2e"
    equal res [(0, [(0, NN.c4), (8, NN.c4)])]
    stringsLike logs ["below lowest string: 38nn", "below lowest string: 40nn"]
    -- All separate strings doesn't do anything interesting.
    equal (run $ track "4c" "4d" "4e")
        ([(0, [(0, NN.c4), (8, NN.c4)])
        , (5, [(5, 62), (13, 62)])
        , (10, [(10, 64), (18, 64)])
        ], [])
    equal (run $ track "4c" "4c#" "4d")
        ( [ (0, [(0, 60), (3, 60), (5, 61)]) -- bend up for attack
          , (5, [(5, 61), (11, 61), (13, 60)]) -- bend back down for release
          , (10, [(10, NN.d4), (18, NN.d4)])
          ]
        , []
        )
    stringsLike (snd $ DeriveTest.extract id $ DeriveTest.deriveTracks
            "import idiom.string | bent-string" [(">", [(0, 1, "")])])
        ["open-strings required"]

    -- Notes closer than the transition time still play properly.
    equal (run [(1, 1, "4d"), (2, 1, "4d")])
        ([(1, [(1, 62), (2, 62)]), (2, [(2, 62), (6, 62)])], [])

test_string_select :: Test
test_string_select = do
    let run = runString extract "bent-string 0 0 1"
        extract e = (eString e, eNns e)
    equal (run $ UiTest.noteTrack [(0, 1, "4c")])
        ([("0", (0, [(0, NN.c4), (2, NN.c4)]))], [])
    equal (run $ UiTest.noteTrack [(0, 1, "4d")])
        ([("1", (0, [(0, NN.d4), (2, NN.d4)]))], [])
    -- The string is assigned based on the lowest pitch.
    equal (run [(">", [(0, 2, "")]), ("*", [(0, 0, "4d"), (1, 0, "4c")])])
        ([("0", (0, [(0, NN.d4), (1, NN.d4), (1, NN.c4), (3, NN.c4)]))], [])


-- TODO I have to test this ad-hoc per call until I figure out a better
-- solution, see NOTE [signal-discontinuity]
test_signal_discontinuity :: Test
test_signal_discontinuity = do
    let run = DeriveTest.extract DeriveTest.eNns
            . DeriveTest.deriveTracks (title <> " | bent-string 0 0 1")
    equal (run [(">", [(0, 2, "")]), ("*", [(0, 0, "4c"), (1, 0, "4d")])])
        ([[(0, NN.c4), (1, NN.c4), (1, NN.d4), (3, NN.d4), (3, NN.c4)]], [])

test_mute_end :: Test
test_mute_end = do
    let run args = runString extract ("mute-end " <> args) . UiTest.noteTrack
        extract e = (DeriveTest.eStartDur e, Score.initialDynamic e,
            DeriveTest.eAttributes e)
    equal (run "1 1 .5" [(0, 1, "3c")])
        ([((0, 1), 1, "+"), ((1, 1), 0.5, "+"), ((2, 0), 1, "+mute")], [])
    equal (run "1 1 .5" [(0, 1, "+ring -- 3c")])
        ([((0, 1), 1, "+ring")], [])
    equal (run "1 0 .5" [(0, 1, "3c")])
        ([((0, 1), 1, "+"), ((1, 0), 1, "+mute")], [])

test_gliss :: Test
test_gliss = do
    let run gliss dest = DeriveTest.extract Score.initialNn $
            DeriveTest.deriveTracks title
            [ ("> | scale=twelve-r | key=g-maj", [(4, 1, gliss)])
            , ("*", [(4, 0, dest)])
            ]
    -- Pitches are correct for relative scale.
    equal (run "gliss -1 1" "4s") ([Just NN.e4, Just NN.g4], [])

test_gliss_a :: Test
test_gliss_a = do
    let run gliss dest = DeriveTest.extract extract $
            DeriveTest.deriveTracks title
            [(">", [(4, 1, gliss)]), ("*", [(4, 0, dest)])]
        extract e = (DeriveTest.eNote e, Score.initialDynamic e)
    equal (run "gliss-a 2 1 .5" "4f")
        ([((3, 0.5, "4a"), 0.5), ((3.5, 0.5, "4g"), 0.75), ((4, 1, "4f"), 1)],
            [])
    equal (run "gliss-a -2 1" "4f")
        ([((3, 0.5, "4d"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4f"), 1)], [])
    equal (run "gliss-a 2 1" "4d")
        ([((3, 0.5, "4g"), 1), ((3.5, 0.5, "4e"), 1), ((4, 1, "4d"), 1)], [])

    let run2 gliss dest = DeriveTest.extract Score.eventStart $
            DeriveTest.deriveTracks title
            [ ("tempo", [(0, 0, "1"), (2, 0, "2")]), (">", [(4, 1, gliss)])
            , ("*", [(4, 0, dest)])
            ]
    -- 0   1   2   3   4
    -- 0   1   2   2.5 3
    equal (run2 "gliss-a 2 1t" "4f") ([2.5, 2.75, 3], [])
    equal (run2 "gliss-a 2 1s" "4f") ([2, 2.5, 3], [])

test_nth_harmonic :: Test
test_nth_harmonic = do
    let run call pitch = DeriveTest.extract DeriveTest.eNnRounded $
            DeriveTest.deriveTracks title $
            UiTest.noteTrack [(0, 1, call <> " -- " <> pitch)]
    stringsLike (snd $ run "on 0" "4c") ["expected Signal (>0)"]
    equal (run "string=(4c) | on 1" "4c") ([NN.c4], [])
    equal (run "string=(4c) | on 2" "4c") ([NN.c5], [])
    equal (run "string=(4c) | on 3" "4c") ([harmonic 3 NN.c4], [])

test_harmonic :: Test
test_harmonic = do
    let run call pitch = DeriveTest.extract extract $
            DeriveTest.deriveTracks
                "import idiom.string | open-strings = (list (4c) (5c))" $
            UiTest.noteTrack [(0, 1, call <> " -- " <> pitch)]
        extract e = (DeriveTest.eNnRounded e, eString e)
    -- Pick the harmonic, even though an open string is available.
    equal (run "o" "5c") ([(harmonic 2 NN.c4, "4c")], [])
    -- 1st harmonic not allowed unless I say so.
    stringsLike (snd $ run "o" "4c") ["can't find"]
    equal (run "harmonic-h1=t | o" "4c") ([(NN.c4, "4c")], [])

    -- Pick lowest harmonic.
    equal (run "o" "6c") ([(harmonic 2 NN.c5, "5c")], [])
    equal (run "o" "5g") ([(harmonic 3 NN.c4, "4c")], [])
    -- Unless I force a string.
    equal (run "string=(4c) | o" "6c") ([(harmonic 4 NN.c4, "4c")], [])

harmonic :: Int -> Pitch.NoteNumber -> Pitch.NoteNumber
harmonic n = Num.roundDigits 2 . Pitch.modifyHz (* fromIntegral n)

eString :: Score.Event -> Text
eString = maybe "" showVal . DeriveTest.eEnvironVal EnvKey.string

showVal :: DeriveT.Val -> Text
showVal val = maybe (ShowVal.showVal val) showPitch $ PSignal.pitchVal val

showPitch :: PSignal.Pitch -> Text
showPitch = either pretty Pitch.noteText . PSignal.pitchNote . PSignal.coerce

runString :: (Score.Event -> a) -> Text -> [UiTest.TrackSpec] -> ([a], [Text])
runString extract call =
    DeriveTest.extract extract
        . DeriveTest.deriveTracks (title <> " | " <> call)

title :: Text
title = "import idiom.string | open-strings = (list (4c) (4d) (4e) (4g) (4a))"

eNns :: Score.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
eNns e = (Score.eventStart e, stripFlat $ DeriveTest.eNns e)
    -- 'merge_curve' can't see that pitches are the same, so it can produce
    -- duplicate pitches.

stripFlat :: Eq y => [(x, y)] -> [(x, y)]
stripFlat ((x1, y1) : (_, y2) : (x3, y3) : xs)
    | y1 == y2 && y2 == y3 = stripFlat ((x1, y1) : (x3, y3) : xs)
stripFlat (x:xs) = x : stripFlat xs
stripFlat [] = []

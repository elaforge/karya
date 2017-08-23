-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Articulation_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Prelude.Articulation as Articulation
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Lilypond.LilypondTest as LilypondTest
import Global


test_harmonic_ly = do
    let run p = LilypondTest.measures ["harmonic"] $
            LilypondTest.derive_tracks $ UiTest.note_track [(0, 4, p)]
    equal (run "o nat -- 3c") (Right "c1-\\flageolet", [])
    strings_like (snd $ run "string=(nn c3) | o -- 3c")
        ["can't find 48nn as a harmonic of 48nn"]
    equal (run "o nat -- 4c") (Right "c'1-\\flageolet", [])
    equal (run "harmonic-force-diamond=t | o nat -- 4c")
        (Right "<c c'\\harmonic>1", [])
    equal (run "string=(nn c3) | o nat -- 4g") (Right "<c g\\harmonic>1", [])
    equal (run "string=(nn c3) | o nat -- 5c") (Right "<c f\\harmonic>1", [])

test_slur = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract = DeriveTest.e_note
    let (evts, logs) = run $ (">", [(1, 3, "( .5 .5")]) : UiTest.regular_notes 5
    equal logs []
    equal evts
        [ (0, 1, "3c")
        , (1, 1.5, "3d"), (2, 1.5, "3e"), (3, 0.5, "3f")
        , (4, 1, "3g")
        ]

test_attr_slur = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_setup
                (with_call <> DeriveTest.with_linear) ""
        with_call = CallTest.with_note_generator "(" Articulation.c_attr_slur
        extract e = (s, d, p, a)
            where
            ((s, d, p), a) = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    let (events, logs) = run $ (">", [(0, 2, "( .5")]) : UiTest.regular_notes 3
    equal logs []
    equal events
        [ (0, 1.02, "3c", "+legato"), (1, 0.5, "3d", "+legato")
        , (2, 1, "3e", "+")
        ]

test_slur_ly = do
    let run = LilypondTest.measures [] . LilypondTest.derive_tracks_linear
    equal (run $ (">", [(1, 2, "(")]) : UiTest.regular_notes 4)
        (Right "c4 d4( e4) f4", [])
    -- Arg is ignored in lilypond mode.
    equal (run $
            (">", [(0, 4, "( .5")]) : UiTest.note_track1 ["3a", "3b", "3c"])
        (Right "a4( b4 c4) r4", [])
    let tracks =
            -- Irregularly overlapping sub events.
            [ (">", [(0, 3, "("), (3, 3, "(")])
            , (">", [(0, 2, "+legato"), (2, 1, "+sul"), (3, 3, "+sul")])
            ] ++ UiTest.regular_notes 6
    equal (run tracks)
        (Right "c4( d4 e4) f4( | g4 a4) r2", [])
    -- Goes to the last note in a tie.
    let tracks =
            [ (">", [(0, 12, "(")])
            , (">", [(0, 4, ""), (4, 8, "")])
            , ("*", [(0, 0, "4a"), (4, 0, "4b")])
            ]
    equal (run tracks) (Right "a'1( | b'1~ | b'1)", [])
    -- Legato on a single note does nothing.
    equal (run $ (">", [(0, 1, "(")]) : UiTest.regular_notes 1)
        (Right "c4 r4 r2", [])

test_attributed_note_ly = do
    let run = LilypondTest.measures [] . LilypondTest.derive_tracks_linear
    -- Works as a note transformer.
    equal (run $
        (">", [(0, 2, "m")]) : UiTest.note_track
            [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c")])
        (Right "a'4-+ b'4-+ c'4 r4", [])
    -- Works as both a transformer and a generator.
    equal (run $
        [ (">", [(0, 1, "m |"), (1, 1, ""), (2, 1, "m")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b"), (2, 0, "4c")])
        ])
        (Right "a'4-+ b'4 c'4-+ r4", [])

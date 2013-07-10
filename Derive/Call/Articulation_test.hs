-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Articulation_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Articulation as Articulation
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Call.Note as Note
import qualified Derive.DeriveTest as DeriveTest

import qualified Perform.Lilypond.LilypondTest as LilypondTest


test_legato = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear
        extract e = (s, d, p, a)
            where
            ((s, d, p), a) = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    let (evts, logs) = run
            [ ("> | %legato-overlap = .5", [(1, 3, "(")])
            , (">", [(n, 1, "") | n <- Seq.range 0 4 1])
            , ("*", [(n, 0, p) | (n, p) <-
                zip (Seq.range_ 0 1) ["4a", "4b", "4c", "4d", "4e"]])
            ]
    equal logs []
    equal evts
        [ (0, 1, "4a", "+")
        , (1, 1.5, "4b", "+legato")
        , (2, 1.5, "4c", "+legato")
        , (3, 1, "4d", "+")
        , (4, 1, "4e", "+")
        ]

    -- Legato events are extended to always overlap.
    let (evts, logs) = run
            [ (">", [(0, 4, "(")])
            , (">", [(0, 1, ""), (2, 1, ""), (4, 1, "")])
            ]
    equal logs []
    equal evts
        [ (0, 2.1, "?", "+legato")
        , (2, 1, "?", "+")
        , (4, 1, "?", "+")
        ]

test_attr_legato = do
    let run = DeriveTest.extract extract
            . DeriveTest.derive_tracks_with_ui with DeriveTest.with_linear
        with = CallTest.with_note_call "(" Articulation.c_attr_legato
            . CallTest.with_note_call "" (Note.note_call "" "" mempty
                (Note.default_note note_config))
        note_config = Note.use_attributes { Note.config_legato = False }
        extract e = (s, d, p, a)
            where
            ((s, d, p), a) = (DeriveTest.e_note e, DeriveTest.e_attributes e)
    let (events, logs) = run $ (">", [(0, 2, "( .5")]) : UiTest.regular_notes 3
    equal logs []
    equal events
        [ (0, 1.02, "3c", "+legato"), (1, 0.5, "3d", "+legato")
        , (2, 1, "3e", "+")
        ]

test_legato_ly = do
    let run = LilypondTest.measures [] . LilypondTest.derive_linear
    equal (run $ (">", [(1, 2, "(")]) : UiTest.regular_notes 4)
        (Right "c4 d4( e4) f4", [])
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
    let run = LilypondTest.measures [] . LilypondTest.derive_linear
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

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Parent_test where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.Call.Prelude.Parent as Parent
import qualified Derive.Call.Sub as Sub
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Lilypond.LilypondTest as LilypondTest

import Global


test_tuplet = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.derive_tracks_linear ""

    let tracks =
            [ (">", [(0, 12, "t")])
            , ("*", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            ]
    equal (run tracks) [(0, 4, "4c"), (4, 4, "4d"), (8, 4, "4e")]

    -- tuplet + inversion
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            , ("*", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            ]
    equal (run tracks) [(0, 4, "4c"), (4, 4, "4d"), (8, 4, "4e")]

    -- notes of unequal length
    let tracks =
            [ (">", [(0, 6, "t")])
            , (">", [(0, 1, ""), (1, 2, "")])
            ]
    equal (run tracks) [(0, 2, "?"), (2, 4, "?")]

    let tracks =
            [ (">", [(12, 12, "t")])
            , (">", [(12, 3, ""), (15, 3, ""), (18, 3, "")])
            ]
    equal (run tracks) [(12, 4, "?"), (16, 4, "?"), (20, 4, "?")]

    -- longer than tuplet is shrunk
    equal (run [(">", [(0, 1, "t")]), (">", [(0, 2, "")])]) [(0, 1, "?")]

    -- Leading space is a rest.
    equal (run [(">", [(0, 8, "t")]), (">", [(2, 1, ""), (3, 1, "")])])
        [(4, 2, "?"), (6, 2, "?")]

    -- All zero duration notes will infer a duration.
    equal (run $ (">", [(0, 4, "t")])
            : UiTest.note_track [(0, 0, "4c"), (1, 0, "4d")])
        [(0, 0, "4c"), (2, 0, "4d")]

test_tuplet_multiple_tracks = do
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_setup
                (DeriveTest.with_skel [(1, 2), (1, 3)]) ""
        extract e = (Score.event_instrument e, Score.event_start e,
            Score.event_duration e)
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">i1", [(0, 1, ""), (1, 1, "")])
            , (">i2", [(0, 1, "")])
            ]
    let i1 = Score.Instrument "i1"
        i2 = Score.Instrument "i2"
    equal (run tracks) [(i1, 0, 6), (i2, 0, 6), (i1, 6, 6)]

test_tuplet_ly = do
    let run skel = LilypondTest.measures ["times"]
            . LilypondTest.derive_tracks_setup (DeriveTest.with_skel skel)
    equal (run [(1, 2), (2, 3)] $
            (">", [(0, 4, "t")]) : UiTest.note_track1 ["4c", "4d", "4e"])
        (Right "\\times 2/3 { c'2 d'2 e'2 }", [])
    equal (run [(1, 2), (2, 3), (1, 4), (4, 5)] $
            (">", [(0, 4, "t")]) : concatMap UiTest.note_track
                [ [(0, 1, "4c"), (1, 1, "4d"), (2, 1, "4e")]
                , [(1, 1, "5d")]
                ])
        (Right "\\times 2/3 { c'2 <d' d''>2 e'2 }", [])

test_tuplet_ly_complex = do
    let run = LilypondTest.measures ["times", "acciaccatura"]
            . LilypondTest.derive_tracks_linear
        pitches = map ("3"<>) (map Text.singleton "abcdefg")
        notes dur ts =
            UiTest.note_track [(t, dur, p) | (t, p) <- zip ts pitches]
    -- The tuplet is not confused by a pitch already being in scope.
    equal (run $
        ("*", [(0, 0, "3c")]) : (">", [(0, 2, "t")]) : notes 0.5 [0, 0.5, 1])
        (Right "\\times 2/3 { a4 b4 c4 } r2", [])
    equal (run $ (">", [(2, 2, "t")]) : notes 0.5 [2, 2.5, 3])
        (Right "r2 \\times 2/3 { a4 b4 c4 }", [])
    equal (run $ (">", [(0, 2, "t")]) : notes 0.25 (Seq.range 0 1.25 0.25))
        (Right "\\times 4/6 { a8 b8 c8 d8 e8 f8 } r2", [])

    equal (run $ (">", [(0, 4, "t")]) : notes 0.5 (Seq.range 0 2 0.5))
        (Right "\\times 4/5 { a4 b4 c4 d4 e4 }", [])
    equal (run $ (">", [(0, 2, "t")]) : notes 0.25 (Seq.range 0 1 0.25))
        (Right "\\times 4/5 { a8 b8 c8 d8 e8 } r2", [])

    -- Ensure Lily.note_pitch preserves enharmonics.
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 1 0.5)
                ["3c#", "3db", "3cx"]])
        (Right "\\times 2/3 { cs4 df4 css4 } r2", [])

    -- Grace notes nested inside a tuplet work.
    equal (run
        [ (">", [(0, 2, "t")])
        , (">", [(0, 1, "g (3c) (3b)"), (1, 1, "")])
        , ("*", [(0, 0, "3a"), (1, 0, "3b")])
        ])
        (Right "\\times 1/2 { \\acciaccatura { c8[ b8] } a2 b2 } r2", [])

    -- Slur inside a tuplet.
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [ (0, 0.5, "3a"), (0.5, 0.5, "ly-( | -- 3b"), (1, 0.5, "3c"),
                (2, 1, "ly-) | -- 3d")])
        (Right "\\times 2/3 { a4 b4( c4 } d4) r4", [])

    equal (run $
            (">", [(0, 4, "t")])
            : (">", [(0, 3, "+stac")])
            : UiTest.regular_notes 3)
        (Right "\\times 2/3 { c2-. d2-. e2-. }", [])

    -- 0 dur code event doesn't confuse it.
    equal (run $ (">", [(0, 4, "t")]) : UiTest.note_track
            [(0, 1, "dyn p | ly-( | -- 3a"), (1, 1, "3b"), (2, 1, "3c")])
        (Right "\\times 2/3 { a2( \\p b2 c2 }", [])

test_arpeggio = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.derive_tracks_setup
                (DeriveTest.with_skel [(1, 2), (2, 3), (1, 4), (4, 5)]) ""
    let tracks arp =
            [ (">", [(10, 0, arp)])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, "4c")])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, "4d")])
            ]
    equal (run (tracks "`arp-up` 1 0")) [(10, 10, "4c"), (11, 9, "4d")]
    equal (run (tracks "`arp-down` 1 0")) [(10, 10, "4d"), (11, 9, "4c")]

test_slur_ly = do
    let run = LilypondTest.measures [] . LilypondTest.derive_tracks_linear
    equal (run $ (">", [(0, 4, "(")]) : UiTest.note_track
        [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c")])
        (Right "a'4( b'4 c'4) r4", [])

test_interpolate = do
    let run at = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_tracks_setup (DeriveTest.with_skel skel) ""
                [ ("at", at)
                , (">", [(4, 4, "interpolate")])
                , (">", [(4, 2, ""), (6, 1, "")])
                , ("*", [(4, 0, "4c"), (6, 0, "4d")])
                , (">", [(4, 1, ""), (7, 1, "")])
                , ("*", [(4, 0, "4d"), (7, 0, "4e")])
                ]
            where skel = [(1, 2), (2, 3), (3, 4), (2, 5), (5, 6)]
    equal (run [(4, 0, "0")]) ([(4, 2, "4c"), (6, 1, "4d")], [])
    equal (run [(4, 0, ".5")]) ([(4, 1.5, "4c"), (6.5, 1, "4d")], [])
    equal (run [(4, 0, "1")]) ([(4, 1, "4d"), (7, 1, "4e")], [])
    equal (run [(4, 0, "0"), (6, 0, "1")]) ([(4, 2, "4c"), (7, 1, "4e")], [])

test_interpolate_events = do
    let f at = fmap unevent . Parent.interpolate_events at . map mkevent
        mkevent (s, d, n) = Sub.Event s d n
        unevent (Sub.Event s d n) = (s, d, n)
        events = [(0, 1, 'a'), (1, 2, 'b'), (2, 3, 'c'), (3, 4, 'd')]
    equal (f 0 (take 1 events)) $ Just (0, 1, 'a')
    equal (f 1 (take 1 events)) $ Just (0, 1, 'a')
    -- x-----x
    equal (f 0.5 (take 2 events)) $ Just (0.5, 1.5, 'a')
    -- x--x--x
    equal (f 0.5 (take 3 events)) $ Just (1, 2, 'b')
    -- x-x-x-x
    equal (f 0.5 (take 4 events)) $ Just (1.5, 2.5, 'b')
    equal (f 2 (take 4 events)) $ Just (3, 4, 'd')

test_cycle = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run
            [ (">", [(1, 4, "cycle +a +b")])
            , (">", [(t, 1, "") | t <- Seq.range 1 4 1])
            ])
        ([(1, "+a"), (2, "+b"), (3, "+a"), (4, "+b")], [])

test_cycle_t = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run
            [ (">", [(1, 6, "cycle-t +a 2 +b 1")])
            , (">", [(t, 1, "") | t <- Seq.range 1 6 1])
            ])
        ([(1, "+a"), (2, "+a"), (3, "+b"), (4, "+a"), (5, "+a"), (6, "+b")], [])

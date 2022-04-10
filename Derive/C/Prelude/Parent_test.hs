-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.C.Prelude.Parent_test where
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.C.Prelude.Parent as Parent
import qualified Derive.Call.SubT as SubT
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Lilypond.LilypondTest as LilypondTest
import qualified Ui.UiTest as UiTest

import           Util.Test


test_tuplet :: Test
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

test_tuplet_multiple_tracks :: Test
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
    let i1 = "i1"
        i2 = "i2"
    equal (run tracks) [(i1, 0, 6), (i2, 0, 6), (i1, 6, 6)]

test_tuplet_ly :: Test
test_tuplet_ly = do
    let run = LilypondTest.measures ["tuplet"]
            . LilypondTest.derive_tracks_linear

    equal (run $ (">", [(0, 4, "t")]) : UiTest.note_track1 ["3c", "3d", "3e"])
        (Right "\\tuplet 3/2 { c2 d2 e2 }", [])

    -- Leading rests.
    equal (run $ (">", [(0, 4, "t")]) : UiTest.note_track [(2, 1, "3c")])
        (Right "\\tuplet 3/2 { r1 c2 }", [])

    -- -- Trailing rests.
    -- TODO I can't actually express this with the 't' call since I don't have
    -- explicit rests.  I'd need it to specially recognize -- or something.
    -- equal (run $ (">", [(0, 4, "t")])
    --         : UiTest.note_track [(1, 1, "3c"), (2, 0, "--")])
    --     (Right "\\tuplet 3/2 { r2 c2 r2 }", [])

    let run_skel skel = LilypondTest.measures ["tuplet"]
            . LilypondTest.derive_tracks_setup (DeriveTest.with_skel skel)
    equal (run_skel [(1, 2), (2, 3), (1, 4), (4, 5)] $
            (">", [(0, 4, "t")]) : concatMap UiTest.note_track
                [ [(0, 1, "3c"), (1, 1, "3d"), (2, 1, "3e")]
                , [(1, 1, "4d")]
                ])
        (Right "\\tuplet 3/2 { c2 <d d'>2 e2 }", [])

    -- While triplets make the notes go faster, duplets make them go slower.
    equal (run  $ (">", [(0, 3, "t")]) : UiTest.note_track1 ["3c", "3d"])
        (Right "\\tuplet 2/3 { c4 d4 } r4", [])

    -- -- Nested triplets.
    -- -- 0   .   1   .   2   .   3   .   4   .   |
    -- -- t------------------------------>
    -- --         t-------------->
    -- -- c------>d-->e-->f-->
    -- -- TODO this doesn't work.  It's because tuplet_note_end sees the end of
    -- -- 'f' and gets 2.5, when it should have applied the 't' above it and
    -- -- gotten 3.
    -- equal (run $ [(">", [(0, 4, "t")]), (">", [(1, 2, "t")])]
    --         ++ UiTest.note_track [(0, 1, "3c"), (1, 0.5, "3d"),
    --             (1.5, 0.5, "3e"), (2, 0.5, "3f"),
    --             (4, 1, "3g")])
    --     (Right "\\tuplet 2/3 { c2 \\tuplet 2/3 { d2 e2 f2 } } | g2 r2", [])

test_tuplet_ly_articulations :: Test
test_tuplet_ly_articulations = do
    -- Slurs and articulations work on tuplets.
    let run = LilypondTest.measures ["tuplet"]
            . LilypondTest.derive_tracks_linear
    let parents call = [(">", [(0, 4, call)]), (">", [(0, 4, "t")])]
    equal (run $ parents "(" ++ UiTest.regular_notes 3)
        (Right "\\tuplet 3/2 { c2 ( d2 e2 ) }", [])

    let notes = UiTest.note_track
            [(0, 1, "3c"), (1, 1, "3d"), (2, 1, "3e"), (4, 2, "3f")]
    -- putStrLn $ untxt $ UiTest.fmt_tracks $ parents "+pizz" ++ notes
    equal (run $ parents "+pizz" ++ notes)
        (Right "\\tuplet 3/2 { c2 ^\"pizz.\" d2 e2 } | f2 ^\"arco\" r2", [])

test_tuplet_ly_complex :: Test
test_tuplet_ly_complex = do
    let run = LilypondTest.measures ["tuplet", "acciaccatura", "p"]
            . LilypondTest.derive_tracks_linear
        pitches = map ("3"<>) (map Text.singleton "abcdefg")
        notes dur ts =
            UiTest.note_track [(t, dur, p) | (t, p) <- zip ts pitches]
    -- The tuplet is not confused by a pitch already being in scope.
    equal (run $
        ("*", [(0, 0, "3c")]) : (">", [(0, 2, "t")]) : notes 0.5 [0, 0.5, 1])
        (Right "\\tuplet 3/2 { a4 b4 c4 } r2", [])
    equal (run $ (">", [(2, 2, "t")]) : notes 0.5 [2, 2.5, 3])
        (Right "r2 \\tuplet 3/2 { a4 b4 c4 }", [])
    equal (run $ (">", [(0, 2, "t")]) : notes 0.25 (Seq.range 0 1.25 0.25))
        (Right "\\tuplet 6/4 { a8 b8 c8 d8 e8 f8 } r2", [])

    equal (run $ (">", [(0, 4, "t")]) : notes 0.5 (Seq.range 0 2 0.5))
        (Right "\\tuplet 5/4 { a4 b4 c4 d4 e4 }", [])
    equal (run $ (">", [(0, 2, "t")]) : notes 0.25 (Seq.range 0 1 0.25))
        (Right "\\tuplet 5/4 { a8 b8 c8 d8 e8 } r2", [])

    -- Ensure Lily.note_pitch preserves enharmonics.
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 1 0.5)
                ["3c#", "3db", "3cx"]])
        (Right "\\tuplet 3/2 { cs4 df4 css4 } r2", [])

    -- Grace notes nested inside a tuplet work.
    equal (run $
        (">", [(0, 4, "t")]) : UiTest.note_track
            [(0, 1, "g (3c) (3b) -- 3a"), (1, 1, "3b"), (2, 1, "3c")])
        (Right "\\tuplet 3/2 { \\acciaccatura { c8[ b8] } a2 b2 c2 }", [])

    equal (run $
            (">", [(0, 4, "t")]) : (">", [(0, 3, "+stac")])
            : UiTest.regular_notes 3)
        (Right "\\tuplet 3/2 { c2 -. d2 -. e2 -. }", [])

    -- 0 dur code event doesn't confuse it.
    equal (run $ (">", [(0, 4, "t")]) : UiTest.note_track
            [(0, 1, "dyn p | ly-( | -- 3a"), (1, 1, "3b"), (2, 1, "3c")])
        (Right "\\tuplet 3/2 { a2 ( \\p b2 c2 }", [])

test_arpeggio :: Test
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

test_arpeggio_ly :: Test
test_arpeggio_ly = do
    let run skel = LilypondTest.measures ["arpeggioArrowUp", "arpeggio"]
            . LilypondTest.derive_tracks_setup (DeriveTest.with_skel skel)
    equal (run [(1, 2), (2, 3), (1, 4), (4, 5)]
            [ (">", [(0, 0, "`arp-up`")])
            , (">", [(0, 4, "")]), ("*", [(0, 0, "3c")])
            , (">", [(0, 4, "")]), ("*", [(0, 0, "3d")])
            ])
        (Right "\\arpeggioArrowUp <c d>1 \\arpeggio", [])

test_interpolate :: Test
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

test_interpolate_subs :: Test
test_interpolate_subs = do
    let f at = fmap unevent . Parent.interpolate_subs at . map mkevent
        mkevent (s, d, n) = SubT.EventT s d n
        unevent (SubT.EventT s d n) = (s, d, n)
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

test_event_interpolate :: Test
test_event_interpolate = do
    let run at = DeriveTest.extract Score.event_start $
            DeriveTest.derive_tracks_setup (DeriveTest.with_skel skel) ""
                [ ("at", UiTest.control_track at)
                , (">", [(4, 4, "e-interpolate")])
                , (">", [(4, 2, ""), (6, 1, "")])
                , (">", [(4, 1, ""), (7, 1, "")])
                ]
            where skel = [(1, 2), (2, 3), (2, 4)]
    equal (run [(4, "0")]) ([4, 7], [])
    equal (run [(4, ".5")]) ([4, 6.5], [])
    equal (run [(4, "1")]) ([4, 6], [])

    let run = DeriveTest.extract Score.event_start . DeriveTest.derive_blocks
    let blocks at call =
            [ ("top", [("at", UiTest.control_track at), (">", [call])])
            , ("b1=ruler", [(">", [(0, 1, ""), (1, 1, "")])])
            , ("b2=ruler", [(">", [(0, 1, ""), (1.5, 0.5, "")])])
            ]
    equal (run $ blocks [(0, ".5")] (4, 4, "e-interpolate b1 b2"))
        ([4, 6.5], [])
    let loop = (0, 6, "e-interpolate \"(loop | b1) \"(loop | b2)")
    equal (run $ blocks [(0, "0")] loop) ([0, 1, 2, 3, 4, 5], [])
    equal (run $ blocks [(0, "1")] loop) ([0, 1.5, 2, 3.5, 4, 5.5], [])
    equal (run $ blocks [(0, "0"), (2, ".5"), (4, "1")] loop)
        ([0, 1, 2, 3.25, 4, 5.5], [])

test_cycle :: Test
test_cycle = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run
            [ (">", [(1, 4, "cycle +a +b")])
            , (">", [(t, 1, "") | t <- Seq.range 1 4 1])
            ])
        ([(1, "+a"), (2, "+b"), (3, "+a"), (4, "+b")], [])

test_cycle_t :: Test
test_cycle_t = do
    let run = DeriveTest.extract extract . DeriveTest.derive_tracks_linear ""
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
    equal (run
            [ (">", [(1, 6, "cycle-t +a 2 +b 1")])
            , (">", [(t, 1, "") | t <- Seq.range 1 6 1])
            ])
        ([(1, "+a"), (2, "+a"), (3, "+b"), (4, "+a"), (5, "+a"), (6, "+b")], [])

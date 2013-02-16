module Derive.Call.NoteTransformer_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.Lilypond.LilypondTest as LilypondTest
import Perform.Lilypond.LilypondTest (convert_staves)


test_tuplet = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.linear_derive_tracks id

    let tracks =
            [ (">", [(0, 12, "t")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            ]
    equal (run tracks) [(0, 4, "4c"), (4, 4, "4d"), (8, 4, "4e")]

    -- tuplet + inversion
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
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

    -- leading space is ignored (what should it mean?)
    equal (run [(">", [(0, 4, "t")]), (">", [(2, 1, ""), (3, 1, "")])])
        [(0, 2, "?"), (2, 2, "?")]

    -- not really testing tuplet: make sure empty tracks are stripped
    equal (run [(">", [(0, 1, "")]), (">", []), ("*twelve", [(0, 0, "4c")])])
        [(0, 1, "4c")]

test_tuplet_multiple_tracks = do
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_with_ui id
                (DeriveTest.set_skel [(1, 2), (1, 3)])
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
    let run = first (convert_staves ["times", "acciaccatura"])
            . LilypondTest.derive_linear True id
        pitches = map ('4':) (map (:"") "abcdefg")
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 1 0.5) pitches])
        (Right ["\\times 2/3 { a'4 b'4 c'4 } r2"], [])
    equal (run $
        (">", [(2, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 2 3 0.5) pitches])
        (Right ["r2 \\times 2/3 { a'4 b'4 c'4 }"], [])
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.25, p) | (t, p) <- zip (Seq.range 0 1.25 0.25) pitches])
        (Right ["\\times 4/6 { a'8 b'8 c'8 d'8 e'8 f'8 } r2"], [])

    equal (run $
        (">", [(0, 4, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 2 0.5) pitches])
        (Right ["\\times 4/5 { a'4 b'4 c'4 d'4 e'4 }"], [])
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.25, p) | (t, p) <- zip (Seq.range 0 1 0.25) pitches])
        (Right ["\\times 4/5 { a'8 b'8 c'8 d'8 e'8 } r2"], [])
    -- Ensure Lily.note_pitch preserves enharmonics.
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 1 0.5)
                ["4c#", "4db", "4cx"]])
        (Right ["\\times 2/3 { cs'4 df'4 css'4 } r2"], [])

    -- Grace notes nested inside a tuplet work.
    equal (run
        [ (">", [(0, 2, "t")])
        , (">", [(0, 1, "g (4c) (4b)"), (1, 1, "")])
        , ("*", [(0, 0, "4a"), (1, 0, "4b")])
        ])
        (Right ["\\times 1/2 { \\acciaccatura { c'8[ b'8] } a'2 b'2 } r2"], [])

test_arpeggio = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.derive_tracks_with_ui id
                (DeriveTest.set_skel [(1, 2), (2, 3), (1, 4), (4, 5)])
    let tracks arp =
            [ (">", [(10, 0, arp)])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, "4c")])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, "4d")])
            ]
    equal (run (tracks "`arp-up` 1")) [(10, 10, "4c"), (11, 9, "4d")]
    equal (run (tracks "`arp-down` 1")) [(10, 10, "4d"), (11, 9, "4c")]

test_slur_ly = do
    let run = first (LilypondTest.convert_staves [])
            . LilypondTest.derive_linear True id
    equal (run $ (">", [(0, 4, "(")]) : UiTest.note_track
        [(0, 1, "4a"), (1, 1, "4b"), (2, 1, "4c")])
        (Right ["a'4( b'4 c'4) r4"], [])

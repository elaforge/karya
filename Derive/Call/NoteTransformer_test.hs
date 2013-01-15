module Derive.Call.NoteTransformer_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.UiTest as UiTest
import qualified Derive.Call.NoteTransformer as NoteTransformer
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.Lilypond.Lilypond as Lilypond
import qualified Perform.Lilypond.LilypondTest as LilypondTest


test_tuplet = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.linear_derive_tracks id

    let tracks =
            [ (">", [(0, 12, "t")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            ]
    equal (run tracks) [(0, 4, 60), (4, 4, 62), (8, 4, 64)]

    -- tuplet + inversion
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            ]
    equal (run tracks) [(0, 4, 60), (4, 4, 62), (8, 4, 64)]

    -- notes of unequal length
    let none = -1 -- no pitch track
    let tracks =
            [ (">", [(0, 6, "t")])
            , (">", [(0, 1, ""), (1, 2, "")])
            ]
    equal (run tracks) [(0, 2, none), (2, 4, none)]

    let tracks =
            [ (">", [(12, 12, "t")])
            , (">", [(12, 3, ""), (15, 3, ""), (18, 3, "")])
            ]
    equal (run tracks) [(12, 4, none), (16, 4, none), (20, 4, none)]

    -- longer than tuplet is shrunk
    equal (run [(">", [(0, 1, "t")]), (">", [(0, 2, "")])]) [(0, 1, none)]

    -- leading space is ignored (what should it mean?)
    equal (run [(">", [(0, 4, "t")]), (">", [(2, 1, ""), (3, 1, "")])])
        [(0, 2, none), (2, 2, none)]

    -- not really testing tuplet: make sure empty tracks are stripped
    equal (run [(">", [(0, 1, "")]), (">", []), ("*twelve", [(0, 0, "4c")])])
        [(0, 1, 60)]

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

test_tuplet_code = do
    let f tuplet_dur note_dur pitches = NoteTransformer.tuplet_code
            tuplet_dur note_dur (map parse_pitch pitches)
    equal (f Lilypond.D2 Lilypond.D4 ["4a", "4b", "4c"])
        "\\times 2/3 { a'4 b'4 c'4 }"

test_tuplet_ly = do
    let run = first (LilypondTest.convert_staves ["times"])
            . LilypondTest.derive_linear True id
        pitches = map ('4':) (map (:"") "abcdefg")
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 1 0.5) pitches])
        (Right [["\\times 2/3 { a'4 b'4 c'4 }", "r2"]], [])
    equal (run $
        (">", [(0, 4, "t")]) : UiTest.note_track
            [(t, 0.5, p) | (t, p) <- zip (Seq.range 0 2 0.5) pitches])
        (Right [["\\times 4/5 { a'4 b'4 c'4 d'4 e'4 }"]], [])
    equal (run $
        (">", [(0, 2, "t")]) : UiTest.note_track
            [(t, 0.25, p) | (t, p) <- zip (Seq.range 0 1 0.25) pitches])
        (Right [["\\times 4/5 { a'8 b'8 c'8 d'8 e'8 }", "r2"]], [])
    -- prettyp $ LilypondTest.derive_linear True id $
    --     (">", [(0, 4, "t")]) : UiTest.note_track
    --         [(t, 0.25, p) | (t, p) <- zip (Seq.range 0 1 0.25) pitches]

test_arpeggio = do
    let run = DeriveTest.extract_events DeriveTest.e_note2
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

parse_pitch :: String -> Theory.Pitch
parse_pitch s = fromMaybe (error $ "can't parse pitch: " ++ show s) $
    Theory.read_pitch Twelve.layout s

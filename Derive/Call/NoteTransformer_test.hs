module Derive.Call.NoteTransformer_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


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
    let i1 = Just (Score.Instrument "i1")
        i2 = Just (Score.Instrument "i2")
    equal (run tracks) [(i1, 0, 6), (i2, 0, 12), (i1, 6, 6)]

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

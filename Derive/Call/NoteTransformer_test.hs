module Derive.Call.NoteTransformer_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


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
    let tracks =
            [ (">", [(0, 6, "t")])
            , (">", [(0, 1, ""), (1, 2, "")])
            ]
    equal (run tracks) [(0, 2, -1), (2, 4, -1)]

    let tracks =
            [ (">", [(12, 12, "t")])
            , (">", [(12, 3, ""), (15, 3, ""), (18, 3, "")])
            ]
    equal (run tracks) [(12, 4, -1), (16, 4, -1), (20, 4, -1)]

    -- not really testing tuplet: make sure empty tracks are stripped
    equal (run [(">", [(0, 1, "")]), (">", []), ("*twelve", [(0, 0, "4c")])])
        [(0, 1, 60)]


test_arpeggio = do
    let run = DeriveTest.extract_events DeriveTest.e_note
            . DeriveTest.derive_tracks_with_ui id
                (DeriveTest.set_skel [(1, 2), (2, 3), (1, 4), (4, 5)])
    let tracks arp n1 n2 =
            [ (">", [(10, 0, arp)])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, n1)])
            , (">", [(10, 10, "")])
            , ("*", [(10, 0, n2)])
            ]
    equal (run (tracks "`arp-up` 1" "4c" "4d"))
        [(10, 10, 60), (11, 9, 62)]
    equal (run (tracks "`arp-down` 1" "4c" "4d"))
        [(10, 10, 62), (11, 9, 60)]

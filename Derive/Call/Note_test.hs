module Derive.Call.Note_test where
import Util.Test
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score
import qualified Perform.PitchSignal as PitchSignal


test_tuplet = do
    let extract e = (Score.event_start e, Score.event_duration e,
            PitchSignal.unsignal_degree $ Score.event_pitch e)
    let run = DeriveTest.extract_events extract
            . DeriveTest.derive_tracks_with_ui id set_skel
        set_skel state = UiTest.exec state $
            State.set_skeleton UiTest.default_block_id $
                Skeleton.make [(1, 2), (2, 3)]

    let tracks =
            [ (">", [(0, 12, "t")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            ]
    equal (run tracks)
        [ (0, 4, [(0, 60)])
        , (4, 4, [(4, 62)])
        , (8, 4, [(8, 64)])
        ]

    -- tuplet + inversion
    let tracks =
            [ (">", [(0, 12, "t")])
            , (">", [(0, 3, ""), (3, 3, ""), (6, 3, "")])
            , ("*twelve", [(0, 0, "4c"), (3, 0, "4d"), (6, 0, "4e")])
            ]
    equal (run tracks)
        [ (0, 4, [(0, 60)])
        , (4, 4, [(4, 62)])
        , (8, 4, [(8, 64)])
        ]

    -- notes of unequal length
    let tracks =
            [ (">", [(0, 6, "t")])
            , (">", [(0, 1, ""), (1, 2, "")])
            ]
    equal (run tracks)
        [ (0, 2, [(0, -1)])
        , (2, 4, [(0, -1)])
        ]

    let tracks =
            [ (">", [(12, 12, "t")])
            , (">", [(12, 3, ""), (15, 3, ""), (18, 3, "")])
            ]
    equal (run tracks)
        [ (12, 4, [(0, -1)])
        , (16, 4, [(0, -1)])
        , (20, 4, [(0, -1)])
        ]

    -- not really testing tuplet: make sure empty tracks are stripped
    equal (run [(">", [(0, 1, "")]), (">", []), ("*twelve", [(0, 0, "4c")])])
        [(0, 1, [(0, 60)])]

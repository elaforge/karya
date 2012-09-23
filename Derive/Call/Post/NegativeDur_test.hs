module Derive.Call.Post.NegativeDur_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_negative_duration = do
    let extract = DeriveTest.extract DeriveTest.e_event
    let run evts = extract $ DeriveTest.derive_tracks
            [("> | negative-duration", evts)]

    let deflt = 1
    -- events get lined up
    equal (run [(1, -1, "--1"), (3, -2, "--2")])
        ([(1, 2, "--1"), (3, deflt, "--2")], [])

    -- rest
    equal (run [(1, -1, "--1"), (3, -1, "--2")])
        ([(1, 1, "--1"), (3, deflt, "--2")], [])
    -- 0 dur is treated as negative
    equal (run [(1, -1, "--1"), (3, 0, "--2")])
        ([(1, 2, "--1"), (3, deflt, "--2")], [])

    let run evts = extract $ DeriveTest.derive_blocks
            [ ("b1", [(">  | negative-duration", evts)])
            , ("sub", [(">", [(1, -1, "--1"), (2, -1, "--2")])])
            ]
    -- last event extends up to "rest" at 5
    equal (run [(4, -4, "sub"), (6, -1, "")])
        ([(2, 2, "--1"), (4, 1, "--2"), (6, deflt, "")], [])

    -- events between derivers work
    equal (run [(4, -4, "sub"), (8, -4, "sub")])
        ([(2, 2, "--1"), (4, 2, "--2"), (6, 2, "--1"), (8, deflt, "--2")], [])
    let run evts = extract $ DeriveTest.derive_blocks
            [ ("b1", [("> | negative-duration", evts)])
            , ("sub",
                [ (">i1", [(1, -1, "--11"), (2, -1, "--12")])
                , (">i2", [(1, -1, "--21")])
                ])
            ]
    -- as above but both last events are extended
    equal (run [(4, -4, "sub"), (6, -1, "")])
        ([(2, 2, "--11"), (2, 3, "--21"), (4, 1, "--12"), (6, deflt, "")], [])

    -- events between derivers work
    let (events, logs) = run [(4, -4, "sub"), (8, -4, "sub")]
    equal events
        [ (2, 2, "--11"), (2, 2, "--21"), (4, 2, "--12")
        , (6, 2, "--11"), (6, deflt, "--21"), (8, deflt, "--12")
        ]
    equal logs []

-- test_slice = do
--     let extract = DeriveTest.extract DeriveTest.e_note2
--     let (events, logs) = extract $ DeriveTest.derive_blocks
--             [ ("root",
--                 [ ("> | negative-duration", [(2, -2, "sub"), (4, -2, "sub")])
--                 , ("*", [(0, 0, "4c"), (2, 0, "4e")])
--                 ])
--             , ("sub",
--                 [ (">", [(1, -1, ""), (2, -1, "")])
--                 , ("t-diatonic", [(1, -0, "1"), (2, -0, "0")])
--                 ])
--             ]
--     equal logs []
--     equal events
--         [ (1, 1, "4d"), (2, 1, "4c")
--         , (3, 1, "4f"), (4, 1, "4e")
--         ]

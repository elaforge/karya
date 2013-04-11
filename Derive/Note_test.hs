module Derive.Note_test where
import Util.Test

import qualified Derive.DeriveTest as DeriveTest


test_sub_tracks = do
    let run = DeriveTest.derive_tracks
    let extract_c e = (DeriveTest.e_event e, DeriveTest.e_control "c1" e,
            DeriveTest.e_control "c2" e)
    let (events, logs) = DeriveTest.extract extract_c $ run
            [ (">", [(0, 2, "--1"), (2, 2, "--2")])
            , ("c1", [(0, 0, "0"), (1, 0, "1"), (2, 0, "2")])
            , ("c2", [(0, 0, "3"), (6, 0, "4")])
            ]
    equal logs []
    -- Unfortunately the comment is lost since the expression is recreated
    -- from the parsed version.
    equal events
        [ ((0, 2, ""), [(0, 0), (1, 1)], [(0, 3)])
        , ((2, 2, ""), [(2, 2)], [(0, 3), (6, 4)])
        ]

    let extract_p e = (DeriveTest.e_event e, DeriveTest.e_nns e)
    let (events, logs) = DeriveTest.extract extract_p $ run
            [ (">", [(0, 2, ""), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (2, 0, "4d"), (3, 0, "4e")])
            ]
    equal logs []
    equal events
        [ ((0, 2, ""), [(0, 60)])
        , ((2, 2, ""), [(2, 62), (3, 64)])
        ]

    -- controls that straddle the note are properly sliced and clipped
    let (events, _logs) = DeriveTest.extract extract_p $ run
            [ (">", [(0, 2, ""), (2, 2, "")])
            , ("*", [(0, 0, "4c"), (4, 0, "i (4d)")])
            ]
    equal events
        [ ((0, 2, ""), [(0, 60), (1, 60.5)])
        , ((2, 2, ""), [(2, 61), (3, 61.5), (4, 62)])
        ]

-- c_subs :: Derive.NoteCall
-- c_subs = Derive.generator "subs" $ \args -> do
--     let subs = Derive.info_sub_tracks (Derive.passed_info args)
--     Log.warn $ show subs
--     return []

-- * derivers

test_c_note = do
    -- Test basic Derive.d_note_track plumbing and the note (null) deriver
    -- along with it.
    let run title evts = DeriveTest.extract DeriveTest.e_everything $
            DeriveTest.derive_tracks [(title, evts)]
    let inst = "i"

    let evt s d = (s, d, "", inst, [])
    equal (run ">i" [(0, 1, ""), (1, 2, ""), (3, 3, "")])
        ([evt 0 1, evt 1 2, evt 3 3], [])

    let (evts, logs) = run ">i" [(0, 1, "n +a 42")]
    equal evts []
    strings_like logs ["expected Instrument or RelativeAttrs"]

    let (evts, logs) = run ">i" [(0, 1, "x (")]
    equal evts []
    strings_like logs ["parse error"]

    -- title error throws exception
    let (evts, logs) = run ">i $parse/err" [(0, 1, "")]
    equal evts []
    strings_like logs ["parse error"]

    -- comment only event is filtered out
    equal (run ">i" [(0, 1, "--")]) ([], [])
    equal (run ">" [(0, 1, "n >i +a")])
        ([(0, 1, "n >i +a", inst, ["a"])], [])
    equal (run ">i +a" [(0, 1, "")])
        ([(0, 1, "", inst, ["a"])], [])

    -- event overrides attrs
    equal (run "> +a" [(0, 1, "n =b"), (1, 1, "n -a")])
        ([ (0, 1, "n =b", "", ["b"])
        , (1, 1, "n -a", "", [])
        ], [])
    -- alternate syntax
    equal (run ">i" [(0, 1, ""), (1, 1, "n >i2 |")])
        ([ (0, 1, "", inst, [])
        , (1, 1, "n >i2 |", "i2", [])
        ], [])

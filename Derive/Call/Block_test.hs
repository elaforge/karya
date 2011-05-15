module Derive.Call.Block_test where
import Util.Test
import qualified Derive.DeriveTest as DeriveTest


test_c_block = do
    -- This also tests Derive.Call.Block.lookup_note_call
    let run evts = DeriveTest.extract DeriveTest.e_everything $
            DeriveTest.derive_blocks
                [ ("b1", [(">", evts)])
                , ("sub", [(">", [(0, 22, "--sub")])])
                ]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts []
    strings_like logs ["call not found: nosuch"]

    strings_like (snd (run [(0, 1, "sub >arg")]))
        ["args for block call not implemented yet"]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, "n >i +a | sub")]
    equal logs []
    equal evts
        [ (0, 1, "--sub", Nothing, [])
        , (1, 2, "--sub", Just "i", ["a"])
        ]


test_c_clip = do
    let extract = DeriveTest.extract DeriveTest.e_event
        run tracks = extract $ DeriveTest.derive_blocks tracks
    equal (run [("b1", [(">", [(0, 1, "clip b2")])])])
        ([], ["DeriveError: block not found: b2"])
    -- make sure out of range notes are clipped
    equal (run
        [ ("b1", [(">", [(0, 1, "clip b2")])])
        , ("b2", [(">", [(0, 1, ""), (1, 1, "")])])
        ])
        ([(0, 1, "")], [])
    -- the tempo of the block is not affected by the duration of the event
    equal (run
        [ ("b1", [(">", [(1, 2, "clip b2")])])
        , ("b2", [(">", [(0, 1, ""), (1, 1, "")])])
        ])
        ([(1, 1, ""), (2, 1, "")], [])

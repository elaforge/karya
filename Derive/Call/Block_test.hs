-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Block_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest


test_block = do
    -- This also tests Derive.Call.Block.lookup_note_call
    let run evts = DeriveTest.extract DeriveTest.e_everything $
            DeriveTest.derive_blocks
                [ ("b1", [(">", evts)])
                , ("sub=ruler", [(">", [(0, 22, "")])])
                ]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts []
    strings_like logs ["note generator or val call not found: nosuch"]

    strings_like (snd (run [(0, 1, "sub >arg")])) ["too many arguments"]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, "n >i +a | sub")]
    equal logs []
    equal evts
        [ (0, 1, "", "", [])
        , (1, 2, "", "i", ["a"])
        ]

test_relative_block = do
    let run call = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_blocks
            [ ("top", [(">", [(0, 1, call)])])
            , ("top.sub=ruler", UiTest.regular_notes 1)
            ]
    equal (run ".sub") ([(0, 1, "3c")], [])
    equal (run "top.sub") ([(0, 1, "3c")], [])
    equal (run "test/top.sub") ([(0, 1, "3c")], [])
    strings_like (snd (run ".bub")) ["call not found"]

test_clip = do
    let extract = DeriveTest.extract DeriveTest.e_event
        run tracks = extract $ DeriveTest.derive_blocks tracks
    equal (run [("b1", [(">", [(0, 1, "clip b2")])])])
        ([], ["Error: block not found: b2"])
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

test_clip_start = do
    let extract = DeriveTest.extract DeriveTest.e_note
        run tracks = extract $ DeriveTest.derive_blocks tracks
    -- Aligned to the end.
    equal (run
        [ ("b1", [(">", [(0, 2, "Clip b2")])])
        , ("b2=ruler", UiTest.regular_notes 1)
        ])
        ([(1, 1, "3c")], [])
    -- Get the last two notes.
    equal (run
        [ ("b1", [(">", [(0, 2, "Clip b2")])])
        , ("b2=ruler", UiTest.regular_notes 3)
        ])
        ([(0, 1, "3d"), (1, 1, "3e")], [])

test_control_block = do
    let extract = DeriveTest.e_control "cont"
        derive caller callee = DeriveTest.extract extract $
            DeriveTest.derive_blocks
                [ ("top", [("cont", caller), (">", [(0, 4, "")])])
                , ("sub=ruler", [("%", callee)])
                ]
        sub = [(0, 0, "1"), (16, 0, "2"), (32, 0, "4")]
    let (evts, logs) = derive [(0, 2, "nosuch")] []
    equal evts [[(0, 0)]]
    strings_like logs ["control generator or val call not found: nosuch"]

    -- The last sample is clipped off since it's at the end of the block.
    equal (derive [(0, 0, "0"), (1, 2, "sub"), (3, 0, "3")] sub)
        ([[(0, 0), (1, 1), (2, 2), (3, 3)]], [])

test_control_block_stack = do
    -- Ensure the stack is correct for control block calls.
    let blocks = [("top", top), ("sub", sub)]
        top =
            [ (">", [(3, 1, "")])
            , ("c", [(0, 2, "sub")])
            ]
        -- Failed call will produce a log msg, which has the stack.
        sub = [("%", [(0, 0, "no-call")])]
    let res = DeriveTest.derive_blocks blocks
    strings_like (map DeriveTest.show_log_stack (DeriveTest.r_logs res))
        ["top top.t2 0-2: sub sub.t1 0-0: Error"]

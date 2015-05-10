-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Block_test where
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score


test_block = do
    -- This also tests Derive.Call.Prelude.Block.lookup_note_call
    let run evts = DeriveTest.extract DeriveTest.e_everything $
            DeriveTest.derive_blocks
                [ ("b1", [(">", evts)])
                , ("sub=ruler", [(">", [(0, 22, "")])])
                ]
    let (evts, logs) = run [(0, 1, "nosuch")]
    equal evts []
    strings_like logs ["note generator not found: nosuch"]

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
            , ("top-sub=ruler", UiTest.regular_notes 1)
            ]
    equal (run "-sub") ([(0, 1, "3c")], [])
    equal (run "top-sub") ([(0, 1, "3c")], [])
    equal (run "test/top-sub") ([(0, 1, "3c")], [])
    strings_like (snd (run "-bub")) ["note generator not found"]

test_control_scope = do
    let (result, logs) = run_sub extract
            [ ("pedal", [(1, 0, "1"), (3, 0, "0")])
            , ("dia", [(2, 0, "1")])
            , (">", [(0, 2, "sub")])
            ]
            [ (">", [(0, 2, ""), (2, 0, "")])
            , ("local", [(0, 0, "2"), (2, 0, "3")])
            ]
        extract event = (c "pedal", c "dia", c "local")
            where c = flip DeriveTest.e_control event
    equal logs []
    equal result
        -- pedal is visible to both
        -- dia is omitted, since it belongs to the call after "sub"
        -- local is visible since it's local
        [ ([(1, 1), (3, 0)],    [], [(0, 2)])
        , ([(1, 1), (3, 0)],    [], [(2, 3)])
        ]

test_trim_controls_problem = do
    let run = DeriveTest.extract (DeriveTest.e_control "c")
            . DeriveTest.derive_blocks
    equal (run
            [ ("top", [("c", [(1, 0, "1")]), (">", [(0, 1, "sub")])])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ])
        -- ([[(1, 1)]], []) -- should be this
        ([[]], [])

run_sub :: (Score.Event -> a) -> [UiTest.TrackSpec] -> [UiTest.TrackSpec]
    -> ([a], [String])
run_sub extract top sub = DeriveTest.extract extract $ DeriveTest.derive_blocks
    [("top", top), ("sub=ruler", sub)]

test_control_block = do
    let extract = DeriveTest.e_control "cont"
        derive caller callee = DeriveTest.extract extract $
            DeriveTest.derive_blocks
                [ ("top", [("cont", caller), (">", [(0, 4, "")])])
                , ("sub=ruler", [("%", callee)])
                ]
        sub = [(0, 0, "1"), (16, 0, "2"), (32, 0, "4")]
    strings_like (snd (derive [(0, 2, "nosuch")] []))
        ["control generator not found: nosuch"]

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

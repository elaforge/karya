-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Block_test where
import Util.Test
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.UiTest as UiTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Perform as Perform
import qualified Perform.Midi.PerformTest as PerformTest


test_block = do
    -- This also tests Derive.Call.Block.lookup_note_call
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
            , ("top.sub=ruler", UiTest.regular_notes 1)
            ]
    equal (run ".sub") ([(0, 1, "3c")], [])
    equal (run "top.sub") ([(0, 1, "3c")], [])
    equal (run "test/top.sub") ([(0, 1, "3c")], [])
    strings_like (snd (run ".bub")) ["note generator not found"]

test_clip = do
    let run top = run_sub DeriveTest.e_start_dur [(">", top)]
            [(">", [(0, 1, ""), (1, 1, "")])]
    equal (run [(0, 1, "clip blub")])
        ([], ["Error: block not found: blub"])
    -- make sure out of range notes are clipped
    equal (run [(0, 1, "clip sub")]) ([(0, 1)], [])
    -- the tempo of the block is not affected by the duration of the event
    equal (run [(1, 2, "clip sub")]) ([(1, 1), (2, 1)], [])

    equal (run [(1, 2, "clip sub 1")]) ([(1, 0.5), (1.5, 0.5)], [])
    equal (run [(1, 2, "clip sub .5")]) ([(1, 0.25), (1.25, 0.25)], [])

test_clip_start = do
    let run = run_sub DeriveTest.e_note
    -- Aligned to the end.
    equal (run [(">", [(0, 2, "Clip sub")])] (UiTest.regular_notes 1))
        ([(1, 1, "3c")], [])
    -- Get the last two notes.
    equal (run [(">", [(0, 2, "Clip sub")])] (UiTest.regular_notes 3))
        ([(0, 1, "3d"), (1, 1, "3e")], [])

test_loop = do
    let run = run_sub DeriveTest.e_start_dur
    equal (run [(">", [(0, 4, "loop sub")])] [(">", [(0, 1, "")])])
        ([(0, 1), (1, 1), (2, 1), (3, 1)], [])
    -- Cuts off the last event.
    let sub = [(">", [(0, 1, ""), (1, 3, "")])]
    equal (run [(">", [(0, 5, "loop sub")])] sub)
        ([(0, 1), (1, 3), (4, 1)], [])
    equal (run [(">", [(0, 4, "loop sub 2")])] sub)
        ([(0, 0.5), (0.5, 1.5), (2, 0.5), (2.5, 1.5)], [])

test_tile = do
    let run top = run_sub Score.event_start [(">", top)]
            [(">", [(0, 1, ""), (1, 3, "")])]
    -- Just like 'loop'.
    equal (run [(0, 5, "tile sub")]) ([0, 1, 4], [])
    equal (run [(1, 5, "tile sub")]) ([1, 4, 5], [])
    equal (run [(9, 5, "tile sub")]) ([9, 12, 13], [])

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

test_control_scope_unsliced = do
    let run = DeriveTest.extract DeriveTest.e_nns . DeriveTest.derive_tracks ""
    equal (run
            [ ("*", [(0, 0, "4c"), (1, 0, "4d")])
            , (">", [(0, 1, ""), (1, 1, "")])
            ])
        ([[(0, 60), (1, 62)], [(1, 62)]], [])

    -- Show that even though signals are no longer trimmed, I still don't carry
    -- the signal on one note forever.
    --
    -- Previously I trimmed at the start of the next event, or the end of the
    -- block, which put a natural boundary on the amount of control rendered.
    -- But that means the control stops at the end of the block regardless,
    -- so perhaps not so natural.
    let perform = (\(_, midi, logs) -> (midi, logs))
            . DeriveTest.perform_defaults . Derive.r_events
        extract = PerformTest.msg_ts
            . PerformTest.extract (PerformTest.e_cc 1)
        cc_lead = Perform.min_control_lead_time
    let result = DeriveTest.derive_tracks ""
            [ ("*", [(0, 0, "4c")])
            , ("mod", [(ScoreTime.double n, 0, val) |
                (val, n) <- zip (cycle ["0", ".5", "1"]) [0..16]])
            , (">s/1", [(0, 1, "")])
            ]
    let (midi, logs) = perform result
    equal logs []
    -- Decay of 1s means only one sample.
    equal (extract midi) [(-cc_lead, 0), (1, 64)]

    let result = DeriveTest.derive_blocks
            [ ("top",
                [ ("*", [(0, 0, "4c")])
                , ("mod", [(1.5, 0, ".5")])
                , (">s/1", [(0, 1, "sub")])
                ])
            , ("sub=ruler", [(">", [(0, 1, "")])])
            ]
    let (midi, logs) = perform result
    equal logs []
    equal (extract midi) [(-cc_lead, 0), (1.5, 64)]

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

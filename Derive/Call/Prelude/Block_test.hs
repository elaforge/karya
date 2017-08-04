-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Call.Prelude.Block_test where
import qualified Util.Log as Log
import Util.Test
import qualified Ui.Ruler as Ruler
import qualified Ui.UiTest as UiTest
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Derive.Attrs as Attrs
import qualified Derive.Call.CallTest as CallTest
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Instrument.DUtil as DUtil
import qualified Derive.Score as Score
import qualified Derive.Sig as Sig
import qualified Derive.Stream as Stream

import Global


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
    strings_like (snd (run [(0, 1, "sub arg")])) ["too many arguments"]

    -- subderived stuff is stretched and placed, inherits instrument
    let (evts, logs) = run [(0, 1, "sub"), (1, 2, "n i +a | sub")]
    equal logs []
    equal evts
        [ (0, 1, "", "", [])
        , (1, 2, "", "i", ["a"])
        ]

test_block_call_overrides_other_calls = do
    let run sub_name evts = DeriveTest.extract extract $
            DeriveTest.derive_blocks
                [("b1", [(">", evts)]), (sub_name <> "=ruler", sub)]
        extract e = (Score.event_start e, DeriveTest.e_attributes e)
        sub = UiTest.note_track [(0, 1, "4c"), (1, 1, "4d")]
    equal (run "xyz" [(0, 2, "o")]) ([(0, "+harm")], [])
    equal (run "o" [(0, 2, "o")]) ([(0, "+"), (1, "+")], [])

test_block_call_overridden_by_instrument_call = do
    let run sub_name evts = DeriveTest.extract DeriveTest.e_attributes $
            DeriveTest.derive_blocks_setup (DeriveTest.with_patch set_code "x")
                [ ("b1", [(">x", evts)])
                , (sub_name <> "=ruler", [(">", [(0, 1, "+block")])])
                ]
        set_code = MidiInst.code #= MidiInst.note_generators
            [("inst", DUtil.attributes_note Attrs.mute)]
    equal (run "sub" [(0, 1, "sub")]) (["+block"], [])
    -- Even though the block is named inst, the inst call still happens.
    equal (run "inst" [(0, 1, "inst")]) (["+mute"], [])

test_block_logical_range = do
    let run s e tempo sub = DeriveTest.extract DeriveTest.e_start_dur $
            DeriveTest.derive_blocks_setup
                (DeriveTest.with_ruler (UiTest.bid "sub") (mkruler s e))
                [ ("top", [(">", [(1, 1, "sub")])])
                , ("sub=ruler", [("tempo", tempo), (">", sub)])
                ]
        mkruler s e = Ruler.set_bounds s e $ UiTest.mkruler_44 2 1
    let sub01 = [(0, 1, ""), (1, 1, "")]
        sub012 = [(0, 1, ""), (1, 1, ""), (2, 1, "")]
        tempo1 = [(0, 0, "1")]

    equal (run Nothing Nothing tempo1 sub01) ([(1, 0.5), (1.5, 0.5)], [])
    equal (run Nothing (Just 1) tempo1 sub01) ([(1, 1), (2, 1)], [])
    equal (run (Just 1) Nothing tempo1 sub01) ([(0, 1), (1, 1)], [])
    equal (run (Just 1) (Just 2) tempo1 sub012)
        ([(0, 1), (1, 1), (2, 1)], [])

    -- The start point should line up to the event start, end point to the
    -- event end.  To do that, I stretch inner to 1, then translate back.
    equal (run (Just 1) (Just 2) [(0, 0, "2"), (1, 0, "1"), (2, 0, "2")] sub012)
        ([(0.5, 0.5), (1, 1), (2, 0.5)], [])

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
    -> ([a], [Text])
run_sub extract top sub = DeriveTest.extract extract $ DeriveTest.derive_blocks
    [("top", top), ("sub=ruler", sub)]

test_score_duration = do
    let run = snd . DeriveTest.extract Score.event_start
            . DeriveTest.derive_blocks_setup
                (CallTest.with_note_transformer "t" trans)
        trans = Derive.transformer "module" "trans" mempty "doc" $
            Sig.call0t $ \_ deriver -> do
                Log.warn . showt =<< Derive.get_score_duration deriver
                return Stream.empty
    equal (run [("top", [(">", [(0, 1, "t |")])])])
        ["CallDuration 1.0"]
    equal (run
            [ ("top", [(">", [(0, 1, "t | sub")])])
            , ("sub=ruler", [(">", [(0, 3, ""), (3, 1, "")])])
            ])
        ["CallDuration 4.0"]

test_real_duration = do
    let run = DeriveTest.r_log_strings
            . DeriveTest.derive_blocks_setup
                (CallTest.with_note_transformer "t" trans)
        trans = Derive.transformer "module" "trans" mempty "doc" $
            Sig.call0t $ \_ deriver -> do
                Log.warn . showt =<< Derive.get_real_duration deriver
                return Stream.empty
    equal (run [("top", [(">", [(0, 1, "t |")])])]) ["CallDuration 1.0"]
    equal (run [("top", [(">", [(0, 2, "t |")])])]) ["CallDuration 2.0"]
    equal (run [("top", [("tempo", [(0, 0, "2")]), (">", [(0, 2, "t |")])])])
        ["CallDuration 1.0"]
    equal (run
            [ ("top", [(">", [(0, 1, "t | sub")])])
            , ("sub=ruler", [(">", [(0, 2, "")])])
            ])
        ["CallDuration 2.0"]
    equal (run
            [ ("top", [(">", [(0, 1, "t | sub")])])
            , ("sub=ruler", [("tempo", [(0, 0, ".5")]), (">", [(0, 2, "")])])
            ])
        ["CallDuration 4.0"]

    -- It's duration, so it remains the same regardless of where you are.
    equal (run
            [ ("top", [(">", [(1, 1, "t | sub")])])
            , ("sub=ruler", [("tempo", [(0, 0, ".5")]), (">", [(0, 2, "")])])
            ])
        ["CallDuration 4.0"]

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

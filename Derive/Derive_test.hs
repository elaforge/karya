-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | This also tests Derive.Note and Derive.Control since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Cmd.Simple as Simple
import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Tempo as Tempo
import qualified Derive.TrackWarp as TrackWarp
import qualified Derive.Warp as Warp

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Midi.Types
import qualified Perform.NN as NN
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import qualified Ui.Id as Id
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let res = DeriveTest.derive_tracks ""
            [ (">i1", [(0, 16, ""), (16, 16, "")])
            , ("*", [(0, 0, "4c"), (16, 0, "4c#")])
            , ("dyn", [(0, 0, "1"), (16, 0, ".5")])
            ]
    let ((perf_events, mmsgs), logs) =
            DeriveTest.perform_defaults (Derive.r_events res)
    equal (e_events res) ([(0, 16, ""), (16, 16, "")], [])

    -- 2: conversion to midi perf events
    let evt = (,,,) "i1"
    equal (map extract_perf_event perf_events)
        [ evt 0 16 (mkstack (0, 16))
        , evt 16 16 (mkstack (16, 32))
        ]

    -- 3: performance to midi protocol events
    equal (DeriveTest.note_on_vel mmsgs) [(0, 60, 127), (16000, 61, 64)]
    equal logs []
    where
    mkstack (s, e) = Stack.from_outermost
        [ block_call UiTest.default_block_id, Stack.Serial 0
        , Stack.Block UiTest.default_block_id
        , Stack.Track (UiTest.mk_tid 1)
        -- track title and event for note track
        , Stack.Call "note-track", Stack.Region s e
        , Stack.Call "note", Stack.Serial 0
        , Stack.Track (UiTest.mk_tid 2)
        , Stack.Track (UiTest.mk_tid 3)
        -- t1 shows up again inverted
        , Stack.Track (UiTest.mk_tid 1)
        , Stack.Call "note-track" -- inverted note track is >
        , Stack.Region s e
        ]
    block_call bid = Stack.Call $ "block " <> showt bid
    extract_perf_event (Midi.Types.Event start dur patch _controls _pitch
            _svel _evel stack) =
        (Midi.Types.patch_name patch, RealTime.to_seconds start,
            RealTime.to_seconds dur, stack)

test_qualified_symbol = do
    let run = DeriveTest.extract DeriveTest.e_attributes
            . DeriveTest.derive_tracks ""
    equal (run [(">", [(0, 2, "bali.gong.cycle \"(+a) 1")])])
        (["+a", "+a"], [])

test_round_pitch = do
    -- A note sufficiently close to 4c becomes 4c.
    let ((_, mmsgs), _) =
            DeriveTest.perform_result DeriveTest.perform_defaults $
            DeriveTest.derive_tracks ""
                [(">i1", [(0, 1, "")]), ("*", [(0, 0, "3b 99.99")])]
    equal (DeriveTest.note_on_vel mmsgs) [(0, Key.c4, 127)]

test_override_default_pitch = do
    let f title pitch = DeriveTest.extract DeriveTest.e_nns $
            DeriveTest.derive_tracks title [(">", [(4, 4, "")]), ("*", pitch)]
    equal (f "" [(4, 0, "4c"), (6, 0, "4d")])
        ([[(4, NN.c4), (6, NN.c4), (6, NN.d4)]], [])
    -- Both overriding PITCH works, and it picks up defaults properly.
    equal (f "*PITCH = set-or-move | set-or-move-time=2"
            [(4, 0, "4c"), (6, 0, "4d")])
        ([[(4, NN.c4), (6, NN.c4), (8, NN.d4)]], [])


test_attributes = do
    -- Test that attributes work, through derivation and performance.
    let convert_lookup = DeriveTest.make_convert_lookup allocs db
        allocs = Simple.allocations
            [("i1", ("s/i1", Simple.Midi [("wdev", 0)]))]
        db = UiTest.make_db [("s", [patch])]
            where
            patch = Patch.attribute_map #= attr_map $ Patch.patch (-1, 1) "i1"
        keyswitches = Patch.single_keyswitches $ map (first Attrs.attrs)
            [ (["a1", "a2"], 0)
            , (["a0"], 1)
            , (["a1"], 2)
            , (["a2"], 3)
            ]
        keymap = Patch.unpitched_keymap [(Attrs.attr "km", 42)]
        attr_map = Common.AttributeMap $ case (keyswitches, keymap) of
            (Common.AttributeMap a, Common.AttributeMap b) -> a ++ b
    let res = DeriveTest.derive_tracks ""
            [ (">i1 | +a1", [(0, 1, "+a0"), (1, 1, "+a2")])
            , ("*twelve", [(0, 0, "4c")])
            ]
        ((_, mmsgs), logs) = DeriveTest.perform convert_lookup
            allocs (Derive.r_events res)

    -- Attribute inheritance thing works.
    equal (DeriveTest.extract DeriveTest.e_attributes res)
        (["+a0+a1", "+a1+a2"], [])
    equal (map DeriveTest.show_log logs)
        -- TODO re-enable when Convert.warn_unused_attributes is configurable
        -- ["attrs have no match in keyswitches or keymap of >s/ks: +a1"]
        []
    -- Attrs get the proper keyswitches and keymap keys.
    equal (note_on_keys mmsgs) [1, Key.c4, 0, Key.c4]

note_on_keys :: [Midi.WriteMessage] -> [Midi.Key]
note_on_keys msgs =
    [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- map Midi.wmsg_msg msgs]

test_stack = do
    let extract = fst . DeriveTest.extract Score.event_stack
    let stacks = extract $ DeriveTest.derive_blocks
            [ ("b0", [(">i1", [(0, 1, ""), (1, 1, "sub")])])
            , ("sub", [(">", [(0, 1, ""), (1, 1, "")])])
            ]
    let block = Stack.Block . UiTest.bid
        block_call bid = Stack.Call $ "block " <> showt (UiTest.bid bid)
        track name num = Stack.Track (UiTest.mk_tid_name name num)
        call = Stack.Call
    equal (map (map Stack.unparse_ui_frame . Stack.to_ui) stacks)
        [ ["test/b0 test/b0.t1 0-1"]
        , ["test/b0 test/b0.t1 1-2", "test/sub test/sub.t1 0-1"]
        , ["test/b0 test/b0.t1 1-2", "test/sub test/sub.t1 1-2"]
        ]

    let b0 s e =
            [ block_call "b0", Stack.Serial 0, block "b0", track "b0" 1
            , call "note-track", Stack.Region s e
            ]
        sub s e =
            [ block_call "sub", Stack.Serial 0, block "sub", track "sub" 1
            , call "note-track", Stack.Region s e, call "note", Stack.Serial 0
            ]
    equal stacks $ map Stack.from_outermost
        [ b0 0 1 ++ [Stack.Call "note", Stack.Serial 0]
        , b0 1 2 ++ sub 0 1
        , b0 1 2 ++ sub 1 2
        ]

test_stack_after_exception = do
    -- The stack is properly rewound after an exception.  This actually tests
    -- that the state is rolled back if event evaluation produces an exception,
    -- which is more directly tested in "Derive.EvalTrack_test".  Amazingly,
    -- it didn't for a very long time, and the bug was only revealed in this
    -- way.
    let result = DeriveTest.derive_blocks
            [ ("top -- >>i1 = \"(%dyn = set .8)",
                [(">", [(0, 4, "b1"), (4, 4, "b2")])])
            , ("b1", [(">i1", [])])
            , ("b2", [(">i1", [])])
            ]
    let extract = fmap Stack.pretty_ui_ . Log.msg_stack
    equal (mapMaybe extract $ DeriveTest.r_logs result)
        [ "top top.t1 0-4 / b1 b1.t1 *"
        , "top top.t1 4-8 / b2 b2.t1 *"
        ]

test_simple_subderive = do
    let (events, msgs) = DeriveTest.extract DeriveTest.e_note $
            DeriveTest.derive_blocks
            [ ("parent", [(">i1", [(0, 2, "sub"), (2, 1, "sub")])])
            , ("sub=ruler", UiTest.regular_notes 2)
            ]
    equal msgs []
    equal events
        [ (0, 1, "3c"), (1, 1, "3d")
        , (2, 0.5, "3c"), (2.5, 0.5, "3d")
        ]

test_subderive = do
    let run evts = DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i1", evts)
                ])
            , ("sub=ruler", [(">i2", [(1, 1, "--sub1")])])
            , ("empty", [(">i1", [])])
            ]
    -- I used to test recursive call, but now that the block call doesn't
    -- catch that explicitly it means I get random crap before it finally
    -- aborts due to the call stack limit.
    let (events, msgs) = DeriveTest.r_split $ run
            [(0, 1, "nosuch"), (1, 1, "empty"), (3, 1, "--x")]
    -- errors don't stop derivation, and an empty sub-block is ignored
    equal (map DeriveTest.e_event events) [(1.5, 0.5, "--x")]
    strings_like (map DeriveTest.show_log msgs) ["not found: nosuch"]
    equal (map (DeriveTest.show_stack . Log.msg_stack) msgs)
        ["b0 b0.t2 0-1: note-track"]

    let res = run [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "--b2")]
        (events, msgs) = DeriveTest.r_split res
    equal (map DeriveTest.e_event events)
        [(0, 4, "--b1"), (6, 2, "--sub1"), (8, 0.5, "--b2")]
    equal (map Score.event_instrument events) ["i1", "i2", "i1"]
    equal msgs []

    let b0 pos = (UiTest.bid "b0", [(UiTest.mk_tid_name "b0" 1, pos),
            (UiTest.mk_tid_name "b0" 2, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mk_tid_name "sub" 1, pos)])
    equal (map (inv_tempo res) (Seq.range 0 10 2))
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16, sub 2], []]

    -- For eyeball verification.
    -- pprint (r_events res)
    -- pprint $ zip [0,2..] $ map (inv_tempo res) (Seq.range 0 10 2)
    -- pprint $ Derive.state_track_warps state

test_subderive_timing = do
    -- Just make sure that sub-blocks stretch to the correct times.
    let (events, logs) = e_events $ DeriveTest.derive_blocks
            [ ("p",
                [ ("tempo", [(0, 0, ".5")])
                , (">i1", [(0, 2, "sub"), (5, 1, "sub")])
                ])
            , ("sub=ruler", [(">i1", [(0, 1, ""), (1, 1, "")])])
            ]
    equal events
        [ (0, 2, ""), (2, 2, "")
        , (10, 1, ""), (11, 1, "")
        ]
    equal logs []

test_subderive_error = do
    let run evts = e_events $ DeriveTest.derive_blocks
            [ ("b0", [ (">i1", evts) ])
            , ("sub", [("blah *error syntax", [(1, 1, "--sub1")]), (">", [])])
            ]
    let (events, logs) = run [(0, 1, "sub")]
    equal events []
    strings_like logs ["track title: parse error"]

test_subderive_multiple = do
    -- make sure subderiving a block with multiple tracks works correctly
    let ((_, mmsgs), logs) =
            DeriveTest.perform_result DeriveTest.perform_defaults $
            DeriveTest.derive_blocks
                [ ("b0",
                    [ ("tempo", [(0, 0, "2")])
                    , ("dyn", [(0, 0, "1"), (8, 0, "i 0")])
                    , (">i1", [(0, 8, "sub")])
                    ])
                , ("sub=ruler",
                    [ (">", [(0, 1, ""), (1, 1, "")])
                    , ("*twelve", [(0, 0, "4c"), (1, 0, "4d")])
                    , (">", [(0, 1, ""), (1, 1, "")])
                    , ("*twelve", [(0, 0, "5c"), (1, 0, "5d")])
                    ])
                ]
    equal (DeriveTest.note_on_vel mmsgs)
        [ (0, Key.c5, 127), (0, Key.c4, 127)
        , (2000, Key.d5, 64), (2000, Key.d4, 64)
        ]
    equal logs []

test_multiple_subderive = do
    -- make sure a sequence of sub calls works
    let res = DeriveTest.derive_blocks
            [ ("b0", [(">i1", [(0, 2, "sub"), (2, 2, "sub"), (4, 2, "sub")])])
            , ("sub=ruler", [(">", [(0, 1, "--sub1")])])
            ]
    equal (e_events res)
        ([(0, 2, "--sub1"), (2, 2, "--sub1"), (4, 2, "--sub1")], [])

    -- Empty inst inherits calling inst.
    equal (fst (DeriveTest.extract Score.event_instrument res))
        (replicate 3 "i1")

    let pos = map (inv_tempo res) (Seq.range 0 6 1)
    let b0 pos = (UiTest.bid "b0", [(UiTest.mk_tid_name "b0" 1, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mk_tid_name "sub" 1, pos)])
    equal (map List.sort pos)
        [ [b0 0, sub 0], [b0 1, sub 0.5], [b0 2, sub 0, sub 1]
        , [b0 3, sub 0.5] , [b0 4, sub 0, sub 1]
        , [b0 5, sub 0.5], [b0 6, sub 1]
        ]

test_tempo_compose = do
    let run tempo events sub_tempo = e_events $ DeriveTest.derive_blocks
            [ ("b0", [("tempo", tempo), (">i1", events)])
            , ("sub=ruler",
                [ ("tempo", sub_tempo)
                , (">", [(0, 1, ""), (1, 1, "")])
                ])
            ]

    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "1")]) $
        ([(2, 1, ""), (3, 1, "")], [])
    -- Tempo of the sub doesn't matter since it will be stretched to fit.
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "2")]) $
        ([(2, 1, ""), (3, 1, "")], [])
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, ".5")]) $
        ([(2, 1, ""), (3, 1, "")], [])

    -- Make sure the top level block doesn't get stretched.
    equal (run [(0, 0, "2")] [(0, 2, "--1"), (2, 2, "sub"), (4, 2, "--2")]
            [(0, 0, ".5")]) $
        ([(0, 1, "--1"), (1, 0.5, ""), (1.5, 0.5, ""), (2, 1, "--2")], [])

    equal (run [(0, 0, "1"), (2, 0, "2")] [(0, 2, "sub")] [(0, 0, "1")]) $
        ([(0, 1, ""), (1, 1, "")], [])
    equal (run [(0, 0, "2"), (4, 0, ".5")] [(0, 4, "sub"), (4, 4, "sub")]
            [(0, 0, "1")]) $
        ([(0, 1, ""), (1, 1, ""), (2, 4, ""), (6, 4, "")], [])

    -- TODO test when the subblock has a tempo too

test_warp_ops = do
    let run op = DeriveTest.eval Ui.empty (op record)
        record = do
            x0 <- Derive.real (0 :: ScoreTime)
            x1 <- Derive.real (2 :: ScoreTime)
            return [x0, x1]

    equal (run id) $ Right [0, 2]
    equal (run (Derive.stretch 2)) $ Right [0, 4]
    equal (run (Derive.at 2)) $ Right [2, 4]
    equal (run (Derive.at 1 . Derive.stretch 2)) $ Right [1, 5]
    equal (run (Derive.at 2 . Derive.stretch 2)) $ Right [2, 6]
    equal (run (Derive.at 2 . Derive.stretch 0.5)) $ Right [2, 3]
    equal (run (Derive.stretch 2 . Derive.at 1)) $ Right [2, 6]
    equal (run (Derive.stretch 2 . Derive.at 2)) $ Right [4, 8]
    equal (run (Derive.stretch 0.5 . Derive.at 2)) $ Right [1, 2]
    equal (run (Derive.place 1 2)) $ Right [1, 5]

    -- test compose
    let plain = Warp.from_signal $ Signal.from_pairs [(0, 0), (100, 100)]
        slow = Warp.from_signal $ Signal.from_pairs [(0, 0), (100, 200)]

    equal (run (Internal.warp plain)) $ Right [0, 2]
    equal (run (Derive.at 2 . Internal.warp plain)) $ Right [2, 4]
    equal (run (Derive.stretch 2 . Internal.warp plain)) $ Right [0, 4]

    equal (run (Internal.warp plain . Internal.warp plain)) $ Right [0, 2]
    equal (run (Internal.warp plain . Internal.warp slow)) $ Right [0, 4]
    equal (run (Internal.warp slow . Internal.warp plain)) $ Right [0, 4]
    equal (run (Internal.warp slow . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.stretch 2 . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.stretch 2 . Internal.warp slow . Internal.warp slow)) $
        Right [0, 16]

    equal (run (Internal.warp slow)) $ Right [0, 4]
    equal (run (Derive.stretch 2 . Internal.warp slow)) $ Right [0, 8]
    equal (run (Derive.at 1 . Internal.warp slow)) $ Right [1, 5]
    equal (run (Derive.at 1 . Derive.stretch 2 . Derive.stretch 2)) $
        Right [1, 9]
    equal (run (Derive.at 1 . Derive.stretch 2 . Internal.warp slow)) $
        Right [1, 9]

    equal (run (Internal.warp slow . Derive.at 1)) $ Right [2, 6]
    equal (run (Derive.at 1 . Derive.stretch 2 . Internal.warp slow
            . Internal.warp slow)) $
        Right [1, 17]

test_real_to_score_round_trip = do
    let f do_warp = DeriveTest.eval Ui.empty $
            do_warp (Derive.real_to_score =<< Derive.score_to_real 1)
    let slow = Warp.from_signal $ Signal.from_pairs [(0, 0), (100, 200)]
    equal (f id) (Right 1)
    equal (f (Derive.at 5)) (Right 1)
    equal (f (Derive.stretch 5)) (Right 1)
    equal (f (Derive.stretch 5 . Derive.at 5)) (Right 1)
    equal (f (Internal.warp slow . Derive.stretch 5 . Derive.at 5)) (Right 1)
    equal (f (Derive.stretch 5 . Derive.at 5 . Internal.warp slow)) (Right 1)

test_shift_controls = do
    let controls = Map.fromList
            [("cont", ScoreT.untyped $
                Signal.from_pairs [(0, 1), (2, 2), (4, 0)])]
        psig = DeriveTest.psignal [(0, "4c")]
    let set_controls = DeriveTest.modify_dynamic $ \st -> st
            { Derive.state_controls = controls
            , Derive.state_pitch = psig
            }
    let run op = DeriveTest.extract_run extract $
            DeriveTest.run Ui.empty (set_controls >> op get)
            where
            get = do
                conts <- Internal.get_dynamic Derive.state_controls
                psig <- Internal.get_dynamic Derive.state_pitch
                return (conts, psig)
            extract (conts, pitch) =
                (unsignal conts, first Signal.to_pairs (PSignal.to_nn pitch))
            unsignal =
                Signal.to_pairs . ScoreT.typed_val . snd . head . Map.toList
    equal (run id) $ Right ([(0, 1), (2, 2), (4, 0)], ([(0, 60)], []))
    equal (run $ Derive.shift_controls 2) $
        Right ([(2, 1), (4, 2), (6, 0)], ([(2, 60)], []))

test_tempo_funcs1 = do
    let ([t_tid, tid1], ui_state) = UiTest.run_mkblock
            [ ("tempo", [(0, 0, "2")])
            , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
            ]
        bid = UiTest.default_block_id
    let res = DeriveTest.derive_block ui_state bid
    equal (DeriveTest.r_log_strings res) []

    -- [(BlockId, [(TrackId, ScoreTime)])]
    let b0 pos = (bid, [(t_tid, pos), (tid1, pos)])
    equal (map (inv_tempo res) [0, 2, 4, 6]) [[b0 0], [b0 4], [b0 8], [b0 12]]
    equal (inv_tempo res (RealTime.from_score UiTest.default_block_end)) []

    equal (map (r_tempo res bid t_tid) (Seq.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Seq.range 0 5 1))

test_tempo_funcs2 = do
    let ([t_tid1, tid1, t_tid2, tid2], ui_state) =
            UiTest.run_mkblock
            -- UiTest.run Ui.empty $ UiTest.mkblocks $ (:[]) $ ("b0",) $
                [ ("tempo", [(0, 0, "2")])
                , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
                , ("tempo", [(0, 0, "1")])
                , (">i2", [(0, 16, "--2b1")])
                ]
        bid = UiTest.default_block_id
    let res = DeriveTest.derive_block ui_state bid
    equal (DeriveTest.r_log_strings res) []
    equal (map (r_tempo res bid t_tid1) (Seq.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Seq.range 0 5 1))
    equal (map (r_tempo res bid t_tid2) (Seq.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Seq.range 0 10 2))
    let b0 pos = (bid, [(t_tid1, pos), (tid1, pos)])
        b1 pos = (bid, [(t_tid2, pos), (tid2, pos)])
    equal (map (inv_tempo res) [0, 2, 4, 6])
        [[b0 0, b1 0], [b0 4, b1 2], [b0 8, b1 4], [b0 12, b1 6]]

    -- test multiple tempo
    -- test subderive

-- | Map through inv tempo and sort the results since their order isn't
-- relevant.
inv_tempo :: Derive.Result -> RealTime -> [(BlockId, [(TrackId, ScoreTime)])]
inv_tempo res =
    map (second List.sort) . List.sort . r_inv_tempo res Transport.StopAtEnd

test_tempo_funcs_multiple_subblocks = do
    -- A single score time can imply multiple real times.
    let res = DeriveTest.derive_blocks
            [ ("parent", [(">i1", [(0, 1, "sub"), (1, 1, "sub")])])
            , ("sub=ruler", [(">i1", [(0, 1, "")])])
            ]
    equal (r_tempo res (UiTest.bid "sub") (UiTest.mk_tid_name "sub" 1) 0.5)
        [0.5, 1.5]

test_fractional_pitch = do
    -- A pitch that requires pitch bends should distribute across multiple
    -- channels.  Yes, this is also tested in Perform.Midi.Perform, but this
    -- also tests that the pitch signal is trimmed properly by
    -- Note.trim_pitches.
    let ((_perf_events, mmsgs), logs) =
            DeriveTest.perform_result DeriveTest.perform_defaults $
                DeriveTest.derive_tracks ""
                    [ (">i1", [(0, 16, ""), (16, 16, "")])
                    , ("*just", [(0, 16, "4c"), (16, 16, "4d")])
                    ]

    equal logs []
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _)
            <- map Midi.wmsg_msg mmsgs]
        [(0, Key.c4), (1, Key.d4)]

test_control = do
    let ((perf_events, mmsgs), logs) =
            DeriveTest.perform_result DeriveTest.perform_defaults res
        res = DeriveTest.derive_tracks ""
            [ (">i1", [(0, 1, ""), (1, 1, "")])
            , ("*", [(0, 1, "4c"), (1, 1, "4c#")])
            , ("cc1", [(0, 0, "1"), (1, 0, "i .75"), (2, 0, "i 0")])
            ]

    -- Cursory checks, more detailed checks are in more Note_test and
    -- Control_test.
    equal logs []
    equal (fst $ e_events res) [(0, 1, ""), (1, 1, "")]
    equal (length perf_events) 2

    -- Just make sure it did in fact emit ccs.
    check ("has cc: " <> pretty mmsgs) $
        any Midi.is_cc (map Midi.wmsg_msg mmsgs)

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    -- TODO and it belongs in TrackWarp_test now
    let track_id = Id.TrackId (UiTest.mkid "warp")
        warp = Tempo.tempo_to_warp (Signal.constant 2)
        track_warps = [TrackWarp.TrackWarp
                0 2 UiTest.default_block_id (Set.singleton track_id) warp]
    let f = TrackWarp.inverse_tempo_func track_warps Transport.StopAtEnd
        with_block pos = [(UiTest.default_block_id, [(track_id, pos)])]
    -- Fast tempo means ScoreTime passes quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is at the end of the block.
    equal (map (f . RealTime.seconds) [0..3])
        [with_block 0, with_block 2, with_block 4, []]

test_tempo_roundtrip = do
    let track_id = Id.TrackId (UiTest.mkid "warp")
        warp = Tempo.tempo_to_warp (Signal.constant 0.987)
        track_warps = [TrackWarp.TrackWarp
                0 10 UiTest.default_block_id (Set.singleton track_id) warp]
    let inv = TrackWarp.inverse_tempo_func track_warps Transport.StopAtEnd
        tempo = TrackWarp.tempo_func track_warps
    let rtimes = concatMap (tempo UiTest.default_block_id track_id)
            (Seq.range 0 3 1)
        stimes = concatMap inv rtimes
    pprint rtimes
    pprint stimes
    -- expected failure
    -- equal (map snd (concatMap snd stimes)) [0..3]

test_named_pitch = do
    let run op = DeriveTest.eval Ui.empty (op $ Derive.named_nn_at "psig" 2)
        pitch = DeriveTest.mkpitch12 "4c"
        with_const pname = Derive.with_named_pitch pname
            (PSignal.constant pitch)
    equal (run (with_const "psig")) (Right (Just 60))
    equal (run (with_const "bad")) (Right Nothing)

test_block_end = do
    -- Make sure the pitch for the sub block event is trimmed to the end
    -- of the block, since there's no next event for it.
    let res = DeriveTest.extract DeriveTest.e_nns $ DeriveTest.derive_blocks
            [ ("p",
                [ (">i1", [(0, 1, "sub"), (1, 1, "")])
                , ("*twelve", [(0, 0, "5d"), (1, 0, "5e")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    equal res ([[(0, 74), (1, 74)], [(1, 76)]], [])

-- * regression

-- These tests are derived from debugging and test specific things that went
-- wrong.  I don't think they're likely enough to come up again that I want
-- to figure out the right place to test, but as long as they exist I might
-- as well keep them for regressions.

test_regress_pedal = do
    -- Make sure a pedal halfway through a note really only turns on halfway
    -- through the note.
    let ((_perf_events, mmsgs), _logs) = DeriveTest.perform_result
            DeriveTest.perform_defaults (derive_blocks blocks)
    let pedal_on =
            [ (ts, c)
            | (ts, Midi.ChannelMessage _ (Midi.ControlChange 64 c))
                <- DeriveTest.extract_midi mmsgs
            , c /= 0
            ]
    equal pedal_on [(12500, 127)]
    where
    blocks = [(("b10",
        [("pedal", [(12.5, 2, "`ped`")]),
         (">i1", [ (10.0, 5.0, "")]),
         ("*", [ (10.0, 0.0, "4f")])
        ]), [(1, 2), (2, 3)])]

test_regress_event_end1 = do
    -- Ensure that notes get the proper next event even when it has been
    -- sliced off.  Previously it extended to the block end and the notes
    -- wound up with too much pitch signal.
    let res = derive_blocks blocks
        extract e = (Score.event_start e, Score.event_duration e,
                DeriveTest.e_nns e)
    equal (DeriveTest.extract extract res)
        ( [ (0, 2, [(0, 60), (2, 60)])
          , (2, 2, [(2, 62)])
          ]
        , []
        )
    where
    blocks = [(("b0", b0), [(1, 2), (2, 3)])]
    b0 =
        [ (">", [(0, 0, "`arp-up`")])
        , (">i1", [(0, 2, ""), (2, 2, "")])
        , ("*", [(0, 0, "4c"), (2, 0, "4d")])
        ]

test_regress_event_end2 = do
    let res = derive_blocks blocks
        extract e = (Score.event_start e, Score.event_duration e,
                DeriveTest.e_nns e)
    equal (DeriveTest.extract extract res)
        ( [ (5, 2, [(5, 60), (7, 60)])
          , (7, 2, [(7, 62)])
          ]
        , []
        )
    where
    blocks = [(("b0",
       [(">", [(5, 0, "`arp-up`")]),
        (">", [(5, 2, ""), (7, 2, "")]),
        ("*", [(5, 0, "4c"), (7, 0, "4d")])
        ]),
      [(1, 2), (2, 3)])]

derive_blocks :: [(UiTest.BlockSpec, [Skeleton.Edge])] -> Derive.Result
derive_blocks blocks = DeriveTest.derive_block state (UiTest.bid block_name)
    where
    state = UiTest.exec Ui.empty (UiTest.mkblocks_skel blocks)
    ((block_name, _), _) : _ = blocks

-- * util

r_tempo :: Derive.Result -> Transport.TempoFunction
r_tempo = TrackWarp.tempo_func . Derive.r_track_warps

r_inv_tempo :: Derive.Result -> Transport.InverseTempoFunction
r_inv_tempo = TrackWarp.inverse_tempo_func . Derive.r_track_warps

e_events :: Derive.Result -> ([(RealTime, RealTime, Text)], [Text])
e_events = DeriveTest.extract DeriveTest.e_event

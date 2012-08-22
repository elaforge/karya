{- | This also tests Derive.Note and Derive.Control since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Midi as Midi
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.Transport as Transport

import Types


test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let res = DeriveTest.derive_tracks
            [ (inst_title, [(0, 16, ""), (16, 16, "")])
            , ("*twelve", [(0, 0, "4c"), (16, 0, "4c#")])
            ]
    let (perf_events, mmsgs, logs) =
            DeriveTest.perform_defaults (Derive.r_events res)

    equal (extract_events res) ([(0, 16, ""), (16, 16, "")], [])

    -- 2: conversion to midi perf events
    let evt = (,,,) "1"
    equal (map extract_perf_event perf_events)
        [ evt 0 16 (mkstack (0, 16))
        , evt 16 16 (mkstack (16, 32))
        ]

    -- 3: performance to midi protocol events
    equal (note_on_keys mmsgs) [60, 61]
    equal logs []
    where
    mkstack (s, e) = Stack.from_outermost
        [ block_call UiTest.default_block_id
        , Stack.Block UiTest.default_block_id
        , Stack.Track (UiTest.mk_tid 1)
        -- track title and event for note track
        , Stack.Call "note-track", Stack.Region s e, Stack.Call "note"
        , Stack.Track (UiTest.mk_tid 2)
        -- t1 shows up again inverted
        , Stack.Track (UiTest.mk_tid 1)
        -- inverted note track has no note-track Call for ">"
        , Stack.Region s e, Stack.Call "note"
        ]
    block_call bid = Stack.Call $ "block " ++ show bid
    extract_perf_event (Perform.Event inst start dur _controls _pitch stack) =
        (Instrument.inst_name inst,
            RealTime.to_seconds start, RealTime.to_seconds dur, stack)

test_attributes = do
    -- Test that attributes work, through derivation and performance.
    let convert_lookup = DeriveTest.make_convert_lookup $
            DeriveTest.make_db [("s", [patch])]
        patch = Instrument.set_keymap keymap $
            Instrument.set_keyswitches keyswitches $ Instrument.patch $
                Instrument.instrument "ks" [] (-1, 1)
        keyswitches = map (first Score.attributes)
            [ (["a1", "a2"], 0)
            , (["a0"], 1)
            , (["a1"], 2)
            , (["a2"], 3)
            ]
        keymap = [(Score.attr "km", 42)]
    let res = DeriveTest.derive_tracks
            [ (">s/ks +a1",
                [(0, 1, "n +a0"), (1, 1, "n +a2"), (2, 1, "n -a1 +km")])
            , ("*twelve", [(0, 0, "4c")])
            ]
        attrs = fst $
            DeriveTest.extract (Score.attrs_list . Score.event_attributes) res
        (_, mmsgs, logs) = DeriveTest.perform convert_lookup
            (UiTest.midi_config [("s/ks", [0])]) (Derive.r_events res)

    -- Attribute inheritance thing works.
    equal attrs [["a0", "a1"], ["a1", "a2"], ["km"]]
    equal (map DeriveTest.show_log logs)
        ["attrs have no match in keyswitches or keymap of >s/ks: +a1"]
    -- Attrs get the proper keyswitches and keymap keys.
    equal (note_on_keys mmsgs)
        [1, 60, 0, 60, 42]

note_on_keys :: [(Integer, Midi.Message)] -> [Midi.Key]
note_on_keys msgs =
    [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- map snd msgs]

test_stack = do
    let extract = fst . DeriveTest.extract Score.event_stack
    let stacks = extract $ DeriveTest.derive_blocks
            [ ("b0", [(">i1", [(0, 1, ""), (1, 1, "sub")])])
            , ("sub", [(">", [(0, 1, ""), (1, 1, "")])])
            ]
    let block = Stack.Block . UiTest.bid
        block_call bid = Stack.Call $ "block " ++ show (UiTest.bid bid)
        track name num = Stack.Track (UiTest.mk_tid_name name num)
        call = Stack.Call
    equal (map (map Stack.unparse_ui_frame . Stack.to_ui) stacks)
        [ ["test/b0 test/b0.t01 0-1"]
        , ["test/b0 test/b0.t01 1-2", "test/sub test/sub.t01 0-1"]
        , ["test/b0 test/b0.t01 1-2", "test/sub test/sub.t01 1-2"]
        ]

    let b0 s e = [block_call "b0", block "b0", track "b0" 1, call "note-track",
            Stack.Region s e]
        sub s e = [block_call "sub", block "sub", track "sub" 1,
            Stack.Region s e, call "note"]
    equal stacks $ map Stack.from_outermost
        [ b0 0 1 ++ [Stack.Call "note"]
        , b0 1 2 ++ sub 0 1
        , b0 1 2 ++ sub 1 2
        ]

test_track_environ = do
    let extract = map extract1 . Map.assocs . Derive.r_track_environ
        extract1 ((bid, tid), env) =
            -- (Pretty.pretty (take 2 (Stack.innermost stack)), env)
            (bid, tid,
                Map.lookup TrackLang.v_scale env,
                Map.lookup TrackLang.v_instrument env)
    let res = DeriveTest.derive_blocks
            [ ("b", [("*semar", [(0, 0, "1")]), (">i1", [(0, 1, "sub")])])
            , ("sub", [(">", [(0, 1, "")]), ("*", [(0, 0, "2")])])
            ]
    let inst = Just $ TrackLang.VInstrument $ Score.Instrument "i1"
        scale = Just $ TrackLang.VScaleId $ Pitch.ScaleId "semar"
    equal (extract res)
        [ (UiTest.bid "b", UiTest.mk_tid_name "b" 1, scale, Nothing)
        , (UiTest.bid "b", UiTest.mk_tid_name "b" 2, scale, inst)
        , (UiTest.bid "sub", UiTest.mk_tid_name "sub" 1, scale, inst)
        , (UiTest.bid "sub", UiTest.mk_tid_name "sub" 2, scale, inst)
        ]

test_simple_subderive = do
    let (events, msgs) = extract_events $ DeriveTest.derive_blocks
            [ ("parent", [(">i1", [(0, 2, "sub"), (2, 1, "sub")])])
            , ("sub", [(">", [(0, 1, "--1"), (1, 1, "--2")])])
            ]
    equal msgs []
    equal events
        [ (0, 1, "--1"), (1, 1, "--2")
        , (2, 0.5, "--1"), (2.5, 0.5, "--2")
        ]

test_subderive = do
    let run evts = DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i1", evts)
                ])
            , ("sub", [(">i2", [(1, 1, "--sub1")])])
            , ("empty", [(">i", [])])
            ]
    -- I used to test recursive call, but now that the block call doesn't
    -- catch that explicitly it means I get random crap before it finally
    -- aborts due to the call stack limit.
    let (events, msgs) = DeriveTest.r_split $ run
            [(0, 1, "nosuch"), (1, 1, "empty"), (3, 1, "--x")]
    -- errors don't stop derivation, and an empty sub-block is ignored
    equal (map DeriveTest.e_event events) [(1.5, 0.5, "--x")]
    strings_like (map DeriveTest.show_log msgs) ["call not found: nosuch"]

    equal (map (DeriveTest.show_stack . Log.msg_stack) msgs)
        ["b0 b0.t02 0-1"]

    let res = run [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "--b2")]
        (events, msgs) = DeriveTest.r_split res
    equal (map DeriveTest.e_event events)
        [(0, 4, "--b1"), (6, 2, "--sub1"), (8, 0.5, "--b2")]
    equal (map Score.event_instrument events)
        (map Score.Instrument ["i1", "i2", "i1"])
    equal msgs []

    let b0 pos = (UiTest.bid "b0", [(UiTest.mk_tid_name "b0" 1, pos),
            (UiTest.mk_tid_name "b0" 2, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mk_tid_name "sub" 1, pos)])
    equal (map (inv_tempo res) [0, 2 .. 10])
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16], []]

    -- For eyeball verification.
    -- pprint (r_events res)
    -- pprint $ zip [0,2..] $ map (inv_tempo res) [0, 2 .. 10]
    -- pprint $ Derive.state_track_warps state

test_subderive_timing = do
    -- Just make sure that sub-blocks stretch to the correct times.
    let (events, logs) = extract_events $ DeriveTest.derive_blocks
            [ ("p",
                [ ("tempo", [(0, 0, ".5")])
                , (">i", [(0, 2, "sub"), (5, 1, "sub")])
                ])
            , ("sub", [(">i", [(0, 1, ""), (1, 1, "")])])
            ]
    equal events
        [ (0, 2, ""), (2, 2, "")
        , (10, 1, ""), (11, 1, "")
        ]
    equal logs []

test_subderive_error = do
    let run evts = extract_events $ DeriveTest.derive_blocks
            [ ("b0", [ (">i1", evts) ])
            , ("sub", [("blah *error syntax", [(1, 1, "--sub1")]), (">", [])])
            ]
    let (events, logs) = run [(0, 1, "sub")]
    equal events []
    strings_like logs ["track title: control track must be one of"]

test_subderive_multiple = do
    -- make sure subderiving a block with multiple tracks works correctly
    let res = DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , ("dyn", [(0, 0, "1"), (8, 0, "i 0")])
                , (inst_title, [(0, 8, "sub")])
                ])
            , ("sub",
                [ (">", [(0, 1, "--1-1"), (1, 1, "--1-2")])
                , ("*twelve", [(0, 0, "4c"), (1, 0, "4d")])
                , (">", [(0, 1, "--2-1"), (1, 1, "--2-2")])
                , ("*twelve", [(0, 0, "5c"), (1, 0, "5d")])
                ])
            ]
    let (_, mmsgs, logs) = DeriveTest.perform_defaults (Derive.r_events res)
    equal (DeriveTest.note_on_times mmsgs)
        [ (0, 60, 127), (0, 72, 127)
        , (2000, 62, 64), (2000, 74, 64)
        ]
    equal logs []

test_multiple_subderive = do
    -- make sure a sequence of sub calls works
    let res = DeriveTest.derive_blocks
            [ ("b0", [(">i1", [(0, 2, "sub"), (2, 2, "sub"), (4, 2, "sub")])])
            , ("sub", [(">", [(0, 1, "--sub1")])])
            ]
    equal (extract_events res)
        ([(0, 2, "--sub1"), (2, 2, "--sub1"), (4, 2, "--sub1")], [])

    -- Empty inst inherits calling inst.
    equal (fst (DeriveTest.extract Score.event_instrument res))
        (replicate 3 (Score.Instrument "i1"))

    let pos = map (inv_tempo res) [0..6]
    let b0 pos = (UiTest.bid "b0", [(UiTest.mk_tid_name "b0" 1, pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.mk_tid_name "sub" 1, pos)])
    equal (map List.sort pos)
        [ [b0 0, sub 0], [b0 1, sub 0.5], [b0 2, sub 0], [b0 3, sub 0.5]
        , [b0 4, sub 0], [b0 5, sub 0.5], []
        ]

test_tempo_compose = do
    let run tempo events sub_tempo = extract_events $ DeriveTest.derive_blocks
            [ ("b0", [("tempo", tempo), (">i1", events)])
            , ("sub",
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

test_initial_environ = do
    let extract = DeriveTest.extract DeriveTest.e_pitch
    let run title pitch = extract $ DeriveTest.derive_tracks
            [ (">", [(0, 1, "")])
            , (title, [(0, 0, pitch)])
            ]
    -- picks up scale from initial environ
    equal (run "*" "3c") ([[(0, 48)]], [])
    -- calls replaced by legong calls
    equal (run "*legong" "3c") ([[]], ["Error: pitch call not found: 3c"])
    -- just make sure legong actually works
    equal (run "*legong" "1") ([[(0, 72.46)]], [])

    -- I'd like to test inst, but it's just too hard.  I would have to get
    -- DeriveTest.default_constant to get the inst lookup like
    -- Cmd.PlayUtil.get_lookup_inst_calls
    -- let env inst = Map.insert TrackLang.v_instrument
    --         (TrackLang.VInstrument (Score.Instrument inst))
    --         (DeriveTest.default_environ)
    -- DeriveTest.derive_tracks_with (Derive.with_inital_scope (env "dmx/x"))
    --         [ (">", [(0, 1, "sn")])
    --         ]

test_warp_ops = do
    let run op = DeriveTest.eval State.empty (op record)
        record = do
            x0 <- Derive.real 0
            x1 <- Derive.real 2
            return [x0, x1]

    equal (run id) $ Right [0, 2]
    equal (run (Derive.d_stretch 2)) $ Right [0, 4]
    equal (run (Derive.d_at 2)) $ Right [2, 4]
    equal (run (Derive.d_at 2 . Derive.d_stretch 2)) $ Right [2, 6]
    equal (run (Derive.d_stretch 2 . Derive.d_at 2)) $ Right [4, 8]
    equal (run (Derive.d_at 2 . Derive.d_stretch 0.5)) $ Right [2, 3]
    equal (run (Derive.d_stretch 0.5 . Derive.d_at 2)) $ Right [1, 2]

    -- test compose
    let plain = Score.signal_to_warp $
            Signal.signal [(RealTime.seconds n, n) | n <- [0..100]]
        slow = Score.signal_to_warp $
            Signal.signal [(RealTime.seconds n, n*2) | n <- [0..40]]


    equal (run (Internal.d_warp plain)) $ Right [0, 2]
    equal (run (Derive.d_at 2 . Internal.d_warp plain)) $ Right [2, 4]
    equal (run (Derive.d_stretch 2 . Internal.d_warp plain)) $
        Right [0, 4]

    equal (run (Internal.d_warp plain . Internal.d_warp plain)) $
        Right [0, 2]
    equal (run (Internal.d_warp plain . Internal.d_warp slow)) $
        Right [0, 4]
    equal (run (Internal.d_warp slow . Internal.d_warp plain)) $
        Right [0, 4]
    equal (run (Internal.d_warp slow . Internal.d_warp slow)) $
        Right [0, 8]
    equal (run (Derive.d_stretch 2 . Internal.d_warp slow)) $
        Right [0, 8]
    equal (run (Derive.d_stretch 2 . Internal.d_warp slow
            . Internal.d_warp slow)) $
        Right [0, 16]

    -- If you start at 1, but time is twice as slow, you really start at 2.
    -- But that is backwards.  Twice as slow time starts at 1.
    equal (run (Derive.d_at 1 . Internal.d_warp slow)) $ Right [1, 5]
    equal (run (Internal.d_warp slow . Derive.d_at 1)) $ Right [2, 6]
    equal (run (Derive.d_at 1 . Derive.d_stretch 2)) $ Right [1, 5]
    equal (run (Derive.d_stretch 2 . Derive.d_at 1)) $ Right [2, 6]

    equal (run (Derive.d_at 1 . Derive.d_stretch 2 . Internal.d_warp slow)) $
            Right [1, 9]
    equal (run (Derive.d_at 1 . Derive.d_stretch 2 . Internal.d_warp slow
        . Internal.d_warp slow)) $
            Right [1, 17]

test_real_to_score = do
    let f do_warp pos = DeriveTest.eval State.empty $
            do_warp (Derive.score =<< Derive.real pos)
    equal (f id 1) (Right 1)
    equal (f (Derive.d_at 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5) 1) (Right 1)
    let slow = Score.signal_to_warp $
            Signal.signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 200)]
    equal (f (Internal.d_warp slow . Derive.d_stretch 5 . Derive.d_at 5) 1)
        (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5 . Internal.d_warp slow) 1)
        (Right 1)

test_shift_control = do
    let controls = Map.fromList [(Score.Control "cont",
            Score.untyped $ Signal.signal [(0, 1), (2, 2), (4, 0)])]
        psig = DeriveTest.pitch_signal [(0, "a")]
    let set_controls = DeriveTest.modify_dynamic $ \st -> st
            { Derive.state_controls = controls
            , Derive.state_pitch = psig
            }
    let run op = DeriveTest.extract_run extract $
            DeriveTest.run State.empty (set_controls >> op get)
            where
            get = do
                conts <- Internal.get_dynamic Derive.state_controls
                psig <- Internal.get_dynamic Derive.state_pitch
                return (conts, psig)
            extract (conts, pitch) =
                (unsignal conts, DeriveTest.signal_to_nn pitch)
            unsignal =
                Signal.unsignal . Score.typed_val . snd . head . Map.toList
    equal (run id) $ Right
        ([(0, 1), (2, 2), (4, 0)], [(0, 60)])
    equal (run $ Derive.shift_control 2) $ Right
        ([(2, 1), (4, 2), (6, 0)], [(2, 60)])

track_specs =
    [ ("tempo", [(0, 0, "2")])
    , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
    ]

test_tempo_funcs1 = do
    let bid = UiTest.bid "b0"

    let ([t_tid, tid1], ui_state) = UiTest.run State.empty $
            UiTest.mkblock ("b0", track_specs)
    let res = DeriveTest.derive_block ui_state bid
    equal (DeriveTest.r_log_strings res) []

    -- [(BlockId, [(TrackId, ScoreTime)])]
    let b0 pos = (bid, [(t_tid, pos), (tid1, pos)])
    equal (map (inv_tempo res) [0, 2 .. 10])
        [[b0 0], [b0 4], [b0 8], [b0 12], [b0 16], []]

    equal (map (r_tempo res bid t_tid) (Seq.range 0 10 2))
        (map ((:[]) . RealTime.seconds) (Seq.range 0 5 1))

test_tempo_funcs2 = do
    let ([t_tid1, tid1, t_tid2, tid2], ui_state) = UiTest.run State.empty $
            UiTest.mkblock ("b0", track_specs
                ++ [ ("tempo", [(0, 0, "1")])
                , (">i2", [(0, 16, "--2b1")])
                ])
        bid = UiTest.bid "b0"
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
inv_tempo :: Derive.Result -> Double -> [(BlockId, [(TrackId, ScoreTime)])]
inv_tempo res = map (second List.sort) . List.sort . r_inv_tempo res
    . RealTime.seconds

test_tempo_funcs_multiple_subblocks = do
    -- A single score time can imply multiple real times.
    let res = DeriveTest.derive_blocks
            [ ("parent", [(">i", [(0, 1, "sub"), (1, 1, "sub")])])
            , ("sub", [(">i", [(0, 1, "")])])
            ]
    equal (r_tempo res (UiTest.bid "sub") (UiTest.mk_tid_name "sub" 1) 0.5)
        [0.5, 1.5]

test_fractional_pitch = do
    -- A pitch that requires pitch bends should distribute across multiple
    -- channels.  Yes, this is also tested in Perform.Midi.Perform, but this
    -- also tests that the pitch signal is trimmed properly by
    -- Note.trim_pitches.
    let res = DeriveTest.derive_tracks
            [ (inst_title, [(0, 16, ""), (16, 16, "")])
            , ("*legong", [(0, 16, "1"), (16, 16, "2")])
            ]
    let (_perf_events, mmsgs, logs) =
            DeriveTest.perform_defaults (Derive.r_events res)

    equal logs []
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _)
            <- map snd mmsgs]
        [(0, 72), (1, 73)]

test_overlapping_controls = do
    -- Make sure an event that has a different control during its decay winds
    -- up in its own channel.
    let res = DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("cc1", [(0, 0, "0"), (1, 0, "1")])
            ]
    let (_, mmsgs, _) = DeriveTest.perform_defaults (Derive.r_events res)
    let extract_mm = filter (Midi.is_note . snd)
    equal (fst $ DeriveTest.extract (e_control "cc1") res)
        [Just [(0, 0)], Just [(1, 1)]]
    equal (extract_mm mmsgs)
        [ (0, Midi.ChannelMessage 0 (Midi.NoteOn 60 100))
        , (1000, Midi.ChannelMessage 0 (Midi.NoteOff 60 100))
        , (1000, Midi.ChannelMessage 1 (Midi.NoteOn 60 100))
        , (2000, Midi.ChannelMessage 1 (Midi.NoteOff 60 100))
        ]

    -- Event is far enough away for the control to not interfere.
    let res = DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, ""), (5, 1, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("cc1", [(0, 0, "0"), (5, 0, "1")])
            ]
    let (_, mmsgs, _) = DeriveTest.perform_defaults (Derive.r_events res)
    equal (extract_mm mmsgs)
        [ (0, Midi.ChannelMessage 0 (Midi.NoteOn 60 100))
        , (1000, Midi.ChannelMessage 0 (Midi.NoteOff 60 100))
        , (5000, Midi.ChannelMessage 0 (Midi.NoteOn 60 100))
        , (6000, Midi.ChannelMessage 0 (Midi.NoteOff 60 100))
        ]

e_control :: String -> Score.Event -> Maybe [(Signal.X, Signal.Y)]
e_control cont = fmap (Signal.unsignal . Score.typed_val)
    . Map.lookup (Score.Control cont) . Score.event_controls

test_control = do
    let res = DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, ""), (1, 1, "")])
            , ("*twelve", [(0, 1, "4c"), (1, 1, "4c#")])
            , ("cc1", [(0, 0, "1"), (1, 0, "i .75"), (2, 0, "i 0")])
            ]
    let (perf_events, mmsgs, logs) =
            DeriveTest.perform_defaults (Derive.r_events res)

    -- Cursory checks, more detailed checks are in more Note_test and
    -- Control_test.
    equal (map DeriveTest.show_log logs) []
    equal (fst $ extract_events res) [(0, 1, ""), (1, 1, "")]
    equal (length perf_events) 2

    -- Just make sure it did in fact emit ccs.
    check $ any Midi.is_cc (map snd mmsgs)

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    -- TODO and it belongs in TrackWarp_test now
    let track_id = Types.TrackId (UiTest.mkid "warp")
        warp = Internal.tempo_to_warp (Signal.constant 2)
        track_warps = [TrackWarp.Collection
                0 2 UiTest.default_block_id [track_id] warp]
    let f = TrackWarp.inverse_tempo_func track_warps
        with_block pos = [(UiTest.default_block_id, [(track_id, pos)])]
    -- Fast tempo means ScoreTime passes quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is past the end of the block.
    equal (map (f . RealTime.seconds) [0..2])
        [with_block 0, with_block 2, []]

test_tempo_roundtrip = do
    let track_id = Types.TrackId (UiTest.mkid "warp")
        warp = Internal.tempo_to_warp (Signal.constant 0.987)
        track_warps = [TrackWarp.Collection
                0 10 UiTest.default_block_id [track_id] warp]
    let inv = TrackWarp.inverse_tempo_func track_warps
        tempo = TrackWarp.tempo_func track_warps
    let rtimes = concatMap (tempo UiTest.default_block_id track_id)
            (Seq.range 0 3 1)
        stimes = concatMap inv rtimes
    pprint rtimes
    pprint stimes
    -- expected failure
    -- equal (map snd (concatMap snd stimes)) [0..3]

test_tempo = do
    let extract = e_floor . DeriveTest.e_event
        e_floor (start, dur, text) =
            (floor (secs start), floor (secs dur), text)
        secs = RealTime.to_seconds
    let f tempo_track =
            DeriveTest.extract extract $ DeriveTest.derive_tracks
                [ ("tempo", tempo_track)
                , ("*twelve", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
                , (">", [(0, 10, "--1"), (10, 10, "--2"), (20, 10, "--3")])
                ]

    equal (f [(0, 0, "2")]) $
        ([(0, 5, "--1"), (5, 5, "--2"), (10, 5, "--3")], [])

    -- Slow down.
    equal (f [(0, 0, "2"), (20, 0, "i 1")]) $
        ([(0, 5, "--1"), (5, 7, "--2"), (13, 10, "--3")], [])
    equal (f [(0, 0, "2"), (20, 0, "i 0")]) $
        ([(0, 6, "--1"), (6, 29, "--2"), (35, 10000, "--3")], [])
    -- Speed up.
    equal (f [(0, 0, "1"), (20, 0, "i 2")]) $
        ([(0, 8, "--1"), (8, 5, "--2"), (14, 5, "--3")], [])
    equal (f [(0, 0, "0"), (20, 0, "i 2")]) $
        ([(0, 1028, "--1"), (1028, 7, "--2"), (1035, 5, "--3")], [])

    -- Change tempo.
    equal (f [(0, 0, "1"), (10, 0, "2")]) $
        ([(0, 10, "--1"), (10, 5, "--2"), (15, 5, "--3")], [])

test_named_pitch = do
    let pname = Score.Control "psig"
        run op = DeriveTest.eval State.empty (op $ Derive.named_nn_at pname 2)
        pitch = DeriveTest.mkpitch "a"
        with_const pname = Derive.with_constant_pitch
            (Just (Score.Control pname)) DeriveTest.default_scale pitch
    equal (run (with_const "psig")) (Right (Just 60))
    equal (run (with_const "bad")) (Right Nothing)
    -- I don't have relative pitch signals anymore.  There's no reason
    -- I couldn't add them back, but now that transposition is handled by
    -- separate control signals I don't need it so much anymore.
    -- let add1 = Derive.with_relative_pitch (Just pname) (??)
    --         (PitchSignal.constant Pitch.relative 1)
    -- equal (run (with_const . add1))
    --     (Right (Just (Pitch.Degree 43)))

test_block_end = do
    -- Make sure the pitch for the sub block event is trimmed to the end
    -- of the block, since there's no next event for it.
    let res = DeriveTest.extract DeriveTest.e_pitch $ DeriveTest.derive_blocks
            [ ("p",
                [ (">i1", [(0, 1, "sub"), (1, 1, "")])
                , ("*twelve", [(0, 0, "5d"), (1, 0, "5e")])
                ])
            , ("sub", [(">", [(0, 1, "")])])
            ]
    equal res ([[(0, 74)], [(1, 76)]], [])

-- test_negative_duration = do
--     let extract = DeriveTest.extract (\e -> DeriveTest.e_event e) Log.msg_string
--     let run evts = extract $ DeriveTest.derive_tracks
--             [(DeriveTest.default_inst_title, evts)]
-- 
--     let deflt = Derive.negative_duration_default
--     -- events get lined up
--     equal (run [(1, -1, "--1"), (3, -2, "--2")])
--         (Right [(1, 2, "--1"), (3, deflt, "--2")], [])
--     -- rest
--     equal (run [(1, -1, "--1"), (3, -1, "--2")])
--         (Right [(1, 1, "--1"), (3, deflt, "--2")], [])
--     -- 0 dur is treated as negative
--     equal (run [(1, -1, "--1"), (3, 0, "--2")])
--         (Right [(1, 2, "--1"), (3, deflt, "--2")], [])
-- 
--     let run evts = extract $ DeriveTest.derive_blocks
--             [ ("b1", [(">", evts)])
--             , ("sub", [(">", [(1, -1, "--1"), (2, -1, "--2")])])
--             ]
--     -- last event extends up to "rest" at 5
--     equal (run [(4, -4, "sub"), (6, -1, "")])
--         (Right [(2, 2, "--1"), (4, 1, "--2"), (6, deflt, "")], [])
-- 
--     -- events between derivers work
--     equal (run [(4, -4, "sub"), (8, -4, "sub")])
--         (Right [(2, 2, "--1"), (4, 2, "--2"), (6, 2, "--1"),
--             (8, deflt, "--2")],
--         [])
--     let run evts = extract $ DeriveTest.derive_blocks
--             [ ("b1", [(">", evts)])
--             , ("sub",
--                 [ (">i1", [(1, -1, "--11"), (2, -1, "--12")])
--                 , (">i2", [(1, -1, "--21")])
--                 ])
--             ]
--     -- as above but both last events are extended
--     equal (run [(4, -4, "sub"), (6, -1, "")])
--         (Right [(2, 2, "--11"), (2, 3, "--21"), (4, 1, "--12"),
--             (6, deflt, "")], [])
-- 
--     -- events between derivers work
--     equal (run [(4, -4, "sub"), (8, -4, "sub")])
--         (Right
--             [ (2, 2, "--11"), (2, 2, "--21"), (4, 2, "--12")
--             , (6, 2, "--11"), (6, deflt, "--21"), (8, deflt, "--12") ],
--         [])

-- * regression

-- These tests are derived from debugging and test specific things that went
-- wrong.  I don't think they're likely enough to come up again that I want
-- to figure out the right place to test, but as long as they exist I might
-- as well keep them for regressions.

test_regress_pedal = do
    -- Make sure a pedal halfway through a note really only turns on halfway
    -- through the note.
    let res = derive_blocks blocks
    let (_perf_events, mmsgs, _logs) =
            DeriveTest.perform_defaults (Derive.r_events res)
    let pedal_on = [(ts, c)
            | (ts, Midi.ChannelMessage _ (Midi.ControlChange 64 c)) <- mmsgs
            , c /= 0]
    equal pedal_on [(12500, 127)]
    where
    blocks = [(("b10",
        [("damper-pedal", [(12.5, 2, "`ped`")]),
         (">s/1", [ (10.0, 5.0, "")]),
         ("*", [ (10.0, 0.0, "4f")])
        ]), [(1, 2), (2, 3)])]

test_regress_event_end1 = do
    -- Ensure that notes get the proper next event even when it has been
    -- sliced off.  Previously it extended to the block end and the notes
    -- wound up with too much pitch signal.
    let res = derive_blocks blocks
        extract e = (Score.event_start e, Score.event_duration e,
                DeriveTest.e_pitch e)
    equal (DeriveTest.extract extract res)
        ([ (0, 2, [(0, 60)])
        , (2, 2, [(2, 62)])
        ], [])
    where
    blocks = [(("b0", b0), [(1, 2), (2, 3)])]
    b0 =
        [ (">", [(0, 0, "`arp-up`")])
        , (">s/1", [(0, 2, ""), (2, 2, "")])
        , ("*", [(0, 0, "4c"), (2, 0, "4d")])
        ]

test_regress_event_end2 = do
    let res = derive_blocks blocks
        extract e = (Score.event_start e, Score.event_duration e,
                DeriveTest.e_pitch e)
    equal (DeriveTest.extract extract res)
        ([ (5, 2, [(5, 60)])
        , (7, 2, [(7, 62)])
        ], [])
    where
    blocks = [(("b0",
       [(">", [(5, 0, "`arp-up`")]),
        (">", [(5, 2, ""), (7, 2, "")]),
        ("*", [(5, 0, "4c"), (7, 0, "4d")])
        ]),
      [(1, 2), (2, 3)])]

-- TODO do something about this, probably have to use quickcheck since
-- the values are so fiddly, but until then this is documentation for the
-- problem.
test_roundoff = do
    let res = derive_blocks blocks
    pprint $ first head (DeriveTest.extract id res)
    pprint (DeriveTest.extract DeriveTest.e_note2 res)
    let (_perf_events, mmsgs, _logs) =
            DeriveTest.perform_defaults (Derive.r_events res)
    pprint mmsgs
    where
    blocks = [b9, b28]
    b9 = (("b9",
       [("tempo", [(0, 0, "1.5")]),
        (">s/1", [(10.0, 0.0, "`arp-up`")]),
        (">s/1", [(10.0, 3.5, "b28")]),
        ("*", [(10.0, 0.0, "5c")])]),
      [(1, 2), (2, 3), (3, 4)])
    b28 = (("b28",
       [("tempo", [(0.0, 0.0, "1"), (3.5, 0.0, "i 15")]),
        (">",
         [(0.0, 0.5, ""), (0.5, 0.5, ""), (1.0, 0.5, ""), (1.5, 0.5, ""),
          (2.0, 0.5, ""), (2.5, 0.5, ""), (3.0, 0.5, ""), (3.5, 0.5, ""),
          (4.0, 0.5, ""), (4.5, 0.5, ""), (5.0, 0.5, "")])]),
      [(1, 2)])

derive_blocks :: [(UiTest.BlockSpec, [Skeleton.Edge])] -> Derive.Result
derive_blocks blocks = DeriveTest.derive_block state (UiTest.bid block_name)
    where
    state = UiTest.exec State.empty (UiTest.mkblocks_skel blocks)
    ((block_name, _), _) : _ = blocks

-- * util

r_tempo :: Derive.Result -> Transport.TempoFunction
r_tempo = TrackWarp.tempo_func . Derive.r_track_warps

r_inv_tempo :: Derive.Result -> Transport.InverseTempoFunction
r_inv_tempo = TrackWarp.inverse_tempo_func . Derive.r_track_warps

inst_title = DeriveTest.default_inst_title

extract_events = DeriveTest.extract DeriveTest.e_event

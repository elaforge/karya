{- | This also tests Derive.Note and Derive.Control since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Numeric

import Util.Test
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import Ui
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import qualified Midi.Midi as Midi

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let (events, logs) = DeriveTest.e_val_right $ DeriveTest.derive_tracks
            [ (inst_title ++ " +a1", [(0, 16, "n +a0"), (16, 16, "n +a2")])
            , ("*twelve", [(0, 0, "4c"), (16, 0, "4c#")])
            ]
    let (perf_events, convert_warns, mmsgs, midi_warns) = DeriveTest.perform
            DeriveTest.default_inst_config events

    equal logs []
    equal (extract_events events) [(0, 16, "n +a0"), (16, 16, "n +a2")]

    -- 2: conversion to midi perf events
    equal convert_warns []
    let evt = (,,,,) (Instrument.inst_name perf_inst)
    equal (map extract_perf_event perf_events)
        [ evt (Just "a0") 0 16 (mkstack [("b1", "b1.t0", (0, 16))])
        , evt (Just "a1+a2") 16 16 (mkstack [("b1", "b1.t0", (16, 32))])
        ]

    -- 3: performance to midi protocol events
    equal [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- map snd mmsgs]
        [1, 60, 0, 61]
    equal midi_warns []
    where
    mkstack = map (\(bid, tid, pos) ->
        (UiTest.bid bid, Just (UiTest.tid tid), Just pos))
    extract_perf_event (Perform.Event inst start dur _controls _pitch stack) =
        (Instrument.inst_name inst,
            fmap ks_name (Instrument.inst_keyswitch inst), start, dur, stack)
    ks_name (Instrument.Keyswitch name _) = name

test_call = do
    let (events, logs) = DeriveTest.e_val_right $ DeriveTest.derive_tracks
            [ (">", [(0, 8, ""), (8, 8, "abs-trill |")])
            , ("*twelve", [(0, 0, "4c"), (8, 0, "4c#")])
            ]
    plist (map Log.msg_string logs)
    pprint events

test_subderive = do
    let run evts = DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i1", evts)
                ])
            , ("sub", [(">i2", [(1, 1, "--sub1")])])
            , ("empty", [(">i", [])])
            ]
    let (events, msgs) = DeriveTest.e_val (run [(0, 1, "nosuch"),
            (1, 1, "empty"), (2, 1, "b0"), (3, 1, "--x")])
    -- errors don't stop derivation
    equal (fmap extract_events events) (Right [(1.5, 0.5, "--x")])
    strings_like (map Log.msg_string msgs)
        ["call not found: nosuch", "block with zero duration",
            "recursive block"]
    let mkstack (from, to) = Just
            [(UiTest.bid "b0", Just (UiTest.tid "b0.t1"), Just (from, to))]
    equal (map Log.msg_stack msgs) $ map mkstack
        [(0, 1), (1, 2), (2, 3)]

    let (events, _tempo, inv_tempo, logs, _state) = run
            [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "--b2")]
    equal (fmap extract_events events) $
        Right [(0, 4, "--b1"), (6, 2, "--sub1"), (8, 0.5, "--b2")]
    equal (fmap (map Score.event_instrument) events) $
        Right (map (Just . Score.Instrument) ["i1", "i2", "i1"])

    equal (map Log.msg_string logs) []

    let b0 pos = (UiTest.bid "b0",
            [(UiTest.tid ("b0.t" ++ show n), ScoreTime pos) | n <- [ 1, 0]])
        sub pos = (UiTest.bid "sub", [(UiTest.tid "sub.t0", ScoreTime pos)])
        pos = map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    equal (map List.sort pos)
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16], []]

    -- For eyeball verification.
    -- pprint events
    -- pprint $ zip [0,2..] $ map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    -- pprint $ Derive.state_track_warps state

test_subderive_error = do
    let run evts = DeriveTest.derive_blocks
            [ ("b0", [ (">i1", evts) ])
            , ("sub", [(">", []), ("add *error syntax", [(1, 1, "--sub1")])])
            ]
    let (val, logs) = (DeriveTest.e_logs $ run [(0, 1, "sub")])
    equal val (Right [])
    strings_like logs ["subderiving: * failed to parse"]

test_subderive_multiple = do
    -- make sure subderiving a block with multiple tracks works correctly
    let (Right events, logs) = DeriveTest.e_val $ DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i", [(0, 8, "sub")])
                , ("vel", [(0, 0, "1"), (8, 0, "i 0")])
                ])
            , ("sub",
                [ (">", [(0, 1, "--1-1"), (1, 1, "--1-2")])
                , ("*twelve", [(0, 0, "4c"), (1, 0, "4d")])
                , (">", [(0, 1, "--2-1"), (1, 1, "--2-2")])
                , ("*twelve", [(0, 0, "5c"), (1, 0, "5d")])
                ])
            ]
    let (_, _, mmsgs, _) = DeriveTest.perform
            DeriveTest.default_inst_config events
    equal logs []
    equal (DeriveTest.note_on_times mmsgs)
        [ (0, 60, 127), (0, 72, 127)
        , (2000, 62, 63), (2000, 74, 63)
        ]

test_multiple_subderive = do
    -- make sure a sequence of sub calls works
    let (events, _, inv_tempo, logs, _) =
            DeriveTest.derive_blocks
            [ ("b0", [(">i1", [(0, 2, "sub"), (2, 2, "sub"), (4, 2, "sub")])])
            , ("sub", [(">", [(0, 1, "--sub1")])])
            ]
    equal logs []
    equal (fmap extract_events events) $
        Right [(0, 2, "--sub1"), (2, 2, "--sub1"), (4, 2, "--sub1")]
    -- Empty inst inherits calling inst.
    equal (fmap (map Score.event_instrument) events) $
        Right (replicate 3 (Just (Score.Instrument "i1")))

    let pos = map inv_tempo (map Timestamp.seconds [0..6])
    let b0 pos = (UiTest.bid "b0", [(UiTest.tid ("b0.t0"), ScoreTime pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.tid "sub.t0", ScoreTime pos)])
    equal (map List.sort pos)
        [ [b0 0, sub 0], [b0 1, sub 0.5], [b0 2, sub 0], [b0 3, sub 0.5]
        , [b0 4, sub 0], [b0 5, sub 0.5], []
        ]

test_tempo_compose = do
    let run tempo events sub_tempo =
            Log.trace_logs logs $ fmap extract_events score_events
            where
            (score_events, _tempo, _inv_tempo, logs, _state) =
                DeriveTest.derive_blocks
                    [ ("b0", [("tempo", tempo), (">i1", events)])
                    , ("sub",
                        [ ("tempo", sub_tempo)
                        , (">", [(0, 1, ""), (1, 1, "")])
                        ])
                    ]

    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "1")]) $
        Right [(2, 1, ""), (3, 1, "")]
    -- Tempo of the sub doesn't matter since it will be stretched to fit.
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, "2")]) $
        Right [(2, 1, ""), (3, 1, "")]
    equal (run [(0, 0, "1")] [(2, 2, "sub")] [(0, 0, ".5")]) $
        Right [(2, 1, ""), (3, 1, "")]

    -- Make sure the top level block doesn't get stretched.
    equal (run [(0, 0, "2")] [(0, 2, "--1"), (2, 2, "sub"), (4, 2, "--2")]
        [(0, 0, ".5")]) $
            Right [(0, 1, "--1"), (1, 0.5, ""), (1.5, 0.5, ""), (2, 1, "--2")]

    equal (run [(0, 0, "1"), (2, 0, "2")] [(0, 2, "sub")] [(0, 0, "1")]) $
            Right [(0, 1, ""), (1, 1, "")]
    equal (run [(0, 0, "2"), (4, 0, ".5")] [(0, 4, "sub"), (4, 4, "sub")]
        [(0, 0, "1")]) $
            Right [(0, 1, ""), (1, 1, ""), (2, 4, ""), (6, 4, "")]

    -- TODO test when the subblock has a tempo too
    -- type TempoFunction = BlockId -> TrackId -> ScoreTime
    --     -> Maybe Timestamp.Timestamp

show_log msg
    | null (Log.msg_signal msg) = Log.format_msg msg
    | otherwise = "*** " ++ Log.msg_string msg ++ "\n"
        ++ show_sig (Log.msg_signal msg)

show_sig sig =
    Seq.join "\n" [showf x ++ "\t -> " ++ showf y | (x, y) <- sig]
    where showf f = Numeric.showFFloat (Just 3) f ""

test_warp_ops = do
    let run op = fmap extract $ DeriveTest.run State.empty (op record)
            where extract (val, _, logs) = Log.trace_logs logs val
        record = do
            x0 <- Derive.now
            x1 <- Derive.score_to_real 2
            return [x0, x1]

    equal (run id) $ Right [0, 2]
    equal (run (Derive.d_stretch 2)) $ Right [0, 4]
    equal (run (Derive.d_at 2)) $ Right [2, 4]
    equal (run (Derive.d_at 2 . Derive.d_stretch 2)) $ Right [2, 6]
    equal (run (Derive.d_stretch 2 . Derive.d_at 2)) $ Right [4, 8]
    equal (run (Derive.d_at 2 . Derive.d_stretch 0.5)) $ Right [2, 3]
    equal (run (Derive.d_stretch 0.5 . Derive.d_at 2)) $ Right [1, 2]

    -- test compose
    let plain = Signal.signal [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = Signal.signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 200)]

    equal (run (Derive.d_warp plain)) $ Right [0, 2]
    equal (run (Derive.d_at 2 . Derive.d_warp plain)) $ Right [2, 4]
    equal (run (Derive.d_stretch 2 . Derive.d_warp plain)) $
        Right [0, 4]

    equal (run (Derive.d_warp plain . Derive.d_warp plain)) $
        Right [0, 2]
    equal (run (Derive.d_warp plain . Derive.d_warp slow)) $
        Right [0, 4]
    equal (run (Derive.d_warp slow . Derive.d_warp plain)) $
        Right [0, 4]
    equal (run (Derive.d_warp slow . Derive.d_warp slow)) $
        Right [0, 8]

    equal (run (Derive.d_at 1 . Derive.d_stretch 2 . Derive.d_warp slow
        . Derive.d_warp slow)) $
            Right [1, 17]

test_real_to_score = do
    let extract (val, _, logs) = Log.trace_logs logs val
    let f do_warp pos = fmap extract $ DeriveTest.run State.empty $
            do_warp (Derive.real_to_score =<< Derive.score_to_real pos)
    equal (f id 1) (Right 1)
    equal (f (Derive.d_at 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5) 1) (Right 1)
    let slow = Signal.signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 200)]
    equal (f (Derive.d_warp slow . Derive.d_stretch 5 . Derive.d_at 5) 1)
        (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5 . Derive.d_warp slow) 1)
        (Right 1)

test_control_warp_ops = do
    let controls = [(Score.Control "cont",
            Signal.signal [(0, 1), (2, 2), (4, 0)])]
    let set_controls = Derive.modify $ \st ->
            st { Derive.state_controls = Score.warped_controls controls }
    let run op = fmap extract $
            DeriveTest.run State.empty
                (set_controls >> op Derive.unwarped_controls)
            where
            extract ((conts, pitch), _, logs) = Log.trace_logs logs
                (Signal.unsignal (snd (head (Map.toList conts))),
                    PitchSignal.unsignal pitch)
    equal (run id) $ Right
        ([(0, 1), (2, 2), (4, 0)], [(0, (60, 60, 0))])
    equal (run $ Derive.d_control_at 2) $ Right
        ([(2, 1), (4, 2), (6, 0)], [(2, (60, 60, 0))])
    equal (run $ Derive.d_control_stretch 2) $ Right
        ([(0, 1), (4, 2), (8, 0)], [(0, (60, 60, 0))])

    let tempo = Derive.d_control_warp . Derive.tempo_to_warp . Signal.signal
    equal (run $ tempo [(0, 1)]) $ Right
        ([(0, 1), (2, 2), (4, 0)], [(0, (60, 60, 0))])
    equal (run $ tempo [(0, 1), (2, 0.5)]) $ Right
        ([(0, 1), (2, 2), (6, 0)], [(0, (60, 60, 0))])

track_specs =
    [ ("tempo", [(0, 0, "2")])
    , (">i1", [(0, 8, "--b1"), (8, 8, "--b2"), (16, 1, "--b3")])
    ]

test_tempo_funcs1 = do
    let bid = UiTest.bid "b0"

    let ([t_tid, tid1], ui_state) = UiTest.run State.empty $
            UiTest.mkstate "b0" track_specs
    let (_, tempo, inv_tempo, logs, _) = DeriveTest.derive_block ui_state bid
    equal logs []

    -- [(BlockId, [(TrackId, ScoreTime)])]
    let b0 pos = (bid, [(tid1, pos), (t_tid, pos)])
    equal (map inv_tempo (map Timestamp.seconds [0, 2 .. 10]))
        [[b0 0], [b0 4], [b0 8], [b0 12], [b0 16], []]

    equal (map (tempo bid t_tid) [0, 2 .. 10])
        (map (Just . Timestamp.Timestamp) [0, 1000 .. 5000])

test_tempo_funcs2 = do
    let ([t_tid1, tid1, t_tid2, tid2], ui_state) = UiTest.run State.empty $
            UiTest.mkstate "b0" $ track_specs
                ++ [ ("tempo", [(0, 0, "1")])
                , (">i2", [(0, 16, "--2b1")])
                ]
        bid = UiTest.bid "b0"
    let (_, tempo, inv_tempo, logs, _) = DeriveTest.derive_block ui_state bid
    equal logs []
    equal (map (tempo bid t_tid1) [0, 2 .. 10])
        (map (Just . Timestamp.Timestamp) [0, 1000 .. 5000])
    equal (map (tempo bid t_tid2) [0, 2 .. 10])
        (map (Just . Timestamp.Timestamp) [0, 2000 .. 10000])
    let b0 pos = (bid, [(tid1, pos), (t_tid1, pos)])
        b1 pos = (bid, [(tid2, pos), (t_tid2, pos)])

    equal (map inv_tempo (map Timestamp.seconds [0, 2 .. 10]))
        [[b1 0, b0 0], [b1 2, b0 4], [b1 4, b0 8], [b1 6, b0 12],
            [b1 8, b0 16], [b1 10]]

    -- test multiple tempo
    -- test subderive

test_fractional_pitch = do
    -- A pitch that requires pitch bends should distribute across multiple
    -- channels.  Yes, this is also tested in Perform.Midi.Perform, but this
    -- also tests that the pitch signal is trimmed properly by
    -- Note.trim_pitches.
    let (events, derive_logs) = DeriveTest.e_val_right $
            DeriveTest.derive_tracks
                [ (inst_title, [(0, 16, ""), (16, 16, "")])
                , ("*semar", [(0, 16, "1"), (16, 16, "2")])
                ]
    let (_perf_events, convert_warns, mmsgs, perform_warns) =
            DeriveTest.perform DeriveTest.default_inst_config events

    equal derive_logs []
    -- pprint events
    equal convert_warns []
    -- pprint _perf_events
    equal perform_warns []
    -- pprint mmsgs
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _)
            <- map snd mmsgs]
        [(0, 72), (1, 73)]

test_control = do
    let (events, logs) = DeriveTest.e_val_right $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, "n +a1"), (1, 1, "n +a2")])
            , ("*twelve", [(0, 1, "4c"), (1, 1, "4c#")])
            , ("cc1", [(0, 0, "1"), (1, 0, "i .75"), (2, 0, "i 0")])
            ]
    let (perf_events, convert_warns, mmsgs, midi_warns) = DeriveTest.perform
            DeriveTest.default_inst_config events

    -- Cursory checks, more detailed checks are in more Note_test and
    -- Control_test.
    equal logs []
    equal (extract_events events) [(0, 1, "n +a1"), (1, 1, "n +a2")]
    equal (map (Set.toList . Score.attrs_set . Score.event_attributes) events)
        [["a1"], ["a2"]]

    equal convert_warns []
    equal (length perf_events) 2
    equal (map Perform.event_instrument perf_events)
        [set_ks perf_inst "a1" 2, set_ks perf_inst "a2" 3]

    -- Just make sure it did in fact emit ccs.
    check $ any Midi.is_cc (map snd mmsgs)
    equal midi_warns []
    where
    set_ks inst ks nn = inst
        { Instrument.inst_keyswitch = Just (Instrument.Keyswitch ks nn) }

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    let track_id = Types.TrackId (UiTest.mkid "warp")
        warp = Score.signal_to_warp (Derive.tempo_to_warp (Signal.constant 2))
        track_warps =
            [Derive.TrackWarp 0 2 UiTest.default_block_id [track_id] warp]
    let f = Derive.make_inverse_tempo_func track_warps
        with_block pos = [(UiTest.default_block_id, [(track_id, pos)])]
    -- Fast tempo means ScoreTime passes quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is past the end of the block.
    equal (map f (map Timestamp.seconds [0..2]))
        [with_block 0, with_block 2, []]

test_tempo = do
    let f tempo_track =
            Log.trace_logs logs (fmap (map floor_event . extract_events) events)
            where
            (events, logs) = DeriveTest.e_val $ DeriveTest.derive_tracks
                [ ("tempo", tempo_track)
                , (">", [(0, 10, "--1"), (10, 10, "--2"), (20, 10, "--3")])
                , ("*twelve", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
                ]
        floor_event :: (Double, Double, String) -> (Integer, Integer, String)
        floor_event (start, dur, text) = (floor start, floor dur, text)

    equal (f [(0, 0, "2")]) $
        Right [(0, 5, "--1"), (5, 5, "--2"), (10, 5, "--3")]

    -- Slow down.
    equal (f [(0, 0, "2"), (20, 0, "i 1")]) $
        Right [(0, 5, "--1"), (5, 7, "--2"), (13, 10, "--3")]
    equal (f [(0, 0, "2"), (20, 0, "i 0")]) $
        Right [(0, 6, "--1"), (6, 29, "--2"), (35, 10000, "--3")]
    -- Speed up.
    equal (f [(0, 0, "1"), (20, 0, "i 2")]) $
        Right [(0, 8, "--1"), (8, 5, "--2"), (14, 5, "--3")]
    equal (f [(0, 0, "0"), (20, 0, "i 2")]) $
        Right [(0, 1028, "--1"), (1028, 7, "--2"), (1035, 5, "--3")]

    -- Change tempo.
    equal (f [(0, 0, "1"), (10, 0, "2")]) $
        Right [(0, 10, "--1"), (10, 5, "--2"), (15, 5, "--3")]

test_negative_duration = do
    let extract = DeriveTest.extract (\e -> DeriveTest.e_event e) Log.msg_string
    let run evts = extract $ DeriveTest.derive_tracks_tempo
            [(DeriveTest.default_inst_title, evts)]

    let deflt = Derive.negative_duration_default
    -- events get lined up
    equal (run [(1, -1, "--1"), (3, -2, "--2")])
        (Right [(1, 2, "--1"), (3, deflt, "--2")], [])
    -- rest
    equal (run [(1, -1, "--1"), (3, -1, "--2")])
        (Right [(1, 1, "--1"), (3, deflt, "--2")], [])
    -- 0 dur is treated as negative
    equal (run [(1, -1, "--1"), (3, 0, "--2")])
        (Right [(1, 2, "--1"), (3, deflt, "--2")], [])

    let run evts = extract $ DeriveTest.derive_blocks
            [ ("b1", [(">", evts)])
            , ("sub", [(">", [(1, -1, "--1"), (2, -1, "--2")])])
            ]
    -- last event extends up to "rest" at 5
    equal (run [(4, -4, "sub"), (6, -1, "")])
        (Right [(2, 2, "--1"), (4, 1, "--2"), (6, deflt, "")], [])

    -- events between derivers work
    equal (run [(4, -4, "sub"), (8, -4, "sub")])
        (Right [(2, 2, "--1"), (4, 2, "--2"), (6, 2, "--1"),
            (8, deflt, "--2")],
        [])
    let run evts = extract $ DeriveTest.derive_blocks
            [ ("b1", [(">", evts)])
            , ("sub",
                [ (">i1", [(1, -1, "--11"), (2, -1, "--12")])
                , (">i2", [(1, -1, "--21")])
                ])
            ]
    -- as above but both last events are extended
    equal (run [(4, -4, "sub"), (6, -1, "")])
        (Right [(2, 2, "--11"), (2, 3, "--21"), (4, 1, "--12"),
            (6, deflt, "")], [])

    -- events between derivers work
    equal (run [(4, -4, "sub"), (8, -4, "sub")])
        (Right
            [ (2, 2, "--11"), (2, 2, "--21"), (4, 2, "--12")
            , (6, 2, "--11"), (6, deflt, "--21"), (8, deflt, "--12") ],
        [])

-- * setup

inst_title = DeriveTest.default_inst_title
perf_inst = DeriveTest.default_perf_inst

extract_events :: [Score.Event] -> [(Double, Double, String)]
extract_events = map $ \event ->
    (realToFrac (Score.event_start event),
        realToFrac (Score.event_duration event), Score.event_string event)

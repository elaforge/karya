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
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Midi.Midi as Midi

import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.Pitch as Pitch
import Perform.SignalBase (Method(..))
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


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
        ["CallNotFound: Symbol \"nosuch\"", "block with zero duration",
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
            [(UiTest.tid ("b0.t" ++ show n), TrackPos pos) | n <- [ 1, 0]])
        sub pos = (UiTest.bid "sub", [(UiTest.tid "sub.t0", TrackPos pos)])
        pos = map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    equal (map List.sort pos)
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16], []]

    -- For eyeball verification.
    -- pprint events
    -- pprint $ zip [0,2..] $ map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    -- pprint $ Derive.state_track_warps state

test_subderive_multiple = do
    -- make sure subderiving a block with multiple tracks works correctly
    let (Right events, _) = DeriveTest.e_val $ DeriveTest.derive_blocks
            [ ("b0",
                [ ("tempo", [(0, 0, "2")])
                , (">i", [(0, 8, "sub")])
                , ("vel", [(0, 0, "1"), (8, 0, "i, 0")])
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
    let b0 pos = (UiTest.bid "b0", [(UiTest.tid ("b0.t0"), TrackPos pos)])
        sub pos = (UiTest.bid "sub", [(UiTest.tid "sub.t0", TrackPos pos)])
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
    -- type TempoFunction = BlockId -> TrackId -> TrackPos
    --     -> Maybe Timestamp.Timestamp

show_log msg
    | null (Log.msg_signal msg) = Log.format_msg msg
    | otherwise = "*** " ++ Log.msg_string msg ++ "\n"
        ++ show_sig (Log.msg_signal msg)

show_sig sig =
    Seq.join "\n" [showf x ++ "\t -> " ++ showf y | (x, y) <- sig]
    where showf f = Numeric.showFFloat (Just 3) f ""

test_tempo_ops = do
    let run op = fmap extract $ DeriveTest.run State.empty op
            where
            extract :: ([TrackPos], b, [Log.Msg]) -> [TrackPos]
            extract (val, _, logs) = Log.trace_logs logs val
        record = do
            x0 <- Derive.local_to_global 0
            x1 <- Derive.local_to_global 2
            return [x0, x1]

    equal (run record) $ Right [0, 2]
    equal (run (Derive.d_stretch 2 $ record)) $ Right [0, 4]
    equal (run (Derive.d_at 2 $ record)) $ Right [2, 4]
    equal (run (Derive.d_at 2 $ Derive.d_stretch 0.5 $ record)) $ Right [2, 3]
    equal (run (Derive.d_stretch 0.5 $ Derive.d_at 2 $ record)) $ Right [1, 2]

    -- test compose
    let plain = Signal.signal [(0, 0), (1, 1), (2, 2), (3, 3), (100, 100)]
        slow = Signal.signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 200)]

    equal (run (Derive.d_warp plain $ record)) $ Right [0, 2]
    equal (run (Derive.d_at 2 $ Derive.d_warp plain $ record)) $ Right [2, 4]
    equal (run (Derive.d_stretch 2 $ Derive.d_warp plain $ record)) $
        Right [0, 4]

    equal (run (Derive.d_warp plain $ Derive.d_warp plain $ record)) $
        Right [0, 2]
    equal (run (Derive.d_warp plain $ Derive.d_warp slow $ record)) $
        Right [0, 4]
    equal (run (Derive.d_warp slow $ Derive.d_warp plain $ record)) $
        Right [0, 4]
    equal (run (Derive.d_warp slow $ Derive.d_warp slow $ record)) $
        Right [0, 8]

    equal (run (Derive.d_at 1 $ Derive.d_stretch 2 $ Derive.d_warp slow $
        Derive.d_warp slow $ record)) $
            Right [1, 17]

test_global_to_local = do
    let extract (val, _, logs) = Log.trace_logs logs val
    let f do_warp pos = fmap extract $ DeriveTest.run State.empty $
            do_warp (Derive.global_to_local =<< Derive.local_to_global pos)
    equal (f id 1) (Right 1)
    equal (f (Derive.d_at 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5) 1) (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5) 1) (Right 1)
    let slow = Signal.signal [(0, 0), (1, 2), (2, 4), (3, 6), (100, 200)]
    equal (f (Derive.d_warp slow . Derive.d_stretch 5 . Derive.d_at 5) 1)
        (Right 1)
    equal (f (Derive.d_stretch 5 . Derive.d_at 5 . Derive.d_warp slow) 1)
        (Right 1)

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

    -- [(BlockId, [(TrackId, TrackPos)])]
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

test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let (events, logs) = DeriveTest.e_val_right $ DeriveTest.derive_tracks
            [ (inst_title ++ " +a1", [(0, 16, "+a0"), (16, 16, "+a2")])
            , ("*twelve", [(0, 16, "4c"), (16, 16, "4c#")])
            ]
    let (perf_events, convert_warns, mmsgs, midi_warns) = DeriveTest.perform
            DeriveTest.default_inst_config events

    equal logs []
    equal (extract_events events) [(0, 16, "+a0"), (16, 16, "+a2")]

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

test_control = do
    let (events, logs) = DeriveTest.e_val_right $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 1, "+a1"), (1, 1, "+a2")])
            , ("*twelve", [(0, 1, "4c"), (1, 1, "4c#")])
            , (Midi.Control.c_mod,
                [(0, 0, "1"), (1, 0, "i, .75"), (2, 0, "i, 0")])
            ]
    let (perf_events, convert_warns, mmsgs, midi_warns) = DeriveTest.perform
            DeriveTest.default_inst_config events

    -- Cursory checks, more detailed checks are in more Note_test and
    -- Control_test.
    equal logs []
    equal (extract_events events) [(0, 1, "+a1"), (1, 1, "+a2")]
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

test_relative_control = do
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (">", [(0, 1, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("+, cont", [(0, 0, "1")])
            , ("cont", [(0, 0, "0"), (2, 0, "i, 2"), (4, 0, "i, 0")])
            ]
    let extract = (\sig -> map (flip Signal.at sig) [0..5])
            . (Map.! Score.Control "cont") . Score.event_controls
    equal logs []
    equal (fmap (map extract) events) $ Right [[1, 2, 3, 2, 1, 1]]

    -- putting relative and absolute in the wrong order causes a warning
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("cont", [(0, 0, "1")])
            , ("+, cont", [(0, 0, "1")])
            ]
    let controls = Map.union Derive.initial_controls $
            Map.fromList [(Score.Control "cont", Signal.signal [(0, 1)])]
    equal (fmap (map Score.event_controls) events) $ Right [controls]
    strings_like logs ["no absolute control is in scope"]

test_relative_pitch = do
    let extract result = (fmap (map Score.event_pitch) events, logs)
            where (events, logs) = DeriveTest.e_logs result
    let f track = extract $ DeriveTest.derive_tracks
                [ (inst_title, [(0, 10, "")])
                , ("+, *semar", track)
                , ("*semar", [(0, 0, "1.")])
                ]
        base = Pitch.Degree 55 -- that's "1."
    let mksig = DeriveTest.pitch_signal (Pitch.ScaleId "semar")
    equal (f []) (Right [mksig [(0, Set, base)]], [])
    equal (f [(0, 0, "1/"), (1, 0, "i, 0")])
        (Right [mksig [(0, Set, base + 5), (1, Linear, base)]], [])

    -- empty relative pitch defaults to scale in scope
    let (pitches, logs) = extract $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("+, *", [(0, 0, "1/")])
            , ("*semar", [(0, 0, "1.")])
            ]
    equal logs []
    equal pitches $ Right [mksig [(0, Set, base + 5)]]

    -- putting relative and absolute in the wrong order overrides the relative
    let (pitches, logs) = extract $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("*semar", [(0, 0, "1.")])
            , ("+, *semar", [(0, 0, "1")])
            ]
    equal pitches $ Right [mksig [(0, Set, base)]]
    -- no warning because of default pitch
    strings_like logs []

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    let track_id = Types.TrackId (UiTest.mkid "warp")
        warp = Derive.make_warp (Derive.tempo_to_warp (Signal.constant 2))
        track_warps = [Derive.TrackWarp
            (TrackPos 0) (TrackPos 2) UiTest.default_block_id [track_id] warp]
    let f = Derive.make_inverse_tempo_func track_warps
        with_block pos = [(UiTest.default_block_id, [(track_id, pos)])]
    -- Fast tempo means TrackPos pass quickly relative to Timestamps.
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
    equal (f [(0, 0, "2"), (20, 0, "i, 1")]) $
        Right [(0, 5, "--1"), (5, 8, "--2"), (13, 10, "--3")]
    equal (f [(0, 0, "2"), (20, 0, "i, 0")]) $
        Right [(0, 6, "--1"), (6, 58, "--2"), (65, 10000, "--3")]
    -- Speed up.
    equal (f [(0, 0, "1"), (20, 0, "i, 2")]) $
        Right [(0, 8, "--1"), (8, 5, "--2"), (13, 4, "--3")]
    equal (f [(0, 0, "0"), (20, 0, "i, 2")]) $
        Right [(0, 108, "--1"), (108, 6, "--2"), (115, 5, "--3")]

    -- Change tempo.
    equal (f [(0, 0, "1"), (10, 0, "2")]) $
        Right [(0, 10, "--1"), (10, 5, "--2"), (15, 5, "--3")]


profile_deriver_performance = do
    -- test a large score for profiling
    let size = 10000
    let notes = cycle ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]
        pos = take size [0..]
        vels = cycle ["1", ".2", ".4", ".6"]
        tempo_pos = take (size `div` 10) [0, 10..]
        tempos = cycle ["1", "2", "3", "i1"]
        note_tracks name =
            [ (name, [(p, 1, "") | p <- pos])
            , ("*twelve", [(p, 0, note) | (p, note) <- zip pos notes])
            , ("vel", [(p, 0, vel) | (p, vel) <- zip pos vels])
            ]
    let (_, ui_state) = UiTest.run_mkstate $
            [("tempo", [(p, 0, t) | (p, t) <- zip tempo_pos tempos])]
            ++ note_tracks ">i1" ++ note_tracks ">i2"
    let blocks = State.state_blocks ui_state
    print (Map.size blocks)
    print $ Map.map (Track.events_length . Track.track_events)
        (State.state_tracks ui_state)

    -- let (events, logs) = derive_events ui_state d


-- * setup

inst_title = DeriveTest.default_inst_title
perf_inst = DeriveTest.default_perf_inst

extract_events :: [Score.Event] -> [(Double, Double, String)]
extract_events = map $ \event ->
    (realToFrac (Score.event_start event),
        realToFrac (Score.event_duration event), Score.event_string event)

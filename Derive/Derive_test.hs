{- | This also tests Derive.Note and Derive.Control since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Util.Test
import qualified Util.Log as Log

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


-- TODO
-- don't test subderive stuff already tested in Note_test

{-
-- Inspect the final state after running a derivation.
-- TODO what useful tests is this performing?
-- test_derive_state = do
    let ui_state = snd $ UiTest.run_mkstate
            [ ("tempo", [(0, 0, "2")])
            , (">1", [(0, 16, ""), (16, 16, "")])
            , ("cont", [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")])
            ]
    let skel = fst $ UiTest.run ui_state $ do
            block <- State.get_block block_id
            skel <- Schema.get_skeleton Map.empty block
            return skel
    pprint skel
    -- pprint (State.state_tracks ui_state)
    let (_, state, logs) = either (error . show) id
            (run ui_state (Derive.d_block block_id))
    -- pmlist "logs" (map Log.msg_string logs)
    -- TODO real test
    -- pprint $ Derive.state_warp state
    pmlist "track warps" $ Derive.state_track_warps state

    let tw = Derive.state_track_warps state
    let inv_tempo = Derive.make_inverse_tempo_func tw
    -- pprint $ (Derive.make_inverse_tempo_func tw) (Timestamp.seconds 1)
    pprint $ map inv_tempo (map Timestamp.seconds [0..3])
-}


test_subderive = do
    let ui_state = snd $ UiTest.run State.empty $ do
            UiTest.mkstate "sub"
                [ (">i2", [(1, 1, "--sub1")]) ]
            UiTest.mkstate "b0"
                [ ("tempo", [(0, 0, "2")])
                , (">i1", [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "blub")])
                , ("cont", [(0, 0, "1"), (16, 0, "i, 0")])
                ]
    let (events, _tempo, inv_tempo, logs, _state) =
            DeriveTest.derive_block ui_state (UiTest.bid "b0")

    let b0 pos = (UiTest.bid "b0",
            [(UiTest.tid ("b0.t" ++ show n), TrackPos pos) | n <- [ 1, 2, 0]])
        sub pos = (UiTest.bid "sub", [(UiTest.tid "sub.t0", TrackPos pos)])
    equal (fmap extract_events events) $
        Right [(0, 4, "--b1"), (6, 2, "--sub1")]

    strings_like (map Log.msg_string logs) ["unknown \\(bid \"test/blub\"\\)"]
    let pos = map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    equal (map List.sort pos)
        [[b0 0], [b0 4], [b0 8, sub 0], [b0 12, sub 1], [b0 16], []]
    equal (fmap (map Score.event_instrument) events) $
        Right [Just (Score.Instrument "i1"), Just (Score.Instrument "i2")]

    -- TODO test when the subblock has a tempo too
    -- type TempoFunction = BlockId -> TrackId -> TrackPos
    --     -> Maybe Timestamp.Timestamp

    -- For eyeball verification.
    -- pprint events
    -- pprint $ zip [0,2..] $ map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    -- pprint $ Derive.state_track_warps state

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

test_multiple_subderive = do
    let ui_state = snd $ UiTest.run State.empty $ do
            UiTest.mkstate "sub"
                [ (">", [(0, 1, "--sub1")]) ]
            UiTest.mkstate "b0"
                [ (">i1", [(0, 2, "sub"), (2, 2, "sub"), (4, 2, "sub")])
                ]
    let (events, _, inv_tempo, logs, _) =
            DeriveTest.derive_block ui_state (UiTest.bid "b0")
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
    -- pprint perf_events
    equal perform_warns []
    -- pprint mmsgs
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _) <- mmsgs]
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
    equal [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- mmsgs]
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
    equal (map (Set.toList . Score.event_attributes) events) [["a1"], ["a2"]]

    equal convert_warns []
    equal (length perf_events) 2
    equal (map Perform.event_instrument perf_events)
        [set_ks perf_inst "a1" 2, set_ks perf_inst "a2" 3]

    -- Just make sure it did in fact emit ccs.
    check $ any Midi.is_cc mmsgs
    equal midi_warns []
    where
    set_ks inst ks nn = inst
        { Instrument.inst_keyswitch = Just (Instrument.Keyswitch ks nn) }

test_relative_control = do
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (">", [(0, 1, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("+, vel", [(0, 0, "1")])
            , ("vel", [(0, 0, "0"), (2, 0, "i, 2"), (4, 0, "i, 0")])
            ]
    let extract = (\sig -> map (flip Signal.at sig) [0..5])
            . (Map.! Score.Control "vel") . Score.event_controls
    equal logs []
    equal (fmap (map extract) events) $ Right [[1, 2, 3, 2, 1, 1]]

    -- putting relative and absolute in the wrong order causes a warning
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("vel", [(0, 0, "1")])
            , ("+, vel", [(0, 0, "1")])
            ]
    equal (fmap (map Score.event_controls) events) $
        Right [Map.fromList [(Score.Control "vel", Signal.signal [(0, 1)])]]
    strings_like logs ["no absolute control is in scope"]

test_relative_pitch = do
    let extract result = (fmap (map Score.event_pitch) events, logs)
            where (events, logs) = DeriveTest.e_logs result
    let f track = extract $ DeriveTest.derive_tracks
                [ (inst_title, [(0, 10, "")])
                , ("+, *semar", track)
                , ("*semar", [(0, 0, "1")])
                ]
    let mksig = DeriveTest.pitch_signal (Pitch.ScaleId "semar")
    equal (f []) (Right [mksig [(0, Set, 10)]], [])
    equal (f [(0, 0, "1/"), (1, 0, "i, 0")])
        (Right [mksig [(0, Set, 15), (1, Linear, 10)]], [])

    -- empty relative pitch defaults to scale
    let (pitches, logs) = extract $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("+, *", [(0, 0, "1/")])
            , ("*semar", [(0, 0, "1")])
            ]
    equal logs []
    equal pitches $ Right [mksig [(0, Set, 15)]]

    -- putting relative and absolute in the wrong order causes a warning
    let (pitches, logs) = extract $ DeriveTest.derive_tracks
            [ (inst_title, [(0, 10, "")])
            , ("*semar", [(0, 0, "1")])
            , ("+, *semar", [(0, 0, "1")])
            ]
    equal pitches $ Right [mksig [(0, Set, 10)]]
    strings_like logs ["no absolute pitch is in scope"]

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
    let f tempo_track = (fmap (map floor_event . extract_events) events, logs)
            where
            (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
                [ ("tempo", tempo_track)
                , (">", [(0, 10, "--1"), (10, 10, "--2"), (20, 10, "--3")])
                , ("*twelve", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
                ]
        floor_event :: (Double, Double, String) -> (Integer, Integer, String)
        floor_event (start, dur, text) = (floor start, floor dur, text)

    equal (f [(0, 0, "2")])
        (Right [(0, 5, "--1"), (5, 5, "--2"), (10, 5, "--3")], [])

    -- Slow down.
    equal (f [(0, 0, "2"), (20, 0, "i, 1")])
        (Right [(0, 5, "--1"), (5, 8, "--2"), (13, 9, "--3")], [])
    equal (f [(0, 0, "2"), (20, 0, "i, 0")])
        (Right [(0, 6, "--1"), (6, 83, "--2"), (90, 10000, "--3")], [])
    -- Speed up.
    equal (f [(0, 0, "1"), (20, 0, "i, 2")])
        (Right [(0, 8, "--1"), (8, 5, "--2"), (13, 5, "--3")], [])
    equal (f [(0, 0, "0"), (20, 0, "i, 2")])
        (Right [(0, 83, "--1"), (83, 6, "--2"), (90, 5, "--3")], [])


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

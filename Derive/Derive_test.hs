{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | This also tests Derive.Note and Derive.Controller since it uses them to
    piece together a complete deriver.
-}
module Derive.Derive_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Util.Test
import qualified Util.Log as Log

import Ui
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Midi.Midi as Midi
import qualified Instrument.MidiDb as MidiDb

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Note as Note
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Scale.Twelve as Twelve

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport
import qualified Perform.Warning as Warning
import qualified Perform.Midi.Controller as Midi.Controller
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


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
            skel <- Schema.get_skeleton default_schema_map block
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
    let bid = UiTest.bid "b0"
    let ui_state = snd $ UiTest.run State.empty $ do
            tids1 <- UiTest.mkstate "sub"
                [ (">i2", [(1, 1, "--sub1")]) ]
            tids2 <- UiTest.mkstate "b0"
                [ ("tempo", [(0, 0, "2")])
                , (">i1", [(0, 8, "--b1"), (8, 8, "sub"), (16, 1, "blub")])
                , ("cont", [(0, 0, "1"), (16, 0, "i0")])
                ]
            return ()
    let look = Schema.lookup_deriver default_schema_map ui_state
    -- pprint $ State.state_blocks ui_state
    let (Right events, tempo, inv_tempo, logs, state) =
            Derive.derive look ui_state False (Derive.d_block bid)

    let b0 pos = (UiTest.bid "b0",
            [(UiTest.tid ("b0.t"++show n), TrackPos pos) | n <- [ 1, 2, 0]])
        sub pos =
            (UiTest.bid "sub", [(UiTest.tid "sub.t0", TrackPos pos)])
    equal (extract_events events) [(0, 4, "--b1"), (6, 2, "--sub1")]

    strings_like (map Log.msg_string logs) ["error sub-deriving.*test/blub"]
    equal (map inv_tempo (map Timestamp.seconds [0, 2 .. 10]))
        [ [b0 0], [b0 4], [sub 0, b0 8], [sub 1, b0 12], [b0 16], [] ]

    -- TODO test when the subblock has a tempo too

    -- For eyeball verification.
    -- pprint events
    -- pprint $ zip [0,2..] $ map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    -- pprint $ Derive.state_track_warps state


-- | Simple deriver with one track and one instrument.
basic_deriver :: (Monad m) => TrackId -> TrackId
    -> Derive.DeriveT m [Score.Event]
basic_deriver note_tid pitch_tid =
    Controller.d_controller (Score.Controller "*twelve")
        (Controller.d_pitch_signal Twelve.scale_id =<<
            Derive.with_track_warp Controller.d_controller_track pitch_tid)
        (Derive.with_instrument (Just default_inst) Score.no_attrs $
            Derive.with_track_warp Note.d_note_track note_tid)

-- | Do a default derivation on the state, and return events and logs from each
-- step.
default_derive :: State.State -> Instrument.Config
    -> ([Score.Event], [Log.Msg], [Perform.Event], [Warning.Warning],
        [Midi.Message], [Warning.Warning])
default_derive ui_state inst_config =
    (events, derive_logs, midi_events, convert_warns, mmsgs, perform_warns)
    where
    deriver = UiTest.eval ui_state $
        Schema.schema_deriver Schema.default_schema UiTest.default_block_id
    (events, derive_logs) = derive_events ui_state deriver
    (midi_events, convert_warns) = Convert.convert default_lookup events
    (msgs, perform_warns) =
        Perform.perform default_lookup inst_config midi_events
    mmsgs = map Midi.wmsg_msg msgs

test_fractional_pitch = do
    -- A pitch that requires pitch bends should distribute across multiple
    -- channels.  Yes, this is also tested in Perform.Midi.Perform, but this
    -- also tests that the pitch signal is trimmed properly by
    -- Note.trim_pitches.
    let (tids, ui_state) = UiTest.run_mkstate
            [ (">synth/patch", [(0, 16, ""), (16, 16, "")])
            , ("*semar", [(0, 16, "1"), (16, 16, "2")])
            ]
    let inst_config = Instrument.config
            [(default_inst,
                [(default_dev, 0), (default_dev, 1), (default_dev, 2)])]
            Nothing
    let (events, derive_logs, midi_events, convert_warns, mmsgs, perform_warns)
            = default_derive ui_state inst_config

    equal derive_logs []
    -- pprint events
    equal convert_warns []
    -- pprint midi_events
    equal perform_warns []
    -- pprint mmsgs
    equal [(chan, nn) | Midi.ChannelMessage chan (Midi.NoteOn nn _) <- mmsgs]
        [(0, 72), (1, 73)]


test_basic = do
    -- verify the three phases of derivation
    -- 1: derivation to score events
    let (tids, ui_state) = UiTest.run_mkstate
            [ ("> +a1", [(0, 16, "+a0"), (16, 16, "+a2")])
            , ("*twelve", [(0, 16, "4c"), (16, 16, "4c#")])
            ]
    let (events, logs) = derive_events ui_state
            (basic_deriver (tids!!0) (tids!!1))
    equal logs []
    equal (extract_events events) [(0, 16, "+a0"), (16, 16, "+a2")]

    -- 2: conversion to midi perf events
    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    let evt = (,,,,) (Instrument.inst_name default_perf_inst)
    equal (map extract_perf_event midi_events)
        [ evt (Just "a0") 0 16 (mkstack [("b1", "b1.t0", (0, 16))])
        , evt (Just "a1+a2") 16 16 (mkstack [("b1", "b1.t0", (16, 16))])
        ]

    -- 3: performance to midi protocol events
    let (msgs, warns) = perform midi_events
        mmsgs = map Midi.wmsg_msg msgs
    equal [nn | Midi.ChannelMessage _ (Midi.NoteOn nn _) <- mmsgs]
        [1, 60, 0, 61]
    equal warns []

ks_name (Instrument.Keyswitch name _) = name
set_ks inst ks nn = inst
    { Instrument.inst_keyswitch = Just (Instrument.Keyswitch ks nn) }
extract_perf_event (Perform.Event inst start dur controls stack) =
    ( Instrument.inst_name inst, fmap ks_name (Instrument.inst_keyswitch inst)
    , start, dur, stack)
mkstack = map (\(bid, tid, pos) ->
    (UiTest.bid bid, Just (UiTest.tid tid), Just pos))


-- | Slightly more complicated with pitch and mod controller tracks.
controller_deriver :: (Monad m) => TrackId -> TrackId
    -> TrackId -> String -> Derive.DeriveT m [Score.Event]
controller_deriver note_tid pitch_tid cont_tid cont_name =
    Controller.d_controller (Score.Controller cont_name)
        (Controller.d_signal =<<
            Derive.with_track_warp d_cont_track cont_tid)
        (basic_deriver note_tid pitch_tid)
    where
    d_cont_track = Controller.d_controller_track

test_controller = do
    let (tids, ui_state) = UiTest.run_mkstate
            [ (">", [(0, 1, "+a1"), (1, 1, "+a2")])
            , ("*twelve", [(0, 1, "4c"), (1, 1, "4c#")])
            , (c_mod, [(0, 0, "1"), (1, 0, "i.75"), (2, 0, "i0")])
            ]
    let (events, logs) = derive_events ui_state
            (controller_deriver (tids!!0) (tids!!1) (tids!!2) c_mod)
    -- Cursory checks, more detailed checks are in more Note_test and
    -- Controller_test.
    equal logs []
    equal (extract_events events) [(0, 1, "+a1"), (1, 1, "+a2")]
    equal (map (Set.toList . Score.event_attributes) events) [["a1"], ["a2"]]

    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    equal (length midi_events) 2
    equal (map Perform.event_instrument midi_events)
        [set_ks default_perf_inst "a1" 2, set_ks default_perf_inst "a2" 3]

    let (msgs, warns) = perform midi_events
    -- Just make sure it did in fact emit ccs.
    check $ any (Midi.is_cc . Midi.wmsg_msg) msgs
    equal warns []

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    let track_id = Types.TrackId (UiTest.mkid "warp")
        warp = Derive.make_warp (Derive.tempo_to_warp (Signal.constant 2))
        track_warps = [Derive.TrackWarp
            (TrackPos 0) (TrackPos 2) block_id [track_id] warp]
    let f = Derive.make_inverse_tempo_func track_warps
        with_block pos = [(block_id, [(track_id, pos)])]
    -- Fast tempo means TrackPos pass quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is past the end of the block.
    equal (map f (map Timestamp.seconds [0..2]))
        [with_block 0, with_block 2, []]

tempo_deriver :: (Monad m) => TrackId -> Signal.Signal -> TrackId
    -> TrackId -> TrackId -> Derive.DeriveT m [Score.Event]
tempo_deriver sig_tid tempo note_tid pitch_tid vel_tid = do
    Derive.d_tempo sig_tid (return tempo) $
        controller_deriver note_tid pitch_tid vel_tid "velocity"

test_tempo = do
    let (tids, state) = UiTest.run_mkstate
            [ (">", [(0, 10, "--1"), (10, 10, "--2"), (20, 10, "--3")])
            , ("*twelve", [(0, 10, "5a"), (10, 10, "5b"), (20, 10, "5c")])
            , ("1", [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")])
            ]
        mkderiver sig = tempo_deriver
            (Types.TrackId (UiTest.mkid "b0.tempo")) (mksignal sig)
            (tids!!0) (tids!!1) (tids!!2)
        floor_event :: (Double, Double, String) -> (Integer, Integer, String)
        floor_event (start, dur, text) = (floor start, floor dur, text)
        derive_with sig = extract_events events
            where (events, _logs) = derive_events state (mkderiver sig)
    let f = map floor_event . derive_with

    equal (f [(0, Signal.Set, 2)])
        [(0, 5, "--1"), (5, 5, "--2"), (10, 5, "--3")]

    -- Slow down.
    equal (f [(0, Signal.Set, 2), (20, Signal.Linear, 1)])
        [(0, 6, "--1"), (6, 8, "--2"), (15, 10, "--3")]
    equal (f [(0, Signal.Set, 2), (20, Signal.Linear, 0)])
        [(0, 2505, "--1"), (2505, 7495, "--2"), (10000, 9, "--3")]
    -- Speed up.
    equal (f [(0, Signal.Set, 1), (20, Signal.Linear, 2)])
        [(0, 8, "--1"), (8, 6, "--2"), (15, 5, "--3")]
    equal (f [(0, Signal.Set, 0), (20, Signal.Linear, 2)])
        [(0, 7509, "--1"), (7509, 2500, "--2"), (10009, 5, "--3")]

test_deriver_performance = do
    -- test a large score for profiling
    let size = 10000
    let notes = cycle ["4a", "4b", "4c", "4d", "4e", "4f", "4g", "5c"]
        pos = take size [0..]
        vels = cycle ["1", ".2", ".4", ".6"]
        tempo_pos = take (size `div` 10) [0, 10..]
        tempos = cycle ["1", "2", "3", "i1"]
        inst_tracks name =
            [ (name, [(p, 1, "") | p <- pos])
            , ("*twelve", [(p, 0, note) | (p, note) <- zip pos notes])
            , ("vel", [(p, 0, vel) | (p, vel) <- zip pos vels])
            ]
    let (_, ui_state) = UiTest.run_mkstate $
            [("tempo", [(p, 0, t) | (p, t) <- zip tempo_pos tempos])]
            ++ inst_tracks ">i1" ++ inst_tracks ">i2"
    let blocks = State.state_blocks ui_state
    print (Map.size blocks)
    print $ Map.map (Track.events_length . Track.track_events)
        (State.state_tracks ui_state)

    -- let (events, logs) = derive_events ui_state d


-- * setup

derive :: Derive.LookupDeriver -> State.State
    -> Derive.DeriveT Identity.Identity a
    -> (a, Transport.TempoFunction, Transport.InverseTempoFunction, [Log.Msg])
derive lookup_deriver ui_state deriver = case result of
        Left err -> error $ "derive error: " ++ show err
        Right val -> (val, tempo, inv_tempo, logs)
    where
    (result, tempo, inv_tempo, logs, _) = Derive.derive
        lookup_deriver ui_state False (setup_deriver deriver)

-- | Fake up a stack and track warp, so I can derive without d_block or
-- d_warp.
setup_deriver d = Derive.with_stack_block block_id (Derive.start_new_warp >> d)

derive_events_lookup lookup_deriver ui_state deriver = (val, logs)
    where (val, _, _, logs) = derive lookup_deriver ui_state deriver

-- | Derive with lookup defaulted to empty_lookup_deriver.
derive_events :: State.State -> Derive.DeriveT Identity.Identity a
    -> (a, [Log.Msg])
derive_events = derive_events_lookup Derive.empty_lookup_deriver

run :: State.State -> Derive.DeriveT Identity.Identity a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m =
    case Identity.runIdentity (Derive.run derive_state m) of
        (Left err, _, _logs) -> Left (Derive.error_message err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    -- Good to have a minimal fake stack so there someplace to put trackpos.
    fake_stack = [(UiTest.bid "blck", Just (UiTest.tid "trck"), Nothing)]
    derive_state = (Derive.initial_state ui_state
        (Schema.lookup_deriver default_schema_map ui_state) False)
            { Derive.state_stack = fake_stack }

perform = Perform.perform default_lookup default_inst_config

block_id = UiTest.default_block_id

c_mod = Midi.Controller.c_mod

default_inst = Score.Instrument "synth/patch"
default_synth = Instrument.synth "synth" "wdev" []
default_dev = Midi.WriteDevice "out"
default_inst_config = Instrument.config
    [(default_inst, [(default_dev, 0)])] Nothing
default_perf_inst = Instrument.instrument "synth" "patch" Nothing
            Midi.Controller.empty_map (-2, 2)
default_ksmap = Instrument.KeyswitchMap $
    map (\(attr, name, nn) -> (Set.fromList attr, Instrument.Keyswitch name nn))
        [ (["a1", "a2"], "a1+a2", 0)
        , (["a0"], "a0", 1)
        , (["a1"], "a1", 2)
        , (["a2"], "a2", 3)
        ]

default_lookup :: MidiDb.LookupMidiInstrument
default_lookup attrs (Score.Instrument inst)
    | inst == "synth/patch" = Just (default_perf_inst
        { Instrument.inst_keyswitch =
            Instrument.get_keyswitch default_ksmap attrs })
    | otherwise = Nothing

default_schema_map :: Schema.SchemaMap
default_schema_map = Map.empty

mksignal segs = Signal.track_signal (TrackPos 1)
    [(TrackPos p, m, v) | (p, m, v) <- segs]

extract_events :: [Score.Event] -> [(Double, Double, String)]
extract_events = map $ \event ->
    (realToFrac (Score.event_start event),
        realToFrac (Score.event_duration event), Score.event_string event)

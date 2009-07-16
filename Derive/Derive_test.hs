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
import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Ui.TestSetup as TestSetup

import qualified Midi.Midi as Midi

import qualified Cmd.Cmd as Cmd

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Note as Note
import qualified Derive.Scale as Scale
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Twelve as Twelve

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform

import qualified Perform.Transport as Transport

-- TODO convert to use mkstate


-- Inspect the final state after running a derivation.
test_derive_state = do
    let ui_state = snd $ TestSetup.run_mkstate
            [ ("tempo", [(0, 0, "2")])
            , (">1", [(0, 16, "4c-"), (16, 16, "4c#")])
            , ("cont", [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")])
            ]
    let skel = fst $ TestSetup.run ui_state $ do
            block <- State.get_block block_id
            skel <- Schema.get_skeleton default_schema_map block
            return skel
    -- pprint skel
    -- pprint (State.state_tracks ui_state)
    let (_, state, logs) = either (error . show) id
            (run ui_state (Derive.d_block block_id))
    -- pmlist "logs" (map Log.msg_text logs)
    -- TODO real test
    -- pprint $ Derive.state_warp state
    pmlist "track warps" $ Derive.state_track_warps state

    let tw = Derive.state_track_warps state
    let inv_tempo = Derive.make_inverse_tempo_func tw
    -- pprint $ (Derive.make_inverse_tempo_func tw) (Timestamp.seconds 1)
    pprint $ map inv_tempo (map Timestamp.seconds [0..3])


test_subderive = do
    let bid = TestSetup.bid "b0"
    let ui_state = snd $ TestSetup.run State.empty $ do
            tids1 <- TestSetup.mkstate "sub"
                [ (">2", [(1, 1, "7c-")]) ]
            tids2 <- TestSetup.mkstate "b0"
                [ ("tempo", [(0, 0, "2")])
                , (">1", [(0, 8, "4c-"), (8, 8, "<sub"), (16, 1, "<blub")])
                , ("cont", [(0, 0, "1"), (16, 0, "i0")])
                ]
            return ()
    let look = Schema.lookup_deriver default_schema_map ui_state
    let (Right events, tempo, inv_tempo, logs, state) =
            Derive.derive look ui_state False (Derive.d_block bid)

    let b0 pos = (TestSetup.bid "b0",
            [(TestSetup.tid ("b0.t"++show n), TrackPos pos) | n <- [ 1, 2, 0]])
        sub pos =
            (TestSetup.bid "sub", [(TestSetup.tid "sub.t0", TrackPos pos)])
    equal (extract_events events) [(0, 4, "4c-"), (6, 2, "7c-")]
    equal (map inv_tempo (map Timestamp.seconds [0, 2 .. 10]))
        [ [b0 0], [b0 4], [sub 0, b0 8], [sub 1, b0 12], [b0 16], [] ]
    strings_like (map Log.msg_text logs) ["error sub-deriving.*test/blub"]

    -- TODO test when the subblock has a tempo too

    -- For eyeball verification.
    -- pprint events
    -- pprint $ zip [0,2..] $ map inv_tempo (map Timestamp.seconds [0, 2 .. 10])
    -- pprint $ Derive.state_track_warps state


-- | Simple deriver with one track and one instrument.
basic_deriver :: (Monad m) => Derive.TrackDeriver m
basic_deriver track_id =
    Derive.with_instrument (Just default_inst) Set.empty $
        Derive.with_track_warp Note.d_note_track track_id

test_basic = do
    let (tids, ui_state) = TestSetup.run_mkstate
            [ ("1", [(0, 16, "4c-"), (16, 16, "5e-")]) ]
    let (events, logs) = derive_events_e ui_state (basic_deriver (head tids))
    equal logs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    -- equal (length midi_events) 2
    -- pmlist "midi_events" midi_events
    -- let (msgs, _warns) = Perform.perform
    --         default_lookup default_inst_config midi_events
    -- equal (length msgs) 4 -- (noteon + noteoff) * 2
    -- pmlist "msgs" msgs


-- | Slightly more complicated with a controller track.
controller_deriver :: (Monad m) => Track.TrackId -> String
    -> Derive.TrackDeriver m
controller_deriver cont_track_id cont_name track_id =
    Controller.d_controller (Score.Controller cont_name)
        (Controller.d_signal =<<
            Derive.with_track_warp Controller.d_controller_track cont_track_id)
        (basic_deriver track_id)

test_controller = do
    let (tids, ui_state) = TestSetup.run_mkstate
            [ ("1", [(0, 1, "4c-"), (1, 1, "4c#")])
            , (c_mod, [(0, 0, "1"), (1, 0, "i.75"), (2, 0, "i0")])
            ]
    let (events, logs) = derive_events_e ui_state
            (controller_deriver (tids!!1) c_mod (tids!!0))
    equal logs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, warns) = perform midi_events
    equal warns []
    -- Should have a bunch of controller signals, but they depend on the srate.
    -- TODO better test?
    pmlist "msgs" msgs

test_make_inverse_tempo_func = do
    -- This is actually also tested in test_subderive.
    let track_id = Track.TrackId (TestSetup.mkid "warp")
        warp = Derive.make_warp (Derive.tempo_to_warp (Signal.constant 2))
        track_warps = [Derive.TrackWarp
            (TrackPos 0) (TrackPos 2) block_id [track_id] warp]
    let f = Derive.make_inverse_tempo_func track_warps
        with_block pos = [(block_id, [(track_id, pos)])]
    -- Fast tempo means TrackPos pass quickly relative to Timestamps.
    -- Second 2 at tempo 2 is trackpos 4, which is past the end of the block.
    equal (map f (map Timestamp.seconds [0..2]))
        [with_block 0, with_block 2, []]

tempo_deriver :: Track.TrackId -> Signal.Signal -> Track.TrackId
    -> Track.TrackId -> Derive.EventDeriver
tempo_deriver sig_tid tempo note_tid vel_tid = do
    Derive.d_tempo sig_tid (return tempo) $
        controller_deriver vel_tid "velocity" note_tid

test_tempo = do
    let (tids, state) = TestSetup.run_mkstate
            [ ("0", [(0, 10, "5a-"), (10, 10, "5b-"), (20, 10, "5c-")])
            , ("1", [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")])
            ]
        mkderiver sig = tempo_deriver
            (Track.TrackId (TestSetup.mkid "b0.tempo"))
            (mksignal sig) (tids!!0) (tids!!1)
        floor_event (start, dur, text) = (floor start, floor dur, text)
        derive_with sig = extract_events events
            where (events, _logs) = derive_events_e state (mkderiver sig)

    equal (derive_with [(0, Signal.Set, 2)])
        [(0, 5, "5a-"), (5, 5, "5b-"), (10, 5, "5c-")]

    -- Slowing down.
    equal (map floor_event
            (derive_with [(0, Signal.Set, 2), (20, Signal.Linear, 1)]))
        [(0, 6, "5a-"), (6, 8, "5b-"), (14, 10, "5c-")]

-- extract_controller name = map $ \event -> Map.assocs $ Signal.signal_map $
--     Score.event_controllers event Map.! Score.Controller name
extract_logs = map $ \log -> (Log.msg_text log, Log.msg_stack log)

-- * misc other modules

-- I test some other Derive modules here so I don't have to make a whole new
-- module for each one.

test_controller_parse = do
    let evt = Score.event (TrackPos 0) (TrackPos 0)
        (_, state) = TestSetup.run_mkstate []
        t_eval s = eval state $ Controller.parse_event () (evt s)

    let result = t_eval "hi there"
    check $ "Left \"parse error on char 1" `List.isPrefixOf` (show result)

    equal (t_eval "-2e.2") (Right (TrackPos 0, Signal.Exp (-2), 0.2))
    equal (t_eval "-.2") (Right (TrackPos 0, Signal.Set, -0.2))

-- * setup

derive lookup_deriver ui_state deriver = case result of
        Left err -> error $ "derive error: " ++ show err
        Right val -> (val, tempo, inv_tempo, logs)
    where
    (result, tempo, inv_tempo, logs, _) = Derive.derive
        lookup_deriver ui_state False (setup_deriver deriver)

-- | Fake up a stack and track warp, so I can derive without d_block or
-- d_warp.
setup_deriver d = Derive.with_stack_block block_id (Derive.start_new_warp >> d)

derive_events ui_state lookup_deriver deriver =
    (\(val, _, _, logs) -> (val, logs)) (derive lookup_deriver ui_state deriver)

-- | Derive with lookup defaulted to empty_lookup_deriver.
derive_events_e ui_state deriver =
    derive_events ui_state Derive.empty_lookup_deriver deriver

run :: State.State -> Derive.DeriveT Identity.Identity a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m =
    case Identity.runIdentity (Derive.run derive_state m) of
        (Left err, _, _logs) -> Left (Derive.error_message err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    -- Good to have a minimal fake stack so there someplace to put trackpos.
    fake_stack = [(TestSetup.bid "blck", Just (TestSetup.tid "trck"), Nothing)]
    derive_state = (Derive.initial_state ui_state
        (Schema.lookup_deriver default_schema_map ui_state) False)
            { Derive.state_stack = fake_stack }

eval ui_state m = fmap (\(val, _, _) -> val) (run ui_state m)

perform = Perform.perform default_chan_map default_lookup default_inst_config

block_id = TestSetup.default_block_id

c_mod = Controller.c_mod

default_inst = Score.Instrument "synth/patch"
default_synth = Instrument.synth "synth" "wdev" []
default_dev = Midi.WriteDevice "out"
default_inst_config = Instrument.config
    [(default_inst, [(default_dev, 0)])] Nothing
default_chan_map = fst $ Cmd.inst_addr_to_chan_map default_lookup
    (Instrument.config_alloc default_inst_config)

default_lookup (Score.Instrument inst)
    | inst == "synth/patch" = Just $
        Instrument.instrument default_synth "patch" Nothing
            Controller.default_controllers (-2, 2)
    | otherwise = Nothing

default_schema_map :: Schema.SchemaMap
default_schema_map = Map.empty


-- ** ui stetup

mkscore (pos, dur, text) = Score.event pos dur text

track1 = TestSetup.mktrack ("1", [(0, 16, "4c-"), (16, 16, "4c#")])
track_cont = TestSetup.mktrack
    ("cont", [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")])

mksignal segs = Signal.track_signal (TrackPos 1)
    [(TrackPos p, m, v) | (p, m, v) <- segs]

extract_events :: [Score.Event] -> [(Double, Double, String)]
extract_events = map $ \event ->
    (realToFrac (Score.event_start event),
        realToFrac (Score.event_duration event), Score.event_text event)

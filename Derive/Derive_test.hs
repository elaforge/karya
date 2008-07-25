{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Derive.Derive_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
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

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Derive.Twelve as Twelve

import qualified Perform.Signal2 as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform

import qualified Perform.Transport as Transport

-- TODO convert to use mkstate


-- simple single instrument
basic_deriver :: (Monad m) => Derive.DeriveT m [Score.Event]
basic_deriver = bass =<< twelve =<< Derive.d_track track_id

derive_events ui_state deriver = case result of
        Left err -> error $ "derive error: " ++ show err
        Right val -> (val, logs)
    where
    (result, _tempo, _inv_tempo, logs) =
        Derive.derive ui_state block_id deriver

test_basic = do
    let (events, logs) = derive_events ui_state basic_deriver
    equal logs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, _warns) = Perform.perform
            default_lookup default_inst_config midi_events
    equal (length msgs) 4 -- (noteon + noteoff) * 2
    pmlist "msgs" msgs

controller_deriver :: (Monad m) => Derive.DeriveT m [Score.Event]
controller_deriver = Controller.d_controller (Score.Controller "breath")
    (Controller.d_signal =<< Derive.d_track cont_track_id)
    (bass =<< twelve =<< Derive.d_track track_id)

test_controller = do
    let (events, logs) = derive_events ui_state controller_deriver
    equal logs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert default_lookup events
    equal warns []
    equal (length midi_events) 2
    -- The exact msgs that come out depend on the srate, and besides this stuff
    -- is tested in Perform_test.
    -- pmlist "midi_events" midi_events
    -- let (msgs, _warns) = Perform.perform default_inst_config midi_events
    -- equal (length msgs) 4 -- (noteon + noteoff) * 2
    -- pmlist "msgs" msgs

cont_deriver name tid = Controller.d_controller (Score.Controller name)
    (Controller.d_signal =<< Derive.d_track tid)

test_inject_pos = do
    let mkmap m = [(TrackPos p0, TrackPos p1) | (p0, p1) <- m]
        inject score =
            extract_events (Derive.inject_pos pmap (map mkscore score))
        pmap = mkmap
            [(0, 0), (5, 10), (10, 20), (15, 30)]
    equal (inject [(0, 5, "a"), (5, 5, "b")])
        [(0, 10, "a"), (10, 10, "b")]

test_make_inverse_tempo_func = do
    let warp = Derive.tempo_to_warp (Signal.constant 1)
    let f = Derive.make_inverse_tempo_func
            block_id (TrackPos 0) (TrackPos 3) warp
    equal (map f (map Timestamp.seconds [0..3]))
        [[(block_id, 0)], [(block_id, 1)], [(block_id, 2)], []]

test_block = do
    let (deriver, ui_state) = TestSetup.run State.empty $ do
        TestSetup.initial_state
        block <- State.get_block block_id
        Schema.get_deriver schema_map block

    let (_result, derive_state, _logs) = Identity.runIdentity $
            Derive.run ui_state deriver
    -- TODO real test
    print $ Derive.state_warp derive_state
    -- derive events?

tempo_deriver :: Signal.Signal -> Track.TrackId -> Track.TrackId
    -> Derive.Deriver
tempo_deriver tempo note_tid vel_tid = do
    Derive.d_tempo (return tempo) $
        cont_deriver "velocity" vel_tid
            (bass =<< twelve =<< Derive.d_track note_tid)

test_tempo = do
    let (tids, state) = TestSetup.run_mkstate
            [ ("0", [(0, 10, "5a-"), (10, 10, "5b-"), (20, 10, "5c-")])
            , ("1", [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")])
            ]
        mkderiver sig = tempo_deriver (mksignal sig) (tids!!0) (tids!!1)
        derive_with sig = extract_events events
            where (events, _logs) = derive_events state (mkderiver sig)

    equal (derive_with [(0, Signal.Set, 2)])
        [(0, 5, "5a-"), (5, 5, "5b-"), (10, 5, "5c-")]

    -- Slowing down.
    equal (derive_with [(0, Signal.Set, 2), (20, Signal.Linear, 1)])
        [(0, 6, "5a-"), (6, 8, "5b-"), (14, 10, "5c-")]

-- extract_controller name = map $ \event -> Map.assocs $ Signal.signal_map $
--     Score.event_controllers event Map.! Score.Controller name
extract_logs = map $ \log -> (Log.msg_text log, Log.msg_stack log)


-- * derivers

midi_instrument inst = map (\evt -> evt { Score.event_instrument = Just inst })

twelve events = Derive.map_events realize_note () id events
realize_note _ event = case Twelve.event_pitch (Score.event_text event) of
    Nothing -> Derive.throw $
        "can't realize event " ++ show (Score.event_text event)
    Just pitch -> return (event { Score.event_pitch = Just pitch })

-- * misc other modules

-- I test some other Derive modules here so I don't have to make a whole new
-- module for each one.

test_controller_parse = do
    let evt = Score.event (TrackPos 0) (TrackPos 0)
    let run s = test_run $ Controller.parse_event () (evt s)

    let result = run "hi there"
    check $ "Left \"parse error on char 1" `List.isPrefixOf` (show result)

    equal (run "-2e.2") (Right (TrackPos 0, Signal.Exp (-2), 0.2))
    equal (run "-.2") (Right (TrackPos 0, Signal.Set, -0.2))

test_run :: Derive.DeriveT Identity.Identity a -> Either String a
test_run m = case Identity.runIdentity (Derive.run State.empty m) of
    (Left err, _, _logs) -> Left (Derive.error_message err)
    (Right val, _, _) -> Right val


-- * setup

bass events = return (midi_instrument default_inst events)


mkid = TestSetup.mkid

block_id = TestSetup.default_block_id
track_id = Track.TrackId (mkid "b0.t0")
cont_track_id = Track.TrackId (mkid "b0.cont")

default_inst = Score.Instrument "synth/patch"
default_dev = Midi.WriteDevice "out"
default_inst_config = Instrument.config
    [((default_dev, 0), Score.Instrument "synth/patch")] Nothing

default_lookup (Score.Instrument inst)
    | inst == "synth/patch" = Just $
        Instrument.instrument "synth/patch" Controller.empty_map (-2, 2) Nothing
    | otherwise = Nothing

schema_map :: Schema.SchemaMap
schema_map = Map.empty


-- ** ui stetup

ui_state = snd $ TestSetup.run_mkstate
    [ ("1", [(0, 16, "4c-"), (16, 16, "4c#")])
    , ("2", [(0, 16, "4c-"), (16, 16, "4c#")])
    , ("cont", [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")])
    ]

{-
ui_state = snd $ State.run_state State.empty $ do
    ruler <- State.create_ruler (mkid "r1")
        (TestSetup.ruler [TestSetup.marklist 0 1])
    overlay <- State.create_ruler "r1.overlay"
        =<< fmap TestSetup.overlay_ruler (State.get_ruler ruler)
    t1 <- State.create_track "b1.t1" track1
    t2 <- State.create_track "b1.t2" track1
    t3 <- State.create_track "b1.cont" track_cont
    b1 <- State.create_block "b1" $
        Block.block "b1 title" TestSetup.default_block_config
            [ (Block.RId ruler, 20)
            , (Block.TId t1 overlay, 40), (Block.TId t2 overlay, 40)
            ]
            (Block.SchemaId "no schema")
    return ()
-}

mkscore (pos, dur, text) = Score.event pos dur text

track1 = TestSetup.mktrack ("1", [(0, 16, "4c-"), (16, 16, "4c#")])
track_cont = TestSetup.mktrack
    ("cont", [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")])

mksignal segs = Signal.track_signal (TrackPos 1)
    [(TrackPos p, m, v) | (p, m, v) <- segs]

extract_events :: [Score.Event] -> [(Integer, Integer, String)]
extract_events = map $ \event ->
    (floor (Score.event_start event),
        floor (Score.event_duration event), Score.event_text event)

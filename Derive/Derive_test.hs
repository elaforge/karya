module Derive.Derive_test where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List

import Util.Test
import qualified Util.Log as Log
import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Track as Track
import qualified Ui.State as State

import qualified Ui.TestSetup as TestSetup

import qualified Midi.Midi as Midi

import qualified Derive.Controller as Controller
import qualified Derive.Derive as Derive
import qualified Derive.Parse as Parse
import qualified Derive.Score as Score
import qualified Derive.Schema as Schema
import qualified Derive.Twelve as Twelve

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.InstrumentDb as InstrumentDb
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
        Derive.derive ui_state (Block.BlockId "b1") deriver

test_basic = do
    let (events, logs) = derive_events ui_state basic_deriver
    equal logs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, _warns) = Perform.perform default_inst_config midi_events
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
    let (midi_events, warns) = Convert.convert events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, _warns) = Perform.perform default_inst_config midi_events
    equal (length msgs) 4 -- (noteon + noteoff) * 2
    pmlist "msgs" msgs

tempo_deriver :: Signal.Signal -> Track.TrackId -> Track.TrackId
    -> Derive.Deriver
tempo_deriver tempo note_tid vel_tid = do
    Derive.d_tempo (return tempo) $
        cont_deriver "velocity" vel_tid
            (bass =<< twelve =<< Derive.d_track note_tid)

cont_deriver name tid = Controller.d_controller (Score.Controller name)
    (Controller.d_signal =<< Derive.d_track tid)

test_inject_pos = do
    let mkmap m = [(TrackPos p0, TrackPos p1) | (p0, p1) <- m]
        inject score =
            extract_events (Derive.inject_pos pmap (map mkscore score))
        pmap = mkmap
            [(0, 0), (5, 10), (10, 20), (15, 30)]
    print $ inject [(0, 5, "a"), (5, 5, "b")]

test_inverse_tempo_map = do
    let pos_map = map (\(x, y) -> (TrackPos x, TrackPos y))
            [(0, 1), (2, 1), (4, 3) , (6, 4)]
        tmap = Derive.make_inverse_tempo_map
            (Block.BlockId "b") (TrackPos 0) (TrackPos 30) pos_map
    plist $ map (tmap . Timestamp.Timestamp) [0..8]

test_block = do
    let (deriver, ui_state) = State.run_state State.empty $ do
        TestSetup.initial_state
        block <- State.get_block (Block.BlockId "b1")
        Schema.get_deriver block

    let (result, derive_state, logs) = Identity.runIdentity $
            Derive.run ui_state deriver
    print $ Derive.state_tempo derive_state
    plist $ Derive.state_pos_map derive_state

    -- let (events, tempo, inv_tempo, logs) = derive_events2 ui_state deriver
    -- plist (extract_events events)
    -- -- plist $ inverse inv_tempo (map Timestamp.Timestamp [0, 100..3000])
    -- print $ inverse inv_tempo [(Timestamp.Timestamp 2600)]
    -- print pmap
    -- print (head events)

inverse (Transport.InverseTempoMap samples tfunc) ts =
    map (tfunc samples) ts

derive_events2 ui_state deriver = case result of
        Left err -> error $ "derive error: " ++ show err
        Right val -> (val, tempo, inv_tempo, logs)
    where
    (result, tempo, inv_tempo, logs) =
        Derive.derive ui_state (Block.BlockId "b1") deriver

pos_map (Transport.InverseTempoMap t _) = t

test_tempo_map = do
    let pmap2 = [(TrackPos 0,TrackPos 0),(TrackPos 0,TrackPos 0)]
    print $ (Derive.make_tempo_map pmap2) (TrackPos 0)
    print $ Signal.interpolate_samples [(0, 0), (0, 0)] 0

    -- let pos_map = map (\(x, y) -> (TrackPos x, TrackPos y))
    --         [(0, 1), (2, 1), (4, 3) , (6, 4)]
    --     tmap = Derive.make_tempo_map pos_map
    -- plist $ map (tmap . TrackPos) [0..8]

test_tempo = do
    let (tids, state) = mkstate
            [ [(0, 10, "5a-"), (10, 10, "5b-"), (20, 10, "5c-")]
            , [(0, 10, ".1"), (10, 10, ".2"), (20, 10, ".4")]
            ]
        sig = mksignal
            [(0, Signal.Set, 0), (40, Signal.Linear, 80)]
            -- , (TrackPos 40, Signal.Linear, 600)]
        mkderiver sig = tempo_deriver (mksignal sig) (tids!!0) (tids!!1)
        derive_with sig = extract_events events
            where (events, _logs) = derive_events state (mkderiver sig)

    equal (derive_with [(0, Signal.Set, 2)])
        [(0, 5, "5a-"), (5, 5, "5b-"), (10, 5, "5c-")]

    plist $ derive_with [(0, Signal.Set, 2), (20, Signal.Linear, 1)]

    {-
    let events = fst $ derive_events state (mkderiver
            [(0, Signal.Set, 1), (20, Signal.Linear, 2)])
    let (midi_events, warns) = Convert.convert events

    pmlist "midi_events" midi_events
    let (msgs, mwarns) = Perform.perform default_inst_config midi_events
    pmlist "msgs" msgs

    -- TODO verify that the controllers are lined up
    -- let (events, logs) = derive_events state (mkderiver
    --         [(TrackPos 0, Signal.Set, 0.5), (TrackPos 30, Signal.Linear, 1)])
    -- pmlist "vel" $ extract_controller "velocity" events


    pmlist "conts" (extract_controller "velocity" events)

    -- equal warns []
    -- equal (length midi_events) 2
    pmlist "warns" warns
    pmlist "midi_events" midi_events
    let (msgs, mwarns) = Perform.perform default_inst_config midi_events
    pmlist "mw" mwarns
    pmlist "msgs" msgs
    -}

-- extract_controller name = map $ \event -> Map.assocs $ Signal.signal_map $
--     Score.event_controllers event Map.! Score.Controller name
extract_logs = map $ \log -> (Log.msg_text log, Log.msg_stack log)


-- * derivers

midi_instrument inst = map (\evt -> evt { Score.event_instrument = Just inst })

twelve events = Derive.map_events () realize_note id events
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

    equal (run "-2e.2") (Right (TrackPos 0, Signal.Exp (-2), 0.2, 0.2))
    equal (run "-.2") (Right (TrackPos 0, Signal.Linear, -0.2, -0.2))

test_run :: Derive.DeriveT Identity.Identity a -> Either String a
test_run m = case Identity.runIdentity (Derive.run State.empty m) of
    (Left err, _, logs) -> Left (Derive.error_message err)
    (Right val, _, _) -> Right val


-- * setup

bass events = return (midi_instrument default_inst events)


track_id = Track.TrackId "b1.t1"
cont_track_id = Track.TrackId "b1.cont"

default_inst = Score.Instrument "synth/patch"
default_midi_inst = InstrumentDb.generic "synth" "patch"
default_dev = Midi.WriteDevice "out"
default_inst_config =
    Instrument.config [((default_dev, 0), default_midi_inst)] Nothing


-- ** ui stetup

mkstate tracks = State.run_state State.empty $ do
    ruler <- State.create_ruler "r1" TestSetup.no_ruler
    tids <- forM (zip [0..] tracks) $ \(i, track) -> do
        State.create_track ("b1." ++ show i) (mktrack track)
    State.create_block "b1" $
        Block.block "b1 title" TestSetup.default_block_config
            ((Block.RId ruler, 20) : [(Block.TId tid ruler, 40) | tid <- tids])
            (Block.SchemaId "no schema")
    return tids

ui_state = snd $ State.run_state State.empty $ do
    ruler <- State.create_ruler "r1"
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

mkevent (pos, dur, text) =
    (TrackPos pos, TestSetup.event text dur)
mkscore (pos, dur, text) = Score.event pos dur text
mktrack triplets = Track.modify_events (TestSetup.empty_track "mktrack")
    (Track.insert_events (map mkevent triplets))

track1 = mktrack [(0, 16, "4c-"), (16, 16, "4c#")]
track_cont = mktrack [(0, 0, "1"), (16, 0, "i.75"), (32, 0, "i0")]

mksignal segs = Signal.signal [(TrackPos p, m, v, v) | (p, m, v) <- segs]

extract_events :: [Score.Event] -> [(Integer, Integer, String)]
extract_events = map $ \event ->
    (fromIntegral (Score.event_start event),
        fromIntegral (Score.event_duration event), Score.event_text event)

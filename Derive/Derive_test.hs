module Derive.Derive_test where
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List

import Util.Test
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
import qualified Derive.Twelve as Twelve

import qualified Perform.Signal as Signal
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.InstrumentDb as InstrumentDb
import qualified Perform.Midi.Perform as Perform


-- simple single instrument
basic_schema :: (Monad m) => Derive.DeriveT m [Score.Event]
basic_schema = bass =<< twelve =<< Derive.d_track track_id

test_basic = do
    let (Right events, _tempo, msgs) = Derive.derive ui_state basic_schema
    equal msgs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, _warns) = Perform.perform default_inst_config midi_events
    equal (length msgs) 4 -- (noteon + noteoff) * 2
    pmlist "msgs" msgs

-- with velocity track
controller_schema :: (Monad m) => Derive.DeriveT m [Score.Event]
controller_schema = Controller.d_controller (Score.Controller "breath")
    (Controller.d_signal =<< Derive.d_track cont_track_id)
    (bass =<< twelve =<< Derive.d_track track_id)

test_controller = do
    let (Right events, _tempo, msgs) = Derive.derive ui_state controller_schema
    equal msgs []
    pmlist "score" events
    let (midi_events, warns) = Convert.convert events
    equal warns []
    equal (length midi_events) 2
    pmlist "midi_events" midi_events
    let (msgs, _warns) = Perform.perform default_inst_config midi_events
    equal (length msgs) 4 -- (noteon + noteoff) * 2
    pmlist "msgs" msgs

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

    equal (run "-2e.2") (Right (TrackPos 0, Signal.Exp (-2), 0.2))
    equal (run "-.2") (Right (TrackPos 0, Signal.Linear, -0.2))

test_run :: Derive.DeriveT Identity.Identity a -> Either String a
test_run m = case Identity.runIdentity (Derive.run State.empty m) of
    (Left err, _, logs) -> Left (Derive.error_message err)
    (Right val, _, _) -> Right val


-- * setup

bass events = return (midi_instrument default_inst events)


track_id = Track.TrackId "b1.t1"
cont_track_id = Track.TrackId "b1.cont"

default_inst = Score.Instrument "fm8/bass"
default_midi_inst = InstrumentDb.generic "fm8" "bass" 
default_dev = Midi.WriteDevice "CoreMIDI/IAC Driver Bus 1"
default_inst_config =
    Instrument.config [((default_dev, 0), default_midi_inst)] Nothing


-- ** ui stetup

ui_state = State.run_state State.empty $ do
    ruler <- State.create_ruler "r1"
        (TestSetup.ruler [TestSetup.marklist 0 1])
    overlay <- State.create_ruler "r1.overlay"
        =<< fmap TestSetup.overlay_ruler (State.get_ruler ruler)
    t1 <- State.create_track "b1.t1" track1
    t2 <- State.create_track "b1.t2" track1
    t3 <- State.create_track "b1.cont" track_cont
    b1 <- State.create_block "b1" $
        Block.block "b1 title" TestSetup.default_block_config
            (Block.RId ruler)
            [(Block.TId t1 overlay, 40), (Block.TId t2 overlay, 40)]
            (Block.SchemaId "no schema")
    return ()

empty_track = Track.track "" [] Color.white
mkevent (pos, dur, text) =
    (TrackPos (pos * 100), TestSetup.event text (dur * 100))
mktrack triplets = Track.modify_events TestSetup.empty_track
    (Track.insert_events (map mkevent triplets))

track1 = mktrack [(0, 16, "4c-"), (16, 16, "4c#")]
track_cont = mktrack [(0, 0, "s 1"), (16, 0, "i .75"), (32, 0, "i 0")]

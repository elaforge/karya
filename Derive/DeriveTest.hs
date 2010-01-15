module Derive.DeriveTest where
import qualified Control.Monad.Identity as Identity
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Util.Log as Log

import qualified Midi.Midi as Midi
import qualified Instrument.MidiDb as MidiDb

import Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.State as State

import qualified Derive.Call.All as Call.All
import qualified Derive.Derive as Derive
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Transport as Transport
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Control as Midi.Control
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


scale_id = Twelve.scale_id

pitch_signal :: Pitch.ScaleId -> [PitchSignal.Segment]
    -> PitchSignal.PitchSignal
pitch_signal scale_id =
    PitchSignal.track_signal scale_id PitchSignal.default_srate

signal = Signal.track_signal Signal.default_srate

-- * run

run :: State.State -> Derive.DeriveT Identity.Identity a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m =
    case Identity.runIdentity (Derive.run derive_state m) of
        (Left err, _, _logs) -> Left (Derive.error_message err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    -- Good to have a minimal fake stack so there's someplace to put
    -- trackpos.
    fake_stack = [(UiTest.bid "blck", Just (UiTest.tid "trck"), Nothing)]
    derive_state = (Derive.initial_state ui_state
        (Schema.lookup_deriver Map.empty ui_state) Call.All.call_map False)
            { Derive.state_stack = fake_stack }

-- * derive

type Result a = (Either String a, Transport.TempoFunction,
    Transport.InverseTempoFunction, [Log.Msg], Derive.State)

derive_tracks :: [UiTest.TrackSpec] -> Result [Score.Event]
derive_tracks tracks = derive_block ui_state UiTest.default_block_id
    where (_, ui_state) = UiTest.run_mkstate tracks

derive_tracks_tempo :: [UiTest.TrackSpec] -> Result [Score.Event]
derive_tracks_tempo tracks = derive_tracks (("tempo", [(0, 0, "1")]) : tracks)

perform :: Instrument.Config -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning],
        [(Timestamp.Timestamp, Midi.Message)], [Warning.Warning])
perform inst_config events = (perf_events, convert_warns, mmsgs, perform_warns)
    where
    (perf_events, convert_warns) = Convert.convert default_lookup events
    (msgs, perform_warns) =
        Perform.perform default_lookup inst_config perf_events
    mmsgs = map (\m -> (Midi.wmsg_ts m, Midi.wmsg_msg m)) msgs

derive_block :: State.State -> BlockId -> Result [Score.Event]
derive_block ui_state block_id = derive lookup_deriver ui_state deriver
    where
    lookup_deriver = Schema.lookup_deriver Map.empty ui_state
    deriver = Derive.d_block block_id

derive :: Derive.LookupDeriver -> State.State
    -> Derive.DeriveT Identity.Identity a -> Result a
derive lookup_deriver ui_state d =
    case Derive.derive lookup_deriver ui_state Call.All.call_map False d of
        (Left err, b, c, d, e) -> (Left (show err), b, c, d, e)
        (Right a, b, c, d, e) -> (Right a, b, c, d, e)

-- ** extract

e_val :: Result a -> (Either String a, [Log.Msg])
e_val (val, _, _, msgs, _) = (val, msgs)

e_val_right :: Result a -> (a, [Log.Msg])
e_val_right result = case e_val result of
    (Left err, _) -> error $ "e_val_right: unexpected Left: " ++ err
    (Right v, logs) -> (v, logs)

e_logs :: Result a -> (Either String a, [String])
e_logs result = let (val, msgs) = e_val result in (val, map Log.msg_string msgs)

extract :: (Score.Event -> a) -> (Log.Msg -> b) -> Result [Score.Event]
    -> (Either String [a], [b])
extract e_event e_log result = (fmap (map e_event) val, map e_log logs)
    where (val, logs) = e_val result

extract_events :: (Score.Event -> a) -> Result [Score.Event]
    -> (Either String [a], [Log.Msg])
extract_events e_event = extract e_event id

-- | Get standard event info.
e_event :: Score.Event -> (TrackPos, TrackPos, String)
e_event e = (Score.event_start e, Score.event_duration e, Score.event_string e)

note_on_times :: [(Timestamp.Timestamp, Midi.Message)]
    -> [(Integer, Midi.Key, Midi.Velocity)]
note_on_times mmsgs = [(ts, nn, vel)
    | (Timestamp.Timestamp ts, Midi.ChannelMessage _ (Midi.NoteOn nn vel))
        <- mmsgs]


-- * inst

default_lookup :: MidiDb.LookupMidiInstrument
default_lookup attrs (Score.Instrument inst)
    | inst == "i" = Just (default_perf_inst
        { Instrument.inst_keyswitch =
            Instrument.get_keyswitch default_ksmap attrs })
    | otherwise = Nothing

default_inst = Score.Instrument "i"
default_perf_inst = Instrument.instrument "synth" "patch" Nothing
            Midi.Control.empty_map (-2, 2)
default_inst_title = ">i"

default_inst_config =
    Instrument.config [(default_inst, [dev 0, dev 1, dev 2])] Nothing
    where dev = (,) (Midi.WriteDevice "out")

default_ksmap = Instrument.KeyswitchMap $
    map (\(attr, name, nn) -> (Set.fromList attr, Instrument.Keyswitch name nn))
        [ (["a1", "a2"], "a1+a2", 0)
        , (["a0"], "a0", 1)
        , (["a1"], "a1", 2)
        , (["a2"], "a2", 3)
        ]

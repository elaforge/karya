module Derive.DeriveTest where
import Control.Monad
import qualified Control.Monad.Identity as Identity
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
-- import qualified Util.SrcPos as SrcPos
import Util.Test

import qualified Midi.Midi as Midi

import Ui
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Call.All as Call.All
import qualified Derive.Call as Call
import qualified Derive.Derive as Derive
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified Instrument.Search as Search


scale_id = Twelve.scale_id

-- Drop the first sample to make this match output from the derive call
-- interpolators.
pitch_interpolate :: RealTime -> Float -> RealTime -> Float
    -> [(RealTime, PitchSignal.Y)]
pitch_interpolate x0 y0 x1 y1 = drop 1 [(x, (y0, y1, to_n x)) | x <- [x0 .. x1]]
    where to_n x = realToFrac (x - x0) / fromIntegral (length [x0 .. x1] - 1)

-- * run

run :: State.State -> Derive.Deriver a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m =
    case Identity.runIdentity (Derive.run derive_state m) of
        (Left err, _, _logs) -> Left (Pretty.pretty err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    -- Make sure Derive.get_current_block_id, called by add_track_warp, doesn't
    -- throw.
    initial_stack = Stack.make [Stack.Block (UiTest.bid "fakeblock")]
    derive_state = (Derive.initial_state default_scopes Derive.empty_cache
        Monoid.mempty default_environ (default_constant ui_state))
            { Derive.state_stack = initial_stack }

default_constant ui_state =
    Derive.initial_constant ui_state (default_lookup_deriver ui_state)
        default_lookup_scale (const Nothing) False

eval :: State.State -> Derive.Deriver a -> Either String a
eval state m = case run state m of
    Left err -> Left err
    Right (val, _, _) -> Right val


-- * perform

perform :: MidiDb.LookupMidiInstrument -> Instrument.Config -> [Score.Event]
    -> ([Perform.Event], [Warning.Warning],
        [(Timestamp.Timestamp, Midi.Message)], [Warning.Warning])
perform lookup_inst midi_config events =
    (perf_events, convert_warns, mmsgs, perform_warns)
    where
    (perf_events, convert_warns) =
        Convert.convert default_lookup_scale lookup_inst events
    (msgs, perform_warns, _) = Perform.perform Perform.initial_state
        midi_config perf_events
    mmsgs = map (\m -> (Midi.wmsg_ts m, Midi.wmsg_msg m)) msgs

perform_defaults :: [Score.Event]
    -> ([Perform.Event], [Warning.Warning],
        [(Timestamp.Timestamp, Midi.Message)], [Warning.Warning])
perform_defaults = perform default_lookup_inst default_midi_config

-- * derive

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result [Score.Event]
derive_tracks = derive_tracks_with id

derive_tracks_tempo :: [UiTest.TrackSpec] -> Derive.Result [Score.Event]
derive_tracks_tempo tracks = derive_tracks (("tempo", [(0, 0, "1")]) : tracks)

derive_tracks_with :: Transform [Score.Event] -> [UiTest.TrackSpec]
    -> Derive.Result [Score.Event]
derive_tracks_with with tracks =
    derive_blocks_with with [(UiTest.default_block_name, tracks)]

-- | Create multiple blocks, and derive the first one.
derive_blocks :: [(String, [UiTest.TrackSpec])] -> Derive.Result [Score.Event]
derive_blocks = derive_blocks_with id

derive_blocks_with :: Transform [Score.Event] -> [(String, [UiTest.TrackSpec])]
    -> Derive.Result [Score.Event]
derive_blocks_with with block_tracks = derive_block_with with ui_state bid
    where
    (_, ui_state) = UiTest.run State.empty $ do
        forM_ block_tracks $ \(bid, tracks) -> UiTest.mkstate bid tracks
        set_defaults
    bid = UiTest.bid (fst (head block_tracks))

derive_block :: State.State -> BlockId -> Derive.Result [Score.Event]
derive_block = derive_block_with id

derive_block_with :: Transform [Score.Event] -> State.State
    -> BlockId -> Derive.Result [Score.Event]
derive_block_with with ui_state block_id = derive ui_state deriver
    where deriver = with (Call.eval_root_block block_id)

derive :: State.State -> Derive.Deriver a -> Derive.Result a
derive ui_state deriver =
    Derive.derive (default_constant ui_state) default_scopes
        Derive.empty_cache Monoid.mempty default_environ deriver

type Transform a = Derive.Deriver a -> Derive.Deriver a

-- ** defaults

-- | Set UI state defaults that every derivation should have.
set_defaults :: (State.UiStateMonad m) => m ()
set_defaults = State.set_midi_config default_midi_config

default_lookup_scale :: Derive.LookupScale
default_lookup_scale scale_id = Map.lookup scale_id Scale.All.scales

default_scopes :: [Derive.Scope]
default_scopes = Call.All.scopes

default_environ :: TrackLang.Environ
default_environ = Map.fromList
    -- tests are easier to write and read with integral interpolation
    [ (TrackLang.v_srate, TrackLang.VNum 1)
    , (TrackLang.v_scale, TrackLang.VScaleId Twelve.scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    ]

default_lookup_deriver ui_state = Schema.lookup_deriver Map.empty ui_state

-- ** extract

-- | Tests generally shouldn't depend on logs below a certain priority since
-- those don't indicate anything interesting.
filter_logs :: [Log.Msg] -> [Log.Msg]
filter_logs logs =
    Log.trace_logs (filter (not . cache_msg) low) high
    where
    (low, high) = List.partition ((< Log.Warn) . Log.msg_prio) logs
    -- It's a hack, but the cache logs are annoying.
    -- I can't use the srcpos because it doesn't exist in ghci.
    -- cache = (== Just (Just "cached_generator")) . fmap SrcPos.srcpos_func
    --     . Log.msg_caller
    cache_msg msg =
        any (`List.isInfixOf` s) ["using cache", "rederived generator"]
        where s = Log.msg_string msg

quiet_filter_logs :: [Log.Msg] -> [Log.Msg]
quiet_filter_logs = filter ((>=Log.Warn) . Log.msg_prio)

e_val :: Derive.Result a -> (Either String a, [Log.Msg])
e_val res = (map_left show (Derive.r_result res),
    filter_logs (Derive.r_logs res))

e_val_right :: Derive.Result a -> (a, [Log.Msg])
e_val_right result = case e_val result of
    (Left err, _) -> error $ "e_val_right: unexpected Left: " ++ err
    (Right v, logs) -> (v, logs)

r_logs :: Derive.Result a -> [String]
r_logs = map Log.msg_string . filter_logs . Derive.r_logs

e_logs :: Derive.Result a -> (Either String a, [String])
e_logs result = (val, map Log.msg_string (filter_logs msgs))
    where (val, msgs) = e_val result

extract :: (Score.Event -> a) -> (Log.Msg -> b)
    -> Derive.Result [Score.Event] -> (Either String [a], [b])
extract e_event e_log result =
    (fmap (map e_event) val, map e_log (filter_logs logs))
    where (val, logs) = e_val result

extract_events :: (Score.Event -> a) -> Derive.Result [Score.Event]
    -> (Either String [a], [String])
extract_events ex_event = extract ex_event Log.msg_string

extract_events_only :: (Score.Event -> a) -> Derive.Result [Score.Event]
    -> Either String [a]
extract_events_only ex_event result = Log.trace_logs logs vals
    where (vals, logs) = extract ex_event id result

-- | Get standard event info.
e_event :: Score.Event -> (RealTime, RealTime, String)
e_event e = (Score.event_start e, Score.event_duration e, Score.event_string e)

e_pitch :: Score.Event -> (RealTime, RealTime, String, Pitch.Degree)
e_pitch e = (Score.event_start e, Score.event_duration e, Score.event_string e,
    Score.initial_pitch e)

e_everything :: Score.Event
    -> (RealTime, RealTime, String, Maybe String, [String])
e_everything e =
        ( Score.event_start e
        , Score.event_duration e
        , Score.event_string e
        , fmap uninst (Score.event_instrument e)
        , Score.attrs_list (Score.event_attributes e)
        )
    where uninst (Score.Instrument inst) = inst

e_control :: String -> Score.Event -> Maybe [(RealTime, Signal.Y)]
e_control cont event = fmap Signal.unsignal $
    Map.lookup (Score.Control cont) (Score.event_controls event)

note_on_times :: [(Timestamp.Timestamp, Midi.Message)]
    -> [(Integer, Midi.Key, Midi.Velocity)]
note_on_times mmsgs = [(Timestamp.to_millis ts, nn, vel)
    | (ts, Midi.ChannelMessage _ (Midi.NoteOn nn vel)) <- mmsgs]


-- * call

passed_args :: String -> [TrackLang.Val] -> Derive.PassedArgs derived
passed_args call vals = Derive.PassedArgs vals Map.empty
    (TrackLang.Symbol call) (Derive.dummy_call_info "DeriveTest")

derive_note :: Derive.Deriver a -> Derive.Result a
derive_note = derive State.empty

empty_lookup_deriver :: Derive.LookupDeriver
empty_lookup_deriver = const (Right Derive.empty_events)

d_note :: Derive.EventDeriver
d_note = do
    start <- Derive.now
    end <- Derive.score_to_real 1
    inst <- Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    st <- Derive.get
    let controls = Derive.state_controls st
        pitch_sig = Derive.state_pitch st
    return [Score.Event start (end-start) (Text.pack "evt")
        controls pitch_sig Stack.empty inst attrs]

-- * inst

make_lookup_inst :: [Instrument.Patch] -> MidiDb.LookupMidiInstrument
make_lookup_inst patches = MidiDb.lookup_midi (fst (MidiDb.midi_db [sdesc]))
    where sdesc = MidiDb.softsynth "s" (Just "wdev") (-2, 2) patches [] id

make_midi_config :: [(String, [Midi.Channel])] -> Instrument.Config
make_midi_config config = Instrument.config
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.WriteDevice "wdev", chan)

default_lookup_inst :: MidiDb.LookupMidiInstrument
default_lookup_inst = make_lookup_inst default_patches

default_db :: Instrument.Db.Db
default_db = Instrument.Db.db midi_db Search.empty_index
    where
    midi_db = fst $ MidiDb.midi_db
        [MidiDb.softsynth "syn" Nothing (-2, 2) default_patches [] id]

default_patches :: [Instrument.Patch]
default_patches =
    [ Instrument.patch $ Instrument.instrument "1" [] (-2, 2)
    , Instrument.patch $ Instrument.instrument "2" [] (-2, 2)
    ]

default_midi_config :: Instrument.Config
default_midi_config = make_midi_config [("s/1", [0..2]), ("s/2", [3])]

default_inst_title = ">s/1"

-- * mkevents

-- | Name will determine the pitch.  It can be a-z.
type EventSpec = (RealTime, RealTime, String,
    [(Score.Control, Signal.Control)], Score.Instrument)

mkevent :: EventSpec -> Score.Event
mkevent (start, dur, text, controls, inst) =
    Score.Event start dur (Text.pack text) (Map.fromList controls)
        (psig start text) fake_stack (Just inst) Score.no_attrs
    where
    psig pos p = PitchSignal.signal Pitch.twelve [(pos, to_pitch p)]
    to_pitch p = PitchSignal.degree_to_y $ Pitch.Degree $
        Maybe.fromMaybe (error ("no pitch " ++ show p)) (lookup p pitch_map)
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]

fake_stack :: Stack.Stack
fake_stack = Stack.make
    [ Stack.Block (UiTest.bid "fakeblock")
    , Stack.Track (UiTest.tid "faketrack")
    , Stack.Region 42 43
    ]

mkcontrols :: [(String, [(RealTime, Signal.Y)])] -> Score.ControlMap
mkcontrols csigs = Map.fromList
    [(Score.Control c, Signal.signal sig) | (c, sig) <- csigs]

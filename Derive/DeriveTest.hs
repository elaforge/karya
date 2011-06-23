module Derive.DeriveTest where
import qualified Data.ByteString.Char8 as B
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import Ui
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.All as Call.All
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Schema as Schema
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified App.MidiInst as MidiInst


scale_id = Twelve.scale_id

-- Drop the first sample to make this match output from the derive call
-- interpolators.
pitch_interpolate :: RealTime -> Float -> RealTime -> Float
    -> [(RealTime, PitchSignal.Y)]
pitch_interpolate x0 y0 x1 y1 =
    drop 1 [(x, (y0, y1, Num.d2f (to_n x))) | x <- Seq.range x0 x1 1]
    where
    to_n x = RealTime.to_seconds (x - x0) /
        fromIntegral (length (Seq.range x0 x1 1) - 1)

-- * run

run :: State.State -> Derive.Deriver a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m = run_ ui_state (Internal.with_stack_block bid m)
    where
    -- Make sure Derive.get_current_block_id, called by add_new_track_warp,
    -- doesn't throw.
    bid = UiTest.bid "DeriveTest.run fakeblock"

-- | Run without a fake stack.
run_ :: State.State -> Derive.Deriver a
    -> Either String (a, Derive.State, [Log.Msg])
run_ ui_state m = case Derive.run derive_state m of
        (Left err, _, _logs) -> Left (Pretty.pretty err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    derive_state = Derive.initial_state default_scope
        default_environ (default_constant ui_state mempty mempty)

extract_run :: (a -> b) -> Either String (a, Derive.State, [Log.Msg])
    -> Either String b
extract_run _ (Left err) = Left err
extract_run f (Right (val, _, msgs)) = Right $ trace_logs msgs (f val)

default_constant   :: State.State -> Derive.Cache -> Derive.ScoreDamage
    -> Derive.Constant
default_constant ui_state cache damage =
    Derive.initial_constant ui_state (default_lookup_deriver ui_state)
        default_lookup_scale (const Nothing) cache damage

eval :: State.State -> Derive.Deriver a -> Either String a
eval ui_state m = extract_run id (run ui_state m)


-- * perform

type Midi = (Integer, Midi.Message)

perform_block :: [UiTest.TrackSpec] -> ([Midi], [String])
perform_block tracks = perform_blocks [(UiTest.default_block_name, tracks)]

perform_blocks :: [(String, [UiTest.TrackSpec])] -> ([Midi], [String])
perform_blocks blocks = (mmsgs, map show_log (filter interesting_log logs))
    where
    (_, mmsgs, logs) = perform default_lookup_inst default_midi_config
        (Derive.r_events result)
    result = derive_blocks blocks

perform :: MidiDb.LookupMidiInstrument -> Instrument.Config -> Derive.Events
    -> ([Perform.Event], [Midi], [Log.Msg])
perform lookup_inst midi_config events =
    (fst (LEvent.partition perf_events), mmsgs, filter interesting_log logs)
    where
    (perf_events, perf) = perform_stream lookup_inst midi_config events
    (mmsgs, logs) = LEvent.partition perf

perform_defaults :: Derive.Events -> ([Perform.Event], [Midi], [Log.Msg])
perform_defaults = perform default_lookup_inst default_midi_config

perform_stream :: MidiDb.LookupMidiInstrument -> Instrument.Config
    -> Derive.Events
    -> ([LEvent.LEvent Perform.Event], [LEvent.LEvent Midi])
perform_stream lookup_inst midi_config events = (perf_events, mmsgs)
    where
    perf_events = Convert.convert default_lookup_scale lookup_inst events
    (midi, _) = Perform.perform Perform.initial_state midi_config perf_events
    mmsgs = map (fmap extract_m) midi
    extract_m wmsg =
        (RealTime.to_milliseconds (Midi.wmsg_ts wmsg), Midi.wmsg_msg wmsg)

-- * derive

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = derive_tracks_with id

derive_tracks_tempo :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks_tempo tracks = derive_tracks (("tempo", [(0, 0, "1")]) : tracks)

derive_tracks_with :: Transform Derive.Events -> [UiTest.TrackSpec]
    -> Derive.Result
derive_tracks_with with = derive_tracks_with_ui with id

-- | Variant that lets you modify both the Deriver state and the UI state.
-- You can't modify the UI state inside the deriver state because the UI state
-- is embedded in default_lookup_deriver and will revert as soon as a block is
-- called.  TODO if I get rid of schemas this may change
derive_tracks_with_ui :: Transform Derive.Events -> TransformUi
    -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_with_ui with transform_ui tracks = derive_blocks_with_ui
    with transform_ui [(UiTest.default_block_name, tracks)]

-- | Create multiple blocks, and derive the first one.
derive_blocks :: [(String, [UiTest.TrackSpec])] -> Derive.Result
derive_blocks = derive_blocks_with_ui id id

derive_blocks_with   :: Transform Derive.Events
    -> [(String, [UiTest.TrackSpec])] -> Derive.Result
derive_blocks_with with = derive_blocks_with_ui with id

derive_blocks_with_ui :: Transform Derive.Events -> TransformUi
    -> [(String, [UiTest.TrackSpec])] -> Derive.Result
derive_blocks_with_ui with transform_ui block_tracks =
    derive_block_with with (transform_ui ui_state) bid
    where
    (_, ui_state) = UiTest.run State.empty $ do
        UiTest.mkblocks block_tracks
        set_defaults
    bid = UiTest.bid (fst (head block_tracks))

derive_block :: State.State -> BlockId -> Derive.Result
derive_block = derive_block_with id

derive_block_with :: Transform Derive.Events -> State.State
    -> BlockId -> Derive.Result
derive_block_with with ui_state block_id = derive ui_state deriver
    where deriver = with (Call.Block.eval_root_block block_id)

derive :: State.State -> Derive.EventDeriver -> Derive.Result
derive ui_state deriver = Derive.extract_result $
    Derive.derive (default_constant ui_state mempty mempty) default_scope
        default_environ deriver

type Transform a = Derive.Deriver a -> Derive.Deriver a
type TransformUi = State.State -> State.State

-- ** derive with cache

derive_block_cache :: Derive.Cache -> Derive.ScoreDamage -> State.State
    -> BlockId -> Derive.Result
derive_block_cache cache damage ui_state block_id =
    derive_cache cache damage ui_state deriver
    where deriver = Call.Block.eval_root_block block_id

derive_cache :: Derive.Cache -> Derive.ScoreDamage -> State.State
    -> Derive.EventDeriver -> Derive.Result
derive_cache cache damage ui_state deriver = Derive.extract_result $
    Derive.derive constant default_scope default_environ deriver
    where
    constant = default_constant ui_state cache damage

make_damage :: String -> TrackNum -> ScoreTime -> ScoreTime
    -> Derive.ScoreDamage
make_damage block tracknum s e = Derive.ScoreDamage
    (Map.singleton (UiTest.mk_tid_name block tracknum) (Ranges.range s e))
    (Set.singleton (UiTest.bid block)) mempty


-- ** defaults

with_instrument :: State.State -> State.State
with_instrument state = state
    { State.state_midi_config = default_midi_config
    , State.state_default = (State.state_default state)
        { State.default_instrument = Just (Score.Instrument "s/1")
        }
    }

-- | Set UI state defaults that every derivation should have.
set_defaults :: (State.M m) => m ()
set_defaults = State.modify with_instrument

default_lookup_scale :: Derive.LookupScale
default_lookup_scale scale_id = Map.lookup scale_id Scale.All.scales

default_scope :: Derive.Scope
default_scope = Call.All.scope

default_environ :: TrackLang.Environ
default_environ = Map.fromList
    -- tests are easier to write and read with integral interpolation
    [ (TrackLang.v_srate, TrackLang.VNum 1)
    , (TrackLang.v_scale, TrackLang.VScaleId Twelve.scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    ]

default_lookup_deriver ui_state = Schema.lookup_deriver Map.empty ui_state

-- ** extract

-- *** log msgs

trace_logs :: [Log.Msg] -> a -> a
trace_logs logs = Log.trace_logs (filter interesting_log logs)

-- | Tests generally shouldn't depend on logs below a certain priority since
-- those don't indicate anything interesting.
interesting_log :: Log.Msg -> Bool
interesting_log = (>=Log.Warn) . Log.msg_prio

-- It's a hack, but the cache logs are annoying.
-- Srcpos would be better but it doesn't exist in ghci.
-- cache = (== Just (Just "cached_generator")) . fmap SrcPos.srcpos_func
--     . Log.msg_caller
cache_msg :: Log.Msg -> Bool
cache_msg msg = any (`List.isInfixOf` s) ["using cache", "rederived generator"]
    where s = Log.msg_string msg

quiet_filter_logs :: [Log.Msg] -> [Log.Msg]
quiet_filter_logs = filter ((>=Log.Warn) . Log.msg_prio)

-- ** extract

extract :: (Score.Event -> a) -> Derive.Result -> ([a], [String])
extract e_event result = (map e_event events, map show_log logs)
    where (events, logs) = r_split result

extract_events :: (Score.Event -> a) -> Derive.Result -> [a]
extract_events e_event result = Log.trace_logs logs (map e_event events)
    where (events, logs) = r_split result

extract_stream :: (Score.Event -> a) -> Derive.Result -> [Either a String]
extract_stream e_event =
    map (either (Left . e_event) (Right . show_log) . to_either)
        . filter interesting . Derive.r_events
    where
    interesting (LEvent.Log log) = interesting_log log
    interesting _ = True
    to_either (LEvent.Event e) = Left e
    to_either (LEvent.Log m) = Right m

r_split :: Derive.Result -> ([Score.Event], [Log.Msg])
r_split = second (filter interesting_log) . LEvent.partition . Derive.r_events

r_logs :: Derive.Result -> [String]
r_logs = snd . extract id

e_event :: Score.Event -> (RealTime, RealTime, String)
e_event e = (Score.event_start e, Score.event_duration e, Score.event_string e)

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

e_pitch :: Score.Event -> [(RealTime, PitchSignal.Degree)]
e_pitch = PitchSignal.unsignal_degree . Score.event_pitch

-- ** extract log msgs

show_log_stack :: Log.Msg -> String
show_log_stack msg = show_stack (Log.msg_stack msg) ++ ": " ++ show_log msg

show_stack :: Maybe Stack.Stack -> String
show_stack Nothing = "<nothing>"
show_stack (Just stack)
    | null ui = "<no stack>"
    -- This uses ': ' so 'x: *' works regardless of where in the stack x is.
    | otherwise = Seq.join ": " (map Stack.unparse_ui_frame ui)
    where ui = Stack.to_ui stack

show_log :: Log.Msg -> String
show_log = Log.msg_string

-- ** extract midi msgs

note_on_times :: [(Integer, Midi.Message)]
    -> [(Integer, Midi.Key, Midi.Velocity)]
note_on_times mmsgs =
    [(ts, nn, vel) | (ts, Midi.ChannelMessage _ (Midi.NoteOn nn vel)) <- mmsgs]

extract_midi :: Perform.MidiEvents -> [(Integer, Midi.Message)]
extract_midi events = [(RealTime.to_milliseconds ts, msg)
    | Midi.WriteMessage _ ts msg <- LEvent.events_of events]

-- * call

passed_args :: String -> [TrackLang.Val] -> Derive.PassedArgs derived
passed_args call vals = Derive.PassedArgs vals Map.empty
    (TrackLang.Symbol call) (Derive.dummy_call_info "DeriveTest")

empty_lookup_deriver :: Derive.LookupDeriver
empty_lookup_deriver = const (Right (return mempty))

c_note :: ScoreTime -> ScoreTime -> Derive.EventDeriver
c_note s_start dur = do
    start <- Derive.score_to_real s_start
    end <- Derive.score_to_real (s_start + dur)
    inst <- Derive.lookup_val TrackLang.v_instrument
    attrs <- Maybe.fromMaybe Score.no_attrs <$>
        Derive.lookup_val TrackLang.v_attributes
    st <- Derive.gets Derive.state_dynamic
    let controls = Derive.state_controls st
        pitch_sig = Derive.state_pitch st
    return $ LEvent.one $ LEvent.Event $
        Score.Event start (end-start) (B.pack "evt") controls pitch_sig
            Stack.empty inst attrs

-- | Not supposed to do this in general, but it's ok for tests.
modify_dynamic :: (Derive.Dynamic -> Derive.Dynamic) -> Derive.Deriver ()
modify_dynamic f = Derive.modify $ \st ->
    st { Derive.state_dynamic = f (Derive.state_dynamic st) }

-- * inst

make_lookup_inst :: [Instrument.Patch] -> MidiDb.LookupMidiInstrument
make_lookup_inst patches = Instrument.Db.db_lookup_midi (make_db patches)

make_midi_config :: [(String, [Midi.Channel])] -> Instrument.Config
make_midi_config config = Instrument.config
    [(Score.Instrument inst, map mkaddr chans) | (inst, chans) <- config]
    where mkaddr chan = (Midi.WriteDevice "wdev", chan)

default_lookup_inst :: MidiDb.LookupMidiInstrument
default_lookup_inst = make_lookup_inst default_patches

default_db :: Cmd.InstrumentDb
default_db = make_db default_patches

make_db :: [Instrument.Patch] -> Cmd.InstrumentDb
make_db patches = Instrument.Db.db midi_db
    where
    sdescs = MidiInst.make $ (MidiInst.softsynth "s" (Just "wdev") (-2, 2) [])
        { MidiInst.extra_patches =
            map (\p -> (p, MidiInst.empty_code)) patches }
    midi_db = fst $ MidiDb.midi_db sdescs

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
    Score.Event start dur (B.pack text) (Map.fromList controls)
        (psig start text) fake_stack (Just inst) Score.no_attrs
    where
    psig pos p = PitchSignal.signal Pitch.twelve [(pos, to_pitch p)]
    to_pitch p = PitchSignal.degree_to_y $ Pitch.Degree $
        Maybe.fromMaybe (error ("no pitch " ++ show p)) (lookup p pitch_map)
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]

fake_stack :: Stack.Stack
fake_stack = Stack.from_outermost
    [ Stack.Block (UiTest.bid "fakeblock")
    , Stack.Track (UiTest.tid "faketrack")
    , Stack.Region 42 43
    ]

mkcontrols :: [(String, [(RealTime, Signal.Y)])] -> Score.ControlMap
mkcontrols csigs = Map.fromList
    [(Score.Control c, Signal.signal sig) | (c, sig) <- csigs]

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.DeriveTest where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.IO.Unsafe as Unsafe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Ranges as Ranges
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Midi.StubMidi as StubMidi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Ruler as Ruler
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Simple as Simple

import qualified Derive.Call.All as Call.All
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.Eval as Eval
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Stack as Stack
import Derive.TestInstances ()
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config
import Types


-- | Simulate the linear interpolate call, to check deriver output that
-- involves that particular call.
--
-- Drops the first sample to make this match output from the derive call
-- interpolators.
signal_interpolate :: RealTime -> Signal.Y -> RealTime -> Signal.Y
    -> [(RealTime, Signal.Y)]
signal_interpolate x0 y0 x1 y1 =
    drop 1 [(x, project x) | x <- Seq.range_end x0 x1 1]
    where
    project = Num.scale y0 y1 . Num.normalize (to_y x0) (to_y x1) . to_y
    to_y = RealTime.to_seconds

-- * run

run :: State.State -> Derive.Deriver a
    -> Either String (a, Derive.State, [Log.Msg])
run ui_state m = run_ ui_state (Internal.with_stack_block bid m)
    where
    -- Make sure Derive.get_current_block_id, called by add_new_track_warp,
    -- doesn't throw.
    bid = UiTest.bid "derive-test.run-fakeblock"

-- | Run without a fake stack.
run_ :: State.State -> Derive.Deriver a
    -> Either String (a, Derive.State, [Log.Msg])
run_ ui_state m = case Derive.run derive_state m of
        (Left err, _, _logs) -> Left (pretty err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    derive_state = Derive.initial_state
        default_environ (default_constant ui_state mempty mempty)

extract_run :: (a -> b) -> Either String (a, Derive.State, [Log.Msg])
    -> Either String b
extract_run _ (Left err) = Left err
extract_run f (Right (val, _, msgs)) = Right $ trace_logs msgs (f val)

run_events :: (a -> b)
    -> Either String ([LEvent.LEvent a], Derive.State, [Log.Msg])
    -> Either String ([b], [String])
run_events f = extract_run $
    first (map f) . second (map show_log . filter interesting_log)
        . LEvent.partition

default_constant :: State.State -> Derive.Cache -> Derive.ScoreDamage
    -> Derive.Constant
default_constant ui_state cache damage = Derive.initial_constant ui_state
    default_library default_lookup_scale (const Nothing) cache damage

eval :: State.State -> Derive.Deriver a -> Either String a
eval ui_state m = extract_run id (run ui_state m)


-- * perform

perform_block :: [UiTest.TrackSpec] -> ([Midi.WriteMessage], [String])
perform_block tracks = perform_blocks [(UiTest.default_block_name, tracks)]

perform_blocks :: [UiTest.BlockSpec] -> ([Midi.WriteMessage], [String])
perform_blocks blocks = (mmsgs, map show_log (filter interesting_log logs))
    where
    (_, mmsgs, logs) = perform default_convert_lookup default_midi_config
        (Derive.r_events result)
    result = derive_blocks blocks

perform :: Convert.Lookup -> Instrument.Configs -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform lookup midi_config events =
    (fst (LEvent.partition perf_events), mmsgs, filter interesting_log logs)
    where
    (perf_events, perf) = perform_stream lookup midi_config events
    (mmsgs, logs) = LEvent.partition perf

perform_defaults :: Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform_defaults = perform default_convert_lookup default_midi_config

perform_stream :: Convert.Lookup -> Instrument.Configs -> Derive.Events
    -> ([LEvent.LEvent Perform.Event], [LEvent.LEvent Midi.WriteMessage])
perform_stream lookup midi_config events = (perf_events, midi)
    where
    perf_events = Convert.convert lookup (LEvent.events_of events)
    (midi, _) = Perform.perform Perform.initial_state midi_config perf_events

-- | Perform events with the given instrument config.
perform_inst :: [Cmd.SynthDesc] -> [(Text, [Midi.Channel])] -> Derive.Events
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform_inst synths config =
    perform (synth_to_convert_lookup mempty synths) (UiTest.midi_config config)

-- * derive

type Transform a = Derive.Deriver a -> Derive.Deriver a

derive_tracks :: String -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks = derive_tracks_with id

derive_tracks_with :: Transform Derive.Events -> String -> [UiTest.TrackSpec]
    -> Derive.Result
derive_tracks_with with = derive_tracks_with_ui with id

-- | Variant that lets you modify both the Deriver state and the UI state.
-- Technically I could modify Derive.State's state_ui, but that's not supposed
-- to be modified, and it's too late for e.g. the initial environ anyway.
derive_tracks_with_ui :: Transform Derive.Events -> (State.State -> State.State)
    -> String -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_with_ui with transform_ui title tracks =
    derive_blocks_with_ui with transform_ui
        [(UiTest.default_block_name <> " -- " <> title, tracks)]

-- ** derive block variations

-- These are all built on 'derive_block_standard', with various things
-- specialized.

-- | Create multiple blocks, and derive the first one.
derive_blocks :: [UiTest.BlockSpec] -> Derive.Result
derive_blocks = derive_blocks_with_ui id id

derive_blocks_with :: Transform Derive.Events -> [UiTest.BlockSpec]
    -> Derive.Result
derive_blocks_with with = derive_blocks_with_ui with id

derive_blocks_with_ui :: Transform Derive.Events -> (State.State -> State.State)
    -> [UiTest.BlockSpec] -> Derive.Result
derive_blocks_with_ui with transform_ui block_tracks =
    derive_block_with with (transform_ui ui_state) bid
    where (bid : _, ui_state) = mkblocks block_tracks

derive_block :: State.State -> BlockId -> Derive.Result
derive_block = derive_block_with id

-- | Derive a block with the testing environ.
derive_block_with :: Transform Derive.Events -> State.State -> BlockId
    -> Derive.Result
derive_block_with with =
    derive_block_standard default_db mempty mempty
        (with . with_environ default_environ)

-- | Like 'derive_block_with', but exec the StateId.
derive_block_with_m :: Transform Derive.Events -> State.StateId a -> BlockId
    -> Derive.Result
derive_block_with_m with create =
    derive_block_standard default_db mempty mempty
        (with . with_environ default_environ) (UiTest.exec State.empty create)

-- | Derive tracks but with a linear skeleton.  Good for testing note
-- transformers since the default skeleton parsing won't create those.
derive_tracks_linear :: String -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_linear = derive_tracks_with_ui id with_linear

-- | Derive a block in the same way that the app does.
derive_block_standard :: Cmd.InstrumentDb -> Derive.Cache -> Derive.ScoreDamage
    -> Transform Derive.Events -> State.State -> BlockId -> Derive.Result
derive_block_standard inst_db cache damage with ui_state block_id =
    case result of
        Right (Just result, _, _) -> result
        Right (Nothing, _, _) -> error "derive_block_with: Cmd aborted"
        Left err -> error $ "derive_block_with: Cmd error: " ++ show err
    where
    cmd_state = Cmd.initial_state (cmd_config inst_db)
    global_transform = State.config#State.global_transform #$ ui_state
    deriver = with $ Call.Block.eval_root_block global_transform block_id
    (_cstate, _midi_msgs, _logs, result) = Cmd.run_id ui_state cmd_state $
        Derive.extract_result True <$> PlayUtil.run cache damage deriver

-- | Derive the results of a "Cmd.Repl.LDebug".dump_block.
derive_dump :: [MidiInst.SynthDesc] -> Simple.State -> BlockId -> Derive.Result
derive_dump synths dump@(_, _, aliases, _) =
    derive_block_with (with_inst_db_aliases aliases synths) state
    where
    state = UiTest.eval State.empty (Simple.convert_state dump)

perform_dump :: [MidiInst.SynthDesc] -> Simple.State -> Derive.Result
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform_dump synths (_, midi, aliases, _) =
    perform lookup config . Derive.r_events
    where
    lookup = synth_to_convert_lookup aliases synths
    config = Simple.midi_config midi

-- * misc

derive :: State.State -> Derive.NoteDeriver -> Derive.Result
derive ui_state deriver = Derive.extract_result True $
    Derive.derive (default_constant ui_state mempty mempty)
        default_environ deriver

-- | Config to initialize the Cmd.State, without the instrument db.
cmd_config :: Cmd.InstrumentDb -> Cmd.Config
cmd_config inst_db = Cmd.Config
    { Cmd.state_app_dir = "."
    , Cmd.state_midi_interface = Unsafe.unsafePerformIO StubMidi.interface
    , Cmd.state_rdev_map = mempty
    , Cmd.state_wdev_map = mempty
    , Cmd.state_instrument_db = inst_db
    , Cmd.state_library = Call.All.library
    , Cmd.state_lookup_scale = Cmd.LookupScale $
        \scale_id -> Map.lookup scale_id Scale.All.scales
    , Cmd.state_highlight_colors = Config.highlight_colors
    }

-- | Turn BlockSpecs into a state.
mkblocks :: [UiTest.BlockSpec] -> ([BlockId], State.State)
mkblocks block_tracks = UiTest.run State.empty $
    set_defaults >> UiTest.mkblocks block_tracks

-- * transform ui

-- | Set the skeleton of the tracks to be linear, i.e. each track is the child
-- of the one to the left.  This overrides the default behaviour of figuring
-- out a skeleton by making note tracks start their own branches.
with_linear :: State.State -> State.State
with_linear = with_linear_block UiTest.default_block_id

with_linear_block :: BlockId -> State.State -> State.State
with_linear_block block_id state = with_skel_block block_id skel state
    where
    -- Start at 1 to exclude the ruler.
    skel = [(x, y) | (x, Just y) <- Seq.zip_next [1..ntracks-1]]
    ntracks = length $ maybe [] Block.block_tracks $
        Map.lookup block_id (State.state_blocks state)

with_skel :: [Skeleton.Edge] -> State.State -> State.State
with_skel = with_skel_block UiTest.default_block_id

with_skel_block :: BlockId -> [Skeleton.Edge] -> State.State -> State.State
with_skel_block block_id skel state = UiTest.exec state $
    State.set_skeleton block_id (Skeleton.make skel)

set_ruler :: Ruler.Ruler -> State.State -> State.State
set_ruler ruler = State.rulers %= Map.insert UiTest.default_ruler_id ruler

with_tsigs :: [TrackId] -> State.State -> State.State
with_tsigs = with_tsig_sources . map (flip (,) Nothing)

with_tsig_tracknums :: [TrackNum] -> State.State -> State.State
with_tsig_tracknums = with_tsigs . map UiTest.mk_tid

with_tsig_sources :: [(TrackId, Maybe Track.RenderSource)] -> State.State
    -> State.State
with_tsig_sources track_ids = State.tracks %= Map.mapWithKey enable
    where
    enable track_id track = case lookup track_id track_ids of
        Just source -> track { Track.track_render =
            Track.RenderConfig (Track.Line source) Color.blue }
        Nothing -> track

with_transform :: Text -> State.State -> State.State
with_transform = (State.config#State.global_transform #=)

-- * transform derive

with_key :: Text -> Derive.Deriver a -> Derive.Deriver a
with_key key = Derive.with_val Environ.key key

with_environ :: TrackLang.Environ -> Derive.Deriver a -> Derive.Deriver a
with_environ env = Internal.local $ \st -> st { Derive.state_environ = env }

-- | Set UI state defaults that every derivation should have.
set_defaults :: State.M m => m ()
set_defaults = State.modify set_default_midi_config

set_default_midi_config :: State.State -> State.State
set_default_midi_config = State.config#State.midi #= default_midi_config

-- ** defaults

default_lookup_scale :: Derive.LookupScale
default_lookup_scale scale_id = Map.lookup scale_id Scale.All.scales

default_library :: Derive.Library
default_library = Call.All.library

default_environ :: TrackLang.Environ
default_environ = TrackLang.make_environ
    -- tests are easier to write and read with integral interpolation
    [ (Environ.srate, TrackLang.num 1)
    , (Environ.scale,
        TrackLang.VSymbol (TrackLang.scale_id_to_sym Twelve.scale_id))
    , (Environ.attributes, TrackLang.VAttributes Score.no_attrs)
    ]

-- ** extract

-- *** log msgs

trace_logs :: [Log.Msg] -> a -> a
trace_logs logs = Log.trace_logs (filter interesting_log logs)

-- | Filter out low-priority logs and trace them.
trace_low_prio :: [Log.Msg] -> [Log.Msg]
trace_low_prio msgs = Log.trace_logs low high
    where (high, low) = List.partition interesting_log msgs

-- | Tests generally shouldn't depend on logs below a certain priority since
-- those don't indicate anything interesting.
interesting_log :: Log.Msg -> Bool
interesting_log = (>=Log.Warn) . Log.msg_priority

-- It's a hack, but the cache logs are annoying.
-- Srcpos would be better but it doesn't exist in ghci.
-- cache = (== Just (Just "cached_generator")) . fmap SrcPos.srcpos_func
--     . Log.msg_caller
cache_msg :: Log.Msg -> Bool
cache_msg msg = any (`List.isInfixOf` s) ["using cache", "rederived generator"]
    where s = Log.msg_string msg

quiet_filter_logs :: [Log.Msg] -> [Log.Msg]
quiet_filter_logs = filter ((>=Log.Warn) . Log.msg_priority)

-- ** extract

extract :: (Score.Event -> a) -> Derive.Result -> ([a], [String])
extract e_event = extract_levents e_event . Derive.r_events

extract_events :: (Score.Event -> a) -> Derive.Result -> [a]
extract_events e_event result = Log.trace_logs logs (map e_event events)
    where (events, logs) = r_split result

extract_levents :: (Score.Event -> a) -> Derive.Events -> ([a], [String])
extract_levents e_event levents =
    (map e_event events, map show_log (filter interesting_log logs))
    where (events, logs) = LEvent.partition levents

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

r_logs :: Derive.Result -> [Log.Msg]
r_logs = snd . r_split

stack_to_ui :: Stack.Stack -> [Text]
stack_to_ui = map Stack.unparse_ui_frame . Stack.to_ui

r_log_strings :: Derive.Result -> [String]
r_log_strings = snd . extract id

e_event :: Score.Event -> (RealTime, RealTime, String)
e_event e = (Score.event_start e, Score.event_duration e, untxt $ Score.event_text e)

e_start_dur :: Score.Event -> (RealTime, RealTime)
e_start_dur e = (Score.event_start e, Score.event_duration e)

e_everything :: Score.Event -> (RealTime, RealTime, String, Text, [Text])
e_everything e =
    ( Score.event_start e
    , Score.event_duration e
    , untxt $ Score.event_text e
    , e_inst e
    , Score.attrs_list (Score.event_attributes e)
    )

e_inst :: Score.Event -> Text
e_inst = Score.inst_name . Score.event_instrument

e_control :: Score.Control -> Score.Event -> [(RealTime, Signal.Y)]
e_control cont event = maybe [] (Signal.unsignal . Score.typed_val) $
    Map.lookup cont (Score.event_transformed_controls event)

e_dyn :: Score.Event -> [(RealTime, Signal.Y)]
e_dyn = e_control Score.c_dynamic

e_nns :: Score.Event -> [(RealTime, Pitch.NoteNumber)]
e_nns e = signal_to_nn $
    PitchSignal.apply_controls (Score.event_environ e)
        (Score.event_transformed_controls e)
        (Score.event_transformed_pitch e)

signal_to_nn :: PitchSignal.Signal -> [(RealTime, Pitch.NoteNumber)]
signal_to_nn psig
    | not (null errs) =
        error $ "DeriveTest.signal_to_nn: errors flattening signal: "
            ++ show errs
    | otherwise = map (second Pitch.NoteNumber) (Signal.unsignal sig)
    where (sig, errs) = PitchSignal.to_nn psig

e_pitch :: Score.Event -> String
e_pitch e = maybe "?" (untxt . Pitch.note_text) (Score.initial_note e)

-- | (start, dur, pitch), the melodic essentials of a note.
e_note :: Score.Event -> (RealTime, RealTime, String)
e_note e = (Score.event_start e, Score.event_duration e, e_pitch e)

e_attributes :: Score.Event -> String
e_attributes = untxt . ShowVal.show_val . Score.event_attributes

e_environ :: (String -> Bool) -> Score.Event -> [(TrackLang.ValName, String)]
e_environ f event =
    [ (TrackLang.Symbol k, untxt $ ShowVal.show_val v)
    | (TrackLang.Symbol k, v)
        <- TrackLang.environ_to_list (Score.event_environ event)
    , f (untxt k)
    ]

e_environ_val :: TrackLang.Typecheck a => TrackLang.ValName -> Score.Event
    -> Maybe a
e_environ_val name = TrackLang.maybe_val name . Score.event_environ

e_tsigs :: Derive.Result -> [((BlockId, TrackId), [(Signal.X, Signal.Y)])]
e_tsigs =
    filter (not . null . snd) . Map.toList . Map.map tsig
        . Derive.r_track_signals
    where tsig t = Signal.unsignal $ Track.ts_signal t

e_tsig_logs :: Derive.Result -> [String]
e_tsig_logs = filter ("Track signal: " `List.isPrefixOf`) . map show_log
    . LEvent.logs_of . Derive.r_events

-- ** extract log msgs

show_log_stack :: Log.Msg -> String
show_log_stack msg = show_stack (Log.msg_stack msg) ++ ": " ++ show_log msg

show_stack :: Maybe Stack.Stack -> String
show_stack Nothing = "<nothing>"
show_stack (Just stack)
    | null ui = "<no stack>"
    -- This uses ': ' so 'x: *' works regardless of where in the stack x is.
    | otherwise = Seq.join ": " (map Stack.unparse_ui_frame_ ui)
    where ui = Stack.to_ui stack

show_log :: Log.Msg -> String
show_log = Log.msg_string

-- ** extract midi msgs

note_on_vel :: [Midi.WriteMessage] -> [(Integer, Midi.Key, Midi.Velocity)]
note_on_vel msgs =
    [ (ts, nn, vel)
    | (ts, Midi.ChannelMessage _ (Midi.NoteOn nn vel)) <- extract_midi msgs
    ]

note_on :: [Midi.WriteMessage] -> [Midi.Key]
note_on msgs = [nn | (_, nn, _) <- note_on_vel msgs]

midi_channel :: [Midi.WriteMessage] -> [(Midi.Channel, Midi.ChannelMessage)]
midi_channel midi =
    [ (chan, msg)
    | Midi.ChannelMessage chan msg <- map Midi.wmsg_msg (interesting_midi midi)
    ]

extract_midi :: [Midi.WriteMessage] -> [(Integer, Midi.Message)]
extract_midi midi =
    [ (RealTime.to_milliseconds ts, msg)
    | Midi.WriteMessage _ ts msg <- interesting_midi midi
    ]

-- | Filter out boring msgs that I don't want tests to rely on.
interesting_midi :: [Midi.WriteMessage] -> [Midi.WriteMessage]
interesting_midi = filter $ \wmsg -> case Midi.wmsg_msg wmsg of
    Midi.ChannelMessage _ (Midi.PitchBend 0) -> False
    _ -> True

-- ** ui state

e_state :: Derive.Result -> State.State
e_state = Derive.state_ui . Derive.state_constant . Derive.r_state

-- * call

passed_args :: Text -> [TrackLang.Val] -> Derive.PassedArgs derived
passed_args name vals = Derive.PassedArgs
    { Derive.passed_vals = vals
    , Derive.passed_call_name = name
    , Derive.passed_info = Derive.dummy_call_info 0 1 "DeriveTest"
    }

c_note :: ScoreTime -> ScoreTime -> Derive.NoteDeriver
c_note s_start dur = do
    start <- Derive.real s_start
    end <- Derive.real (s_start + dur)
    inst <- Derive.get_val Environ.instrument
    environ <- Internal.get_environ
    st <- Derive.gets Derive.state_dynamic
    let controls = Derive.state_controls st
        pitch_sig = Derive.state_pitch st
    return [LEvent.Event $ Score.empty_event
        { Score.event_start = start
        , Score.event_duration = end - start
        , Score.event_text = "evt"
        , Score.event_untransformed_controls = controls
        , Score.event_untransformed_pitch = pitch_sig
        , Score.event_untransformed_pitches = Derive.state_pitches st
        , Score.event_instrument = inst
        , Score.event_environ = environ
        }]

-- | Not supposed to do this in general, but it's ok for tests.
modify_dynamic :: (Derive.Dynamic -> Derive.Dynamic) -> Derive.Deriver ()
modify_dynamic f = Derive.modify $ \st ->
    st { Derive.state_dynamic = f (Derive.state_dynamic st) }

-- | Really not supposed to do this, but should be *mostly* ok for tests.
-- It's only mostly ok because some values are baked in to e.g. lookup
-- functions.
modify_constant :: (Derive.Constant -> Derive.Constant)
    -> Derive.Deriver a -> Derive.Deriver a
modify_constant f deriver = do
    Derive.modify $ \st ->
        st { Derive.state_constant = f (Derive.state_constant st) }
    deriver

-- * scale

with_scale :: Scale.Scale -> Derive.Deriver a -> Derive.Deriver a
with_scale scale = modify_constant $ \st ->
    st { Derive.state_lookup_scale = \scale_id -> Map.lookup scale_id
        (Map.insert (Scale.scale_id scale) scale Scale.All.scales) }

mkscale :: Pitch.ScaleId -> [(Text, Pitch.NoteNumber)] -> Scale.Scale
mkscale scale_id notes =
    Scales.make_scale scale_id dmap "pattern" "simple test scale"
    where
    dmap = Scales.degree_map 8 0 0 (map (Pitch.Note . fst) notes)
        (map snd notes)

-- * inst

-- Derive and perform with instrument db.

-- | Derive with a bit of the real instrument db.  Useful for testing
-- instrument calls.
with_inst_db_aliases :: Simple.Aliases -> [Cmd.SynthDesc] -> Derive.Deriver a
    -> Derive.Deriver a
with_inst_db_aliases aliases synth_descs = modify_constant $ \const -> const
    { Derive.state_lookup_instrument =
        synth_to_derive_instrument aliases synth_descs
    }

with_inst_db :: [Cmd.SynthDesc] -> Derive.Deriver a -> Derive.Deriver a
with_inst_db = with_inst_db_aliases mempty

synth_to_derive_instrument :: Simple.Aliases -> [Cmd.SynthDesc]
    -> Score.Instrument -> Maybe Derive.Instrument
synth_to_derive_instrument aliases synth_descs inst =
    fmap Cmd.derive_instrument $ Instrument.Db.db_lookup db inst
    where db = synth_to_db aliases synth_descs

synth_to_convert_lookup :: Simple.Aliases -> [MidiInst.SynthDesc]
    -> Convert.Lookup
synth_to_convert_lookup aliases = make_convert_lookup . synth_to_db aliases

synth_to_db :: Simple.Aliases -> [Cmd.SynthDesc] -> Cmd.InstrumentDb
synth_to_db aliases synth_descs =
    with_aliases $ trace_logs (map (Log.msg Log.Warn Nothing) warns) $
        Instrument.Db.db midi_db
    where
    (midi_db, warns) = MidiDb.midi_db synth_descs
    with_aliases = Instrument.Db.with_aliases
        (Map.fromList $ map (Score.Instrument *** Score.Instrument) aliases)

-- ** older patch creating functions

make_convert_lookup :: Cmd.InstrumentDb -> Convert.Lookup
make_convert_lookup midi_db = Convert.Lookup
    { Convert.lookup_scale = default_lookup_scale
    , Convert.lookup_inst = lookup_inst
    , Convert.lookup_patch = lookup_patch
    , Convert.lookup_default_controls = const mempty
    }
    where
    lookup_inst = Instrument.Db.db_lookup_midi midi_db
    lookup_patch = fmap extract . Instrument.Db.db_lookup midi_db
    extract info = (MidiDb.info_patch info,
        Cmd.inst_postproc $ MidiDb.info_code info)

make_db :: [(Text, [Instrument.Patch])] -> Cmd.InstrumentDb
make_db synth_patches = Instrument.Db.db midi_db
    where
    midi_db = fst $ MidiDb.midi_db $ concatMap make synth_patches
    make (synth, patches) = MidiInst.make
        (MidiInst.softsynth synth "Test Synth" (-2, 2) [])
        { MidiInst.extra_patches =
            map (\p -> (p, MidiInst.empty_code)) patches
        }

lookup_from_insts :: [Score.Instrument] -> Convert.Lookup
lookup_from_insts = make_convert_lookup . make_db . convert
    where
    convert = map (second (map make_patch)) . Seq.keyed_group_on (fst . split)
        . map Score.inst_name
    split name = (pre, Text.drop 1 post)
        where (pre, post) = Text.break (=='/') name

lookup_from_state :: State.State -> Convert.Lookup
lookup_from_state state = lookup_from_insts $
    Seq.drop_dups id $ Map.keys $ State.config#State.midi #$ state

default_convert_lookup :: Convert.Lookup
default_convert_lookup = make_convert_lookup default_db

default_db :: Cmd.InstrumentDb
default_db = Instrument.Db.with_aliases aliases $ make_db
    [("s", map make_patch ["1", "2", "3"])]
    where
    -- Lots of tests use >i and >i1, so let's not break them.
    aliases = Map.fromList $ map (Score.Instrument *** Score.Instrument)
        [("i", "s/1"), ("i1", "s/1"), ("i2", "s/2"), ("i3", "s/3")]

make_patch :: Text -> Instrument.Patch
make_patch name = Instrument.patch $ Instrument.instrument name [] (-2, 2)

default_midi_config :: Instrument.Configs
default_midi_config = UiTest.midi_config [("s/1", [0..2]), ("s/2", [3])]

default_inst_title :: String
default_inst_title = ">s/1"

-- * mkevents

-- | (start, dur, pitch12, controls, inst)
type EventSpec = (RealTime, RealTime, String, Controls, Score.Instrument)
type Controls = [(Score.Control, [(RealTime, Signal.Y)])]
type ControlVals = [(Score.Control, Signal.Y)]

mkevent :: EventSpec -> Score.Event
mkevent = mkevent_scale Twelve.scale

-- | Make an event with a non-twelve scale.
mkevent_scale :: Scale.Scale
    -> (RealTime, RealTime, String, Controls, Score.Instrument)
    -> Score.Event
mkevent_scale scale (start, dur, pitch, controls, inst) = Score.empty_event
    { Score.event_start = start
    , Score.event_duration = dur
    , Score.event_text = txt pitch
    , Score.event_untransformed_controls = mkcontrols controls
    , Score.event_untransformed_pitch =
        PitchSignal.signal [(start, mkpitch scale pitch)]
    , Score.event_stack = fake_stack
    , Score.event_instrument = inst
    }

pitch_signal :: [(RealTime, String)] -> PitchSignal.Signal
pitch_signal = PitchSignal.signal . map (second mkpitch12)

mkcontrols :: Controls -> Score.ControlMap
mkcontrols cs = Map.fromList
    [(c, Score.untyped (Signal.signal sig)) | (c, sig) <- cs]

mkcontrols_const :: ControlVals -> Score.ControlMap
mkcontrols_const cs = mkcontrols [(c, [(0, val)]) | (c, val) <- cs]

mkpitch12 :: String -> PitchSignal.Pitch
mkpitch12 = mkpitch Twelve.scale

mkpitch :: Scale.Scale -> String -> PitchSignal.Pitch
mkpitch scale p = case eval State.empty deriver of
    Left err -> error $ "mkpitch " ++ show p ++ ": " ++ err
    Right pitch -> pitch
    where
    deriver = Derive.with_scale scale $
        Eval.eval_pitch 0 $ TrackLang.call (TrackLang.Symbol (txt p)) []

default_scale :: Scale.Scale
default_scale = Twelve.scale

fake_stack :: Stack.Stack
fake_stack = Stack.from_outermost
    [ Stack.Block (UiTest.bid "fakeblock")
    , Stack.Track (UiTest.tid "faketrack")
    , Stack.Region 42 43
    ]

-- * create misc

make_damage :: String -> TrackNum -> ScoreTime -> ScoreTime
    -> Derive.ScoreDamage
make_damage block tracknum s e = Derive.ScoreDamage
    (Map.singleton (UiTest.mk_tid_name block tracknum) (Ranges.range s e))
    (Set.singleton (UiTest.bid block)) mempty

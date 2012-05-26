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
import qualified Ui.Skeleton as Skeleton
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Derive.Call.All as Call.All
import qualified Derive.Call.Block as Call.Block
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Scale.Util as Scale.Util
import qualified Derive.Score as Score
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
import qualified App.MidiInst as MidiInst
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
        (Left err, _, _logs) -> Left (Pretty.pretty err)
        (Right val, state, logs) -> Right (val, state, logs)
    where
    derive_state = Derive.initial_state default_scope
        default_environ (default_constant ui_state mempty mempty)

extract_run :: (a -> b) -> Either String (a, Derive.State, [Log.Msg])
    -> Either String b
extract_run _ (Left err) = Left err
extract_run f (Right (val, _, msgs)) = Right $ trace_logs msgs (f val)

run_events :: (a -> b)
    -> Either String (Derive.EventStream a, Derive.State, [Log.Msg])
    -> Either String ([b], [String])
run_events f = extract_run $
    first (map f) . second (map show_log . trace_low_prio) . LEvent.partition

default_constant   :: State.State -> Derive.Cache -> Derive.ScoreDamage
    -> Derive.Constant
default_constant ui_state cache damage =
    Derive.initial_constant ui_state default_lookup_scale (const Nothing)
        cache damage

eval :: State.State -> Derive.Deriver a -> Either String a
eval ui_state m = extract_run id (run ui_state m)


-- * perform

type Midi = (Integer, Midi.Message)

perform_block :: [UiTest.TrackSpec] -> ([Midi], [String])
perform_block tracks = perform_blocks [(UiTest.default_block_name, tracks)]

perform_blocks :: [UiTest.BlockSpec] -> ([Midi], [String])
perform_blocks blocks = (mmsgs, map show_log (filter interesting_log logs))
    where
    (_, mmsgs, logs) = perform default_convert_lookup default_midi_config
        (Derive.r_events result)
    result = derive_blocks blocks

perform :: Convert.Lookup -> Instrument.Config -> Derive.Events
    -> ([Perform.Event], [Midi], [Log.Msg])
perform lookup midi_config events =
    (fst (LEvent.partition perf_events), mmsgs, filter interesting_log logs)
    where
    (perf_events, perf) = perform_stream lookup midi_config events
    (mmsgs, logs) = LEvent.partition perf

perform_defaults :: Derive.Events -> ([Perform.Event], [Midi], [Log.Msg])
perform_defaults = perform default_convert_lookup default_midi_config

perform_stream :: Convert.Lookup -> Instrument.Config -> Derive.Events
    -> ([LEvent.LEvent Perform.Event], [LEvent.LEvent Midi])
perform_stream lookup midi_config events = (perf_events, mmsgs)
    where
    perf_events = Convert.convert lookup events
    (midi, _) = Perform.perform Perform.initial_state midi_config perf_events
    mmsgs = map (fmap extract_m) midi
    extract_m wmsg =
        (RealTime.to_milliseconds (Midi.wmsg_ts wmsg), Midi.wmsg_msg wmsg)

-- * derive

derive_tracks :: [UiTest.TrackSpec] -> Derive.Result
derive_tracks = derive_tracks_with id

derive_tracks_with :: Transform Derive.Events -> [UiTest.TrackSpec]
    -> Derive.Result
derive_tracks_with with = derive_tracks_with_ui with id

-- | Variant that lets you modify both the Deriver state and the UI state.
-- Technically I could modify Derive.State's state_ui, but that's not supposed
-- to be modified, and it's too late for e.g. the initial environ anyway.
derive_tracks_with_ui :: Transform Derive.Events -> TransformUi
    -> [UiTest.TrackSpec] -> Derive.Result
derive_tracks_with_ui with transform_ui tracks = derive_blocks_with_ui
    with transform_ui [(UiTest.default_block_name, tracks)]

-- | Create multiple blocks, and derive the first one.
derive_blocks :: [UiTest.BlockSpec] -> Derive.Result
derive_blocks = derive_blocks_with_ui id id

derive_blocks_with :: Transform Derive.Events -> [UiTest.BlockSpec]
    -> Derive.Result
derive_blocks_with with = derive_blocks_with_ui with id

derive_blocks_with_ui :: Transform Derive.Events -> TransformUi
    -> [UiTest.BlockSpec] -> Derive.Result
derive_blocks_with_ui with transform_ui block_tracks =
    derive_block_with with (transform_ui ui_state) bid
    where
    (bid : _, ui_state) = UiTest.run State.empty $
        set_defaults >> UiTest.mkblocks block_tracks

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

-- ** derive utils

-- | Derive tracks but with a linear skeleton.  Good for testing note
-- transformers since the default skeleton parsing won't create those.
linear_derive_tracks :: Transform Derive.Events -> [UiTest.TrackSpec]
    -> Derive.Result
linear_derive_tracks with tracks =
    derive_tracks_with_ui with (linear_skel tracks) tracks

linear_skel :: [UiTest.TrackSpec] -> State.State -> State.State
linear_skel tracks =
    set_skel [(x, y) | (x, Just y) <- Seq.zip_next [1..length tracks]]

set_skel :: [Skeleton.Edge] -> State.State -> State.State
set_skel skel state = UiTest.exec state $
    State.set_skeleton UiTest.default_block_id (Skeleton.make skel)

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
with_instrument state =
    state { State.state_config = set (State.state_config state) }
    where
    set config = config
        { State.config_midi = default_midi_config
        , State.config_default = (State.config_default config)
            { State.default_instrument = Just (Score.Instrument "s/1")
            }
        }

-- | Set UI state defaults that every derivation should have.
set_defaults :: (State.M m) => m ()
set_defaults = do
    State.modify with_instrument
    State.modify_default $ \st -> st
        { State.default_key = Just (Pitch.Key "c-maj") }

default_lookup_scale :: Derive.LookupScale
default_lookup_scale scale_id = Map.lookup scale_id Scale.All.scales

default_scope :: Derive.Scope
default_scope = Call.All.scope

default_environ :: TrackLang.Environ
default_environ = Map.fromList
    -- tests are easier to write and read with integral interpolation
    [ (TrackLang.v_srate, TrackLang.num 1)
    , (TrackLang.v_scale, TrackLang.VScaleId Twelve.scale_id)
    , (TrackLang.v_attributes, TrackLang.VAttributes Score.no_attrs)
    , (TrackLang.v_key, TrackLang.VString "c-maj")
    ]

-- ** extract

-- *** log msgs

trace_logs :: [Log.Msg] -> a -> a
trace_logs logs = Log.trace_logs (filter interesting_log logs)

trace_low_prio :: [Log.Msg] -> [Log.Msg]
trace_low_prio msgs = Log.trace_logs low high
    where (high, low) = List.partition interesting_log msgs

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

r_logs :: Derive.Result -> [Log.Msg]
r_logs = snd . r_split

r_log_strings :: Derive.Result -> [String]
r_log_strings = snd . extract id

e_event :: Score.Event -> (RealTime, RealTime, String)
e_event e = (Score.event_start e, Score.event_duration e, Score.event_string e)

e_everything :: Score.Event
    -> (RealTime, RealTime, String, Maybe String, [String])
e_everything e =
    ( Score.event_start e
    , Score.event_duration e
    , Score.event_string e
    , fmap Score.inst_name (Score.event_instrument e)
    , Score.attrs_list (Score.event_attributes e)
    )

e_control :: String -> Score.Event -> Maybe [(RealTime, Signal.Y)]
e_control cont event = fmap (Signal.unsignal . Score.typed_val) $
    Map.lookup (Score.Control cont) (Score.event_controls event)

e_pitch :: Score.Event -> [(RealTime, Pitch.NoteNumber)]
e_pitch e = signal_to_nn $
    PitchSignal.apply_controls (Score.event_controls e) (Score.event_pitch e)

signal_to_nn :: PitchSignal.Signal -> [(RealTime, Pitch.NoteNumber)]
signal_to_nn psig
    | not (null errs) =
        error $ "DeriveTest.signal_to_nn: errors flattening signal: "
            ++ show errs
    | otherwise = map (second Pitch.NoteNumber) (Signal.unsignal sig)
    where (sig, errs) = PitchSignal.to_nn psig

e_pitch_err :: Score.Event -> ([(RealTime, Pitch.NoteNumber)], [String])
e_pitch_err e = (map (second Pitch.NoteNumber) (Signal.unsignal sig),
        map (\(PitchSignal.PitchError err) -> err) errs)
    where
    (sig, errs) = PitchSignal.to_nn $ PitchSignal.apply_controls
        (Score.event_controls e) (Score.event_pitch e)

e_twelve :: Score.Event -> String
e_twelve e = Maybe.fromMaybe "?" $ do
    nn <- Score.initial_nn e
    note <- Twelve.input_to_note Nothing (to_input nn)
    return (Pitch.note_text note)
    where to_input (Pitch.NoteNumber n) = Pitch.InputKey n

-- | (start, dur, pitch), the melodic essentials of a note.
e_note :: Score.Event -> (RealTime, RealTime, Pitch.NoteNumber)
e_note e = (Score.event_start e, Score.event_duration e,
    Maybe.fromMaybe (-1) (Score.initial_nn e))

e_note2 :: Score.Event -> (RealTime, RealTime, String)
e_note2 e = (Score.event_start e, Score.event_duration e, e_twelve e)

-- ** extract log msgs

show_log_stack :: Log.Msg -> String
show_log_stack msg = show_stack (Log.msg_stack msg) ++ ": " ++ show_log msg

show_stack :: Maybe Log.Stack -> String
show_stack Nothing = "<nothing>"
show_stack (Just stack)
    | null ui = "<no stack>"
    -- This uses ': ' so 'x: *' works regardless of where in the stack x is.
    | otherwise = Seq.join ": " (map Stack.unparse_ui_frame_ ui)
    where ui = Stack.to_ui (Stack.from_strings stack)

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
passed_args call vals = Derive.PassedArgs vals
    (TrackLang.Symbol call) (Derive.dummy_call_info "DeriveTest")

c_note :: ScoreTime -> ScoreTime -> Derive.EventDeriver
c_note s_start dur = do
    start <- Derive.real s_start
    end <- Derive.real (s_start + dur)
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

-- | Really not supposed to do this, but should be *mostly* ok for tests, but
-- beware of values baked in to e.g. lookup functions.  This is why modifying
-- UI state is not a good idea.
modify_constant :: (Derive.Constant -> Derive.Constant) -> Derive.Deriver ()
modify_constant f = Derive.modify $ \st ->
    st { Derive.state_constant = f (Derive.state_constant st) }

-- * scale

with_scale :: Scale.Scale -> Derive.Deriver a -> Derive.Deriver a
with_scale scale deriver = do
    modify_constant $ \st ->
        st { Derive.state_lookup_scale = \scale_id -> Map.lookup scale_id
            (Map.insert (Scale.scale_id scale) scale Scale.All.scales) }
    deriver

mkscale :: String -> [(String, Pitch.NoteNumber)] -> Scale.Scale
mkscale name notes = Scale.Scale
    { Scale.scale_id = Pitch.ScaleId name
    , Scale.scale_pattern = "test"
    , Scale.scale_map = Scale.Util.make_scale_map scale_map
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Scale.Util.standard_transposers
    , Scale.scale_transpose = Scale.Util.transpose scale_map 5
    , Scale.scale_enharmonics = Scale.Util.no_enharmonics
    , Scale.scale_note_to_call = Scale.Util.note_to_call scale_map
    , Scale.scale_input_to_note = Scale.Util.input_to_note scale_map
    , Scale.scale_input_to_nn = Scale.Util.input_to_nn scale_map
    }
    where
    scale_map = Scale.Util.scale_map (map (Pitch.Note . fst) notes)
        (take (length notes) inputs) (map snd notes)
    inputs = [Scale.Util.i_c + Pitch.InputKey n | n <- [0..]]

-- * inst

make_convert_lookup :: Cmd.InstrumentDb -> Convert.Lookup
make_convert_lookup midi_db = Convert.Lookup
    default_lookup_scale lookup_inst lookup_patch
    where
    lookup_inst = Instrument.Db.db_lookup_midi midi_db
    lookup_patch = fmap MidiDb.info_patch . Instrument.Db.db_lookup midi_db

make_db :: [(String, [Instrument.Patch])] -> Cmd.InstrumentDb
make_db synth_patches = Instrument.Db.db midi_db
    where
    midi_db = fst $ MidiDb.midi_db $ concatMap make synth_patches
    make (synth, patches) = MidiInst.make (MidiInst.softsynth synth (-2, 2) [])
        { MidiInst.extra_patches =
            map (\p -> (p, MidiInst.empty_code)) patches }

lookup_from_insts :: [Score.Instrument] -> Convert.Lookup
lookup_from_insts = make_convert_lookup . make_db . convert
    where
    convert = map (second (map make_patch)) . Seq.keyed_group_on (fst . split)
        . map Score.inst_name
    split name = (pre, drop 1 post)
        where (pre, post) = break (=='/') name

lookup_from_state :: State.State -> Convert.Lookup
lookup_from_state state = lookup_from_insts $
    Seq.drop_dups id $ Map.keys $ Instrument.config_alloc $
    State.config#State.midi #$ state

default_convert_lookup :: Convert.Lookup
default_convert_lookup = make_convert_lookup default_db

default_db :: Cmd.InstrumentDb
default_db = make_db [("s", map make_patch ["1", "2"])]

make_patch :: String -> Instrument.Patch
make_patch name = Instrument.patch $ Instrument.instrument name [] (-2, 2)

default_midi_config :: Instrument.Config
default_midi_config = UiTest.midi_config [("s/1", [0..2]), ("s/2", [3])]

default_inst_title :: String
default_inst_title = ">s/1"

-- * mkevents

-- | Name will determine the pitch.  It can be a-z.
type EventSpec = (RealTime, RealTime, String,
    [(Score.Control, Signal.Control)], Score.Instrument)

mkevent :: EventSpec -> Score.Event
mkevent (start, dur, text, controls, inst) =
    Score.Event start dur (B.pack text)
        (Map.map Score.untyped (Map.fromList controls))
        (pitch_signal [(start, text)]) fake_stack (Just inst) Score.no_attrs

pitch_signal :: [(RealTime, String)] -> PitchSignal.Signal
pitch_signal = PitchSignal.signal scale . map (second mkpitch)
    where scale = Derive.pitch_signal_scale Twelve.scale


mkpitch :: String -> PitchSignal.Pitch
mkpitch p = PitchSignal.pitch $ \controls ->
    let get c = maybe 0 Score.typed_val $ Map.lookup c controls
        chrom = get Score.c_chromatic
        dia = get Score.c_diatonic
    in maybe (Left (PitchSignal.PitchError $ "no pitch " ++ show p))
        (Right . Pitch.NoteNumber . (+ (chrom + dia*2))) (lookup p pitch_map)
    where
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]

default_scale :: Scale.Scale
default_scale = Twelve.scale

fake_stack :: Stack.Stack
fake_stack = Stack.from_outermost
    [ Stack.Block (UiTest.bid "fakeblock")
    , Stack.Track (UiTest.tid "faketrack")
    , Stack.Region 42 43
    ]

-- TODO integrate into mkevent?
mkcontrols :: [(String, [(RealTime, Signal.Y)])] -> Score.ControlMap
mkcontrols csigs = Map.fromList
    [(Score.Control c, Score.untyped (Signal.signal sig)) | (c, sig) <- csigs]

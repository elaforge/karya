-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for cmd tests.
module Cmd.CmdTest where
import qualified Control.Concurrent.Chan as Chan
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import qualified System.IO.Unsafe as Unsafe

import qualified Util.Debug as Debug
import qualified Util.Log as Log
import qualified Util.Thread as Thread

import qualified Midi.Interface as Interface
import qualified Midi.Midi as Midi
import qualified Ui.Diff as Diff
import qualified Ui.Key as Key
import qualified Ui.Sel as Sel
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.InputNote as InputNote
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Performance as Performance

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.Score as Score
import qualified Derive.Stream as Stream

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified App.Config as Config
import Global
import Types


-- * running cmds

data Result val = Result {
    -- | A Nothing val means it aborted.
    result_val :: Either Text (Maybe val)
    , result_cmd_state :: Cmd.State
    , result_ui_state :: Ui.State
    , result_updates :: [Update.CmdUpdate]
    , result_logs :: [Log.Msg]
    , result_midi :: [Interface.Message]
    }

result_failed :: Result a -> Maybe Text
result_failed = either Just (const Nothing) . result_ok

result_ok :: Result a -> Either Text a
result_ok res = case result_val res of
    Right (Just val) -> Right val
    Right Nothing -> Left "aborted"
    Left err -> Left err

-- | Run cmd with the given tracks.
run_tracks :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_tracks tracks = run (make_tracks tracks) default_cmd_state

-- | Like 'run_tracks', but the ruler only extends to the end of the last
-- event.
run_tracks_ruler :: [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_tracks_ruler tracks = run (make_tracks_ruler tracks) default_cmd_state

-- | Derive the tracks and then run the cmd with the performance available.
run_tracks_with_performance :: [UiTest.TrackSpec] -> Cmd.CmdT IO a
    -> IO (Result a)
run_tracks_with_performance tracks =
    run_with_performance (make_tracks tracks) default_cmd_state

run_with_performance :: Ui.State -> Cmd.State -> Cmd.CmdT IO a
    -> IO (Result a)
run_with_performance ustate cstate cmd = do
    cstate <- update_performance Ui.empty ustate cstate []
    run_io ustate cstate cmd

make_tracks :: [UiTest.TrackSpec] -> Ui.State
make_tracks = snd . UiTest.run_mkview

make_tracks_ruler :: [UiTest.TrackSpec] -> Ui.State
make_tracks_ruler = snd . make
    where
    make tracks = UiTest.run Ui.empty $
        UiTest.mkblock_view (UiTest.default_block_name <> "=ruler", tracks)

-- | Run a cmd and return everything you could possibly be interested in.
run :: Ui.State -> Cmd.State -> Cmd.CmdId a -> Result a
run ustate1 cstate1 cmd = Result val cstate2 ustate2 updates logs midi_msgs
    where
    (cstate2, midi_msgs, logs, result) = Cmd.run_id ustate1 cstate1 cmd
    (val, ustate2, updates) = case result of
        Left err -> (Left (pretty err), ustate1, [])
        Right (v, ustate2, updates) -> (Right v, ustate2, updates)

run_io :: Ui.State -> Cmd.State -> Cmd.CmdT IO a -> IO (Result a)
run_io ustate1 cstate1 cmd = do
    (cstate2, midi_msgs, logs, result) <-
        Cmd.run Nothing ustate1 cstate1 (Just <$> cmd)
    let (val, ustate2, updates) = case result of
            Left err -> (Left (pretty err), ustate1, [])
            Right (v, ustate2, updates) -> (Right v, ustate2, updates)
    return $ Result val cstate2 ustate2 updates logs midi_msgs

run_ui :: Ui.State -> Cmd.CmdId a -> Result a
run_ui ustate = run ustate default_cmd_state

run_ui_io :: Ui.State -> Cmd.CmdT IO a -> IO (Result a)
run_ui_io ustate = run_io ustate default_cmd_state

-- | Run a Cmd and return just the value.
eval :: Ui.State -> Cmd.State -> Cmd.CmdId a -> a
eval ustate cstate cmd = case result_val (run ustate cstate cmd) of
    Left err -> errorStack $ "eval got StateError: " <> showt err
    Right Nothing -> errorStack "eval: cmd aborted"
    Right (Just val) -> val

-- | Like 'run', but with a selection on the given track at 0 and note
-- duration set to what will be a ScoreTime 1 with the ruler supplied by
-- UiTest.
run_sel :: TrackNum -> [UiTest.TrackSpec] -> Cmd.CmdId a -> Result a
run_sel tracknum track_specs cmd = run_tracks track_specs $ do
    -- Add one because UiTest inserts a rule at track 0.
    set_sel (tracknum+1) 0 (tracknum+1) 0
    cmd

run_again :: Result a -> Cmd.CmdId b -> Result b
run_again res = run (result_ui_state res) (result_cmd_state res)

-- | Update performances after running a Cmd and getting its Result.
update_perf :: Ui.State -> Result val -> IO (Result val)
update_perf ui_from res = do
    cmd_state <- update_performance ui_from (result_ui_state res)
        (result_cmd_state res) (result_updates res)
    return $ res { result_cmd_state = cmd_state }

-- | Run a DeriveTest extractor on a CmdTest Result.
extract_derive :: (Score.Event -> a) -> Result _a -> ([a], [Text])
extract_derive ex = DeriveTest.extract ex . extract_derive_result

-- | Reconstruct a Derive.Result from the root performance, or throw an
-- exception if there is a problem getting it.
extract_derive_result :: Result a -> Derive.Result
extract_derive_result res =
    maybe (eval (result_ui_state res) (result_cmd_state res) mkres)
        (errorStack . (msg<>)) (result_failed res)
    where
    msg = "extract_derive_result: cmd failed so result is probably not right: "
    mkres = do
        Cmd.Performance cache events logs _logs_written track_dyn integrated
            _damage warps tsigs _ui_state <- Perf.get_root
        let stream = Stream.merge_logs logs $
                Stream.from_sorted_events (Vector.toList events)
        return $ Derive.Result stream cache warps tsigs track_dyn integrated
            (errorStack "can't fake a Derive.State for an extracted Result")

-- | Update the performances based on the UI state change and updates.  This
-- manually runs that part of the responder, and is needed for tests that rely
-- on the performances.
update_performance :: Ui.State -> Ui.State -> Cmd.State
    -> [Update.CmdUpdate] -> IO Cmd.State
update_performance ui_from ui_to cmd_state cmd_updates = do
    let (ui_updates, _) = Diff.diff cmd_updates ui_from ui_to
    chan <- Chan.newChan
    let damage = Diff.derive_diff ui_from ui_to ui_updates
    cstate <- Performance.update_performance
        (\bid status -> Chan.writeChan chan (bid, status))
        ui_to (set_immediate cmd_state) damage
    -- This will delay until the perform thread has derived enough of the
    -- performance to hand over.
    (block_id, perf) <- read_perf chan
    let insert st = st { Cmd.state_performance = Map.insert block_id perf
            (Cmd.state_performance st) }
    return $ cstate { Cmd.state_play = insert (Cmd.state_play cstate) }
    where
    -- Get rid of the derive wait to avoid making tests slow.
    set_immediate state = state
        { Cmd.state_derive_immediately = Map.keysSet (Ui.state_blocks ui_to)
        }
    read_perf chan = do
        (block_id, status) <- Chan.readChan chan
        case status of
            Msg.DeriveComplete perf _ -> return (block_id, perf)
            _ -> read_perf chan

-- | Run several cmds, threading the state through.  The first cmd that fails
-- aborts the whole operation.
thread :: Ui.State -> Cmd.State -> [Cmd.CmdId a]
    -> Either Text (Ui.State, Cmd.State)
thread ustate cstate cmds = foldl f (Right (ustate, cstate)) cmds
    where
    f (Right (ustate, cstate)) cmd = case run ustate cstate cmd of
        Result (Right _) cstate2 ustate2 _ logs _ ->
            Log.trace_logs logs $ Right (ustate2, cstate2)
        Result (Left err) _ _ _ _ _ -> Left (showt err)
    f (Left err) _ = Left err

-- | Make some tracks and call 'thread'.
thread_tracks :: [UiTest.TrackSpec] -> (Cmd.State -> Cmd.State)
    -> [Cmd.CmdId a] -> Either Text (Ui.State, Cmd.State)
thread_tracks tracks modify_cmd_state cmds =
    thread ustate (modify_cmd_state default_cmd_state) cmds
    where (_, ustate) = UiTest.run_mkview tracks

default_cmd_state :: Cmd.State
default_cmd_state = (Cmd.initial_state cmd_config)
    { Cmd.state_focused_view = Just UiTest.default_view_id
    , Cmd.state_edit = default_edit_state
    , Cmd.state_play = default_play_state
    }

cmd_config :: Cmd.Config
cmd_config = DeriveTest.cmd_config UiTest.default_db

default_play_state :: Cmd.PlayState
default_play_state =
    Cmd.initial_play_state { Cmd.state_play_step = UiTest.step1 }

default_edit_state :: Cmd.EditState
default_edit_state = Cmd.initial_edit_state
    { Cmd.state_time_step = UiTest.step1
    , Cmd.state_note_duration = UiTest.step1
    }

-- ** cmds

set_sel :: Cmd.M m => Types.TrackNum -> ScoreTime -> Types.TrackNum
    -> ScoreTime -> m ()
set_sel = set_sel_on UiTest.default_view_id

set_sel_on :: Cmd.M m => ViewId -> Types.TrackNum -> ScoreTime
    -> Types.TrackNum -> ScoreTime -> m ()
set_sel_on view_id start_track start_pos cur_track cur_pos = do
    Cmd.modify $ \st -> st { Cmd.state_focused_view = Just view_id }
    Ui.set_selection view_id Config.insert_selnum (Just sel)
    where
    sel = Sel.Selection
        { start_track = start_track
        , start_pos = start_pos
        , cur_track = cur_track
        , cur_pos = cur_pos
        , orientation = Sel.Positive
        }

set_point_sel :: Ui.M m => Types.TrackNum -> ScoreTime -> m Cmd.Status
set_point_sel tracknum pos = do
    set_point_sel_block UiTest.default_block_name tracknum pos
    return Cmd.Done

-- | Set a point selection on the default view of the given block name.
set_point_sel_block :: Ui.M m => Text -> Types.TrackNum -> ScoreTime -> m ()
set_point_sel_block block_name tracknum pos =
    Ui.set_selection view_id Config.insert_selnum
        (Just (Sel.point tracknum pos Sel.Positive))
    where view_id = UiTest.mk_vid_name block_name

select_all :: Cmd.M m => m ()
select_all = do
    tracks <- Ui.track_count UiTest.default_block_id
    end <- Ui.block_end UiTest.default_block_id
    set_sel_on UiTest.default_view_id 1 0 (tracks - 1) end


-- * extractors

-- | The output of the 'extract' family of functions:
-- Either error (val, [log])
type Extracted val = Either Text (val, [Text])

-- | Run this on either 'extract' or 'extract_state' when you don't care about
-- the logs.
trace_logs :: Extracted a -> Either Text a
trace_logs res = case res of
    Right (b, logs) -> (if null logs then id else trace logs) (Right b)
    Left a -> Left a
    where
    trace = Debug.trace_str . Text.strip . Text.unlines . ("\tlogged:":)

e_logs :: Result a -> [Text]
e_logs = map DeriveTest.show_log . DeriveTest.trace_low_prio . result_logs

e_performance :: BlockId -> Result a -> Maybe Cmd.Performance
e_performance block_id = Map.lookup block_id . Cmd.state_performance
    . Cmd.state_play . result_cmd_state

e_events :: BlockId -> Result a -> [Score.Event]
e_events block_id = maybe [] (Vector.toList . Cmd.perf_events)
    . e_performance block_id

e_midi :: Result a -> [Midi.Message]
e_midi result = [Midi.wmsg_msg msg | Interface.Midi msg <- result_midi result]


-- ** val

-- | Extract the value from a cmd.  This is meant to be used as the single
-- check on a cmd operation, so it also returns logs and whether the cmd
-- failed or not (the latter is mandatory since otherwise there is no value).
extract :: (val -> e) -> Result val -> Extracted (Maybe e)
extract f res = case result_val res of
    Right val -> Right (fmap f val, e_logs res)
    Left err -> Left err

-- ** state

-- | Get something out of the Result from one of the states.  Like 'extract',
-- this is meant to be used as the single check on a cmd operation.
extract_state :: (Ui.State -> Cmd.State -> e) -> Result val -> Extracted e
extract_state f res = maybe
    (Right (f (result_ui_state res) (result_cmd_state res), e_logs res))
    Left (result_failed res)

extract_ui_state :: (Ui.State -> e) -> Result val -> Extracted e
extract_ui_state f = extract_state (\state _ -> f state)

e_tracks :: Result a -> Extracted [UiTest.TrackSpec]
e_tracks = extract_ui_state UiTest.extract_tracks

e_pitch_tracks :: Result a -> Extracted [[UiTest.EventSpec]]
e_pitch_tracks = extract_ui_state $
    UiTest.to_pitch_spec . UiTest.to_note_spec . UiTest.extract_tracks

extract_ui :: Ui.StateId e -> Result v -> Extracted e
extract_ui m = extract_ui_state $ \state -> UiTest.eval state m

-- * inst db

-- | Configure ustate and cstate with the given instruments.
set_synths_simple :: [MidiInst.Synth] -> DeriveTest.SimpleAllocations
    -> Ui.State -> Cmd.State -> (Ui.State, Cmd.State)
set_synths_simple synths simple_allocs ui_state cmd_state =
    ( (Ui.config#Ui.allocations #= allocs) ui_state
    , cmd_state
        { Cmd.state_config = (Cmd.state_config cmd_state)
            { Cmd.config_instrument_db = db }
        }
    )
    where
    db = DeriveTest.synths_to_db synths
    allocs = DeriveTest.simple_allocs_from_db db simple_allocs

-- * msg

empty_context :: UiMsg.Context
empty_context = UiMsg.Context Nothing Nothing False

make_key_mods :: [Key.Modifier] -> UiMsg.KbdState -> Key.Key -> Msg.Msg
make_key_mods mods state k = Msg.Ui
    (UiMsg.UiMsg empty_context (UiMsg.MsgEvent (UiMsg.Kbd state mods k text)))
    where
    text = case (k, state) of
        (Key.Char c, UiMsg.KeyDown) -> Just c
        _ -> Nothing

make_key :: UiMsg.KbdState -> Key.Key -> Msg.Msg
make_key = make_key_mods []

keypress :: Key.Key -> [Msg.Msg]
keypress k = [make_key UiMsg.KeyDown k, make_key UiMsg.KeyUp k]

keypresses :: [Key.Key] -> [Msg.Msg]
keypresses = concatMap keypress

key_down :: Char -> Msg.Msg
key_down = make_key UiMsg.KeyDown . Key.Char

key_up :: Char -> Msg.Msg
key_up = make_key UiMsg.KeyUp . Key.Char

backspace :: Msg.Msg
backspace = make_key UiMsg.KeyDown Key.Backspace

mouse :: Bool -> Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
mouse down btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent $ UiMsg.Mouse $ UiMsg.MouseEvent state [] (42, 2) 0 True
    where
    state = if down then UiMsg.MouseDown btn else UiMsg.MouseUp btn
    context = empty_context { UiMsg.ctx_track = Just (track, UiMsg.Track pos) }

drag :: Types.MouseButton -> TrackNum -> ScoreTime -> Msg.Msg
drag btn track pos = Msg.Ui $ UiMsg.UiMsg context $
    UiMsg.MsgEvent $ UiMsg.Mouse $
        UiMsg.MouseEvent (UiMsg.MouseDrag btn) [] (42, 2) 0 False
    where
    context = empty_context { UiMsg.ctx_track = Just (track, UiMsg.Track pos) }

make_midi :: Midi.ChannelMessage -> Msg.Msg
make_midi chan_msg = Msg.Midi $
    Midi.ReadMessage (Midi.read_device "test") 0
        (Midi.ChannelMessage 0 chan_msg)

note_on :: Int -> Pitch.Pitch -> InputNote.Input
note_on note_id pitch = InputNote.NoteOn (InputNote.NoteId note_id)
    (Pitch.Input Pitch.AsciiKbd pitch 0) 1

note_on_nn :: Pitch.NoteNumber -> InputNote.Input
note_on_nn nn = InputNote.NoteOn note_id (InputNote.nn_to_input nn) 1
    where note_id = InputNote.NoteId $ floor nn

note_off :: Int -> InputNote.GenericInput a
note_off note_id = InputNote.NoteOff (InputNote.NoteId note_id) 1

pitch_change :: Int -> Pitch.Pitch -> InputNote.Input
pitch_change note_id pitch = InputNote.PitchChange (InputNote.NoteId note_id)
    (Pitch.Input Pitch.AsciiKbd pitch 0)

pitch_change_nn :: Int -> Pitch.NoteNumber -> InputNote.Input
pitch_change_nn note_id nn =
    InputNote.PitchChange (InputNote.NoteId note_id) (InputNote.nn_to_input nn)

control :: Int -> Score.Control -> Signal.Y -> InputNote.GenericInput a
control note_id cont val =
    InputNote.Control (InputNote.NoteId note_id) cont val

m_note_on :: Pitch.NoteNumber -> Msg.Msg
m_note_on = Msg.InputNote . note_on_nn

m_note_off :: Int -> Msg.Msg
m_note_off = Msg.InputNote . note_off

m_control :: Int -> Score.Control -> Signal.Y -> Msg.Msg
m_control note_id cont val = Msg.InputNote (control note_id cont val)

m_pitch_change :: Int -> Pitch.NoteNumber -> Msg.Msg
m_pitch_change nid = Msg.InputNote . pitch_change_nn nid

-- * Pitch.Input

ascii_kbd :: Pitch.Pitch -> Pitch.Input
ascii_kbd pitch = Pitch.Input Pitch.AsciiKbd pitch 0

piano_kbd :: Pitch.Pitch -> Pitch.Input
piano_kbd pitch = Pitch.Input Pitch.PianoKbd pitch 0

pitch :: Pitch.Octave -> Pitch.PitchClass -> Pitch.Accidentals -> Pitch.Pitch
pitch oct pc accs = Pitch.Pitch oct (Pitch.Degree pc accs)

oct_pc :: Pitch.Octave -> Pitch.PitchClass -> Pitch.Pitch
oct_pc oct pc = pitch oct pc 0

-- * setup cmds

set_scale :: Cmd.M m => BlockId -> BlockId -> TrackId -> Pitch.ScaleId -> m ()
set_scale root_id block_id track_id scale_id = set_env root_id block_id track_id
    [(EnvKey.scale, BaseTypes.VStr (Expr.scale_id_to_str scale_id))]

-- | Fake up just enough Performance to have environ in it.
set_env :: Cmd.M m => BlockId -> BlockId -> TrackId
    -> [(Env.Key, BaseTypes.Val)] -> m ()
set_env root_id block_id track_id environ =
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_performance_threads = Map.insert root_id
            (Unsafe.unsafePerformIO (Thread.start (return ())))
            (Cmd.state_performance_threads st)
        , Cmd.state_performance = Map.insert root_id perf
            (Cmd.state_performance st)
        }
    where
    track_dyn = Map.singleton (block_id, track_id)
        (Derive.initial_dynamic (Env.from_list environ))
    perf = empty_performance { Cmd.perf_track_dynamic = track_dyn }

empty_performance :: Msg.Performance
empty_performance = Cmd.Performance
    { perf_derive_cache = mempty
    , perf_events = mempty
    , perf_logs = []
    , perf_logs_written = True
    , perf_track_dynamic = mempty
    , perf_integrated = []
    , perf_damage = mempty
    , perf_warps = []
    , perf_track_signals = mempty
    , perf_ui_state = Ui.empty
    }

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{- | Core CmdT monad that cmds run in.

    A Cmd is what user actions turn into.  The main thing they do is edit
    'Ui.State.State', or Cmd 'State', but a special subset can also do IO
    actions like saving and loading files.

    The Cmd monad has two kinds of exception: abort or throw.  Abort means
    that the Cmd decided that it's not the proper Cmd for this Msg (keystroke,
    mouse movement, whatever) and another Cmd should get a crack at it.  Throw
    means that the Cmd failed.  When an exception is thrown, the ui and cmd
    states are rolled back and midi output is discarded.

    Cmds should be in the monad @(Cmd.M m) => m ...@.

    They have to be polymorphic because they run in both IO and Identity.  IO
    because some cmds such saving and loading files require IO, and Identity
    because the rest don't.  REPL cmds run in IO so they can load and save,
    and the result is that any cmd that wants to be used from both Identity
    cmds (bound to keystrokes) and the REPL must be polymorphic in the monad.

    Formerly this was @(Monad m) => CmdT m ...@, but with the upgrade to mtl2
    Functor would have to be added to the class context, but only for cmds
    that happened to use Applicative operators.  Rather than deal such
    messiness, there's a class @Cmd.M@ that brings in Functor and Applicative
    as superclasses.

    It's all a bit messy and unpleasant and should be technically unnecessary
    since Identity monads should be able to run in IO anyway.  Other
    solutions:

    - Run all cmds in IO.  I don't think I actually get anything from
    disallowing IO.  It seems like a nice property to be able to always e.g.
    abort cmds halfway through with nothing to undo.

    - Run all cmds in restricted IO by only giving access to readFile and
    writeFile.

    - Run all cmds in Identity by giving unsafePerformIO access to r/w files.
    The assumption is that read and write ordering is never important within
    a single cmd.

    Unfortunately cmds also use getDirectoryContents, forkIO, killThread, etc.
-}
module Cmd.Cmd (
    module Cmd.Cmd, Performance(..)
) where
import qualified Control.Applicative as Applicative
import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as MonadState
import qualified Control.Monad.Trans as Trans

import qualified Data.Generics as Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import Util.Control
import qualified Util.Log as Log
import qualified Util.Logger as Logger
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Midi.Interface
import qualified Midi.Midi as Midi
import qualified Midi.State

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import Cmd.Msg (Performance(..)) -- avoid a circular import
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Midi.Perform
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport

import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb
import qualified App.Config as Config
import Types


-- | This makes Cmds more specific than they have to be, and doesn't let them
-- run in other monads like IO.  It's unlikely to become a problem, but if it
-- does, I'll have to stop using these aliases.
type Cmd = Msg.Msg -> CmdId Status
type CmdId = CmdT Identity.Identity
-- | Yes this is inconsistent with CmdId, but since IO is in the Prelude a type
-- alias wouldn't help much.
type CmdIO = CmdT IO Status

-- | Cmds used by the REPL, which all run in IO.
type CmdL a = CmdT IO a

data Status = Done | Continue | Quit
    -- | Hack to control import dependencies, see "Cmd.PlayC".
    | PlayMidi !PlayMidiArgs
    deriving (Show, Generics.Typeable)

-- | Arguments for "Cmd.PlayC.play".
--
-- Descriptive name, events, tempo func to display play position, optional time
-- to repeat at.
data PlayMidiArgs = PlayMidiArgs !String !Midi.Perform.MidiEvents
    !(Maybe Transport.InverseTempoFunction) !(Maybe RealTime)
instance Show PlayMidiArgs where
    show _ = "((PlayMidiArgs))"

-- | Cmds can run in either Identity or IO, but are generally returned in IO,
-- just to make things uniform.
type RunCmd cmd_m val_m a =
    State.State -> State -> CmdT cmd_m a -> val_m (CmdVal a)

-- | The result of running a Cmd.
type CmdVal a = (State, [MidiThru], [Log.Msg],
    Either State.Error (a, State.State, [Update.CmdUpdate]))

run :: (Monad m) => a -> RunCmd m m a
run abort_val ustate cstate cmd = do
    (((ui_result, cstate2), midi), logs) <-
        (Log.run . Logger.run . flip MonadState.runStateT cstate
            . State.run ustate . (\(CmdT m) -> m))
        cmd
    -- Any kind of error rolls back state and discards midi, but not log msgs.
    -- Normally 'abort_val' will be Continue, but obviously if 'cmd' doesn't
    -- return Status it can't be.
    return $ case ui_result of
        Left State.Abort -> (cstate, [], logs, Right (abort_val, ustate, []))
        Left _ -> (cstate, [], logs, ui_result)
        _ -> (cstate2, midi, logs, ui_result)

-- | Run the given command in Identity, but return it in IO, just as
-- a convenient way to have a uniform return type with 'run' (provided it is
-- run in IO).
run_id_io :: RunCmd Identity.Identity IO Status
run_id_io ui_state cmd_state cmd =
    return $ Identity.runIdentity (run Continue ui_state cmd_state cmd)

run_io :: RunCmd IO IO Status
run_io = run Continue

-- | Run the Cmd in Identity, returning Nothing if it aborted.
run_id :: State.State -> State -> CmdT Identity.Identity a -> CmdVal (Maybe a)
run_id ui_state cmd_state cmd =
    Identity.runIdentity (run Nothing ui_state cmd_state (fmap Just cmd))

-- | Run a set of Cmds as a single Cmd.  The first one to return non-Continue
-- will return.  Cmds can use this to dispatch to other Cmds.
run_subs :: [Cmd] -> Cmd
run_subs [] _ = return Continue
run_subs (cmd:cmds) msg = do
    status <- catch_abort (cmd msg)
    case status of
        Nothing -> run_subs cmds msg
        Just Continue -> run_subs cmds msg
        Just status -> return status

-- * CmdT and operations

type CmdStack m = State.StateT
    (MonadState.StateT State
        (Logger.LoggerT MidiThru
            (Log.LogT m)))

type MidiThru = Midi.Interface.Message

newtype CmdT m a = CmdT (CmdStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError State.Error,
        Applicative.Applicative)

class (Log.LogMonad m, State.M m) => M m where
    -- Not in MonadState for the same reasons as 'Ui.State.M'.
    get :: m State
    put :: State -> m ()
    -- | Log some midi to send out.  This is the midi thru mechanism.
    -- You can give it a timestamp, but it should be 0 for thru, which
    -- will cause it to go straight to the front of the queue.  Use 'midi'
    -- for normal midi thru.
    write_midi :: Midi.Interface.Message -> m ()
    -- | An abort is an exception to get out of CmdT, but it's considered the
    -- same as returning Continue.  It's so a command can back out if e.g. it's
    -- selected by the 'Keymap' but has an additional prerequisite such as
    -- having an active block.
    abort :: m a
    catch_abort :: m a -> m (Maybe a)

instance (Applicative.Applicative m, Monad m) => M (CmdT m) where
    get = (CmdT . lift) MonadState.get
    put st = (CmdT . lift) (MonadState.put st)
    write_midi msg = (CmdT . lift . lift) (Logger.log msg)
    abort = Error.throwError State.Abort
    catch_abort m = Error.catchError (fmap Just m) catch
        where
        catch State.Abort = return Nothing
        catch err = Error.throwError err

midi :: (M m) => Midi.WriteDevice -> Midi.Message -> m ()
midi dev msg = write_midi $ Midi.Interface.Midi $ Midi.WriteMessage dev 0 msg

-- | For some reason, newtype deriving doesn't work on MonadTrans.
instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift -- whee!!

-- | Give CmdT unlifted access to all the logging functions.
instance (Monad m) => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . Log.write

-- | And to the UI state operations.
instance (Functor m, Monad m) => State.M (CmdT m) where
    get = CmdT State.get
    unsafe_put st = CmdT (State.unsafe_put st)
    update upd = CmdT (State.update upd)
    get_updates = CmdT State.get_updates
    throw msg = CmdT (State.throw msg)

-- | This is the same as State.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (M m) => String -> m a
throw = State.throw

-- | Run a subcomputation that is allowed to abort.
ignore_abort :: (M m) => m a -> m ()
ignore_abort m = void $ catch_abort m

-- | Run an IO action, rethrowing any IO exception as a Cmd exception.
rethrow_io :: IO a -> CmdT IO a
rethrow_io =
    either throw return <=< liftIO . Exception.handle handle . (Right <$>)
    where
    handle :: Exception.SomeException -> IO (Either String a)
    handle = return . Left . ("io exception: "++) . show

-- * State

-- | App global state.  Unlike 'Ui.State.State', this is not saved to disk.
data State = State {
    state_config :: !Config
    -- | If set, the current 'State.State' was loaded from this file.
    -- This is so save can keep saving to the same file.
    , state_save_file :: !(Maybe SaveFile)
    -- | Omit the usual derive delay for these blocks.  This is set by
    -- integration, which modifies a block in response to another block being
    -- derived.  This is cleared after every cmd.
    , state_derive_immediately :: !(Set.Set BlockId)
    -- | History.
    , state_history :: !History
    , state_history_config :: !HistoryConfig
    , state_history_collect :: !HistoryCollect

    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position.  The value has complete info.
    , state_keys_down :: !(Map.Map Modifier Modifier)
    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_focused_view :: !(Maybe ViewId)
    -- | This contains a Rect for each screen.
    , state_screens :: ![Rect.Rect]

    -- | This is similar to 'Ui.Block.view_status', except that it's global
    -- instead of per-view.  So changes are logged with a special prefix so
    -- logview can catch them.  Really I only need this map to suppress log
    -- spam.
    , state_global_status :: !(Map.Map String String)
    , state_play :: !PlayState

    -- External device tracking.
    -- | MIDI state of WriteDevices.
    , state_wdev_state :: !WriteDeviceState

    -- | MIDI state of ReadDevices, including configuration like pitch bend
    -- range.
    , state_rdev_state :: !ReadDeviceState
    , state_edit :: !EditState
    -- | The status return for this Cmd.  This is used only by the REPL, since
    -- non-REPL cmds simply return Status as their return value.  REPL cmds
    -- can't do that because they commonly use the return value to return
    -- an interesting String back to the REPL.
    , state_repl_status :: !Status
    } deriving (Show, Generics.Typeable)

data SaveFile = SaveState !FilePath | SaveGit !SaveGit.Repo
    deriving (Show, Eq, Generics.Typeable)

-- | Directory of the save file.
state_save_dir :: State -> Maybe FilePath
state_save_dir state = path state . Config.RelativePath <$>
    case state_save_file state of
        Nothing -> Nothing
        Just (SaveState fn) -> Just $ FilePath.takeDirectory fn
        Just (SaveGit repo) -> Just $ FilePath.takeDirectory repo

initial_state :: Config -> State
initial_state config = State
    { state_config = config
    , state_save_file = Nothing
    , state_derive_immediately = Set.empty
    -- This is a dummy entry needed to bootstrap a Cmd.State.  Normally
    -- 'hist_present' should always have the current state, but the initial
    -- setup cmd needs a State too.
    , state_history = initial_history (empty_history_entry State.empty)
    , state_history_config = empty_history_config
    , state_history_collect = empty_history_collect
    , state_keys_down = Map.empty
    , state_focused_view = Nothing
    , state_screens = []
    , state_global_status = Map.empty
    , state_play = initial_play_state

    , state_wdev_state = empty_wdev_state
    , state_rdev_state = Map.empty
    , state_edit = initial_edit_state
    , state_repl_status = Continue
    }

-- | Reset the parts of the State which are specific to a \"session\".  This
-- should be called whenever an entirely new state is loaded.
reinit_state :: HistoryEntry -> State -> State
reinit_state present cstate = cstate
    { state_history = initial_history present
    -- Performance threads should have been killed by the caller.
    , state_play = initial_play_state
        { state_play_step = state_play_step (state_play cstate) }
    -- This is essential, otherwise lots of cmds break on the bad reference.
    , state_focused_view = Nothing
    , state_edit = initial_edit_state
        { state_time_step = state_time_step (state_edit cstate) }
    }

-- ** Config

-- | Config type variables that change never or rarely.  These mostly come from
-- the static config.
data Config = Config {
    -- | App root, initialized from 'Config.get_app_dir'.
    state_app_dir :: !FilePath
    , state_midi_interface :: !Midi.Interface.Interface
    -- | Reroute MIDI inputs and outputs.  These come from
    -- 'App.StaticConfig.read_device_map' and
    -- 'App.StaticConfig.write_device_map' and probably shouldn't be changed
    -- at runtime.
    , state_rdev_map :: !(Map.Map Midi.ReadDevice Midi.ReadDevice)
    -- | WriteDevices can be score-specific, though, so another map is kept in
    -- 'State.State', which may override the one here.
    , state_wdev_map :: !(Map.Map Midi.WriteDevice Midi.WriteDevice)
    , state_instrument_db :: !InstrumentDb
    -- | Global namespace for deriver.
    , state_global_scope :: !Derive.Scope
    -- | Turn ScaleIds into Scales.
    , state_lookup_scale :: !LookupScale
    } deriving (Show, Generics.Typeable)

-- | Get a midi writer that takes the 'state_wdev_map' into account.
state_midi_writer :: State -> Midi.Interface.Message -> IO ()
state_midi_writer state imsg = do
    let out = case imsg of
            Midi.Interface.Midi wmsg -> Midi.Interface.Midi $ map_wdev wmsg
            _ -> imsg
    putStrLn $ "PLAY " ++ Pretty.pretty out
    ok <- Midi.Interface.write_message
        (state_midi_interface (state_config state)) out
    unless ok $ Log.warn $ "error writing " ++ Pretty.pretty out
    where
    map_wdev (Midi.WriteMessage wdev time msg) =
        Midi.WriteMessage (lookup_wdev wdev) time msg
    lookup_wdev wdev = Map.findWithDefault wdev wdev
        (state_wdev_map (state_config state))

-- | This is a hack so I can use the default Show instance for 'State'.
newtype LookupScale = LookupScale Derive.LookupScale
instance Show LookupScale where show _ = "((LookupScale))"

-- | Convert a relative path to place it in the app dir.
path :: State -> Config.RelativePath -> FilePath
path state (Config.RelativePath path) =
    state_app_dir (state_config state) </> path

-- ** PlayState

-- | State concerning derivation, performance, and playing the performance.
data PlayState = PlayState {
    -- | Transport control channel for the player, if one is running.
    state_play_control :: !(Maybe Transport.PlayControl)
    -- | When changes are made to a block, its performance will be
    -- recalculated in the background.  When the Performance is forced
    -- \"enough\", it will replace the existing performance in
    -- 'state_performance', if any.  This means there will be a window in
    -- which the performance is out of date, but this is better than hanging
    -- the responder every time it touches an insufficiently lazy part of
    -- the performance.
    , state_performance :: !(Map.Map BlockId Performance)
    -- | However some cmds, like play, want the most up to date performance,
    -- even if they have to wait for it.  This map will be updated
    -- immediately.
    , state_current_performance :: !(Map.Map BlockId Performance)
    -- | Keep track of current thread working on each performance.  If a
    -- new performance is needed before the old one is complete, it can be
    -- killed off.
    , state_performance_threads :: !(Map.Map BlockId Concurrent.ThreadId)
    , state_lilypond_stack_maps :: !(Map.Map BlockId StackMap)
    -- | Some play commands can start playing from a short distance before the
    -- cursor.
    , state_play_step :: !TimeStep.TimeStep
    -- | Contain a StepState if step play is active.  Managed in
    -- "Cmd.StepPlay".
    , state_step :: !(Maybe StepState)
    -- | Globally speed up or slow down performance.  It mutiplies the
    -- timestamps by the reciprocal of this amount, so 2 will play double
    -- speed, and 0.5 will play half speed.
    , state_play_multiplier :: RealTime
    } deriving (Show, Generics.Typeable)

-- | Map char number in a written .ly score to stack position of the generating
-- event.  Lilypond can have the generated PDF can report clicks, but I need to
-- map them back to stacks to highlight the clicked note.
type StackMap = Map.Map Int Stack.UiFrame

initial_play_state :: PlayState
initial_play_state = PlayState
    { state_play_control = Nothing
    , state_performance = Map.empty
    , state_current_performance = Map.empty
    , state_performance_threads = Map.empty
    , state_lilypond_stack_maps = Map.empty
    , state_play_step =
        TimeStep.step (TimeStep.RelativeMark TimeStep.AllMarklists 0)
    , state_step = Nothing
    , state_play_multiplier = RealTime.seconds 1
    }

-- | Step play is a way of playing back the performance in non-realtime.
data StepState = StepState {
    -- - constant
    -- | Keep track of the view step play was started in, so I know where to
    -- display the selection.
    step_view_id :: !ViewId
    -- | If step play only applies to a few tracks, list them.  If null,
    -- step play applies to all tracks.
    , step_tracknums :: [TrackNum]

    -- - modified
    -- | MIDI states before the step play position, in descending order.
    , step_before :: ![(ScoreTime, Midi.State.State)]
    -- | MIDI states after the step play position, in asceding order.
    , step_after :: ![(ScoreTime, Midi.State.State)]
    } deriving (Show, Generics.Typeable)

-- ** EditState

-- | Editing state, modified in the course of editing.
data EditState = EditState {
    -- | Edit mode enables various commands that write to tracks.
    state_edit_mode :: !EditMode
    -- | Whether or not to advance the insertion point after a note is
    -- entered.
    , state_advance :: Bool
    -- | Chord mode means the note is considered entered when all NoteOffs
    -- have been received.  While a note is held down, the insertion point will
    -- move to the next note track with the same instrument so you can
    -- enter chords.
    --
    -- When chord mode is off, the note is considered entered as soon as
    -- its NoteOn is received.
    , state_chord :: Bool
    -- | Try to find or create a 'Score.c_dynamic' track for to record
    -- 'InputNote.Input' velocity, similar to how a pitch track is edited and
    -- created.
    , state_record_velocity :: Bool
    -- | Use the alphanumeric keys to enter notes instead of midi input.
    , state_kbd_entry :: !Bool
    -- | Default time step for cursor movement.
    , state_time_step :: !TimeStep.TimeStep
    -- | Used for note duration.  It's separate from 'state_time_step' to
    -- allow for tracker-style note entry where newly entered notes extend to
    -- the next note or the end of the block.
    , state_note_duration :: !TimeStep.TimeStep
    -- | If this is Rewind, create notes with negative durations.
    , state_note_direction :: !TimeStep.Direction
    -- | New notes get this text by default.  This way, you can enter a series
    -- of notes with the same attributes, or whatever.
    , state_note_text :: !Text
    -- | Transpose note entry on the keyboard by this many octaves.  It's by
    -- octave instead of scale degree since scales may have different numbers
    -- of notes per octave.
    , state_kbd_entry_octave :: !Pitch.Octave

    -- | A LIFO queue of recent notes or transforms, to make it easier to
    -- re-enter them.  Each has an integer key associated with it so they can
    -- remain bound to the same key even if their LIFO position changes.
    , state_recent_notes :: ![(Int, RecentNote)]

    -- | See 'set_edit_box'.
    , state_edit_box :: !(Block.Box, Block.Box)
    } deriving (Eq, Show, Generics.Typeable)

initial_edit_state :: EditState
initial_edit_state = EditState {
    state_edit_mode = NoEdit
    , state_kbd_entry = False
    , state_advance = True
    , state_chord = False
    , state_record_velocity = False
    , state_time_step =
        TimeStep.step (TimeStep.AbsoluteMark TimeStep.AllMarklists 0)
    , state_note_duration = TimeStep.step TimeStep.BlockEnd
    , state_note_direction = TimeStep.Advance
    , state_note_text = ""
    -- This should put middle C in the center of the kbd entry keys.
    , state_kbd_entry_octave = 4
    , state_recent_notes = []
    , state_edit_box = (box, box)
    } where box = uncurry Block.Box Config.bconfig_box

-- | These enable various commands to edit event text.  What exactly val
-- and method mean are dependent on the track.
data EditMode = NoEdit | RawEdit | ValEdit | MethodEdit deriving (Eq, Show)
instance Pretty.Pretty EditMode where pretty = show

data RecentNote =
    -- | Bool is true if the event was zero dur.  This is needed if
    -- 'cmd_insert_recent' winds up creating a new event.
    RecentNote !Text !Bool
    | RecentTransform !Text !Bool
    deriving (Show, Eq)

-- *** midi devices

data WriteDeviceState = WriteDeviceState {
    -- Used by Cmd.MidiThru:
    -- | Last pb val for each Addr.
    wdev_pb :: !(Map.Map Instrument.Addr Midi.PitchBendValue)
    -- | NoteId currently playing in each Addr.  An Addr may have >1 NoteId.
    , wdev_note_addr :: !(Map.Map InputNote.NoteId Instrument.Addr)
    -- | The note id is not guaranteed to have any relationship to the key,
    -- so the MIDI NoteOff needs to know what key the MIDI NoteOn used.
    , wdev_note_key :: !(Map.Map InputNote.NoteId Midi.Key)
    -- | Map an addr to a number that increases when it's assigned a note.
    -- This is used along with 'wdev_serial' to implement addr round-robin.
    , wdev_addr_serial :: !(Map.Map Instrument.Addr Integer)
    , wdev_serial :: !Integer
    -- | Last NoteId seen.  This is needed to emit controls (rather than just
    -- mapping them from MIDI input) because otherwise there's no way to know
    -- to which note they should be assigned.
    , wdev_last_note_id :: !(Maybe InputNote.NoteId)

    -- Used by Cmd.PitchTrack:
    -- | NoteIds being entered into which pitch tracks.  When entering a chord,
    -- a PitchChange uses this to know which pitch track to update.
    , wdev_pitch_track :: !(Map.Map InputNote.NoteId (BlockId, TrackNum))

    -- Used by no one, yet:  (TODO should someone use this?)
    -- | Remember the current inst of each addr.  More than one instrument or
    -- keyswitch can share the same addr, so I need to keep track which one is
    -- active to minimize switches.
    , wdev_addr_inst :: !(Map.Map Instrument.Addr Instrument.Instrument)
    } deriving (Eq, Show, Generics.Typeable)

empty_wdev_state :: WriteDeviceState
empty_wdev_state = WriteDeviceState
    { wdev_pb = Map.empty
    , wdev_note_addr = Map.empty
    , wdev_note_key = Map.empty
    , wdev_addr_serial = Map.empty
    , wdev_serial = 0
    , wdev_last_note_id = Nothing
    , wdev_pitch_track = Map.empty
    , wdev_addr_inst = Map.empty
    }

type ReadDeviceState = Map.Map Midi.ReadDevice InputNote.ControlState

-- *** performance

perf_tempo :: Performance -> Transport.TempoFunction
perf_tempo = TrackWarp.tempo_func . perf_warps

perf_inv_tempo :: Performance -> Transport.InverseTempoFunction
perf_inv_tempo = TrackWarp.inverse_tempo_func . perf_warps

perf_closest_warp :: Performance -> Transport.ClosestWarpFunction
perf_closest_warp = TrackWarp.closest_warp . perf_warps

-- *** instrument

-- | The code part of an instrument, i.e. the calls and cmds it brings into
-- scope.
--
-- This has to be in Cmd.Cmd for circular import reasons.
data InstrumentCode = InstrumentCode {
    inst_calls :: !Derive.InstrumentCalls
    , inst_environ :: !TrackLang.Environ
    , inst_cmds :: ![Cmd]
    }

instance Show InstrumentCode where
    show _ = "((InstrumentCode))"

derive_instrument :: MidiInfo -> Derive.Instrument
derive_instrument info = Derive.Instrument
    { Derive.inst_calls = inst_calls (MidiDb.info_code info)
    , Derive.inst_environ = inst_environ (MidiDb.info_code info)
    }

empty_code :: InstrumentCode
empty_code = InstrumentCode (Derive.InstrumentCalls [] []) mempty []

-- | Instantiate the MidiDb with the code types.  The only reason the MidiDb
-- types have the type parameter is so I can define them in their own module
-- without getting circular imports.
type InstrumentDb = Instrument.Db.Db InstrumentCode
type MidiInfo = MidiDb.Info InstrumentCode
type SynthDesc = MidiDb.SynthDesc InstrumentCode

-- *** history

-- | Ghosts of state past, present, and future.
data History = History {
    hist_past :: ![HistoryEntry]
    -- | The present is actually the immediate past.  When you undo, the
    -- undo itself is actually in the future of the state you want to undo.
    -- So another way of looking at it is that you undo from the past to
    -- a point further in the past.  But since you always require a "recent
    -- past" to exist, it's more convenient to break it out and call it the
    -- \"present\".  Isn't time travel confusing?
    , hist_present :: !HistoryEntry
    , hist_future :: ![HistoryEntry]
    , hist_last_cmd :: !(Maybe LastCmd)
    } deriving (Show, Generics.Typeable)

initial_history :: HistoryEntry -> History
initial_history present = History [] present [] Nothing

-- | Record some information about the last cmd for the benefit of
-- 'Cmd.Undo.maintain_history'.
data LastCmd =
    -- | This cmd set the state because it was an undo or redo.  Otherwise undo
    -- and redo themselves would be recorded and multiple undo would be
    -- impossible!
    UndoRedo
    -- | This cmd set the state because of a load.  This should reset all the
    -- history so I can start loading from the new state's history.
    | Load (Maybe SaveGit.Commit) [String]
    deriving (Show)

data HistoryConfig = HistoryConfig {
    -- | Keep this many previous history entries in memory.
    hist_keep :: !Int
    -- | Checkpoints are saved relative to the state at the next checkpoint.
    -- So it's important to keep the commit of that checkpoint up to date,
    -- otherwise the state and the checkpoints will get out of sync.
    , hist_last_commit :: !(Maybe SaveGit.Commit)
    } deriving (Show)

empty_history_config :: HistoryConfig
empty_history_config = HistoryConfig Config.default_keep_history Nothing

data HistoryCollect = HistoryCollect {
    -- | This is cleared after each cmd.  A cmd can cons its name on, and
    -- the cmd is recorded with the (optional) set of names it returns.
    -- Hopefully each cmd has at least one name, since this makes the history
    -- more readable.  There can be more than one name if the history records
    -- several cmds or if one cmd calls another.
    state_cmd_names :: ![String]
    -- | Suppress history record until the EditMode changes from the given one.
    -- This is a bit of a hack so that every keystroke in a raw edit isn't
    -- recorded separately.
    , state_suppress_edit :: !(Maybe EditMode)
    -- | The Git.Commit in the SaveHistory should definitely be Nothing.
    , state_suppressed :: !(Maybe SaveGit.SaveHistory)
    } deriving (Show, Generics.Typeable)

empty_history_collect :: HistoryCollect
empty_history_collect = HistoryCollect
    { state_cmd_names = []
    , state_suppress_edit = Nothing
    , state_suppressed = Nothing
    }

data HistoryEntry = HistoryEntry {
    hist_state :: !State.State
    -- | Since track event updates are not caught by diff but recorded by
    -- Ui.State, I have to save those too, or else an undo or redo will miss
    -- the event changes.  TODO ugly, can I avoid this?
    --
    -- If this HistoryEntry is in the past, these are the updates that took it
    -- to the future, not the updates emitted by the cmd itself.  If the
    -- HistoryEntry is in the future, the updates take it to the past, which
    -- are the updated emitted by the cmd.  So don't be confused if it looks
    -- like a HistoryEntry has the wrong updates.
    , hist_updates :: ![Update.CmdUpdate]
    -- | Cmds involved creating this entry.
    , hist_names :: ![String]
    -- | The Commit where this entry was saved.  Nothing if the entry is
    -- unsaved.
    , hist_commit :: !(Maybe SaveGit.Commit)
    } deriving (Show, Generics.Typeable)

empty_history_entry :: State.State -> HistoryEntry
empty_history_entry state = HistoryEntry state [] [] Nothing

instance Pretty.Pretty History where
    format (History past present future last_cmd) =
        Pretty.record_title "History"
            [ ("past", Pretty.format past)
            , ("present", Pretty.format present)
            , ("future", Pretty.format future)
            , ("last_cmd", Pretty.text (show last_cmd))
            ]

instance Pretty.Pretty HistoryEntry where
    format (HistoryEntry _state updates commands commit) =
        Pretty.format commit Pretty.<+> Pretty.text_list commands
        Pretty.<+> Pretty.format updates

instance Pretty.Pretty HistoryConfig where
    format (HistoryConfig keep last_commit) =
        Pretty.record_title "HistoryConfig"
            [ ("keep", Pretty.format keep)
            , ("last_commit", Pretty.format last_commit)
            ]

instance Pretty.Pretty HistoryCollect where
    format (HistoryCollect names edit suppressed) =
        Pretty.record_title "HistoryCollect"
            [ ("names", Pretty.format names)
            , ("suppress_edit", Pretty.format edit)
            , ("suppressed", Pretty.format suppressed)
            ]


-- *** modifier

data Modifier = KeyMod Key.Modifier
    -- | Mouse button, and track it went down at, if any.  The block is not
    -- recorded.  You can't drag across blocks so you know any click must
    -- apply to the focused block.
    | MouseMod Types.MouseButton (Maybe (TrackNum, UiMsg.Track))
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the device, this code doesn't know which devices are available.
    -- Block or track level handlers can query the device themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)

mouse_mod_btn :: Modifier -> Maybe Types.MouseButton
mouse_mod_btn (MouseMod btn _) = Just btn
mouse_mod_btn _ = Nothing


-- ** state access

gets :: (M m) => (State -> a) -> m a
gets f = f <$> get

modify :: (M m) => (State -> State) -> m ()
modify f = do
    st <- get
    put $! f st

modify_play_state :: (M m) => (PlayState -> PlayState) -> m ()
modify_play_state f = modify $ \st ->
    st { state_play = f (state_play st) }

-- | Return the rect of the screen closest to the given point.
get_screen :: (M m) => (Int, Int) -> m Rect.Rect
get_screen point = do
    screens <- gets state_screens
    -- There are no screens yet during setup, so pick something somewhat
    -- reasonable so windows don't all try to crunch themselves down to
    -- nothing.
    return $ fromMaybe (Rect.xywh 0 0 800 600) $
        Seq.minimum_on (Rect.distance point) screens

lookup_performance :: (M m) => BlockId -> m (Maybe Performance)
lookup_performance block_id =
    Map.lookup block_id <$> gets (state_performance . state_play)

get_performance :: (M m) => BlockId -> m Performance
get_performance block_id = require =<< lookup_performance block_id

-- | Clear all performances, which will cause them to be rederived.
-- It could get out of IO by using unsafePerformIO to kill the threads (it
-- should be safe), but I won't do it unless I have to.
invalidate_performances :: CmdT IO ()
invalidate_performances = do
    threads <- gets (Map.elems . state_performance_threads . state_play)
    liftIO $ mapM_ Concurrent.killThread threads
    modify_play_state $ \state -> state
        { state_performance = mempty
        , state_performance_threads = mempty
        }

-- | Keys currently held down, as in 'state_keys_down'.
keys_down :: (M m) => m (Map.Map Modifier Modifier)
keys_down = gets state_keys_down

get_focused_view :: (M m) => m ViewId
get_focused_view = gets state_focused_view >>= require

get_focused_block :: (M m) => m BlockId
get_focused_block = fmap Block.view_block (get_focused_view >>= State.get_view)

lookup_focused_view :: (M m) => m (Maybe ViewId)
lookup_focused_view = gets state_focused_view

-- | In some circumstances I don't want to abort if there's no focused block.
lookup_focused_block :: (M m) => m (Maybe BlockId)
lookup_focused_block = do
    maybe_view_id <- lookup_focused_view
    case maybe_view_id of
        -- It's still an error if the view id doesn't exist.
        Just view_id -> fmap (Just . Block.view_block) (State.get_view view_id)
        Nothing -> return Nothing

get_current_step :: (M m) => m TimeStep.TimeStep
get_current_step = gets (state_time_step . state_edit)

-- | Get the leftmost track covered by the insert selection, which is
-- considered the "focused" track by convention.
get_insert_tracknum :: (M m) => m (Maybe TrackNum)
get_insert_tracknum = do
    view_id <- get_focused_view
    sel <- State.get_selection view_id Config.insert_selnum
    return (fmap Types.sel_start_track sel)

-- | This just calls 'State.set_view_status', but all status setting should
-- go through here so they can be uniformly filtered or logged or something.
set_view_status :: (M m) => ViewId -> (Int, Text) -> Maybe Text -> m ()
set_view_status = State.set_view_status

set_global_status :: (M m) => String -> String -> m ()
set_global_status key val = do
    status_map <- gets state_global_status
    when (Map.lookup key status_map /= Just val) $ do
        modify $ \st ->
            st { state_global_status = Map.insert key val status_map }
        Log.debug $ "global status: " ++ key ++ " -- " ++ val

-- | Set a status variable on all views.
set_status :: (M m) => (Int, Text) -> Maybe Text -> m ()
set_status key val = do
    view_ids <- State.gets (Map.keys . State.state_views)
    forM_ view_ids $ \view_id -> set_view_status view_id key val

get_lookup_midi_instrument :: (M m) => m MidiDb.LookupMidiInstrument
get_lookup_midi_instrument =
    gets (Instrument.Db.db_lookup_midi . state_instrument_db . state_config)

lookup_instrument :: (M m) => Score.Instrument -> m (Maybe MidiInfo)
lookup_instrument inst = ($ inst) <$> get_lookup_instrument

get_lookup_instrument :: (M m) => m (Score.Instrument -> Maybe MidiInfo)
get_lookup_instrument = gets $
    Instrument.Db.db_lookup . state_instrument_db . state_config

get_midi_patch :: (M m) => Score.Instrument -> m Instrument.Patch
get_midi_patch inst = do
    info <- require_msg ("get_midi_patch " ++ Pretty.pretty inst)
        =<< lookup_instrument inst
    return $ MidiDb.info_patch info

get_midi_instrument :: (M m) => Score.Attributes -> Score.Instrument
    -> m Instrument.Instrument
get_midi_instrument attrs inst = do
    lookup <- get_lookup_midi_instrument
    require_msg ("get_midi_instrument " ++ Pretty.pretty inst)
        (fst <$> lookup attrs inst)

get_lookup_scale :: (M m) => m Derive.LookupScale
get_lookup_scale = do
    LookupScale lookup_scale <- gets (state_lookup_scale . state_config)
    return lookup_scale

-- | Lookup a scale_id or throw.
get_scale :: (M m) => String -> Pitch.ScaleId -> m Scale.Scale
get_scale caller scale_id = do
    lookup_scale <- get_lookup_scale
    maybe (throw (caller ++ ": unknown " ++ show scale_id)) return
        (lookup_scale scale_id)

get_rdev_state :: (M m) => Midi.ReadDevice -> m InputNote.ControlState
get_rdev_state rdev = do
    cmap <- gets state_rdev_state
    return $ fromMaybe (InputNote.empty_state Config.control_pb_range)
        (Map.lookup rdev cmap)

set_rdev_state :: (M m) => Midi.ReadDevice -> InputNote.ControlState -> m ()
set_rdev_state rdev state = do
    st <- get
    put $ st { state_rdev_state =
        Map.insert rdev state (state_rdev_state st) }

set_pitch_bend_range :: (M m) => Control.PbRange -> Midi.ReadDevice -> m ()
set_pitch_bend_range range rdev = do
    state <- get_rdev_state rdev
    set_rdev_state rdev (state { InputNote.state_pb_range = range })

get_wdev_state :: (M m) => m WriteDeviceState
get_wdev_state = gets state_wdev_state

modify_wdev_state :: (M m) => (WriteDeviceState -> WriteDeviceState) -> m ()
modify_wdev_state f = modify $ \st ->
    st { state_wdev_state = f (state_wdev_state st) }

derive_immediately :: (M m) => [BlockId] -> m ()
derive_immediately block_ids = modify $ \st -> st { state_derive_immediately =
    Set.fromList block_ids <> state_derive_immediately st }

-- *** EditState

modify_edit_state :: (M m) => (EditState -> EditState) -> m ()
modify_edit_state f = modify $ \st -> st { state_edit = f (state_edit st) }

-- | At the Ui level, the edit box is per-block, but I use it to indicate edit
-- mode, which is global.  So it gets stored in Cmd.State and must be synced
-- with new blocks.
set_edit_box :: (M m) => Block.Box -> Block.Box -> m ()
set_edit_box skel track = do
    modify_edit_state $ \st -> st { state_edit_box = (skel, track) }
    block_ids <- State.all_block_ids
    forM_ block_ids $ \bid -> State.set_edit_box bid skel track

is_val_edit :: (M m) => m Bool
is_val_edit = (== ValEdit) <$> gets (state_edit_mode . state_edit)

is_kbd_entry :: (M m) => m Bool
is_kbd_entry = gets (state_kbd_entry . state_edit)

set_note_text :: (M m) => Text -> m ()
set_note_text text = do
    modify_edit_state $ \st -> st { state_note_text = text }
    set_status Config.status_note_text $
        if Text.null text then Nothing else Just text


-- * util

-- | Give a name to a Cmd.  The name is applied when the cmd returns so the
-- names come out in call order, and it doesn't incur overhead for cmds that
-- abort.
name :: (M m) => String -> m a -> m a
name s cmd = cmd <* modify (\st -> st
    { state_history_collect = (state_history_collect st)
        { state_cmd_names = s : state_cmd_names (state_history_collect st) }
    })

-- | Like 'name', but also set the 'state_suppress_edit'.  This will suppress
-- history recording until the edit mode changes from the given one.
suppress_history :: (M m) => EditMode -> String -> m a -> m a
suppress_history mode name cmd = cmd <* modify (\st -> st
    { state_history_collect = (state_history_collect st)
        { state_cmd_names = name : state_cmd_names (state_history_collect st)
        , state_suppress_edit = Just mode
        }
    })

-- | Log an event so that it can be clicked on in logview.
log_event :: BlockId -> TrackId -> Event.Event -> String
log_event block_id track_id event = "{s" ++ show frame ++ "}"
    where
    frame = Stack.unparse_ui_frame
        (Just block_id, Just track_id, Just (Event.range event))

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (M m) => Maybe a -> m a
require = maybe abort return

-- | Like 'require', but throw an exception with the given msg.
require_msg :: (M m) => String -> Maybe a -> m a
require_msg msg = maybe (throw msg) return

require_right :: (M m) => (err -> String) -> Either err a -> m a
require_right mkmsg = either (throw . mkmsg) return

-- | Turn off all sounding notes.
-- TODO clear out WriteDeviceState?
all_notes_off :: (M m) => m ()
all_notes_off = write_midi $ Midi.Interface.AllNotesOff 0

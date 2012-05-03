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
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as MonadState
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)

import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Util.Control
import qualified Util.Git as Git
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
import qualified Ui.Events as Events
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import Cmd.Msg (Performance(..)) -- avoid a circular import
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackWarp as TrackWarp

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
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

-- | Cmds used by the language system, which all run in Identity.
type CmdL a = CmdT IO a

data Status = Done | Continue | Quit
    -- | Hack to control import dependencies, see "Cmd.PlayC".
    | Play UpdaterArgs
    deriving (Show, Generics.Typeable)

data UpdaterArgs = UpdaterArgs
    Transport.UpdaterControl Transport.InverseTempoFunction RealTime RealTime
instance Show UpdaterArgs where
    show _ = "((UpdaterArgs))"

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

newtype CmdT m a = CmdT (CmdStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError State.Error,
        Applicative.Applicative)

class (Log.LogMonad m, State.M m) => M m where
    -- Not in MonadState for the same reasons as 'Ui.State.M'.
    get :: m State
    put :: State -> m ()
    -- | Log some midi to send out immediately.  This is the midi thru
    -- mechanism.
    midi :: Midi.WriteDevice -> Midi.Message -> m ()
    -- | An abort is an exception to get out of CmdT, but it's considered the
    -- same as returning Continue.  It's so a command can back out if e.g. it's
    -- selected by the 'Keymap' but has an additional prerequisite such as
    -- having an active block.
    abort :: m a
    catch_abort :: m a -> m (Maybe a)

instance (Applicative.Applicative m, Monad m) => M (CmdT m) where
    get = (CmdT . lift) MonadState.get
    put st = (CmdT . lift) (MonadState.put st)
    midi dev msg = (CmdT . lift . lift) (Logger.log (dev, msg))
    abort = Error.throwError State.Abort
    catch_abort m = Error.catchError (fmap Just m) catch
        where
        catch State.Abort = return Nothing
        catch err = Error.throwError err

-- | For some reason, newtype deriving doesn't work on MonadTrans.
instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift -- whee!!

-- | Give CmdT unlifted access to all the logging functions.
instance (Monad m) => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . Log.write

-- | And to the UI state operations.
instance (Functor m, Monad m) => State.M (CmdT m) where
    get = CmdT State.get
    put st = CmdT (State.put st)
    update upd = CmdT (State.update upd)
    get_updates = CmdT State.get_updates
    throw msg = CmdT (State.throw msg)

type MidiThru = (Midi.WriteDevice, Midi.Message)

-- | This is the same as State.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (M m) => String -> m a
throw = State.throw

-- | Run a subcomputation that is allowed to abort.
ignore_abort :: (M m) => m a -> m ()
ignore_abort m = catch_abort m >> return ()

-- * State

-- | App global state.  Unlike 'Ui.State.State', this is not saved to disk.
data State = State {
    -- Config type variables that change never or rarely.  These come from the
    -- static config.
    state_midi_interface :: !Midi.Interface.Interface
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

    -- Automatically maintained state.  This means only a few cmds should
    -- modify these.

    -- | History.
    , state_history :: !History
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
    } deriving (Show, Generics.Typeable)

initial_state :: Map.Map Midi.ReadDevice Midi.ReadDevice
    -> Map.Map Midi.WriteDevice Midi.WriteDevice
    -> Midi.Interface.Interface -> InstrumentDb -> Derive.Scope
    -> State
initial_state rdev_map wdev_map interface inst_db global_scope = State {
    state_midi_interface = interface
    , state_rdev_map = rdev_map
    , state_wdev_map = wdev_map
    , state_instrument_db = inst_db
    , state_global_scope = global_scope
    -- TODO later this should also be merged with static config
    , state_lookup_scale = LookupScale $
        \scale_id -> Map.lookup scale_id Scale.All.scales

    , state_history = empty_history
    , state_history_collect = empty_history_collect
    , state_keys_down = Map.empty
    , state_focused_view = Nothing
    , state_screens = []
    , state_global_status = Map.empty
    , state_play = initial_play_state

    , state_wdev_state = empty_wdev_state
    , state_rdev_state = Map.empty
    , state_edit = initial_edit_state
    }

-- | Reset the parts of the State which are specific to a \"session\".  This
-- should be called whenever an entirely new state is loaded.
reinit_state :: State -> State
reinit_state cstate = cstate
    { state_history = empty_history
    -- Performance threads should have been killed by the caller.
    , state_play = initial_play_state
        { state_play_step = state_play_step (state_play cstate) }
    -- This is essential, otherwise lots of cmds break on the bad reference.
    , state_focused_view = Nothing
    , state_edit = initial_edit_state
    }

-- | Get a midi writer that takes the 'state_wdev_map' into account.
state_midi_writer :: State -> (Midi.WriteMessage -> IO ())
state_midi_writer state (Midi.WriteMessage wdev ts msg) = do
    putStrLn $ "PLAY " ++ Pretty.pretty wdev ++ "->" ++ Pretty.pretty wmsg
    ok <- Midi.Interface.write_message (state_midi_interface state) wmsg
    unless ok $ Log.warn $ "error writing " ++ Pretty.pretty wmsg
    where
    wmsg = Midi.WriteMessage real_wdev ts msg
    real_wdev = Map.findWithDefault wdev wdev (state_wdev_map state)

-- | This is a hack so I can use the default Show instance for 'State'.
newtype LookupScale = LookupScale Derive.LookupScale
instance Show LookupScale where show _ = "((LookupScale))"

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
    -- | Some play commands can start playing from a short distance before the
    -- cursor.
    , state_play_step :: !TimeStep.TimeStep
    -- | Contain a StepState if step play is active.  Managed in
    -- "Cmd.StepPlay".
    , state_step :: !(Maybe StepState)
    -- | Multiply performance timestamps by this amount.  This will globally
    -- speed up or slow down performance.
    , state_play_multiplier :: RealTime
    } deriving (Show, Generics.Typeable)

initial_play_state :: PlayState
initial_play_state = PlayState
    { state_play_control = Nothing
    , state_performance = Map.empty
    , state_current_performance = Map.empty
    , state_performance_threads = Map.empty
    , state_play_step =
        TimeStep.step (TimeStep.RelativeMark TimeStep.AllMarklists 2)
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
    , state_note_text :: !String
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
        TimeStep.step (TimeStep.AbsoluteMark TimeStep.AllMarklists 3)
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

data RecentNote =
    -- | Bool is true if the event was zero dur.  This is needed if
    -- 'cmd_insert_recent' winds up creating a new event.
    RecentNote String Bool
    | RecentTransform String Bool
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
    , inst_cmds :: ![Cmd]
    }

empty_code :: InstrumentCode
empty_code = InstrumentCode (Derive.InstrumentCalls [] []) []

-- | Instantiate the MidiDb with the code types.  The only reason the MidiDb
-- types have the type parameter is so I can define them in their own module
-- without getting circular imports.
type InstrumentDb = Instrument.Db.Db InstrumentCode
type MidiInfo = MidiDb.Info InstrumentCode
type SynthDesc = MidiDb.SynthDesc InstrumentCode

-- *** history

data History = History {
    -- | Ghosts of state past and future.
    hist_past :: ![HistoryEntry]
    , hist_future :: ![HistoryEntry]
    -- | True if this state was set by an undo or redo.  Otherwise undo and
    -- redo would be recorded and multiple undo would be impossible!
    , hist_undo_redo :: !Bool
    } deriving (Show, Generics.Typeable)

empty_history :: History
empty_history = History [] [] False

data HistoryCollect = HistoryCollect
    -- | Collect updates from each cmd to be saved with the history, in
    -- 'hist_entry_updates'.
    { state_updates :: ![Update.CmdUpdate]
    -- | This is cleared after each cmd.  A cmd can cons its name on, and
    -- the cmd is recorded with the (optional) set of names it returns.
    -- Hopefully each cmd has at least one name, since this makes the history
    -- more readable.  There can be more than one name if the history records
    -- several cmds or if one cmd calls another.
    , state_cmd_names :: ![String]
    -- | Suppress history record until the EditMode changes from the given one.
    -- This is a bit of a hack so that every keystroke in a raw edit isn't
    -- recorded separately.
    , state_suppress_edit :: !(Maybe EditMode)
    , state_suppressed :: !(Maybe UncommittedHistoryEntry)
    } deriving (Show, Generics.Typeable)

empty_history_collect :: HistoryCollect
empty_history_collect = HistoryCollect [] [] Nothing Nothing

data HistoryEntry = HistoryEntry {
    hist_state :: !State.State
    -- | Since track event updates are not caught by diff but recorded by
    -- Ui.State, I have to save those too, or else an undo or redo will miss
    -- the event changes.  TODO ugly, can I avoid this?
    , hist_updates :: ![Update.CmdUpdate]
    -- | Cmds involved creating this entry.
    , hist_cmd_names :: ![String]
    -- | The Commit where this entry was saved.  Nothing if the entry is
    -- unsaved.
    , hist_commit :: !(Maybe Git.Commit)
    } deriving (Show, Generics.Typeable)

instance Pretty.Pretty History where
    format (History past future _undo_redo) = Pretty.record_title "History"
        [ ("past", Pretty.format past)
        , ("future", Pretty.format future)
        ]

instance Pretty.Pretty HistoryEntry where
    format (HistoryEntry _ _ commands commit) =
        Pretty.format commit Pretty.<+> Pretty.text_list commands

data UncommittedHistoryEntry =
    UncommittedHistoryEntry !State.State ![Update.CmdUpdate] ![String]
    deriving (Show, Generics.Typeable)


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
    return $ maybe Rect.empty id $
        Seq.minimum_on (Rect.distance point) screens

lookup_performance :: (M m) => BlockId -> m (Maybe Performance)
lookup_performance block_id =
    Map.lookup block_id <$> gets (state_performance . state_play)

get_performance :: (M m) => BlockId -> m Performance
get_performance block_id = require =<< lookup_performance block_id

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
set_view_status :: (M m) => ViewId -> String -> Maybe String -> m ()
set_view_status view_id key val = State.set_view_status view_id key val

set_global_status :: (M m) => String -> String -> m ()
set_global_status key val = do
    status_map <- gets state_global_status
    when (Map.lookup key status_map /= Just val) $ do
        modify $ \st ->
            st { state_global_status = Map.insert key val status_map }
        Log.debug $ "global status: " ++ key ++ " -- " ++ val

-- | Set a status variable on all views.
set_status :: (M m) => String -> Maybe String -> m ()
set_status key val = do
    view_ids <- State.gets (Map.keys . State.state_views)
    forM_ view_ids $ \view_id -> set_view_status view_id key val

get_lookup_midi_instrument :: (M m) => m MidiDb.LookupMidiInstrument
get_lookup_midi_instrument =
    gets (Instrument.Db.db_lookup_midi . state_instrument_db)

lookup_instrument_info :: (M m) => Score.Instrument -> m (Maybe MidiInfo)
lookup_instrument_info inst = do
    inst_db <- gets state_instrument_db
    return $ Instrument.Db.db_lookup inst_db inst

get_midi_patch :: (M m) => Score.Instrument -> m Instrument.Patch
get_midi_patch inst = do
    info <- require_msg ("get_midi_patch " ++ Pretty.pretty inst)
        =<< lookup_instrument_info inst
    return $ MidiDb.info_patch info

get_midi_instrument :: (M m) => Score.Attributes -> Score.Instrument
    -> m Instrument.Instrument
get_midi_instrument attrs inst = do
    lookup <- get_lookup_midi_instrument
    require_msg ("get_midi_instrument " ++ Pretty.pretty inst)
        (fst <$> lookup attrs inst)

get_lookup_scale :: (M m) => m Derive.LookupScale
get_lookup_scale = do
    LookupScale lookup_scale <- gets state_lookup_scale
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
    return $ maybe (InputNote.empty_state Config.control_pb_range) id
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

-- *** EditState

modify_edit_state :: (M m) => (EditState -> EditState) -> m ()
modify_edit_state f = modify $ \st -> st { state_edit = f (state_edit st) }

-- | At the Ui level, the edit box is per-block, but I use it to indicate edit
-- mode, which is global.  So it gets stored in Cmd.State and must be synced
-- with new blocks.
set_edit_box :: (M m) => Block.Box -> Block.Box -> m ()
set_edit_box skel track = do
    modify_edit_state $ \st -> st { state_edit_box = (skel, track) }
    block_ids <- State.get_all_block_ids
    forM_ block_ids $ \bid -> State.set_edit_box bid skel track

is_val_edit :: (M m) => m Bool
is_val_edit = (== ValEdit) <$> gets (state_edit_mode . state_edit)

is_kbd_entry :: (M m) => m Bool
is_kbd_entry = gets (state_kbd_entry . state_edit)

set_note_text :: (M m) => String -> m ()
set_note_text txt = do
    modify_edit_state $ \st -> st { state_note_text = txt }
    set_status Config.status_note_text (if null txt then Nothing else Just txt)


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
log_event :: BlockId -> TrackId -> Events.PosEvent -> String
log_event block_id track_id (pos, event) = "{s" ++ show frame ++ "}"
    where
    range = Just (pos, pos + Event.event_duration event)
    frame = Stack.unparse_ui_frame (block_id, Just track_id, range)

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (M m) => Maybe a -> m a
require = maybe abort return

-- | Like 'require', but throw an exception with the given msg.
require_msg :: (M m) => String -> Maybe a -> m a
require_msg msg = maybe (throw msg) return

require_right :: (M m) => (err -> String) -> Either err a -> m a
require_right str = either (throw . str) return

-- | Turn off all sounding notes and reset all controls.
all_notes_off :: (M m) => m ()
all_notes_off = do
    addrs <- concat . Map.elems <$> State.get_midi_alloc
    forM_ addrs $ \(dev, chan) -> do
        midi dev (Midi.ChannelMessage chan Midi.AllNotesOff)
        midi dev (Midi.ChannelMessage chan Midi.ResetAllControls)

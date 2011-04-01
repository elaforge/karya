{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{- | Core CmdT monad that cmds run in.

    Cmds should be in the monad @(Cmd.M m) => m ...@.

    They have to be polymorphic because they run in both IO and Identity.
    IO because some cmds such saving and loading files require IO, and Identity
    because the rest don't.  REPL cmds run in IO so they can load and save,
    and the result is that any cmd that wants to be used from both Identity
    cmds (bound to keystrokes) and the REPL must be polymorphic in the monad.

    Formerly this was @(Monad m) => CmdT m ...@, but with the upgrade to mtl2
    Functor would have to be added to the class context, but only some of the
    time.  Rather than deal such messiness, there's a class @Cmd.M@ that brings
    in Functor and Applicative as superclasses.

    It's all a bit messy and unpleasant and should be technically unnecessary
    since Identity monads should be able to run in IO anyway.  Other solutions:

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
module Cmd.Cmd where
import qualified Control.Applicative as Applicative
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State.Strict as MonadState
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Data.Generics as Generics
import qualified Data.Map as Map

import Util.Control
import qualified Util.Logger as Logger
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Midi.Midi as Midi

import qualified Cmd.InputNote as InputNote
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Transport as Transport
import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config

import qualified Derive.Scale.All as Scale.All
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch


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

-- | Cmds can run in either Identity or IO, but are generally returned in IO,
-- just to make things uniform.
type RunCmd cmd_m val_m a =
    State.State -> State -> CmdT cmd_m a -> val_m (CmdVal a)

-- | The result of running a Cmd.
type CmdVal a = (State, [MidiThru], [Log.Msg],
    Either State.StateError (a, State.State, [Update.Update]))

run :: (Monad m) => a -> RunCmd m m a
run abort_val ustate cstate cmd = do
    (((ui_result, cstate2), midi), logs) <-
        (Log.run . Logger.run . flip MonadState.runStateT cstate
            . State.run ustate . run_cmd_t)
        cmd
    -- An Abort is just like if the cmd immediately returned Continue, except
    -- that log msgs are kept.  Normally 'abort_val' will be Continue, but
    -- obviously if 'cmd' doesn't return Status it can't be.
    return $ case ui_result of
        Left State.Abort -> (cstate, [], logs, Right (abort_val, ustate, []))
        _ -> (cstate2, midi, logs, ui_result)

-- | Run the given command in Identity, but return it in IO, just as
-- a convenient way to have a uniform return type with 'run' (provided it is
-- run in IO).
run_id_io :: RunCmd Identity.Identity IO Status
run_id_io ui_state cmd_state cmd = do
    return $ Identity.runIdentity (run Continue ui_state cmd_state cmd)

run_io :: RunCmd IO IO Status
run_io = run Continue

-- | Run the Cmd in Identity, returning Nothing if it aborted.
run_id :: State.State -> State -> CmdT Identity.Identity a -> CmdVal (Maybe a)
run_id ui_state cmd_state cmd =
    Identity.runIdentity (run Nothing ui_state cmd_state (fmap Just cmd))

-- | Quit is not exported, so that only 'cmd_quit' here has permission to
-- return it.
data Status = Done | Continue | Quit deriving (Eq, Show, Generics.Typeable)

-- * CmdT and operations

type CmdStack m = State.StateT
    (MonadState.StateT State
        (Logger.LoggerT MidiThru
            (Log.LogT m)))

newtype CmdT m a = CmdT (CmdStack m a)
    deriving (Functor, Monad, Trans.MonadIO, Error.MonadError State.StateError)
run_cmd_t (CmdT x) = x

class (Log.LogMonad m, State.M m) => M m where
    get_state :: m State
    put_state :: State -> m ()
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
    get_state = (CmdT . lift) MonadState.get
    put_state st = (CmdT . lift) (MonadState.put st)
    midi dev msg = (CmdT . lift . lift) (Logger.log (dev, msg))
    abort = Error.throwError State.Abort
    catch_abort m = Error.catchError (fmap Just m) catch
        where
        catch State.Abort = return Nothing
        catch err = Error.throwError err

-- | For some reason, newtype deriving doesn't work on MonadTrans.
instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift -- whee!!

-- Give CmdT unlifted access to all the logging functions.
instance (Monad m) => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . Log.write

-- And to the UI state operations.
instance (Functor m, Monad m) => State.M (CmdT m) where
    get = CmdT State.get
    put st = CmdT (State.put st)
    update upd = CmdT (State.update upd)
    throw msg = CmdT (State.throw msg)

instance (Functor m, Monad m) => Applicative.Applicative (CmdT m) where
    pure = return
    (<*>) = ap

type MidiThru = (Midi.WriteDevice, Midi.Message)

is_abort :: State.StateError -> Bool
is_abort State.Abort = True
is_abort _ = False

-- | This is the same as State.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (M m) => String -> m a
throw = State.throw

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (M m) => Maybe a -> m a
require = maybe abort return

-- | Like 'require', but throw an exception with the given msg.
require_msg :: (M m) => String -> Maybe a -> m a
require_msg msg = maybe (throw msg) return

-- | Log an event so that it can be clicked on in logview.
log_event :: BlockId -> TrackId -> Track.PosEvent -> String
log_event block_id track_id (pos, event) = "{s" ++ show frame ++ "}"
    where
    range = Just (pos, pos + Event.event_duration event)
    frame = Stack.unparse_ui_frame (block_id, Just track_id, range)

-- * State

-- | App global state.  Unlike Ui.State, this is not saved to disk.
-- TODO break this up into a couple sections
data State = State {
    -- Config type variables that change never or rarely.  These come from the
    -- static config.
    state_instrument_db :: Instrument.Db.Db
    , state_schema_map :: SchemaMap
    -- | Global namespace for deriver.
    , state_global_scope :: Derive.Scope
    -- | Turn ScaleIds into Scales.
    , state_lookup_scale :: LookupScale
    -- | Copies by default go to a block+tracks with this project.
    , state_clip_namespace :: Id.Namespace

    -- Automatically maintained state.  This means only a few cmds should
    -- modify these.

    -- | History.
    , state_history :: ([HistoryEntry], [HistoryEntry])
    -- | Set to True to disable history recording.  Useful so undo and
    -- save/load cmds aren't recorded.  TODO should go in cmd return val.
    , state_skip_history_record :: Bool

    -- Playing and derivation.

    -- | Transport control channel for the player, if one is running.
    , state_play_control :: Maybe Transport.PlayControl
    -- | As soon as any event changes are made to a block, its performance is
    -- recalculated (in the background) and stored here, so play can be started
    -- without latency.
    , state_performance_threads :: Map.Map BlockId PerformanceThread

    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position, the value has complete info.
    , state_keys_down :: Map.Map Modifier Modifier
    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_focused_view :: Maybe ViewId

    -- | This is similar to 'Ui.Block.view_status', except that it's global
    -- instead of per-view.  So changes are logged with a special prefix so
    -- logview can catch them.  Really I only need this map to suppress log
    -- spam.
    , state_global_status :: Map.Map String String

    -- External device tracking.
    -- | Midi state of WriteDevices.
    , state_wdev_state :: WriteDeviceState
    -- | Midi state of ReadDevices, including configuration like pitch bend
    -- range.
    , state_rdev_state :: ReadDeviceState

    , state_edit :: EditState

    -- | Some play commands can start playing from a short distance before the
    -- cursor.
    , state_play_step :: TimeStep.TimeStep
    } deriving (Show, Generics.Typeable)

initial_state inst_db schema_map global_scope = State {
    state_instrument_db = inst_db
    , state_schema_map = schema_map
    , state_global_scope = global_scope
    -- TODO later this should also be merged with static config
    , state_lookup_scale = LookupScale $
        \scale_id -> Map.lookup scale_id Scale.All.scales
    , state_clip_namespace = Config.clip_namespace

    , state_history = ([], [])
    , state_skip_history_record = False

    , state_play_control = Nothing
    , state_performance_threads = Map.empty

    , state_keys_down = Map.empty
    , state_focused_view = Nothing
    , state_global_status = Map.empty

    , state_wdev_state = empty_wdev_state
    , state_rdev_state = Map.empty

    , state_edit = empty_edit_state

    , state_play_step =
        TimeStep.RelativeMark TimeStep.AllMarklists (TimeStep.MatchRank 1 0)
    }

empty_state :: State
empty_state = initial_state Instrument.Db.empty Map.empty Derive.empty_scope

-- | Reset the parts of the State which are specific to a \"session\".  This
-- should be called whenever an entirely new state is loaded.
reinit_state :: State -> State
reinit_state cstate = cstate
    { state_history = ([], [])
    -- TODO kill performance threads
    , state_performance_threads = Map.empty
    -- This is essential, otherwise lots of cmds break on the bad reference.
    , state_focused_view = Nothing
    , state_edit = empty_edit_state
    }

-- | This is a hack so I can use the default Show instance for 'State'.
newtype LookupScale = LookupScale Derive.LookupScale
instance Show LookupScale where
    show _ = "((LookupScale))"

-- | Editing state, modified in the course of editing.
data EditState = EditState {
    -- | Edit mode enables various commands that write to tracks.
    state_edit_mode :: EditMode
    -- | Use the alphanumeric keys to enter notes instead of midi input.
    , state_kbd_entry :: Bool
    -- | Default time step for cursor movement.
    , state_step :: TimeStep.TimeStep
    -- | Used for note duration.  It's separate from 'state_step' to allow
    -- for tracker-style note entry where newly entered notes extend to the
    -- next note or the end of the block.
    , state_note_duration :: TimeStep.TimeStep
    -- | If this is Rewind, create notes with negative durations.
    , state_note_direction :: TimeStep.Direction
    -- | Transpose note entry on the keyboard by this many octaves.  It's by
    -- octave instead of scale degree since scales may have different numbers
    -- of notes per octave.
    , state_kbd_entry_octave :: Pitch.Octave

    -- | See 'set_edit_box'.
    , state_edit_box :: (Color.Color, Char)
    } deriving (Show, Generics.Typeable)

empty_edit_state :: EditState
empty_edit_state = EditState {
    state_edit_mode = NoEdit
    , state_kbd_entry = False
    , state_step =
        TimeStep.AbsoluteMark TimeStep.AllMarklists (TimeStep.MatchRank 3 0)
    , state_note_duration = TimeStep.BlockEnd
    , state_note_direction = TimeStep.Advance
    -- This should put middle C in the center of the kbd entry keys.
    , state_kbd_entry_octave = 4

    , state_edit_box = Config.bconfig_track_box
    }

data WriteDeviceState = WriteDeviceState {
    -- Used by Cmd.MidiThru:
    -- | Last pb val for each Addr.
    wdev_pb :: Map.Map Instrument.Addr Midi.PitchBendValue
    -- | NoteId currently playing in each Addr.  An Addr may have >1 NoteId.
    , wdev_note_addr :: Map.Map InputNote.NoteId Instrument.Addr
    -- | The note id is not guaranteed to have any relationship to the key,
    -- so the MIDI NoteOff needs to know what key the MIDI NoteOn used.
    , wdev_note_key :: Map.Map InputNote.NoteId Midi.Key
    -- | Map an addr to a number that increases when it's assigned a note.
    -- This is used along with 'wdev_serial' to implement addr round-robin.
    , wdev_addr_serial :: Map.Map Instrument.Addr Integer
    , wdev_serial :: Integer

    -- Used by Cmd.PitchTrack:
    -- | NoteIds being entered into which pitch tracks.  When entering a chord,
    -- a PitchChange uses this to know which pitch track to update.
    , wdev_note_track :: Map.Map InputNote.NoteId (BlockId, TrackNum)

    -- Used by no one, yet:  (TODO should someone use this?)
    -- | Remember the current inst of each addr.  More than one instrument or
    -- keyswitch can share the same addr, so I need to keep track which one is
    -- active to minimize switches.
    , wdev_addr_inst :: Map.Map Instrument.Addr Instrument.Instrument
    } deriving (Eq, Show, Generics.Typeable)

empty_wdev_state :: WriteDeviceState
empty_wdev_state = WriteDeviceState
    Map.empty Map.empty Map.empty Map.empty 0 Map.empty Map.empty

type ReadDeviceState = Map.Map Midi.ReadDevice InputNote.ControlState

-- | This holds the final performance for a given block.  It is used to
-- actually play music, and poked and prodded in a separate thread to control
-- its evaluation.
data Performance = Performance {
    perf_derive_cache :: Derive.Cache
    , perf_events :: Derive.Events
    , perf_track_environ :: Derive.TrackEnviron
    -- | Score damage on top of the Performance, used by the derive cache.
    -- This is empty when the Performance is first created and collects
    -- thereafter.
    , perf_score_damage :: Derive.ScoreDamage

    , perf_tempo :: Transport.TempoFunction
    , perf_closest_warp :: Transport.ClosestWarpFunction
    , perf_inv_tempo :: Transport.InverseTempoFunction
    , perf_track_signals :: Track.TrackSignals
    }

instance Show Performance where
    show perf = "((Performance " ++ Pretty.pretty len ++ "))"
        where len = Derive.cache_size (perf_derive_cache perf)

data PerformanceThread = PerformanceThread {
    pthread_perf :: Performance
    , pthread_id :: Concurrent.ThreadId
    }

instance Show PerformanceThread where
    show (PerformanceThread perf th_id) =
        "((PerformanceThread " ++ show th_id ++ " perf " ++ show perf ++ "))"

data HistoryEntry = HistoryEntry {
    hist_name :: String
    , hist_state :: State.State
    } deriving (Show, Generics.Typeable)

-- | These enable various commands to edit event text.  What exactly val,
-- and method mean are dependent on the schema, but I expect the definitions
-- in Cmd.NoteTrack and Cmd.ControlTrack will be universal.
data EditMode = NoEdit | RawEdit | ValEdit | MethodEdit deriving (Eq, Show)

data Modifier = KeyMod Key.Key
    -- | Mouse button, and (tracknum, pos) in went down at, if any.
    -- The block is not recorded.  You can't drag across blocks so you know any
    -- click must apply to the focused block.
    | MouseMod UiMsg.MouseButton (Maybe (TrackNum, ScoreTime))
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the device, this code doesn't know which devices are available.
    -- Block or track level handlers can query the device themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)

mouse_mod_btn (MouseMod btn _) = Just btn
mouse_mod_btn _ = Nothing

-- ** state access

gets :: (M m) => (State -> a) -> m a
gets f = f <$> get_state

modify_state :: (M m) => (State -> State) -> m ()
modify_state f = do
    st <- get_state
    put_state $! f st

modify_edit_state :: (M m) => (EditState -> EditState) -> m ()
modify_edit_state f =
    modify_state $ \st -> st { state_edit = f (state_edit st) }

lookup_pthread :: (M m) => BlockId -> m (Maybe PerformanceThread)
lookup_pthread block_id = Map.lookup block_id <$> gets state_performance_threads

lookup_performance :: (M m) => BlockId -> m (Maybe Performance)
lookup_performance block_id = fmap (fmap pthread_perf) (lookup_pthread block_id)

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
get_current_step = gets (state_step . state_edit)

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
        modify_state $ \st ->
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

lookup_instrument_info :: (M m) => Score.Instrument -> m (Maybe MidiDb.Info)
lookup_instrument_info inst = do
    inst_db <- gets state_instrument_db
    return $ Instrument.Db.db_lookup inst_db inst

get_schema_map :: (M m) => m SchemaMap
get_schema_map = gets state_schema_map

get_clip_namespace :: (M m) => m Id.Namespace
get_clip_namespace = gets state_clip_namespace

set_clip_namespace :: (M m) => Id.Namespace -> m ()
set_clip_namespace ns = modify_state $ \st -> st { state_clip_namespace = ns }

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
    st <- get_state
    put_state $ st { state_rdev_state =
        Map.insert rdev state (state_rdev_state st) }

set_pitch_bend_range :: (M m) => Control.PbRange -> Midi.ReadDevice -> m ()
set_pitch_bend_range range rdev = do
    state <- get_rdev_state rdev
    set_rdev_state rdev (state { InputNote.state_pb_range = range })

get_wdev_state :: (M m) => m WriteDeviceState
get_wdev_state = gets state_wdev_state

set_wdev_state :: (M m) => WriteDeviceState -> m ()
set_wdev_state wdev_state =
    modify_state $ \st -> st { state_wdev_state = wdev_state }

-- | At the Ui level, the edit box is per-block, but I use it to indicate edit
-- mode, which is global.  So it gets stored in Cmd.State and must be synced
-- with new blocks.
set_edit_box :: (M m) => Color.Color -> Char -> m ()
set_edit_box color char = do
    modify_edit_state $ \st -> st { state_edit_box = (color, char) }
    block_ids <- State.get_all_block_ids
    forM_ block_ids $ \bid -> State.set_edit_box bid color char

create_block :: (M m) => Id.Id -> String -> [Block.BlockTrack] -> m BlockId
create_block block_id title tracks = do
    config <- block_config
    -- TODO get a default schema?
    State.create_block block_id (Block.block config title tracks Config.schema)

block_config :: (M m) => m Block.Config
block_config = do
    track_box <- gets (state_edit_box . state_edit)
    return $ Block.Config Config.bconfig_selection_colors
        Config.bconfig_bg_color track_box Config.bconfig_sb_box

-- ** environ

get_scale_id :: (M m) => BlockId -> TrackId -> m Pitch.ScaleId
get_scale_id block_id track_id = do
    scale <- lookup_env block_id track_id TrackLang.v_scale
    case scale of
        Just (TrackLang.VScaleId scale_id) -> return scale_id
        _ -> State.get_default_scale

lookup_instrument :: (M m) => BlockId -> TrackId -> m (Maybe Score.Instrument)
lookup_instrument block_id track_id = do
    scale <- lookup_env block_id track_id TrackLang.v_instrument
    case scale of
        Just (TrackLang.VInstrument inst) -> return $ Just inst
        _ -> State.gets State.state_default_inst

-- | Lookup value from the deriver's Environ at the given block and track.
-- See 'Derive.TrackEnviron' for details on the limitations here.
--
-- The lookup is done relative to the root block, which means that instruments
-- and scales always default relative to the root.  I suppose I could think of
-- some case where it would be better to look it up relative to some other
-- block, but that seems way too complicated.  This means that the
-- TrackEnvirons from other block derivations are never used.  That leads to
-- a certain amount of void allocation, so maybe I should include a flag to
-- turn off TrackEnviron recording?
lookup_env :: (M m) => BlockId -> TrackId -> TrackLang.ValName
    -> m (Maybe TrackLang.Val)
lookup_env block_id track_id name = do
    justm State.lookup_root_id $ \root_id -> do
    justm (lookup_performance root_id) $ \perf -> do
    let track_env = perf_track_environ perf
    return $ Map.lookup name =<< Map.lookup (block_id, track_id) track_env


-- * basic cmds

-- | Quit the app immediately.
cmd_quit :: CmdId Status
cmd_quit = return Quit

-- | Log incoming msgs.
cmd_log :: Cmd
cmd_log msg = do
    Log.debug ("msg: " ++ show msg)
    return Continue

-- | Record keydowns into the 'State' modifier map.
cmd_record_keys :: Cmd
cmd_record_keys msg = do
    case msg_to_mod msg of
        Nothing -> return ()
        Just (True, mod) -> insert_mod mod
        Just (False, mod) -> delete_mod mod
    return Continue
    where
    insert_mod mod = do
        let key = strip_modifier mod
        mods <- keys_down
        when (key `Map.member` mods) $
            Log.warn $ "keydown for " ++ show mod ++ " already in modifiers"
        modify_keys (Map.insert key mod)
        -- mods <- keys_down
        -- Log.debug $ "keydown " ++ show (Map.elems mods)
    delete_mod mod = do
        let key = strip_modifier mod
        mods <- keys_down
        when (key `Map.notMember` mods) $
            Log.warn $ "keyup for " ++ show mod ++ " not in modifiers"
        modify_keys (Map.delete key)
        -- mods <- keys_down
        -- Log.debug $ "keyup " ++ show (Map.elems mods)
    modify_keys f = modify_state $ \st ->
        st { state_keys_down = f (state_keys_down st) }

-- | Take a modifier to its key in the modifier map which has extra info like
-- mouse down position stripped.
strip_modifier :: Modifier -> Modifier
strip_modifier (MouseMod btn _) = MouseMod btn Nothing
strip_modifier mod = mod

modifier_key :: Modifier -> Maybe Char
modifier_key (KeyMod (Key.KeyChar c)) = Just c
modifier_key _ = Nothing

-- | Convert a Msg to (is_key_down, Modifier).
msg_to_mod :: Msg.Msg -> Maybe (Bool, Modifier)
msg_to_mod msg = case msg of
    Msg.Ui (UiMsg.UiMsg context (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd state key -> case state of
            UiMsg.KeyDown -> Just (True, KeyMod key)
            UiMsg.KeyUp -> Just (False, KeyMod key)
        UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseDown btn } ->
            Just (True, MouseMod btn (mouse_context context))
        UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseUp btn } ->
            Just (False, MouseMod btn (mouse_context context))
        _ -> Nothing
    Msg.Midi (Midi.ReadMessage { Midi.rmsg_msg = msg }) -> case msg of
        Midi.ChannelMessage chan (Midi.NoteOn key _vel) ->
            Just (True, MidiMod chan key)
        Midi.ChannelMessage chan (Midi.NoteOff key _vel) ->
            Just (False, MidiMod chan key)
        _ -> Nothing
    _ -> Nothing
    where
    mouse_context (UiMsg.Context
        { UiMsg.ctx_track = Just n, UiMsg.ctx_pos = Just pos }) = Just (n, pos)
    mouse_context _ = Nothing


-- | Keep 'state_focused_view' up to date.
cmd_record_active :: Cmd
cmd_record_active msg = case msg of
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_block = Just view_id })
        msg) | not (is_up msg) -> do
            set_focused_view view_id
            return $ case msg of
               UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus) -> Done
               _ -> Continue
    _ -> return Continue
    where
    -- If mouse ups and key ups set focus then the key up after creating a new
    -- view will put the focus back on the old one.
    is_up (UiMsg.MsgEvent (UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseUp _ }))
        = True
    is_up (UiMsg.MsgEvent (UiMsg.Kbd UiMsg.KeyUp _)) = True
    is_up _ = False

set_focused_view :: (M m) => ViewId -> m ()
set_focused_view view_id = do
    -- Log.debug $ "active view is " ++ show view_id
    modify_state $ \st -> st { state_focused_view = Just view_id }

-- Responds to the UI's request to close a window.
cmd_close_window :: Cmd
cmd_close_window (Msg.Ui (UiMsg.UiMsg
        (UiMsg.Context { UiMsg.ctx_block = Just view_id }) UiMsg.MsgClose)) =
    State.destroy_view view_id >> return Done
cmd_close_window _ = return Continue

-- | Catch 'UiMsg.UiUpdate's from the UI, and modify the state accordingly to
-- reflect the UI state.
--
-- Unlike all the other Cmds, the state changes this makes are not synced.
-- UiUpdates report changes that have already occurred directly on the UI, so
-- syncing them would be redundant.
cmd_record_ui_updates :: Cmd
cmd_record_ui_updates msg = do
    (ctx, update) <- require (update_of msg)
    ui_update ctx update
    -- return Continue to give 'cmd_update_ui_state' a crack at it
    return Continue

ui_update :: UiMsg.Context -> UiMsg.UiUpdate -> CmdT Identity.Identity ()
ui_update ctx@(UiMsg.Context (Just view_id) track _pos) update = case update of
    UiMsg.UpdateTrackScroll hpos -> State.set_track_scroll view_id hpos
    UiMsg.UpdateZoom zoom -> State.set_zoom view_id zoom
    UiMsg.UpdateViewResize rect track_size -> do
        view <- State.get_view view_id
        when (rect /= Block.view_rect view) $
            State.set_view_rect view_id rect
        when (track_size /= view_track_size view) $
            State.set_track_size view_id track_size
    UiMsg.UpdateTrackWidth width -> case track of
        Just tracknum -> State.set_track_width view_id tracknum width
        Nothing -> State.throw $ show update ++ " with no track: " ++ show ctx
    _ -> return ()
    where
    view_track_size v = (Block.view_visible_track v, Block.view_visible_time v)
ui_update ctx update =
    State.throw $ show update ++ " with no view_id: " ++ show ctx

-- | Except when it's a block update, I have to update the block to update
-- the other views.  So this Cmd goes in with the normal Cmds.
cmd_update_ui_state :: Cmd
cmd_update_ui_state msg = do
    (ctx, update) <- require (update_of msg)
    ui_update_state ctx update
    return Done

sync_zoom_status :: (M m) => ViewId -> m ()
sync_zoom_status view_id = do
    view <- State.get_view view_id
    set_view_status view_id "view"
        (Just (show_zoom_status (Block.view_zoom view)))

show_zoom_status :: Types.Zoom -> String
show_zoom_status (Types.Zoom offset factor) =
    '+' : Pretty.show_float (Just 3) offset
    ++ '*' : Pretty.show_float (Just 1) factor

ui_update_state :: UiMsg.Context -> UiMsg.UiUpdate -> CmdT Identity.Identity ()
ui_update_state ctx@(UiMsg.Context (Just view_id) _track _pos) update =
    case update of
        UiMsg.UpdateInput text -> do
            view <- State.get_view view_id
            update_input ctx (Block.view_block view) text
        -- UiMsg.UpdateTrackScroll hpos -> sync_zoom_status view_id
        UiMsg.UpdateZoom _zoom -> sync_zoom_status view_id
        _ -> return ()
ui_update_state ctx update =
    State.throw $ show update ++ " with no view_id: " ++ show ctx

update_input ctx block_id text = case UiMsg.ctx_track ctx of
    Just tracknum -> do
        track_id <- State.event_track_at block_id tracknum
        case track_id of
            Just track_id -> State.set_track_title track_id text
            Nothing -> State.throw $ show (UiMsg.UpdateInput text) ++ " for "
                ++ show ctx ++ " on non-event track " ++ show tracknum
    Nothing -> State.set_block_title block_id text

update_of (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate update))) =
    Just (ctx, update)
update_of _ = Nothing

-- * schema types

-- $schema_doc
-- These types should be in Derive.Schema, but since they use Cmd and I need
-- Cmd.State to have a SchemaMap, I have to put the types here to avoid
-- a circular import.  They are re-exported by Derive.Schema so we can all just
-- pretend they were defined there in the first place.

type SchemaMap = Map.Map SchemaId Schema

-- | A Schema attaches a number of things to a Block.
data Schema = Schema {
    schema_deriver :: SchemaDeriver Derive.EventDeriver
    -- | Get a set of Cmds that are applicable within the given CmdContext.
    , schema_cmds :: CmdContext -> ContextCmds
    }

-- | (cmds to be run, warnings)
type ContextCmds = ([Cmd], [String])

-- | So Cmd.State can be showable, for debugging.
instance Show Schema where
    show _ = "((Schema))"

-- | A SchemaDeriver generates a Deriver from a given Block.
type SchemaDeriver d = BlockId -> State.StateId d

-- ** cmd types

-- | Information needed to decide what cmds should apply.
data CmdContext = CmdContext {
    ctx_default_inst :: Maybe Score.Instrument
    , ctx_inst_addr :: Score.Instrument -> Maybe Instrument.Addr
    , ctx_lookup_midi :: MidiDb.LookupMidiInstrument
    , ctx_edit_mode :: EditMode
    , ctx_kbd_entry :: Bool
    , ctx_focused_tracknum :: Maybe TrackNum
    , ctx_track_tree :: State.TrackTree
    }

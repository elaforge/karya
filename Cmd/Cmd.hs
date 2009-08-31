{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Cmd.Cmd where

import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
import qualified Data.Generics as Generics
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Text.Printf

import qualified Util.Logger as Logger
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Update as Update

import qualified Midi.Midi as Midi

import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.Transport as Transport
import qualified Instrument.Db
import qualified Instrument.MidiDb as MidiDb

import qualified App.Config as Config

import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Score as Score
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch


-- | This makes Cmds more specific than they have to be, and doesn't let them
-- run in other monads like IO.  It's unlikely to become a problem, but if it
-- does, I'll have to stop using these aliases.
type Cmd = Msg.Msg -> CmdId
type CmdM m = CmdT m Status
type CmdIO = CmdM IO
type CmdId = CmdM Identity.Identity

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
run abort_val ui_state cmd_state cmd = do
    (res, logs) <- (Log.run . Error.runErrorT . Logger.run
        . flip MonadState.runStateT cmd_state . State.run ui_state . run_cmd_t)
        cmd
    -- An Abort is just like if the cmd immediately returned Continue, except
    -- that log msgs are kept.  Normally 'abort_val' will be Continue, but
    -- obviously if 'cmd' doesn't return Status it can't be.
    return $ case res of
        Left Abort -> (cmd_state, [], logs, Right (abort_val, ui_state, []))
        Right ((ui_res, cmd_state2), midi) -> (cmd_state2, midi, logs, ui_res)

-- | Run the given command in Identity, but return it in IO, just as
-- a convenient way to have a uniform return type with 'run' (provided its run
-- in IO).
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
            (Error.ErrorT Abort
                (Log.LogT m))))

data Abort = Abort deriving (Show)
instance Error.Error Abort where
    noMsg = Abort

newtype CmdT m a = CmdT (CmdStack m a)
    deriving (Functor, Monad, Trans.MonadIO)
run_cmd_t (CmdT x) = x

instance Trans.MonadTrans CmdT where
    lift = CmdT . lift . lift . lift . lift . lift -- whee!!

-- Give CmdT unlifted access to all the logging functions.
instance Monad m => Log.LogMonad (CmdT m) where
    write = CmdT . lift . lift . lift . lift . Log.write

-- And to the UI state operations.
instance Monad m => State.UiStateMonad (CmdT m) where
    get = CmdT State.get
    put st = CmdT (State.put st)
    modify f = CmdT (State.modify f)
    update upd = CmdT (State.update upd)
    throw msg = CmdT (State.throw msg)

type MidiThru = (Midi.WriteDevice, Midi.Message)

-- | Log some midi to send out immediately.  This is the midi thru mechanism.
midi :: (Monad m) => Midi.WriteDevice -> Midi.Message -> CmdT m ()
midi dev msg = (CmdT . lift . lift) (Logger.record (dev, msg))

-- | An abort is an exception to get out of CmdT, but it's considered the same
-- as returning Continue.  It's so a command can back out if e.g. it's selected
-- by the 'Keymap' but has an additional prerequisite such as having an active
-- block.
abort :: (Monad m) => CmdT m a
abort = (CmdT . lift . lift . lift) (Error.throwError Abort)

-- | This is the same as State.throw, but it feels like things in Cmd may not
-- always want to reuse State's exceptions, so they should call this one.
throw :: (Monad m) => String -> CmdT m a
throw = State.throw

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (Monad m) => Maybe a -> CmdT m a
require = maybe abort return

-- | Like 'require', but throw an exception with the given msg.
require_msg :: (Monad m) => String -> Maybe a -> CmdT m a
require_msg msg = maybe (State.throw msg) return

-- * State

-- | App global state.  Unlike Ui.State, this is not saved to disk.
-- TODO break this up into a couple sections
data State = State {
    state_history :: ([HistoryEntry], [HistoryEntry])
    -- | Set to True to disable history recording.  Useful so undo and
    -- save/load cmds aren't recorded.  TODO should go in cmd return val.
    , state_skip_history_record :: Bool
    , state_instrument_db :: Instrument.Db.Db
    , state_schema_map :: SchemaMap

    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position, the value has complete info.
    , state_keys_down :: Map.Map Modifier Modifier
    -- | Transport control channel for the player, if one is running.
    , state_play_control :: Maybe Transport.PlayControl
    -- | As soon as any event changes are made to a block, its performance is
    -- recalculated (in the background) and stored here, so play can be started
    -- without latency.
    , state_performance :: Map.Map Block.BlockId Performance
    -- | IDs of background derivation threads, so they can be killed if
    -- a new derivation is needed before they finish.
    , state_derive_threads :: Map.Map Block.BlockId Concurrent.ThreadId
    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_focused_view :: Maybe Block.ViewId

    -- | Copies by default go to a block+tracks with this project.
    , state_clip_namespace :: Id.Namespace
    -- | Running midi state.
    , state_chan_map :: Instrument.ChannelMap

    -- Editing state

    -- | Edit mode enables various commands that write to tracks.
    , state_edit_mode :: EditMode
    -- | Use the alphanumeric keys to enter notes instead of midi input.
    , state_kbd_entry :: Bool
    -- | Default time step.  Used for cursor movement, note duration, and
    -- whatever else.
    , state_step :: TimeStep.TimeStep
    -- | Transpose note entry on the keyboard by this many octaves.  It's by
    -- octave instead of scale degree since scales may have different numbers
    -- of notes per octave.
    , state_kbd_entry_octave :: Octave
    } deriving (Show, Generics.Typeable)

initial_state inst_db schema_map = State {
    state_history = ([], [])
    , state_skip_history_record = False
    , state_instrument_db = inst_db
    , state_schema_map = schema_map
    , state_keys_down = Map.empty
    , state_play_control = Nothing
    , state_performance = Map.empty
    , state_derive_threads = Map.empty
    , state_focused_view = Nothing
    , state_clip_namespace = Config.clip_namespace
    , state_chan_map = Map.empty

    , state_edit_mode = NoEdit
    , state_kbd_entry = False
    , state_step =
        TimeStep.UntilMark TimeStep.AllMarklists (TimeStep.MatchRank 2)
    -- This should put middle C in the center of the kbd entry keys.
    , state_kbd_entry_octave = 4
    }

empty_state = initial_state Instrument.Db.empty Map.empty
clear_history cmd_state = cmd_state { state_history = ([], []) }

type Octave = Int

data Performance = Performance {
    perf_msgs :: [Midi.WriteMessage]
    , perf_logs :: [Log.Msg]
    , perf_tempo :: Transport.TempoFunction
    , perf_inv_tempo :: Transport.InverseTempoFunction
    }
instance Show Performance where
    show perf = "<Performance: " ++ show (take 10 (perf_msgs perf))
        ++ ", " ++ show (take 10 (perf_logs perf)) ++ ">"

data HistoryEntry = HistoryEntry {
    hist_name :: String
    , hist_state :: State.State
    } deriving (Show, Generics.Typeable)

-- | These enable various commands to edit event text.  What exactly val,
-- and method mean are dependent on the schema, but I expect the definitions
-- in Cmd.NoteTrack and Cmd.ControllerTrack will be universal.
data EditMode = NoEdit | RawEdit | ValEdit | MethodEdit deriving (Eq, Show)

data Modifier = KeyMod Key.Key
    -- | Mouse button, and (tracknum, pos) in went down at, if any.
    -- The block is not recorded.  You can't drag across blocks so you know any
    -- click must apply to the focused block.
    | MouseMod UiMsg.MouseButton (Maybe (Block.TrackNum, TrackPos))
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the device, this code doesn't know which devices are available.
    -- Block or track level handlers can query the device themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show, Read)

mouse_mod_btn (MouseMod btn _) = Just btn
mouse_mod_btn _ = Nothing

-- ** state access

get_state :: (Monad m) => CmdT m State
get_state = (CmdT . lift) MonadState.get

put_state :: (Monad m) => State -> CmdT m ()
put_state st = (CmdT . lift) (MonadState.put st)

modify_state :: (Monad m) => (State -> State) -> CmdT m ()
modify_state f = (CmdT . lift) (MonadState.modify f)

-- | Keys currently held down, as in 'state_keys_down'.
keys_down :: (Monad m) => CmdT m (Map.Map Modifier Modifier)
keys_down = fmap state_keys_down get_state

get_focused_view :: (Monad m) => CmdT m Block.ViewId
get_focused_view = fmap state_focused_view get_state >>= require

get_focused_block :: (Monad m) => CmdT m Block.BlockId
get_focused_block =
    fmap Block.view_block (get_focused_view >>= State.get_view)

get_current_step :: (Monad m) => CmdT m TimeStep.TimeStep
get_current_step = fmap state_step get_state

-- | Get the leftmost track covered by the insert selection, which is
-- considered the "focused" track by convention.
get_insert_tracknum :: (Monad m) => CmdT m (Maybe Block.TrackNum)
get_insert_tracknum = do
    view_id <- get_focused_view
    sel <- State.get_selection view_id Config.insert_selnum
    return (fmap Block.sel_start_track sel)

-- | This just calls 'State.set_view_status', but all status setting should
-- go through here so they can be uniformly filtered or logged or something.
set_view_status :: (Monad m) => Block.ViewId -> String -> Maybe String
    -> CmdT m ()
set_view_status view_id key val = State.set_view_status view_id key val

-- | Set a status variable on all views.
set_status :: (Monad m) => String -> Maybe String -> CmdT m ()
set_status key val = do
    view_ids <- fmap (Map.keys . State.state_views) State.get
    forM_ view_ids $ \view_id -> set_view_status view_id key val

get_lookup_midi_instrument :: (Monad m) => CmdT m MidiDb.LookupMidiInstrument
get_lookup_midi_instrument =
    fmap (Instrument.Db.db_lookup_midi . state_instrument_db) get_state

lookup_instrument_info :: (Monad m) => Score.Instrument
    -> CmdT m (Maybe MidiDb.Info)
lookup_instrument_info inst = do
    inst_db <- fmap state_instrument_db get_state
    return $ Instrument.Db.db_lookup inst_db inst

get_schema_map :: (Monad m) => CmdT m SchemaMap
get_schema_map = fmap state_schema_map get_state

get_clip_namespace :: (Monad m) => CmdT m Id.Namespace
get_clip_namespace = fmap state_clip_namespace get_state
set_clip_namespace :: (Monad m) => Id.Namespace -> CmdT m ()
set_clip_namespace ns = modify_state $ \st -> st { state_clip_namespace = ns }

-- | Set State.state_midi_config, but also update the state_chan_map.  This
-- assumes that the instrument is already initialized on its addresses, which
-- should be done explicitly with 'send_initialization'.
set_midi_config :: (Monad m) => Instrument.Config -> CmdT m ()
set_midi_config new_config = do
    old_config <- State.get_midi_config
    lookup_inst <- get_lookup_midi_instrument
    let alloc = Instrument.config_alloc
        new_allocs = alloc new_config `Map.difference` alloc old_config
        (new_map, failed) = inst_addr_to_chan_map lookup_inst new_allocs
    if not (null failed)
        then Log.warn $ "inst lookup failed for: " ++ show failed
        else do
            modify_state $ \st -> st
                { state_chan_map = Map.union new_map (state_chan_map st) }
            State.set_midi_config new_config

inst_addr_to_chan_map :: MidiDb.LookupMidiInstrument
    -> Map.Map Score.Instrument [Instrument.Addr]
    -> (Instrument.ChannelMap, [Score.Instrument])
inst_addr_to_chan_map lookup_inst inst_addr = (chan_map, failed)
    where
    insts = [(lookup_inst Score.no_attrs score_inst, score_inst, addrs)
        | (score_inst, addrs) <- Map.toList inst_addr]
    chan_map = Map.fromList [(addr, inst)
        | (Just inst, _, addrs) <- insts, addr <- addrs]
    failed = [inst | (Nothing, inst, _) <- insts]

-- | Lookup a scale_id or throw.
-- TODO merge in the static config scales.
get_scale :: (Monad m) => String -> Pitch.ScaleId -> CmdT m Pitch.Scale
get_scale caller scale_id = maybe
    (throw (caller ++ ": unknown " ++ show scale_id)) return
    (Map.lookup scale_id Scale.scale_map)

-- * basic cmds

-- | Quit the app immediately.
cmd_quit :: CmdId
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
        msg) -> do
            set_focused_view view_id
            return $ case msg of
               UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus) -> Done
               _ -> Continue
    _ -> return Continue

set_focused_view :: (Monad m) => Block.ViewId -> CmdT m ()
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

sync_zoom_status :: (Monad m) => Block.ViewId -> CmdT m ()
sync_zoom_status view_id = do
    view <- State.get_view view_id
    set_view_status view_id "view"
        (Just (show_zoom_status (Block.view_zoom view)))

show_zoom_status :: Block.Zoom -> String
show_zoom_status (Block.Zoom offset factor) =
    "+" ++ Ui.Types.pretty_pos offset ++ "*" ++ fact
    where fact = printf "%.1f" factor

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
        track <- State.track_at block_id tracknum
        case fmap (Block.track_id_of . Block.tracklike_id) track of
            Just (Just track_id) ->
                State.set_track_title track_id text
            _ -> State.throw $ show (UiMsg.UpdateInput text) ++ " for "
                ++ show ctx ++ " on non-event track " ++ show track
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

type SchemaMap = Map.Map Block.SchemaId Schema

-- | A Schema attaches a number of things to a Block.
data Schema = Schema {
    schema_deriver :: SchemaDeriver Derive.EventDeriver
    , schema_signal_deriver :: SchemaDeriver Derive.SignalDeriver
    -- | Get a set of Cmds that are applicable within the given CmdContext.
    , schema_cmds :: CmdContext -> [Cmd]
    }

-- | So Cmd.State can be showable, for debugging.
instance Show Schema where
    show _ = "<schema>"

-- | A SchemaDeriver generates a Deriver from a given Block.
type SchemaDeriver d =
    Block.BlockId -> State.StateT Identity.Identity d

-- ** cmd types

-- | Information needed to decide what cmds should apply.
data CmdContext = CmdContext {
    ctx_default_inst :: Maybe Score.Instrument
    , ctx_inst_addr :: Score.Instrument -> Maybe Instrument.Addr
    , ctx_lookup_midi :: MidiDb.LookupMidiInstrument
    , ctx_edit_mode :: EditMode
    , ctx_kbd_entry :: Bool
    , ctx_focused_tracknum :: Maybe Block.TrackNum
    , ctx_track_tree :: State.TrackTree
    }

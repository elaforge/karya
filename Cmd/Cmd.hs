{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Cmd.Cmd where

import qualified Control.Concurrent as Concurrent
import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
-- import qualified Control.Monad.Error as Error
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Logger as Logger
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Midi.Midi as Midi

import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Player as Player


type CmdM = CmdT Identity.Identity Status
type Cmd = Msg.Msg -> CmdM


-- | The result of running a Cmd.
type CmdVal = (State, [MidiThru], [Log.Msg],
    Either State.StateError (Status, State.State, [Update.Update]))

run :: (Monad m) => State.State -> State -> CmdT m Status -> m CmdVal
run ui_state cmd_state cmd = do
    (res, logs) <- (Log.run . Error.runErrorT . Logger.run
        . flip MonadState.runStateT cmd_state . State.run ui_state . run_cmd_t)
        cmd
    -- An Abort is just like if the cmd immediately returned Continue, except
    -- that log msgs are kept.
    return $ case res of
        Left Abort -> (cmd_state, [], logs, Right (Continue, ui_state, []))
        Right ((ui_res, cmd_state2), midi) -> (cmd_state2, midi, logs, ui_res)

run_cmd :: State.State -> State -> CmdM -> CmdVal
run_cmd ui_state cmd_state cmd =
    Identity.runIdentity (run ui_state cmd_state cmd)

-- | Quit is not exported, so that only 'cmd_quit' here has permission to
-- return it.
data Status = Done | Continue | Quit deriving (Eq, Show)

-- * CmdT and operations

type CmdStack m = State.StateT
    (MonadState.StateT State
        (Logger.LoggerT MidiThru
            (Error.ErrorT Abort
                (Log.LogT m))))

data Abort = Abort deriving (Show)
instance Error.Error Abort where
    noMsg = Abort

newtype Monad m => CmdT m a = CmdT (CmdStack m a)
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

-- | Extract a Just value, or 'abort'.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (Monad m) => Maybe a -> CmdT m a
require = maybe abort return

-- * State

data State = State {
    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    -- The key is the modifier stripped of extraneous info, like mousedown
    -- position.
    state_keys_down :: Map.Map Modifier Modifier

    -- | Argumentless save commands save to and load from this file.
    , state_default_save_file :: FilePath

    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_active_view :: Maybe Block.ViewId
    , state_active_track :: Maybe Block.TrackNum

    -- | Default time step.  Used for cursor movement, note duration, and
    -- whatever else.
    , state_current_step :: TimeStep.TimeStep

    -- | Edit mode enables various commands that write to tracks.
    , state_edit_mode :: Bool

    -- | Transport control channel for the player, if one is running.
    , state_player_transport :: Maybe Player.Transport
    } deriving (Show)
empty_state = State Map.empty "save/default" Nothing Nothing
    (TimeStep.UntilMark TimeStep.AllMarklists (TimeStep.MatchRank 2))
    False Nothing

data Modifier = KeyMod Key.Key
    -- | Mouse button, and (tracknum, pos) in went down at, if any.
    -- The block is not recorded because you can't drag across blocks.
    | MouseMod Int (Maybe (Block.TrackNum, TrackPos))
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

-- | Modify Cmd 'State'.
modify_state :: (Monad m) => (State -> State) -> CmdT m ()
modify_state f = (CmdT . lift) (MonadState.modify f)

-- | Keys currently held down, as in 'state_keys_down'.
keys_down :: (Monad m) => CmdT m (Map.Map Modifier Modifier)
keys_down = fmap state_keys_down get_state

get_active_view :: (Monad m) => CmdT m Block.ViewId
get_active_view = fmap state_active_view get_state >>= require

get_active_track :: (Monad m) => CmdT m Block.TrackNum
get_active_track = fmap state_active_track get_state >>= require

get_current_step :: (Monad m) => CmdT m TimeStep.TimeStep
get_current_step = fmap state_current_step get_state

-- * basic cmds

-- | Quit the app immediately.
cmd_quit :: CmdM
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
        let key = modifier_key mod
        mods <- keys_down
        when (key `Map.member` mods) $
            Log.warn $ "keydown for " ++ show mod ++ " already in modifiers"
        modify_keys (Map.insert key mod)
        mods <- keys_down
        Log.debug $ "keydown " ++ show (Map.elems mods)

    delete_mod mod = do
        let key = modifier_key mod
        mods <- keys_down
        when (key `Map.notMember` mods) $
            Log.warn $ "keyup for " ++ show mod ++ " not in modifiers"
        modify_keys (Map.delete key)
        mods <- keys_down
        Log.debug $ "keyup " ++ show (Map.elems mods)

    modify_keys f = modify_state $ \st ->
        st { state_keys_down = f (state_keys_down st) }

-- | Take a modifier to its key in the modifier map which has extra info like
-- mouse down position stripped.
modifier_key :: Modifier -> Modifier
modifier_key (MouseMod btn _) = MouseMod btn Nothing
modifier_key mod = mod

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
    Msg.Midi (_dev, _timestamp, msg) -> case msg of
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


-- | Keep 'state_active_view' and 'state_active_track' up to date.
cmd_record_active :: Cmd
cmd_record_active msg = case msg of
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_block = Just view_id })
        (UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus))) -> do
            modify_state $ \st -> st { state_active_view = Just view_id }
            -- Log.debug $ "active view is " ++ show view_id
            return Done
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_track = Just tracknum })
        _) -> do
            modify_state $ \st -> st { state_active_track = Just tracknum }
            -- Log.debug $ "active track is " ++ show tracknum
            return Continue

    _ -> return Continue

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
    case update of
        UiMsg.UpdateInput _ -> abort
        _ -> ui_update ctx update >> return Done

-- | Except when it's a block update, I have to update the block to update
-- the other views.  So this Cmd goes in with the normal Cmds.
cmd_update_ui_state :: Cmd
cmd_update_ui_state msg = do
    (ctx, update) <- require (update_of msg)
    ui_update_state ctx update >> return Done

update_of (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate update))) =
    Just (ctx, update)
update_of _ = Nothing

ui_update_state :: UiMsg.Context -> UiMsg.UiUpdate -> CmdT Identity.Identity ()
ui_update_state ctx@(UiMsg.Context (Just view_id) track _pos) update =
    case update of
        UiMsg.UpdateInput text -> do
            view <- State.get_view view_id
            update_input ctx (Block.view_block view) text
        _ -> return ()
ui_update_state ctx update =
    State.throw $ show update ++ " with no view_id: " ++ show ctx

ui_update :: UiMsg.Context -> UiMsg.UiUpdate -> CmdT Identity.Identity ()
ui_update ctx@(UiMsg.Context (Just view_id) track _pos) update = case update of
    UiMsg.UpdateTrackScroll hpos -> State.set_track_scroll view_id hpos
    UiMsg.UpdateZoom zoom -> State.set_zoom view_id zoom
    UiMsg.UpdateViewResize rect -> do
        view <- State.get_view view_id
        when (rect /= Block.view_rect view) $ do
            Log.debug $ "new view rect " ++ show rect
            State.set_view_rect view_id rect
    UiMsg.UpdateTrackWidth width -> case track of
        Just tracknum -> State.set_track_width view_id tracknum width
        Nothing -> State.throw $ show update ++ " with no track: " ++ show ctx
    _ -> return ()
ui_update ctx update =
    State.throw $ show update ++ " with no view_id: " ++ show ctx

update_input ctx block_id text = case (UiMsg.ctx_track ctx) of
    Just tracknum -> do
        track <- State.track_at block_id tracknum
        case track of
            Just (Block.TId track_id _) -> State.set_track_title track_id text
            _ -> State.throw $ show (UiMsg.UpdateInput text) ++ " for "
                ++ show ctx ++ " on non-event track " ++ show track
    Nothing -> State.set_block_title block_id text

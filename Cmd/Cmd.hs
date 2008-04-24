{-# OPTIONS_GHC -XGeneralizedNewtypeDeriving #-}
module Cmd.Cmd where

import Control.Monad
import qualified Control.Monad.Error as Error
import qualified Control.Monad.Identity as Identity
import qualified Control.Monad.State as MonadState
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (lift)
import qualified Control.Monad.Writer as Writer
-- import qualified Control.Monad.Error as Error
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Util.Logger as Logger
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Key as Key
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Util.Log as Log
import qualified Midi.Midi as Midi

import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep


type CmdM = CmdT Identity.Identity Status
type Cmd = Msg.Msg -> CmdM


-- | The result of running a Cmd.
type CmdVal = (State, [MidiMsg], [Log.Msg],
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

    {-
        case ui_res of
            Left err -> (cmd_state2, midi, log_msgs, Left err)
            Right (status, ui_state2, updates) ->
                (cmd_state2, midi, log_msgs, Right (ui_state2, updates))
    -- This defines the policy for exceptions from Ui.StateT.  I choose to
    -- let the ui state alone, but keep changes to the cmd state, midi, and
    -- log msgs.  This is so I don't throw away the work of previous Cmds.
    let (status, ui_result) = case ui_res of
            Left err -> (Done, Left err)
            Right (status, ui_state2, updates) ->
                (status, Right (ui_state2, updates))
    return (status, cmd_state2, midi, log_msgs, ui_result)
    -}

run_cmd :: State.State -> State -> CmdM -> CmdVal
run_cmd ui_state cmd_state cmd =
    Identity.runIdentity (run ui_state cmd_state cmd)

-- | Quit is not exported, so that only 'cmd_quit' here has permission to
-- return it.
data Status = Done | Continue | Quit deriving (Eq, Show)

-- * CmdT and operations

type CmdStack m = State.StateT
    (MonadState.StateT State
        (Logger.LoggerT MidiMsg
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

type MidiMsg = (Midi.Device, Midi.Message)

-- | Log some midi to send out.
midi :: (Monad m) => Midi.Device -> Midi.Message -> CmdT m ()
midi dev msg = (CmdT . lift . lift) (Logger.record (dev, msg))

-- | An abort is an exception to get out of CmdT, but it's considered the same
-- as returning Continue.  It's for "oops this cmd doesn't apply after all."
abort :: (Monad m) => CmdT m a
abort = (CmdT . lift . lift . lift) (Error.throwError Abort)

-- | Extract a Just value, or abort.  Generally used to check for Cmd
-- conditions that don't fit into a Keymap.
require :: (Monad m) => Maybe a -> CmdT m a
require = maybe abort return

-- * State

data State = State {
    -- | Map of keys held down.  Maintained by cmd_record_keys and accessed
    -- with 'keys_down'.
    state_keys_down :: Set.Set Modifier

    -- | The block and track that have focus.  Commands that address
    -- a particular block or track will address these.
    , state_active_view :: Maybe Block.ViewId
    , state_active_track :: Maybe Block.TrackNum

    -- | Default time step.  Used for cursor movement, note duration, and
    -- whatever else.
    , state_current_step :: TimeStep.TimeStep
    } deriving (Show)
empty_state = State Set.empty Nothing Nothing
    (TimeStep.UntilMark (TimeStep.MatchRank 1))

data Modifier = KeyMod Key.Key
    -- | Mouse button, and (tracknum, pos) in went down at, if any.
    -- The block is not recorded because you can't drag across blocks.
    | MouseMod Int (Maybe (Int, TrackPos))
    -- | Only chan and key are stored.  While it may be useful to map according
    -- to the dev, this code doesn't know which devs are available.  Block or
    -- track level handlers can query the dev themselves.
    | MidiMod Midi.Channel Midi.Key
    deriving (Eq, Ord, Show)

-- ** state access

get_cmd_state :: (Monad m) => CmdT m State
get_cmd_state = (CmdT . lift) MonadState.get

-- | Keys currently held down.
keys_down :: (Monad m) => CmdT m [Modifier]
keys_down = do
    st <- get_cmd_state
    return (Set.elems (state_keys_down st))

get_active_view :: (Monad m) => CmdT m Block.ViewId
get_active_view = do
    st <- get_cmd_state
    case (state_active_view st) of
        Nothing -> abort
        Just view_id -> return view_id

get_active_track :: (Monad m) => CmdT m Block.TrackNum
get_active_track = do
    st <- get_cmd_state
    case (state_active_track st) of
        Nothing -> abort
        Just tracknum -> return tracknum

get_current_step :: (Monad m) => CmdT m TimeStep.TimeStep
get_current_step = fmap state_current_step get_cmd_state

-- | Modify Cmd 'State'.
modify_state :: (Monad m) => (State -> State) -> CmdT m ()
modify_state f = (CmdT . lift) (MonadState.modify f)

-- * basic cmds

-- | Quit on escape.
cmd_quit :: Cmd
cmd_quit msg = return $ case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent (UiMsg.Kbd _ Key.Escape))) -> Quit
    _ -> Continue

-- | Log incoming msgs.
cmd_log :: Cmd
cmd_log msg = do
    Log.debug ("msg: " ++ show msg)
    return Continue

-- | Record keydowns into the 'State' modifier map.
cmd_record_keys :: Cmd
cmd_record_keys msg = do
    case msg of
        Msg.Ui (UiMsg.UiMsg ctx (UiMsg.MsgEvent evt)) -> case evt of
            UiMsg.Kbd UiMsg.KeyDown key ->
                insert_mod (KeyMod key)
            UiMsg.Kbd UiMsg.KeyUp key ->
                delete_mod (KeyMod key)
            UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseDown btn } ->
                insert_mod (MouseMod btn (mouse_context ctx))
            UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseUp btn } -> do
                mods <- keys_down
                delete_mod $ case List.find (has_button btn) mods of
                    Just mod -> mod
                    -- Nothing with this button is in keydown, so the
                    -- below will log a warning.
                    Nothing -> MouseMod btn (mouse_context ctx)
            _ -> return ()
        Msg.Midi (_dev, _timestamp, msg) -> case msg of
            Midi.ChannelMessage chan (Midi.NoteOn key _vel) ->
                insert_mod (MidiMod chan key)
            Midi.ChannelMessage chan (Midi.NoteOff key _vel) ->
                delete_mod (MidiMod chan key)
            _ -> return ()
        _ -> return ()
    return Continue
    where
    mouse_context (UiMsg.Context
        { UiMsg.ctx_track = Just n, UiMsg.ctx_pos = Just pos }) = Just (n, pos)
    mouse_context _ = Nothing
    has_button btn (MouseMod btn2 _) = btn == btn2
    has_button _btn _ = False
    insert_mod mod = do
        mods <- keys_down
        when (mod `elem` mods) $
            Log.warn $ "keydown for " ++ show mod ++ " already in modifiers"
        modify_keys (Set.insert mod)
        Log.debug $ "keydown " ++ show (mod:mods)
    delete_mod mod = do
        mods <- keys_down
        when (mod `notElem` mods) $
            Log.warn $ "keyup for " ++ show mod ++ " not in modifiers"
        modify_keys (Set.delete mod)
        Log.debug $ "keyup " ++ show (List.delete mod mods)
    modify_keys f = modify_state $ \st ->
        st { state_keys_down = f (state_keys_down st) }

-- | Keep 'state_active_view' and 'state_active_track' up to date.
cmd_record_active :: Cmd
cmd_record_active msg = case msg of
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_block = Just view_id })
        (UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus))) -> do
            modify_state $ \st -> st { state_active_view = Just view_id }
            Log.debug $ "active view is " ++ show view_id
            return Done
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_track = Just tracknum })
        _) -> do
            modify_state $ \st -> st { state_active_track = Just tracknum }
            Log.debug $ "active track is " ++ show tracknum
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
cmd_record_ui_updates (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate update))) =
    ui_update ctx update >> return Done
cmd_record_ui_updates _ = return Continue

ui_update :: UiMsg.Context -> UiMsg.UiUpdate -> CmdT Identity.Identity ()
ui_update ctx@(UiMsg.Context (Just view_id) track _pos) update = case update of
    UiMsg.UpdateInput text -> do
        view <- State.get_view view_id
        update_input ctx (Block.view_block view) text
    UiMsg.UpdateTrackScroll hpos -> State.set_track_scroll view_id hpos
    UiMsg.UpdateZoom zoom -> State.set_zoom view_id zoom
    UiMsg.UpdateViewResize rect -> State.set_view_size view_id rect
    UiMsg.UpdateTrackWidth width -> case track of
        Just tracknum -> State.set_track_width view_id tracknum width
        Nothing -> State.throw $ show update ++ " with no track: " ++ show ctx
ui_update ctx update =
    State.throw $ show update ++ " with no view_id: " ++ show ctx

update_input ctx block_id text = case (UiMsg.ctx_track ctx) of
    Just tracknum -> do
        track <- State.track_at block_id tracknum
        case track of
            Just (Block.T track_id _) -> State.set_track_title track_id text
            _ -> State.throw $ show (UiMsg.UpdateInput text) ++ " for "
                ++ show ctx ++ " on non-event track " ++ show track
    Nothing -> State.set_block_title block_id text

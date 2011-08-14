-- | Internal Cmds, that keep bits of Cmd.State up to date that everyone else
-- relies on.
module Cmd.Internal where
import Control.Monad
import qualified Data.Map as Map
import qualified Util.Map as Map

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import Ui
import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg


-- * record keys

-- | Record keydowns into the 'State' modifier map.
cmd_record_keys :: Cmd.Cmd
cmd_record_keys msg = cont $ when_just (msg_to_mod msg) $ \(down, mb_mod) -> do
    mods <- Cmd.keys_down
    -- The kbd model is that absolute sets of modifiers are sent over, but the
    -- other modifiers take downs and ups and incrementally modify the state.
    -- It's rather awkward, but keyups and keydowns may be missed if focus
    -- has left the app.
    let mods2 = set_key_mods mods
    mods3 <- case (down, mb_mod) of
        (True, Just mod) -> insert mod mods2
        (False, Just mod) -> delete mod mods2
        _ -> return mods2
    -- when (not (Map.null mods3)) $
    --     Log.warn $ (if down then "keydown " else "keyup ")
    --         ++ show (Map.elems mods3)
    Cmd.modify_state $ \st -> st { Cmd.state_keys_down = mods3 }
    where
    cont = (>> return Cmd.Continue)
    insert mod mods = do
        let key = strip_modifier mod
        when (key `Map.member` mods) $
            Log.warn $ "keydown for " ++ show mod ++ " already in modifiers"
        return $ Map.insert key mod mods
    delete mod mods = do
        let key = strip_modifier mod
        when (key `Map.notMember` mods) $
            Log.warn $ "keyup for " ++ show mod ++ " not in modifiers"
        return $ Map.delete key mods
    set_key_mods mods = case msg_to_key_mods msg of
        Just kmods -> Map.insert_list
            [(Cmd.KeyMod c, Cmd.KeyMod c) | c <- kmods]
            (Map.filter not_key_mod mods)
        Nothing -> mods
    not_key_mod (Cmd.KeyMod _) = False
    not_key_mod _ = True


-- | Take a modifier to its key in the modifier map which has extra info like
-- mouse down position stripped.
strip_modifier :: Cmd.Modifier -> Cmd.Modifier
strip_modifier (Cmd.MouseMod btn _) = Cmd.MouseMod btn Nothing
strip_modifier mod = mod

modifier_key :: Cmd.Modifier -> Maybe Key.Modifier
modifier_key (Cmd.KeyMod m) = Just m
modifier_key _ = Nothing

-- | Get the set of Key.Modifiers from the msg.
msg_to_key_mods :: Msg.Msg -> Maybe [Key.Modifier]
msg_to_key_mods msg = case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd _ mods _ -> Just mods
        UiMsg.Mouse { UiMsg.mouse_modifiers = mods } -> Just mods
        _ -> Nothing
    _ -> Nothing

-- | Convert a Msg to (is_key_down, Modifier).
msg_to_mod :: Msg.Msg -> Maybe (Bool, Maybe Cmd.Modifier)
msg_to_mod msg = case msg of
    Msg.Ui (UiMsg.UiMsg context (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd state _ _ -> case state of
            UiMsg.KeyDown -> Just (True, Nothing)
            UiMsg.KeyUp -> Just (False, Nothing)
            _ -> Nothing
        UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseDown btn } ->
            Just (True, Just $ Cmd.MouseMod btn (UiMsg.ctx_track context))
        UiMsg.Mouse { UiMsg.mouse_state = UiMsg.MouseUp btn } ->
            Just (False, Just $ Cmd.MouseMod btn (UiMsg.ctx_track context))
        _ -> Nothing
    Msg.Midi (Midi.ReadMessage { Midi.rmsg_msg = msg }) -> case msg of
        Midi.ChannelMessage chan (Midi.NoteOn key _vel) ->
            Just (True, Just $ Cmd.MidiMod chan key)
        Midi.ChannelMessage chan (Midi.NoteOff key _vel) ->
            Just (False, Just $ Cmd.MidiMod chan key)
        _ -> Nothing
    _ -> Nothing

-- * focus

-- | Keep 'Cmd.state_focused_view' up to date.
cmd_record_focus :: Cmd.Cmd
cmd_record_focus msg = case msg of
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_focus = Just view_id }) msg)
            -> do
        set_focused_view view_id
        return $ case msg of
           UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus) -> Cmd.Done
           _ -> Cmd.Continue
    _ -> return Cmd.Continue

set_focused_view :: (Cmd.M m) => ViewId -> m ()
set_focused_view view_id = do
    focus <- Cmd.gets Cmd.state_focused_view
    unless (focus == Just view_id) $ do
        Cmd.set_status "focus" (Just (show view_id))
        Cmd.modify_state $ \st -> st { Cmd.state_focused_view = Just view_id }

-- * record ui updates

-- | Catch 'UiMsg.UiUpdate's from the UI, and modify the state accordingly to
-- reflect the UI state.
--
-- Unlike all the other Cmds, the state changes this makes are not synced.
-- UiUpdates report changes that have already occurred directly on the UI, so
-- syncing them would be redundant.
cmd_record_ui_updates :: Cmd.Cmd
cmd_record_ui_updates (Msg.Ui (UiMsg.UiMsg _
        (UiMsg.UpdateScreenSize screen screens rect))) = do
    Cmd.modify_state $ \st -> st { Cmd.state_screens =
        set_screen screen screens rect (Cmd.state_screens st) }
    return Cmd.Done
    where
    set_screen screen screens rect = take screens
        . Seq.update_at Rect.empty screen (const rect)
cmd_record_ui_updates msg = do
    (ctx, view_id, update) <- Cmd.require (update_of msg)
    ui_update (fst <$> UiMsg.ctx_track ctx) view_id update
    -- return Continue to give 'cmd_update_ui_state' a crack at it
    return Cmd.Continue

ui_update :: Maybe TrackNum -> ViewId -> UiMsg.UiUpdate -> Cmd.CmdId ()
ui_update maybe_tracknum view_id update = case update of
    UiMsg.UpdateTrackScroll hpos -> State.set_track_scroll view_id hpos
    UiMsg.UpdateZoom zoom -> State.set_zoom view_id zoom
    UiMsg.UpdateViewResize rect track_size -> do
        view <- State.get_view view_id
        when (rect /= Block.view_rect view) $ State.set_view_rect view_id rect
        let sz = (Block.view_visible_track view, Block.view_visible_time view)
        when (track_size /= sz) $ State.set_track_size view_id track_size
    UiMsg.UpdateTrackWidth width -> case maybe_tracknum of
        Just tracknum -> State.set_track_width view_id tracknum width
        Nothing -> State.throw $ "update with no track: " ++ show update
    -- Handled by 'ui_update_state'.
    UiMsg.UpdateClose -> return ()
    UiMsg.UpdateInput {} -> return ()

-- | This is the other half of 'cmd_record_ui_updates', whose output is synced
-- like normal Cmds.  When its a block update I have to update the other
-- views.
cmd_update_ui_state :: Cmd.Cmd
cmd_update_ui_state msg = do
    (ctx, view_id, update) <- Cmd.require (update_of msg)
    ui_update_state (fst <$> UiMsg.ctx_track ctx) view_id update
    return Cmd.Done

ui_update_state :: Maybe TrackNum -> ViewId -> UiMsg.UiUpdate -> Cmd.CmdId ()
ui_update_state maybe_tracknum view_id update = case update of
    UiMsg.UpdateInput text -> do
        view <- State.get_view view_id
        update_input (Block.view_block view) text
    -- UiMsg.UpdateTrackScroll hpos -> sync_zoom_status view_id
    UiMsg.UpdateZoom _ -> sync_zoom_status view_id
    UiMsg.UpdateClose -> State.destroy_view view_id
    _ -> return ()
    where
    update_input block_id text = case maybe_tracknum of
        Just tracknum -> do
            track_id <- State.event_track_at block_id tracknum
            case track_id of
                Just track_id -> State.set_track_title track_id text
                Nothing -> State.throw $ show (UiMsg.UpdateInput text)
                    ++ " on non-event track " ++ show tracknum
        Nothing -> State.set_block_title block_id text

sync_zoom_status :: (Cmd.M m) => ViewId -> m ()
sync_zoom_status view_id = do
    view <- State.get_view view_id
    Cmd.set_view_status view_id "view"
        (Just (show_zoom_status (Block.view_zoom view)))

show_zoom_status :: Types.Zoom -> String
show_zoom_status (Types.Zoom offset factor) =
    '+' : Pretty.show_float (Just 3) offset
    ++ '*' : Pretty.show_float (Just 1) factor

update_of :: Msg.Msg -> Maybe (UiMsg.Context, ViewId, UiMsg.UiUpdate)
update_of (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate view_id update))) =
    Just (ctx, view_id, update)
update_of _ = Nothing

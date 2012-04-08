-- | Internal Cmds, that keep bits of Cmd.State up to date that everyone else
-- relies on.
module Cmd.Internal where
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Style as Style
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseBs as ParseBs
import qualified Derive.TrackLang as TrackLang
import qualified Perform.Pitch as Pitch
import qualified App.Config as Config
import Types


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
    -- when_just mb_mod $ \m ->
    --     Log.warn $ (if down then "keydown " else "keyup ")
    --         ++ show (strip_modifier m) ++ " in " ++ show (Map.keys mods)
    Cmd.modify $ \st -> st { Cmd.state_keys_down = mods3 }
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
            Log.warn $ "keyup for " ++ show key ++ " not in modifiers "
                ++ show (Map.keys mods)
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
    unless (focus == Just view_id) $
        Cmd.modify $ \st -> st { Cmd.state_focused_view = Just view_id }

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
    Cmd.modify $ \st -> st { Cmd.state_screens =
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
        Just tracknum -> do
            block_id <- State.block_id_of view_id
            State.set_track_width block_id tracknum width
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
    UiMsg.UpdateZoom {} -> sync_zoom_status view_id
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

update_of :: Msg.Msg -> Maybe (UiMsg.Context, ViewId, UiMsg.UiUpdate)
update_of (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate view_id update))) =
    Just (ctx, view_id, update)
update_of _ = Nothing


-- * set style

-- | Set the style of an event based on its contents.  This is hardcoded
-- for now but it's easy to put in StaticConfig if needed.
set_style :: Event.SetStyle
set_style _pos event
    | Event.event_style event == Config.default_style =
        colorize (Event.event_bs event)
    | otherwise = Event.event_style event

colorize :: UTF8.ByteString -> Style.StyleId
colorize bs = case ParseBs.parse_expr bs of
    Right (TrackLang.Call call _ : _) | call == TrackLang.c_equal ->
        Config.declaration_style
    Right _ -> Config.default_style
    Left _ -> Config.parse_error_style


-- * sync

{-
-- | Sync UI state up with Cmd state and schedule UI updates.
initialize_state :: (Cmd.M m) => m ()
initialize_state = do
    -- TODO these scattered sync functions are kinda grody.  Isn't there a
    -- better way to keep track of state that needs to be synced?  Or avoid
    -- doing it in the first place?
    mapM_ Selection.sync_selection_status =<< State.get_all_view_ids
    mapM_ Internal.sync_zoom_status =<< State.get_all_view_ids
    -- Emit track updates for all tracks, since I don't know where events have
    -- changed.
    State.update_all_tracks
-}

cmd_sync :: (Cmd.M m) => State.State -> Cmd.State -> m Cmd.Status
cmd_sync ui_from cmd_from = do
    edit_state <- Cmd.gets Cmd.state_edit
    ui_to <- State.get
    let updates = view_updates ui_from ui_to
        new_view = any is_create_view updates
    -- Log.error $ "cmd_sync: updates " ++ show updates
    when (new_view || Cmd.state_edit cmd_from /= edit_state) sync_edit_state

    when (State.state_config ui_from /= State.state_config ui_to) $
        sync_ui_config (State.state_config ui_to)
    forM_ (Maybe.mapMaybe selection_update updates) (uncurry sync_selection)
    forM_ (Maybe.mapMaybe zoom_update updates) sync_zoom_status
    return Cmd.Done
    where
    is_create_view (Update.ViewUpdate _ (Update.CreateView _)) = True
    is_create_view _ = False
    selection_update (Update.ViewUpdate view_id (Update.Selection selnum sel))
        | selnum == Config.insert_selnum = Just (view_id, sel)
    selection_update _ = Nothing
    zoom_update (Update.ViewUpdate view_id (Update.Zoom {})) = Just view_id
    zoom_update _ = Nothing

view_updates :: State.State -> State.State -> [Update.CmdUpdate]
view_updates ui_from ui_to = case Diff.run diff of
    Left _ -> []
    Right (cmd_updates, _) -> cmd_updates
    where
    diff = Diff.diff_views ui_from ui_to
        (State.state_views ui_from) (State.state_views ui_to)

-- initial_sync = do
    -- sync_edit_state
    -- sync_ui_config
    --
    -- sync_selection
    -- sync_zoom_status


-- ** sync

sync_edit_state :: (Cmd.M m) => m ()
sync_edit_state = do
    sync_edit_box_status
    sync_step_status
    sync_octave_status
    sync_recent

sync_edit_box_status :: (Cmd.M m) => m ()
sync_edit_box_status = do
    st <- Cmd.gets Cmd.state_edit
    let mode = Cmd.state_edit_mode st
    let skel = Block.Box (skel_color mode (Cmd.state_advance st))
            (if Cmd.state_chord st then 'c' else ' ')
        track = Block.Box (edit_color mode)
            (if Cmd.state_kbd_entry st then 'K' else ' ')
    Cmd.set_status Config.status_record $
        if Cmd.state_record_velocity st then Just "vel" else Nothing
    Cmd.set_edit_box skel track

skel_color :: Cmd.EditMode -> Bool -> Color.Color
skel_color Cmd.NoEdit _ = edit_color Cmd.NoEdit
skel_color _ advance
    -- Advance mode is only relevent for ValEdit, but it looks weird when
    -- switching to
    | advance = Config.advance_color
    | otherwise = Config.no_advance_color

edit_color :: Cmd.EditMode -> Color.Color
edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.RawEdit -> Config.raw_edit_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

sync_step_status :: (Cmd.M m) => m ()
sync_step_status = do
    st <- Cmd.gets Cmd.state_edit
    let status = TimeStep.show_step (Just (Cmd.state_note_direction st))
            (Cmd.state_time_step st)
    Cmd.set_status Config.status_step (Just status)
    Cmd.set_global_status "step" status

sync_octave_status :: (Cmd.M m) => m ()
sync_octave_status = do
    octave <- Cmd.gets (Cmd.state_kbd_entry_octave . Cmd.state_edit)
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.set_status Config.status_octave (Just (show octave))
    Cmd.set_global_status "8ve" (show octave)

sync_recent :: (Cmd.M m) => m ()
sync_recent = do
    recent <- Cmd.gets (Cmd.state_recent_notes . Cmd.state_edit)
    Cmd.set_global_status "recent" $
        Seq.join ", " (map show_recent (Seq.sort_on fst recent))
    where
    show_recent (num, note) = show num ++ ": " ++ case note of
        Cmd.RecentNote s _ -> s
        Cmd.RecentTransform s -> s ++ "|"

-- | Sync State.Config changes.
sync_ui_config :: (Cmd.M m) => State.Config -> m ()
sync_ui_config config = do
    Cmd.set_global_status "proj" $
        Pretty.pretty (State.config_namespace config)
    let (Pitch.ScaleId scale) =
            State.default_scale (State.config_default config)
    Cmd.set_global_status "scale" scale

sync_zoom_status :: (Cmd.M m) => ViewId -> m ()
sync_zoom_status view_id = do
    view <- State.get_view view_id
    Cmd.set_view_status view_id Config.status_zoom
        (Just (Pretty.pretty (Block.view_zoom view)))

-- * selection

sync_selection :: (Cmd.M m) => ViewId -> Maybe Types.Selection -> m ()
sync_selection view_id maybe_sel = do
    Cmd.set_view_status view_id Config.status_selection
        (fmap selection_status maybe_sel)
    block_id <- State.block_id_of view_id
    when_just maybe_sel $
        Info.set_inst_status block_id . Types.sel_cur_track

selection_status :: Types.Selection -> String
selection_status sel =
    Pretty.pretty start
        ++ if start == end then "" else "-" ++ Pretty.pretty end
    where (start, end) = Types.sel_range sel

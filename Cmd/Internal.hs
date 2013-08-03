-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Internal Cmds, that keep bits of Cmd.State up to date that everyone else
-- relies on.
module Cmd.Internal where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

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
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Info as Info
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.ParseBs as ParseBs
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackInfo as TrackInfo

import qualified Perform.RealTime as RealTime
import qualified App.Config as Config
import Types


-- * record keys

-- | Record keydowns into the 'State' modifier map.
cmd_record_keys :: Cmd.Cmd
cmd_record_keys msg = cont $ whenJust (msg_to_mod msg) $ \(down, mb_mod) -> do
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
    -- whenJust mb_mod $ \m ->
    --     Log.warn $ (if down then "keydown " else "keyup ")
    --         ++ show (Cmd.strip_modifier m) ++ " in " ++ show (Map.keys mods)
    Cmd.modify $ \st -> st { Cmd.state_keys_down = mods3 }
    where
    cont = (>> return Cmd.Continue)
    insert mod mods = do
        let key = Cmd.strip_modifier mod
        when (key `Map.member` mods) $
            Log.warn $ "keydown for " ++ show mod ++ " already in modifiers"
        return $ Map.insert key mod mods
    delete mod mods = do
        let key = Cmd.strip_modifier mod
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

modifier_key :: Cmd.Modifier -> Maybe Key.Modifier
modifier_key (Cmd.KeyMod m) = Just m
modifier_key _ = Nothing

-- | Get the set of Key.Modifiers from the msg.
msg_to_key_mods :: Msg.Msg -> Maybe [Key.Modifier]
msg_to_key_mods msg = case msg of
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd _ mods _ _ -> Just mods
        UiMsg.Mouse { UiMsg.mouse_modifiers = mods } -> Just mods
        _ -> Nothing
    _ -> Nothing

-- | Convert a Msg to (is_key_down, Modifier).
msg_to_mod :: Msg.Msg -> Maybe (Bool, Maybe Cmd.Modifier)
msg_to_mod msg = case msg of
    Msg.Ui (UiMsg.UiMsg context (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd state _ _ _ -> case state of
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
record_focus :: Cmd.Cmd
record_focus msg = case msg of
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
    -- return Continue to give 'update_ui_state' a crack at it
    return Cmd.Continue

ui_update :: Maybe TrackNum -> ViewId -> UiMsg.UiUpdate -> Cmd.CmdId ()
ui_update maybe_tracknum view_id update = case update of
    UiMsg.UpdateTrackScroll hpos -> State.set_track_scroll view_id hpos
    UiMsg.UpdateZoom zoom -> State.set_zoom view_id zoom
    UiMsg.UpdateViewResize rect padding -> do
        view <- State.get_view view_id
        when (rect /= Block.view_rect view) $ State.set_view_rect view_id rect
        let old_padding =
                (Block.view_track_padding view, Block.view_time_padding view)
        when (old_padding /= padding) $ State.set_view_padding view_id padding
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
update_ui_state :: Cmd.Cmd
update_ui_state msg = do
    (ctx, view_id, update) <- Cmd.require (update_of msg)
    if UiMsg.ctx_edit_input ctx
        then do
            Cmd.modify_edit_state $ \st -> st { Cmd.state_edit_input = False }
            return Cmd.Continue
        else do
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

set_style :: Track.SetStyleHigh
set_style = (track_bg, event_style)

-- | Set the style of an event based on its contents.  This is hardcoded
-- for now but it's easy to put in StaticConfig if needed.
event_style :: Bool -> Event.EventStyle
event_style has_note_children title event =
    integrated $ Config.event_style
        (syntax (ParseBs.parse_expr (Event.event_bytestring event)))
        (Event.style event)
    where
    integrated
        | Maybe.isNothing (Event.stack event) = id
        | otherwise = Config.integrated_style
    syntax (Left _) = Config.Error
    syntax (Right _)
        | TrackInfo.is_note_track title = if has_note_children
            then Config.NoteTransformer else Config.Default
        | TrackInfo.is_pitch_track title = Config.Pitch
        | otherwise = Config.Control

-- | Set the track background color.
track_bg :: Track.Track -> Color.Color
track_bg track
    | TrackInfo.is_pitch_track title = Color.brightness 1.7 Config.pitch_color
    | TrackInfo.is_control_track title =
        Color.brightness 1.7 Config.control_color
    | otherwise = Track.track_bg track
    where title = Track.track_title track

-- * sync

sync_status :: State.State -> Cmd.State -> Cmd.CmdId Cmd.Status
sync_status ui_from cmd_from = do
    edit_state <- Cmd.gets Cmd.state_edit
    ui_to <- State.get
    let updates = view_updates ui_from ui_to
        new_views = mapMaybe create_view updates
    when (not (null new_views) || Cmd.state_edit cmd_from /= edit_state) $
        sync_edit_state edit_state
    sync_play_state =<< Cmd.gets Cmd.state_play
    flip whenJust sync_save_file =<< Cmd.gets Cmd.state_save_file

    when (State.state_config ui_from /= State.state_config ui_to) $
        sync_ui_config (State.state_config ui_to)
    selection_hooks (mapMaybe selection_update updates)
    forM_ (new_views ++ mapMaybe zoom_update updates) sync_zoom_status
    return Cmd.Continue
    where
    create_view (Update.View view_id Update.CreateView) = Just view_id
    create_view _ = Nothing
    selection_update (Update.View view_id (Update.Selection selnum sel))
        | selnum == Config.insert_selnum = Just (view_id, sel)
    selection_update _ = Nothing
    zoom_update (Update.View view_id (Update.Zoom {})) = Just view_id
    zoom_update _ = Nothing

view_updates :: State.State -> State.State -> [Update.UiUpdate]
view_updates ui_from ui_to = fst $ Diff.run $
    Diff.diff_views ui_from ui_to
        (State.state_views ui_from) (State.state_views ui_to)

-- ** hooks

default_selection_hooks :: [[(ViewId, Maybe Types.Selection)] -> Cmd.CmdId ()]
default_selection_hooks =
    [ mapM_ (uncurry sync_selection)
    , mapM_ (uncurry realtime_selection)
    ]

selection_hooks :: [(ViewId, Maybe Types.Selection)] -> Cmd.CmdId ()
selection_hooks [] = return ()
selection_hooks sels = do
    hooks <- Cmd.gets (Cmd.hooks_selection . Cmd.state_hooks)
    mapM_ ($ sels) hooks


-- ** sync

sync_edit_state :: (Cmd.M m) => Cmd.EditState -> m ()
sync_edit_state st = do
    sync_edit_box st
    sync_step_status st
    sync_octave_status st
    sync_recent st

sync_edit_box :: (Cmd.M m) => Cmd.EditState -> m ()
sync_edit_box st = do
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
    -- Advance mode is only relevent for ValEdit.
    | advance = Config.advance_color
    | otherwise = Config.no_advance_color

edit_color :: Cmd.EditMode -> Color.Color
edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.RawEdit -> Config.raw_edit_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

sync_step_status :: (Cmd.M m) => Cmd.EditState -> m ()
sync_step_status st = do
    let step_status = TimeStep.show_time_step (Cmd.state_time_step st)
        dur_status = TimeStep.show_direction (Cmd.state_note_direction st)
            <> TimeStep.show_time_step (Cmd.state_note_duration st)
    Cmd.set_status Config.status_step (Just step_status)
    Cmd.set_status Config.status_note_duration (Just dur_status)
    Cmd.set_global_status "note dur" dur_status

sync_octave_status :: (Cmd.M m) => Cmd.EditState -> m ()
sync_octave_status st = do
    let octave = Cmd.state_kbd_entry_octave st
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.set_status Config.status_octave (Just (showt octave))

sync_recent :: (Cmd.M m) => Cmd.EditState -> m ()
sync_recent st = do
    let recent = Cmd.state_recent_notes st
    Cmd.set_global_status "recent" $
        Text.intercalate ", " (map show_recent (Seq.sort_on fst recent))
    where
    show_recent (num, note) = showt num <> ": " <> case note of
        Cmd.RecentGenerator s zero_dur -> s <> zero zero_dur
        Cmd.RecentTransform s zero_dur -> s <> "|" <> zero zero_dur
    zero True = " (0 dur)"
    zero False = ""

sync_play_state :: (Cmd.M m) => Cmd.PlayState -> m ()
sync_play_state st = do
    Cmd.set_global_status "play-step" $
        TimeStep.show_time_step (Cmd.state_play_step st)
    Cmd.set_global_status "play-mult" $
        ShowVal.show_val (Cmd.state_play_multiplier st)

sync_save_file :: (Cmd.M m) => Cmd.SaveFile -> m ()
sync_save_file save = Cmd.set_global_status "save" $ case save of
    Cmd.SaveState fn -> txt fn
    Cmd.SaveRepo repo -> txt repo

-- | Sync State.Config changes.
sync_ui_config :: (Cmd.M m) => State.Config -> m ()
sync_ui_config config =
    Cmd.set_global_status "global" $ State.config_global_transform config

-- Zoom is actually not very useful.
sync_zoom_status :: (Cmd.M m) => ViewId -> m ()
sync_zoom_status _view_id = return ()
    -- view <- State.get_view view_id
    -- Cmd.set_view_status view_id Config.status_zoom
    --     (Just (Pretty.pretty (Block.view_zoom view)))

-- * selection

sync_selection :: (Cmd.M m) => ViewId -> Maybe Types.Selection -> m ()
sync_selection view_id maybe_sel = do
    let set = Cmd.set_view_status view_id Config.status_selection
    case maybe_sel of
        Nothing -> set Nothing
        Just sel -> do
            block_id <- State.block_id_of view_id
            ns <- State.get_namespace
            tid <- State.event_track_at block_id (Types.sel_cur_track sel)
            set $ Just (selection_status ns sel tid)
            Info.set_inst_status block_id (Types.sel_cur_track sel)


selection_status :: Id.Namespace -> Types.Selection -> Maybe TrackId -> Text
selection_status ns sel maybe_track_id =
    Pretty.prettytxt start
        <> (if start == end then "" else Text.cons '-' (Pretty.prettytxt end))
    <> " " <> showt tstart
        <> (if tstart == tend then "" else Text.cons '-' (showt tend))
    <> maybe "" (txt . (' ':) . Id.show_short ns . Id.unpack_id) maybe_track_id
    where
    (start, end) = Types.sel_range sel
    (tstart, tend) = Types.sel_track_range sel

realtime_selection :: (Cmd.M m) => ViewId -> Maybe Types.Selection -> m ()
realtime_selection view_id maybe_sel = case maybe_sel of
    Nothing -> set Nothing
    Just sel -> do
        whenJustM (realtime_at_selection view_id sel) $
            set . Just . RealTime.show_units
    where set = Cmd.set_view_status view_id Config.status_realtime

realtime_at_selection :: (Cmd.M m) => ViewId -> Types.Selection
    -> m (Maybe RealTime)
realtime_at_selection view_id sel = do
    block_id <- State.block_id_of view_id
    track_id <- State.event_track_at block_id (Selection.point_track sel)
    justm (Perf.lookup_root) $ \perf ->
        Perf.lookup_realtime perf block_id track_id (Selection.point sel)

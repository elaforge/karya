-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Internal Cmds, that keep bits of Cmd.State up to date that everyone else
-- relies on.
module Cmd.Internal where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ratio as Ratio
import Data.Ratio ((%))
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.GitTypes as GitTypes
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Midi.Midi as Midi
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Diff as Diff
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.Key as Key
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Track as Track
import qualified Ui.Types as Types
import qualified Ui.Ui as Ui
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update
import qualified Ui.Zoom as Zoom

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Attrs as Attrs
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified App.Config as Config
import Global
import Types


-- * record keys

-- | Record keydowns into the 'State' modifier map.
cmd_record_keys :: Cmd.M m => Msg.Msg -> m Cmd.Status
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
            Log.warn $ "keydown for " <> showt mod <> " already in modifiers"
        return $ Map.insert key mod mods
    delete mod mods = do
        let key = Cmd.strip_modifier mod
        when (key `Map.notMember` mods) $
            Log.warn $ "keyup for " <> showt key <> " not in modifiers "
                <> showt (Map.keys mods)
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
record_focus :: Cmd.M m => Msg.Msg -> m Cmd.Status
record_focus msg = case msg of
    Msg.Ui (UiMsg.UiMsg (UiMsg.Context { UiMsg.ctx_focus = Just view_id }) msg)
            -> do
        set_focused_view view_id
        return $ case msg of
           UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus) -> Cmd.Done
           _ -> Cmd.Continue
    _ -> return Cmd.Continue

set_focused_view :: Cmd.M m => ViewId -> m ()
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
cmd_record_ui_updates :: Cmd.M m => Msg.Msg -> m Cmd.Status
cmd_record_ui_updates (Msg.Ui (UiMsg.UiMsg _
        (UiMsg.UpdateScreenSize screen screens rect))) = do
    Cmd.modify $ \st -> st { Cmd.state_screens =
        set_screen screen screens rect (Cmd.state_screens st) }
    return Cmd.Done
    where
    set_screen screen screens rect = take screens
        . Seq.update_at Rect.empty screen (const rect)
cmd_record_ui_updates msg = do
    (ctx, view_id, update) <- Cmd.abort_unless (update_of msg)
    ui_update (fst <$> UiMsg.ctx_track ctx) view_id update
    -- return Continue to give 'update_ui_state' a crack at it
    return Cmd.Continue

ui_update :: Cmd.M m => Maybe TrackNum -> ViewId -> UiMsg.UiUpdate -> m ()
ui_update maybe_tracknum view_id update = case update of
    UiMsg.UpdateTrackScroll hpos -> Ui.set_track_scroll view_id hpos
    UiMsg.UpdateTimeScroll offset -> Ui.modify_zoom view_id $ \zoom ->
        zoom { Zoom.offset = offset }
    UiMsg.UpdateViewResize rect padding -> do
        view <- Ui.get_view view_id
        when (rect /= Block.view_rect view) $ Ui.set_view_rect view_id rect
        when (Block.view_padding view /= padding) $
            Ui.set_view_padding view_id padding
    UiMsg.UpdateTrackWidth width -> case maybe_tracknum of
        Just tracknum -> do
            block_id <- Ui.block_id_of view_id
            collapsed <- (Block.Collapse `Set.member`) <$>
                Ui.track_flags block_id tracknum
            -- fltk shouldn't send widths for collapsed tracks, but it does
            -- anyway because otherwise it would have to cache the track sizes
            -- to know which one changed.  See BlockView::track_tile_cb.
            unless collapsed $
                Ui.set_track_width block_id tracknum width
        Nothing -> Ui.throw $ "update with no track: " <> showt update
    -- Handled by 'ui_update_state'.
    UiMsg.UpdateClose -> return ()
    UiMsg.UpdateInput {} -> return ()

-- | This is the other half of 'cmd_record_ui_updates', whose output is synced
-- like normal Cmds.  When its a block update I have to update the other
-- views.
update_ui_state :: Cmd.M m => Msg.Msg -> m Cmd.Status
update_ui_state msg = do
    (ctx, view_id, update) <- Cmd.abort_unless (update_of msg)
    if UiMsg.ctx_floating_input ctx
        then do
            Cmd.modify_edit_state $ \st -> st
                { Cmd.state_floating_input = False }
            return Cmd.Continue
        else do
            ui_update_state (fst <$> UiMsg.ctx_track ctx) view_id update
            return Cmd.Done

ui_update_state :: Cmd.M m => Maybe TrackNum -> ViewId -> UiMsg.UiUpdate -> m ()
ui_update_state maybe_tracknum view_id update = case update of
    UiMsg.UpdateInput (Just text) -> do
        view <- Ui.get_view view_id
        update_input (Block.view_block view) text
    -- UiMsg.UpdateTimeScroll {} -> sync_zoom_status view_id
    UiMsg.UpdateClose -> Ui.destroy_view view_id
    _ -> return ()
    where
    update_input block_id text = case maybe_tracknum of
        Just tracknum -> do
            track_id <- Ui.event_track_at block_id tracknum
            case track_id of
                Just track_id -> Ui.set_track_title track_id text
                Nothing -> Ui.throw $ showt (UiMsg.UpdateInput (Just text))
                    <> " on non-event track " <> showt tracknum
        Nothing -> Ui.set_block_title block_id text

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
        (syntax (Parse.parse_expr (Event.text event)))
        (Event.style event)
    where
    integrated
        | Maybe.isNothing (Event.stack event) = id
        | otherwise = Config.integrated_style
    syntax (Left _) = Config.Error
    syntax (Right _)
        | ParseTitle.is_note_track title = if has_note_children
            then Config.Parent else Config.Default
        | ParseTitle.is_pitch_track title = Config.Pitch
        | otherwise = Config.Control

-- | Set the track background color.
track_bg :: Track.Track -> Color.Color
track_bg track
    | ParseTitle.is_pitch_track title = Color.brightness 1.7 Config.pitch_color
    | ParseTitle.is_control_track title =
        Color.brightness 1.7 Config.control_color
    | otherwise = Track.track_bg track
    where title = Track.track_title track

-- * sync

-- | This is called after every non-failing cmd.
sync_status :: Ui.State -> Cmd.State -> Cmd.CmdId Cmd.Status
sync_status ui_from cmd_from = do
    ui_to <- Ui.get
    cmd_updates <- Ui.get_updates
    Cmd.modify $ update_saved cmd_updates ui_from ui_to

    cmd_to <- Cmd.get
    let updates = view_updates ui_from ui_to
        new_views = mapMaybe create_view updates
        edit_state = Cmd.state_edit cmd_to
    when (not (null new_views) || Cmd.state_edit cmd_from /= edit_state
            || Cmd.state_saved cmd_from /= Cmd.state_saved cmd_to) $
        sync_edit_state (save_status cmd_to) edit_state
    sync_play_state $ Cmd.state_play cmd_to
    sync_save_file $ Cmd.state_save_file cmd_to
    sync_defaults $ Ui.config#Ui.default_ #$ ui_to

    run_selection_hooks (mapMaybe selection_update updates)
    -- forM_ (new_views ++ mapMaybe zoom_update updates) sync_zoom_status
    return Cmd.Continue
    where
    create_view (Update.View view_id Update.CreateView) = Just view_id
    create_view _ = Nothing
    selection_update (Update.View view_id (Update.Selection selnum sel))
        | selnum == Config.insert_selnum = Just (view_id, sel)
    selection_update _ = Nothing
    -- zoom_update (Update.View view_id (Update.Zoom {})) = Just view_id
    -- zoom_update _ = Nothing

-- | Flip 'Cmd.state_saved' if the score has changed.  "Cmd.Save" will turn it
-- back on after a save.
update_saved :: [Update.CmdUpdate] -> Ui.State -> Ui.State -> Cmd.State
    -> Cmd.State
update_saved updates ui_from ui_to cmd_state = case Cmd.state_saved cmd_state of
    Nothing -> cmd_state { Cmd.state_saved = Just True }
    Just True | Maybe.isNothing (can_checkpoint cmd_state)
            && Diff.score_changed ui_from ui_to updates ->
        cmd_state { Cmd.state_saved = Just False }
    Just _ -> cmd_state
    -- TODO
    -- This involves yet another state diff, and I already have a ton of those.
    -- What I don't like is that this takes time linear in the size of the whole
    -- score, in the common case when there has been no change.  Fortunately I
    -- don't have to do it after state_saved is already false.
    --
    -- Probably the most reasonable way around this would be to split
    -- Ui.unsafe_put into separate functions for views and
    -- Block.block_config, and for the rest of the score state.  The second one
    -- can then turn off 'Cmd.state_saved' unconditionally.  Unfortunately it
    -- seems invasive to make this change, especially if I want to enforce it
    -- with types, so unless this extra diff turns out to cause UI lag I won't
    -- bother.

-- | Return Just if there will be a git checkpoint.  'update_saved' has to
-- predict this because by the time 'Cmd.Undo.save_history' runs, it's too
-- late to make Ui.State changes.
--
-- This is not defined in Cmd.Undo to avoid a circular import.
can_checkpoint :: Cmd.State -> Maybe (GitTypes.Repo, GitTypes.Commit)
    -- ^ I need both a repo and a previous commit to checkpoint.
can_checkpoint cmd_state = case (Cmd.state_save_file cmd_state, prev) of
    (Just (Cmd.ReadWrite, Cmd.SaveRepo repo), Just commit) ->
        Just (repo, commit)
    _ -> Nothing
    where
    prev = Cmd.hist_last_commit $ Cmd.state_history_config cmd_state

view_updates :: Ui.State -> Ui.State -> [Update.UiUpdate]
view_updates ui_from ui_to = fst $ Diff.run $
    Diff.diff_views ui_from ui_to
        (Ui.state_views ui_from) (Ui.state_views ui_to)

-- ** hooks

default_selection_hooks :: [[(ViewId, Maybe Cmd.TrackSelection)]
    -> Cmd.CmdId ()]
default_selection_hooks =
    [ mapM_ (uncurry sync_selection_status)
    , mapM_ (uncurry sync_selection_control)
    ]

run_selection_hooks :: [(ViewId, Maybe Sel.Selection)] -> Cmd.CmdId ()
run_selection_hooks [] = return ()
run_selection_hooks sels = do
    sel_tracks <- forM sels $ \(view_id, maybe_sel) -> case maybe_sel of
        Nothing -> return (view_id, Nothing)
        Just sel -> do
            block_id <- Ui.block_id_of view_id
            maybe_track_id <- Ui.event_track_at block_id
                (Selection.sel_point_track sel)
            return (view_id, Just (sel, block_id, maybe_track_id))
    hooks <- Cmd.gets (Cmd.hooks_selection . Cmd.state_hooks)
    mapM_ ($ sel_tracks) hooks


-- ** sync

sync_edit_state :: Cmd.M m => SaveStatus -> Cmd.EditState -> m ()
sync_edit_state save_status st = do
    sync_edit_box save_status st
    sync_step_status st
    sync_octave_status st
    sync_recorded_actions (Cmd.state_recorded_actions st)
    sync_instrument_attributes (Cmd.state_instrument_attributes st)

-- | The two upper boxes reflect the edit state.  The lower box is red for
-- 'Cmd.ValEdit' or dark red for 'Cmd.MethodEdit'.  The upper box is green
-- for 'Cmd.state_advance' mode, and red otherwise.
--
-- The lower box has a @K@ if 'Cmd.state_kbd_entry' mode is enabled, and the
-- upper box has an @o@ if 'Cmd.state_chord' mode is enabled.  The upper box
-- also has a @\/@ if the score state hasn't been saved to disk, or @x@ if
-- it can't save to disk because there is no save file.
sync_edit_box :: Cmd.M m => SaveStatus -> Cmd.EditState -> m ()
sync_edit_box save_status st = do
    let mode = Cmd.state_edit_mode st
    let skel = Block.Box (skel_color mode (Cmd.state_advance st)) $
            (if Cmd.state_chord st then snd else fst) $ case save_status of
                CantSave -> ('x', '⊗')
                Unsaved ->  ('/', 'ø')
                Saved ->    (' ', 'o')
        track = Block.Box (edit_color mode)
            (if Cmd.state_kbd_entry st then 'K' else ' ')
    Cmd.set_status Config.status_record $
        if Cmd.state_record_velocity st then Just "vel" else Nothing
    Cmd.set_edit_box skel track

data SaveStatus = CantSave | Unsaved | Saved deriving (Eq, Show)

save_status :: Cmd.State -> SaveStatus
save_status state = case Cmd.state_save_file state of
    Nothing -> CantSave
    Just (Cmd.ReadOnly, _) -> CantSave
    Just (Cmd.ReadWrite, _) -> case Cmd.state_saved state of
        Just False -> Unsaved
        _ -> Saved

skel_color :: Cmd.EditMode -> Bool -> Color.Color
skel_color Cmd.NoEdit _ = edit_color Cmd.NoEdit
skel_color _ advance
    -- Advance mode is only relevent for ValEdit.
    | advance = Config.advance_color
    | otherwise = Config.no_advance_color

edit_color :: Cmd.EditMode -> Color.Color
edit_color mode = case mode of
    Cmd.NoEdit -> Config.box_color
    Cmd.ValEdit -> Config.val_edit_color
    Cmd.MethodEdit -> Config.method_edit_color

sync_step_status :: Cmd.M m => Cmd.EditState -> m ()
sync_step_status st = do
    let step_status = TimeStep.show_time_step (Cmd.state_time_step st)
        dur_status =
            orient <> TimeStep.show_time_step (Cmd.state_note_duration st)
        orient = case Cmd.state_note_orientation st of
            Types.Positive -> "+"
            Types.Negative -> "-"
    Cmd.set_status Config.status_step $ Just $ step_status <> dur_status

sync_octave_status :: Cmd.M m => Cmd.EditState -> m ()
sync_octave_status st = do
    let octave = Cmd.state_kbd_entry_octave st
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.set_status Config.status_octave (Just (showt octave))

sync_recorded_actions :: Cmd.M m => Cmd.RecordedActions -> m ()
sync_recorded_actions actions = Cmd.set_global_status "rec" $
    Text.intercalate ", " [Text.singleton i <> "-" <> pretty act |
        (i, act) <- Map.toAscList actions]

sync_instrument_attributes :: Cmd.M m =>
    Map Score.Instrument Attrs.Attributes -> m ()
sync_instrument_attributes inst_attrs =
    Cmd.set_global_status "attrs" $ Text.unwords
        [ ShowVal.show_val inst <> ":" <> ShowVal.show_val attrs
        | (inst, attrs) <- Map.toAscList inst_attrs
        ]

sync_play_state :: Cmd.M m => Cmd.PlayState -> m ()
sync_play_state st = do
    Cmd.set_global_status "play-step" $
        TimeStep.show_time_step (Cmd.state_play_step st)
    Cmd.set_global_status "play-mult" $
        ShowVal.show_val (Cmd.state_play_multiplier st)

sync_save_file :: Cmd.M m => Maybe (Cmd.Writable, Cmd.SaveFile) -> m ()
sync_save_file save = Cmd.set_global_status "save" $ case save of
    Nothing -> ""
    Just (writable, save_file) -> name_of save_file
        <> case writable of
            Cmd.ReadWrite -> ""
            Cmd.ReadOnly -> " (ro)"
    where
    name_of (Cmd.SaveState fn) = txt fn
    name_of (Cmd.SaveRepo repo) = txt repo

sync_defaults :: Cmd.M m => Ui.Default -> m ()
sync_defaults (Ui.Default tempo) =
    Cmd.set_global_status "tempo" (if tempo == 1 then "" else pretty tempo)

-- Zoom is actually not very useful, so this is disabled for now.  I'll leave
-- it here and the callers in place instead of deleting them so if I change my
-- mind then I still know where all the callers should be.
sync_zoom_status :: Cmd.M m => ViewId -> m ()
sync_zoom_status _ = return ()
-- sync_zoom_status view_id = do
--     view <- Ui.get_view view_id
--     Cmd.set_view_status view_id Config.status_zoom
--         (Just (pretty (Block.view_zoom view)))

-- * selection

sync_selection_status :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
sync_selection_status view_id maybe_sel = case maybe_sel of
    Nothing -> do
        set Config.status_selection Nothing
        set Config.status_track_id Nothing
    Just (sel, block_id, maybe_track_id) -> do
        ns <- Ui.get_namespace
        start_secs <- realtime block_id maybe_track_id $ Sel.min sel
        end_secs <- realtime block_id maybe_track_id $ Sel.max sel
        set Config.status_selection $
            Just $ selection_status sel start_secs end_secs
        set Config.status_track_id $
            Just $ track_selection_status ns sel maybe_track_id
        -- This didn't seem too useful, but maybe I'll change my mind?
        -- Info.set_instrument_status block_id (Sel.cur_track sel)
    where
    set = Cmd.set_view_status view_id
    realtime block_id maybe_track_id t = justm Perf.lookup_root $ \perf ->
        Perf.lookup_realtime perf block_id maybe_track_id t

selection_status :: Sel.Selection -> Maybe RealTime -> Maybe RealTime -> Text
selection_status sel start_secs end_secs =
    show_range show_score (Sel.min sel) (Sel.max sel) <> "t"
    `TextUtil.join2` case get start_secs end_secs of
        Just (start, end) -> show_range show_real start end <> "s"
        Nothing -> ""
    where
    get (Just a) (Just b) = Just (a, b)
    get (Just a) Nothing = Just (a, a)
    get Nothing (Just b) = Just (b, b)
    get Nothing Nothing = Nothing
    -- ScoreTime tends to be low numbers and have fractions.  RealTime is
    -- just seconds though, so unless there's a unicode fraction, just use
    -- decimal notation.
    show_score = pretty_rational True . ScoreTime.to_double
    show_real = pretty_rational False . RealTime.to_seconds

show_range :: (Eq a, Num a) => (a -> Text) -> a -> a -> Text
show_range fmt start end = fmt start
    <> if start == end then "" else "-" <> fmt end
    <> " (" <> fmt (end - start) <> ")"

track_selection_status :: Id.Namespace -> Sel.Selection -> Maybe TrackId -> Text
track_selection_status ns sel maybe_track_id =
    Text.unwords $ filter (not . Text.null)
        [ showt tstart
            <> (if tstart == tend then "" else Text.cons '-' (showt tend))
        , maybe "" (Id.show_short ns . Id.unpack_id) maybe_track_id
        ]
    where (tstart, tend) = Sel.track_range sel

-- | If it looks like a low fraction, display it thus, rather than as
-- a decimal.  This is useful because e.g. meters in three will have lots of
-- repeating decimals.  I also use fractions for power of two denominators
-- which are just fine in decimal, but the fraction still takes up less space.
pretty_rational :: Bool -> Double -> Text
pretty_rational favor_fraction d
    | d == 0 = "0"
    | Just frac <- Map.lookup ratio fractions = int_s <> frac
    | favor_fraction && Ratio.denominator ratio <= 12 =
        int_s <> "+" <> pretty ratio
    | otherwise = pretty d
    where
    (int, frac) = properFraction d
    int_s = if int == 0 then "" else showt int
    ratio = Ratio.approxRational frac 0.0001
    fractions = Map.fromList
        [ (0 % 1, "")
        , (1 % 4, "¼"), (1 % 2, "½"), (3 % 4, "¾")
        , (1 % 3, "⅓"), (2 % 3, "⅔")
        , (1 % 5, "⅕"), (2 % 5, "⅖"), (3 % 5, "⅗"), (4 % 5, "⅘")
        , (1 % 6, "⅙"), (5 % 6, "⅚")
        , (1 % 8, "⅛"), (3 % 8, "⅜"), (5 % 8, "⅝"), (7 % 8, "⅞")
        ]

-- ** selection control value

sync_selection_control :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
sync_selection_control view_id (Just (sel, block_id, Just track_id)) = do
    status <- track_control block_id track_id (Selection.sel_point sel)
    Cmd.set_view_status view_id Config.status_control $ ("c"<>) <$> status
sync_selection_control view_id _ =
    Cmd.set_view_status view_id Config.status_control Nothing

-- | This uses 'Cmd.perf_track_signals' rather than 'Cmd.perf_track_dynamic'
-- because track dynamics have the callers controls on a control track
track_control :: Cmd.M m => BlockId -> TrackId -> ScoreTime -> m (Maybe Text)
track_control block_id track_id pos =
    justm (parse_title <$> Ui.get_track_title track_id) $ \ctype ->
    case ctype of
        -- Since I'm using TrackSignals, I just have numbers, not proper
        -- pitches.  So I could show pitches, but it would just be an NN,
        -- which doesn't seem that useful.
        ParseTitle.Pitch {} -> return Nothing
        _ -> justm (Cmd.lookup_performance block_id) $ \perf -> return $ do
            tsig <- Map.lookup (block_id, track_id)
                (Cmd.perf_track_signals perf)
            return $ show_val ctype (Track.ts_signal tsig) $
                Track.signal_at pos tsig
    where
    parse_title = either (const Nothing) Just . ParseTitle.parse_control_type
    show_val ctype sig = case ctype of
        ParseTitle.Tempo {} -> ShowVal.show_val
        _
            | Signal.minimum sig < -1 || Signal.maximum sig > 1 ->
                ShowVal.show_val
            | otherwise -> ShowVal.show_hex_val

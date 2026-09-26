-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
-- | Internal Cmds, that keep bits of Cmd.State up to date that everyone else
-- relies on.
module Cmd.Internal (
    handleUnsynced
    , updateUiState
    , setStyle
    , syncStatus
    , defaultSelectionHooks
    , syncZoomStatus
    , canCheckpoint
#ifdef TESTING
    , recordKeys
#endif
) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.GitT as GitT
import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Texts as Texts

import qualified App.Config as Config
import qualified App.Path as Path
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Msg as Msg
import qualified Cmd.NoteTrackParse as NoteTrackParse
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.Attrs as Attrs
import qualified Derive.Parse as Parse
import qualified Derive.ParseTitle as ParseTitle
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Midi.Midi as Midi
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
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
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiMsg as UiMsg
import qualified Ui.Update as Update
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * handle_unsynced

{- | Catch 'UiMsg.UiUpdate's from the UI, and update Ui.State or Cmd.State
    accordingly to reflect its changes.

    Unlike other Cmds, the state changes this makes are not synced.  This means
    they can't change anything visible on the GUI!  UiUpdates report changes
    that have already occurred directly on the UI, so syncing them would be
    redundant.  But if there are visible changes that happen as a result, such
    as color changing for focus change, the state update should happen here,
    and the color change should happen in 'update_ui_state'.

    The other important difference is that this Cmd is run independently of the
    rest, and is expected to never throw.  This means a downstream Cmd that
    throws can't revert any state changes it made, which is important if they
    are tracking UI state which already changed.  Otherwise, haskell's idea of
    the fltk state will diverge from its real status.
-}
handleUnsynced :: Msg.Msg -> Cmd.CmdId Cmd.Status
handleUnsynced msg = do
    recordKeys msg
    unsyncedUiUpdates msg

-- | Record keydowns into the 'State' modifier map.
-- TODO if it weren't for Cmd.MidiMod, this could work with just
-- (ctx_track, UiMsg.MsgEvent).  I've never used MidiMod, it can be dumped.
recordKeys :: Msg.Msg -> Cmd.CmdId ()
recordKeys msg = whenJust (msgToMod msg) $ \(down, mbMod) -> do
    mods <- Cmd.keysDown
    -- The kbd model is that absolute sets of modifiers are sent over, but the
    -- other modifiers take downs and ups and incrementally modify the state.
    -- It's rather awkward, but keyups and keydowns may be missed if focus
    -- has left the app.
    mods <- pure $ setKeyMods mods
    mods <- case (down, mbMod) of
        (True, Just mod) -> insert mod mods
        (False, Just mod) -> delete mod mods
        _ -> pure mods
    -- whenJust mb_mod $ \m ->
    --     Log.warn $ (if down then "keydown " else "keyup ")
    --         ++ show (Cmd.strip_modifier m) ++ " in " ++ show (Map.keys mods)
    Cmd.modify $ \st -> st { Cmd.stateKeysDown = mods }
    where
    insert mod mods = do
        let key = Cmd.stripModifier mod
        when (key `Map.member` mods) $
            Log.warn $ "keydown for " <> showt mod <> " already in modifiers"
        pure $ Map.insert key mod mods
    delete mod mods = do
        let key = Cmd.stripModifier mod
        when (key `Map.notMember` mods) $
            Log.warn $ "keyup for " <> showt key <> " not in modifiers "
                <> showt (Map.keys mods)
        pure $ Map.delete key mods
    setKeyMods mods = case msgToKeyMods msg of
        Just kmods -> Maps.insertList
            [(Cmd.KeyMod c, Cmd.KeyMod c) | c <- kmods]
            (Map.filter notKeyMod mods)
        Nothing -> mods
    notKeyMod (Cmd.KeyMod _) = False
    notKeyMod _ = True

-- | Get the set of Key.Modifiers from the msg.
msgToKeyMods :: Msg.Msg -> Maybe [Key.Modifier]
msgToKeyMods = \case
    Msg.Ui (UiMsg.UiMsg _ (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd _ mods _ _ -> Just mods
        UiMsg.Mouse (UiMsg.MouseEvent { UiMsg.mouseModifiers = mods }) ->
            Just mods
        _ -> Nothing
    _ -> Nothing

-- | Convert a Msg to (is_key_down, Modifier).
msgToMod :: Msg.Msg -> Maybe (Bool, Maybe Cmd.Modifier)
msgToMod = \case
    Msg.Ui (UiMsg.UiMsg context (UiMsg.MsgEvent evt)) -> case evt of
        UiMsg.Kbd state _ _ _ -> case state of
            UiMsg.KeyDown -> Just (True, Nothing)
            UiMsg.KeyUp -> Just (False, Nothing)
            _ -> Nothing
        UiMsg.Mouse (UiMsg.MouseEvent
                { UiMsg.mouseState = UiMsg.MouseDown btn }) ->
            Just (True, Just $ Cmd.MouseMod btn (UiMsg.ctxTrack context))
        UiMsg.Mouse (UiMsg.MouseEvent
                { UiMsg.mouseState = UiMsg.MouseUp btn }) ->
            Just (False, Just $ Cmd.MouseMod btn (UiMsg.ctxTrack context))
        _ -> Nothing
    -- TODO I never used this, remove it?
    Msg.Midi (Midi.ReadMessage { Midi.rmsgMsg = msg }) -> case msg of
        Midi.ChannelMessage chan (Midi.NoteOn key _vel) ->
            Just (True, Just $ Cmd.MidiMod chan key)
        Midi.ChannelMessage chan (Midi.NoteOff key _vel) ->
            Just (False, Just $ Cmd.MidiMod chan key)
        _ -> Nothing
    _ -> Nothing

-- Returning Done here is a kind of micro-optimization, I could probably
-- also just let them through.
unsyncedUiUpdates :: Msg.Msg -> Cmd.CmdId Cmd.Status
unsyncedUiUpdates (Msg.Ui (UiMsg.UiMsg ctx msg)) = do
    -- Focus is always set in Context, not just UiMsg.Focus.  I think I was
    -- trying to work around the bug where the UI's focus didn't agree with the
    -- haskell state.
    whenJust (UiMsg.ctxFocus ctx) $ \viewId ->
        unlessM ((== Just viewId) <$> Cmd.gets Cmd.stateFocusedView) $
            Cmd.modify $ \st -> st { Cmd.stateFocusedView = Just viewId }
    case msg of
        -- Already set focus above, nothing to do.
        UiMsg.MsgEvent (UiMsg.AuxMsg UiMsg.Focus) -> pure Cmd.Done
        UiMsg.UiUpdate viewId UiMsg.UpdateClose -> do
            whenM ((== Just viewId) <$> Cmd.gets Cmd.stateFocusedView) $
                Cmd.modify $ \st -> st { Cmd.stateFocusedView = Nothing }
            pure Cmd.Continue
        UiMsg.UpdateScreenSize screen screens rect -> do
            Cmd.modify $ \st -> st
                { Cmd.stateScreens =
                    setScreen screen screens rect (Cmd.stateScreens st)
                }
            pure Cmd.Done
            where
            setScreen screen screens rect = take screens
                . Lists.updateAt Rect.empty screen (const rect)
        UiMsg.UiUpdate _ (UiMsg.UpdateInput {}) -> do
            -- The floating input sends its UpdateInput when it closes, so this
            -- implicitly means turn off state_floating_input.  TODO It's a bit
            -- unpleasant that it's implicit, maybe there should be a dedicated
            -- floating input open marker, like there is with focus.
            when (UiMsg.ctxFloatingInput ctx) $
                Cmd.modifyEditState $ \st -> st
                    { Cmd.stateFloatingInput = False }
            pure Cmd.Continue
        UiMsg.UiUpdate viewId update -> do
            uiUpdate ctx viewId update
            -- return Continue to give 'update_ui_state' a crack at it
            pure Cmd.Continue
        -- The rest of the UiUpdates are UiMsg.Mouse etc, handled by
        -- 'record_keys'.
        _ -> pure Cmd.Continue
unsyncedUiUpdates _ = pure Cmd.Continue

-- | Subset of unsynced_ui_updates that operates on Ui.State.
--
-- TODO Ui.M proves it doesn't do Cmd.M things, but it makes it polymorphic.
-- But, it's in the same module as its caller so should get monomorphized, so
-- not a problem?
uiUpdate :: Ui.M m => UiMsg.Context -> ViewId -> UiMsg.UiUpdate -> m ()
uiUpdate ctx viewId update = case update of
    UiMsg.UpdateTrackScroll hpos -> Ui.setTrackScroll viewId hpos
    UiMsg.UpdateTimeScroll offset -> Ui.modifyZoom viewId $ \zoom ->
        zoom { Zoom.offset = offset }
    UiMsg.UpdateViewResize rect padding -> do
        view <- Ui.getView viewId
        when (rect /= Block.viewRect view) $ Ui.setViewRect viewId rect
        when (Block.viewPadding view /= padding) $
            Ui.setViewPadding viewId padding
    UiMsg.UpdateTrackWidth width suggestedWidth -> case maybeTracknum of
        Just tracknum -> do
            blockId <- Ui.blockIdOf viewId
            collapsed <- Block.isCollapsed <$>
                Ui.trackFlags blockId tracknum
            -- fltk shouldn't send widths for collapsed tracks, but it does
            -- anyway because otherwise it would have to cache the track sizes
            -- to know which one changed.  See BlockView::track_tile_cb.
            unless collapsed $
                Ui.setTrackWidth blockId tracknum width
            when (suggestedWidth > 0) $
                Ui.setTrackSuggestedWidth blockId tracknum suggestedWidth
        Nothing -> Ui.throw $ "update with no track: " <> showt update
    -- Handled by 'ui_update_state'.
    UiMsg.UpdateClose -> pure ()
    -- Handled in caller, modifies Cmd.State.
    UiMsg.UpdateInput {} -> pure ()
    where
    maybeTracknum = fst <$> UiMsg.ctxTrack ctx


-- * record ui updates

-- | This is the other half of 'ui_update', whose output is synced like normal
-- Cmds.  When its a block update I have to update the other views.
--
-- TODO these can be rolled back on exception, but should they?  They are also
-- UI updates!  Just they need to diff.
updateUiState :: Cmd.M m => Msg.Msg -> m Cmd.Status
updateUiState msg = do
    (ctx, viewId, update) <- Cmd.abortUnless (updateOf msg)
    uiUpdateState ctx viewId update

uiUpdateState :: Cmd.M m => UiMsg.Context -> ViewId -> UiMsg.UiUpdate
    -> m Cmd.Status
uiUpdateState ctx viewId = \case
    UiMsg.UpdateInput (Just text)
        -- Let Edit.handle_floating_input get it.
        | UiMsg.ctxFloatingInput ctx -> pure Cmd.Continue
        | otherwise -> do
            view <- Ui.getView viewId
            updateInput (Block.viewBlock view) text
            pure Cmd.Done
    -- UiMsg.UpdateTimeScroll {} -> sync_zoom_status view_id
    UiMsg.UpdateClose -> do
        Ui.destroyView viewId
        pure Cmd.Done
    _ -> pure Cmd.Done
    where
    updateInput blockId text = case maybeTracknum of
        Just tracknum -> Ui.eventTrackAt blockId tracknum >>= \case
            Just trackId -> Ui.setTrackTitle trackId text
            Nothing -> Ui.throw $ showt (UiMsg.UpdateInput (Just text))
                <> " on non-event track " <> showt tracknum
        Nothing -> Ui.setBlockTitle blockId text
    maybeTracknum = fst <$> UiMsg.ctxTrack ctx

updateOf :: Msg.Msg -> Maybe (UiMsg.Context, ViewId, UiMsg.UiUpdate)
updateOf (Msg.Ui (UiMsg.UiMsg ctx (UiMsg.UiUpdate viewId update))) =
    Just (ctx, viewId, update)
updateOf _ = Nothing


-- * set style

setStyle :: Track.SetStyleHigh
setStyle = Track.SetStyleHigh trackBg eventStyle

-- | Set the style of an event based on its contents.  This is hardcoded
-- for now but it's easy to put in StaticConfig if needed.
eventStyle :: Id.Namespace -> Map BlockId a -> BlockId -> Bool
    -> Event.EventStyle
eventStyle namespace blocks blockId hasNoteChildren title event =
    integrated $
        Config.eventStyle (styleOf (Event.text event)) (Event.style event)
    where
    integrated
        | Maybe.isNothing (Event.stack event) = id
        | otherwise = Config.integratedStyle
    styleOf text
        | Parse.commentRemove `Text.isPrefixOf` text = Config.Commented
        | otherwise = case Parse.parseExpr text of
            Left _ -> Config.Error
            Right _
                | Parse.comment `Text.isPrefixOf` title -> Config.Commented
                | ParseTitle.isNoteTrack title ->
                    if hasNoteChildren then Config.NoteParent
                    else if isBlockCall text then Config.NoteBlockCall
                    else Config.Note
                | ParseTitle.isPitchTrack title -> Config.Pitch
                | otherwise -> Config.Control
    isBlockCall = not . null . NoteTrackParse.blockCallsOf False toBlockId
    toBlockId = NoteTrackParse.toBlockId blocks namespace (Just blockId)

-- | Set the track background color.
trackBg :: Track.Track -> Color.Color
trackBg track
    | Parse.comment `Text.isPrefixOf` title =
        Color.brightness 0.75 $ Track.trackBg track
    | ParseTitle.isPitchTrack title = Color.brightness 1.7 Config.pitchColor
    | ParseTitle.isControlTrack title =
        Color.brightness 1.7 Config.controlColor
    | otherwise = Track.trackBg track
    where title = Track.trackTitle track

-- * sync

-- | This is called after every non-failing cmd.
syncStatus :: Ui.State -> Cmd.State -> Update.UiDamage -> Cmd.CmdId Cmd.Status
syncStatus uiFrom cmdFrom damage = do
    uiTo <- Ui.get
    Cmd.modify $ updateSaved damage uiFrom uiTo

    cmdTo <- Cmd.get
    let updates = fst $ Diff.run $ Diff.diffViews uiFrom uiTo damage
        newViews = mapMaybe isCreateView updates
        editState = Cmd.stateEdit cmdTo
    when (not (null newViews) || Cmd.stateEdit cmdFrom /= editState
            || Cmd.stateSaved cmdFrom /= Cmd.stateSaved cmdTo) $
        syncEditState (getSaveStatus cmdTo) editState
    syncPlayState $ Cmd.statePlay cmdTo
    syncSaveFile (Cmd.scorePath cmdTo) (fst <$> Cmd.stateSaveFile cmdTo)
    syncDefaults $ Ui.config#UiConfig.default_ #$ uiTo
    runSelectionHooks (mapMaybe selectionUpdate updates)
    -- forM_ (new_views ++ mapMaybe zoom_update updates) sync_zoom_status
    pure Cmd.Continue
    where
    isCreateView (Update.View viewId Update.CreateView) = Just viewId
    isCreateView _ = Nothing
    selectionUpdate (Update.View viewId (Update.Selection selnum sel))
        | selnum == Config.insertSelnum = Just (viewId, sel)
    selectionUpdate _ = Nothing
    -- zoom_update (Update.View view_id (Update.Zoom {})) = Just view_id
    -- zoom_update _ = Nothing

-- | Flip 'Cmd._saved_state' if the score has changed.  "Cmd.Save" will turn it
-- back on after a save.
updateSaved :: Update.UiDamage -> Ui.State -> Ui.State -> Cmd.State
    -> Cmd.State
updateSaved damage uiFrom uiTo cmdState = case savedState of
    Cmd.JustLoaded -> cmdState
        { Cmd.stateSaved = Cmd.Saved Cmd.SavedChanges editorOpen }
    Cmd.SavedChanges
        | Maybe.isNothing (canCheckpoint cmdState)
            && Diff.scoreChanged uiFrom uiTo damage ->
        cmdState { Cmd.stateSaved = Cmd.Saved Cmd.UnsavedChanges editorOpen }
    _ -> cmdState
    where Cmd.Saved savedState editorOpen = Cmd.stateSaved cmdState

-- | Return Just if there will be a git checkpoint.  'update_saved' has to
-- predict this because by the time 'Cmd.Undo.save_history' runs, it's too
-- late to make Ui.State changes.
--
-- This is not defined in Cmd.Undo to avoid a circular import.
canCheckpoint :: Cmd.State -> Maybe (Path.Canonical, GitT.Commit)
    -- ^ I need both a repo and a previous commit to checkpoint.
canCheckpoint cmdState = case (Cmd.stateSaveFile cmdState, prev) of
    (Just (Cmd.ReadWrite, Cmd.SaveRepo repo), Just commit) ->
        Just (repo, commit)
    _ -> Nothing
    where
    prev = Cmd.histLastCommit $ Cmd.stateHistoryConfig cmdState

-- ** hooks

defaultSelectionHooks :: [[(ViewId, Maybe Cmd.TrackSelection)]
    -> Cmd.CmdId ()]
defaultSelectionHooks =
    [ mapM_ (uncurry syncSelectionStatus)
    , mapM_ (uncurry syncSelectionControl)
    ]

runSelectionHooks :: [(ViewId, Maybe Sel.Selection)] -> Cmd.CmdId ()
runSelectionHooks [] = pure ()
runSelectionHooks sels = do
    selTracks <- forM sels $ \(viewId, maybeSel) -> case maybeSel of
        Nothing -> pure (viewId, Nothing)
        Just sel -> do
            blockId <- Ui.blockIdOf viewId
            maybeTrackId <- Ui.eventTrackAt blockId
                (Selection.selPointTrack sel)
            pure (viewId, Just (sel, blockId, maybeTrackId))
    hooks <- Cmd.gets (Cmd.hooksSelection . Cmd.stateHooks)
    mapM_ ($ selTracks) hooks


-- ** sync

syncEditState :: Cmd.M m => SaveStatus -> Cmd.EditState -> m ()
syncEditState saveStatus st = do
    syncEditBox saveStatus st
    syncStepStatus st
    syncOctaveStatus st
    syncRecordedActions (Cmd.stateRecordedActions st)
    syncInstrumentAttributes (Cmd.stateInstrumentAttributes st)

-- | The two upper boxes reflect the edit state.  The lower box is red for
-- 'Cmd.ValEdit' or dark red for 'Cmd.MethodEdit'.  The upper box is green
-- for 'Cmd.state_advance' mode, and red otherwise.
--
-- The lower box has a @K@ if 'Cmd.state_kbd_entry' mode is enabled, and the
-- upper box has an @o@ if 'Cmd.state_chord' mode is enabled.  The upper box
-- also has a @\/@ if the score state hasn't been saved to disk, or @x@ if
-- it can't save to disk because there is no save file.
syncEditBox :: Cmd.M m => SaveStatus -> Cmd.EditState -> m ()
syncEditBox saveStatus st = do
    let mode = Cmd.stateEditMode st
    let skel = Block.Box (skelColor mode (Cmd.stateAdvance st)) $
            (if Cmd.stateChord st then snd else fst) $ case saveStatus of
                -- Behold my cutesy attempt to fit in both bits of info.
                CantSave -> ('x', '⊗')
                Unsaved ->  ('/', 'ø')
                Saved ->    (' ', 'o')
        track = Block.Box (editColor mode)
            (if Cmd.stateKbdEntry st then 'K' else ' ')
    Cmd.setStatus Config.statusRecord $
        if Cmd.stateRecordVelocity st then Just "vel" else Nothing
    Cmd.setEditBox skel track

data SaveStatus = CantSave | Unsaved | Saved deriving (Eq, Show)

getSaveStatus :: Cmd.State -> SaveStatus
getSaveStatus state = case Cmd.stateSaveFile state of
    Nothing -> CantSave
    Just (Cmd.ReadOnly, _) -> CantSave
    Just (Cmd.ReadWrite, _)
        | editorOpen || savedState /= Cmd.SavedChanges -> Unsaved
        | otherwise -> Saved
        where Cmd.Saved savedState editorOpen = Cmd.stateSaved state

skelColor :: Cmd.EditMode -> Bool -> Color.Color
skelColor Cmd.NoEdit _ = editColor Cmd.NoEdit
skelColor _ advance
    -- Advance mode is only relevent for ValEdit.
    | advance = Config.advanceColor
    | otherwise = Config.noAdvanceColor

editColor :: Cmd.EditMode -> Color.Color
editColor mode = case mode of
    Cmd.NoEdit -> Config.boxColor
    Cmd.ValEdit -> Config.valEditColor
    Cmd.MethodEdit -> Config.methodEditColor

syncStepStatus :: Cmd.M m => Cmd.EditState -> m ()
syncStepStatus st = do
    let stepStatus = TimeStep.showTimeStep (Cmd.stateTimeStep st)
        durStatus =
            orient <> TimeStep.showTimeStep (Cmd.stateNoteDuration st)
        orient = case Cmd.stateNoteOrientation st of
            Types.Positive -> "+"
            Types.Negative -> "-"
    Cmd.setStatus Config.statusStep $ Just $ stepStatus <> durStatus

syncOctaveStatus :: Cmd.M m => Cmd.EditState -> m ()
syncOctaveStatus st = do
    let octave = Cmd.stateKbdEntryOctave st
    -- This is technically global state and doesn't belong in the block's
    -- status line, but I'm used to looking for it there, so put it in both
    -- places.
    Cmd.setStatus Config.statusOctave (Just (showt octave))

syncRecordedActions :: Cmd.M m => Cmd.RecordedActions -> m ()
syncRecordedActions actions = Cmd.setGlobalStatus "rec" $
    Text.intercalate ", " [Text.singleton i <> "-" <> pretty act |
        (i, act) <- Map.toAscList actions]

syncInstrumentAttributes :: Cmd.M m =>
    Map ScoreT.Instrument Attrs.Attributes -> m ()
syncInstrumentAttributes instAttrs =
    Cmd.setGlobalStatus "attrs" $ Text.unwords
        [ ShowVal.showVal inst <> ":" <> ShowVal.showVal attrs
        | (inst, attrs) <- Map.toAscList instAttrs
        ]

syncPlayState :: Cmd.M m => Cmd.PlayState -> m ()
syncPlayState st = do
    Cmd.setGlobalStatus "play-step" $
        TimeStep.showTimeStep (Cmd.statePlayStep st)
    Cmd.setGlobalStatus "play-mult" $
        ShowVal.showVal (Cmd.statePlayMultiplier st)

syncSaveFile :: Cmd.M m => FilePath -> Maybe Cmd.Writable -> m ()
syncSaveFile scorePath writable =
    Cmd.setGlobalStatus "save" $ case writable of
        Nothing -> ""
        Just Cmd.ReadWrite -> txt scorePath
        Just Cmd.ReadOnly -> txt scorePath <> " (ro)"

syncDefaults :: Cmd.M m => UiConfig.Default -> m ()
syncDefaults (UiConfig.Default tempo) =
    Cmd.setGlobalStatus "tempo" (if tempo == 1 then "" else pretty tempo)

-- Zoom is actually not very useful, so this is disabled for now.  I'll leave
-- it here and the callers in place instead of deleting them so if I change my
-- mind then I still know where all the callers should be.
syncZoomStatus :: Cmd.M m => ViewId -> m ()
syncZoomStatus _ = pure ()
-- sync_zoom_status view_id = do
--     view <- Ui.get_view view_id
--     Cmd.set_view_status view_id Config.status_zoom
--         (Just (pretty (Block.view_zoom view)))

-- * selection

syncSelectionStatus :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
syncSelectionStatus viewId = \case
    Nothing -> do
        set Config.statusSelection Nothing
        set Config.statusTrackId Nothing
    Just (sel, blockId, maybeTrackId) -> do
        ns <- Ui.getNamespace
        startSecs <- realtime blockId maybeTrackId $ Sel.min sel
        endSecs <- realtime blockId maybeTrackId $ Sel.max sel
        set Config.statusSelection $
            Just $ selectionStatus sel startSecs endSecs
        set Config.statusTrackId $
            Just $ trackSelectionStatus ns sel maybeTrackId
        -- This didn't seem too useful, but maybe I'll change my mind?
        -- Info.set_instrument_status block_id (Sel.cur_track sel)
    where
    set = Cmd.setViewStatus viewId
    realtime blockId maybeTrackId t = justm Perf.lookupRoot $ \perf ->
        Perf.lookupRealtime perf blockId maybeTrackId t

selectionStatus :: Sel.Selection -> Maybe RealTime -> Maybe RealTime -> Text
selectionStatus sel startSecs endSecs =
    showRange showScore (Sel.min sel) (Sel.max sel) <> "t"
    `Texts.unwords2` case get startSecs endSecs of
        Just (start, end) -> showRange showReal start end <> "s"
        Nothing -> ""
    where
    get (Just a) (Just b) = Just (a, b)
    get (Just a) Nothing = Just (a, a)
    get Nothing (Just b) = Just (b, b)
    get Nothing Nothing = Nothing
    -- ScoreTime tends to be low numbers and have fractions.  RealTime is
    -- just seconds though, so unless there's a unicode fraction, just use
    -- decimal notation.
    showScore = Pretty.fraction True . ScoreTime.toDouble
    showReal = Pretty.fraction False . RealTime.toSeconds

showRange :: (Eq a, Num a) => (a -> Text) -> a -> a -> Text
showRange fmt start end = fmt start
    <> if start == end then "" else "-" <> fmt end
    <> " (" <> fmt (end - start) <> ")"

trackSelectionStatus :: Id.Namespace -> Sel.Selection -> Maybe TrackId -> Text
trackSelectionStatus ns sel maybeTrackId =
    Text.unwords $ filter (not . Text.null)
        [ showt tstart
            <> (if tstart == tend then "" else Text.cons '-' (showt tend))
        , maybe "" (Id.showShort ns . Id.unpackId) maybeTrackId
        ]
    where (tstart, tend) = Sel.trackRange sel

-- ** selection control value

syncSelectionControl :: Cmd.M m => ViewId -> Maybe Cmd.TrackSelection -> m ()
syncSelectionControl viewId (Just (sel, blockId, Just trackId)) = do
    status <- trackControl blockId trackId (Selection.selPoint sel)
    Cmd.setViewStatus viewId Config.statusControl $ ("c"<>) <$> status
syncSelectionControl viewId _ =
    Cmd.setViewStatus viewId Config.statusControl Nothing

-- | This uses 'Cmd.perf_track_signals' rather than 'Cmd.perf_track_dynamic'
-- because track dynamics have the callers controls on a control track
trackControl :: Cmd.M m => BlockId -> TrackId -> ScoreTime -> m (Maybe Text)
trackControl blockId trackId pos =
    justm (parseTitle <$> Ui.getTrackTitle trackId) $ \ctype ->
    case ctype of
        -- Since I'm using TrackSignals, I just have numbers, not proper
        -- pitches.  So I could show pitches, but it would just be an NN,
        -- which doesn't seem that useful.
        ParseTitle.Pitch {} -> pure Nothing
        _ -> justm (Cmd.lookupPerformance blockId) $ \perf -> pure $ do
            tsig <- Map.lookup (blockId, trackId)
                (Cmd.perfTrackSignals perf)
            pure $ showVal ctype (Track.tsSignal tsig) $
                Track.signalAt pos tsig
    where
    parseTitle = either (const Nothing) Just . ParseTitle.parseControlType
    showVal ctype sig = case ctype of
        ParseTitle.Tempo {} -> ShowVal.showVal
        _
            | Signal.minimum sig < -1 || Signal.maximum sig > 1 ->
                ShowVal.showVal
            | otherwise -> ShowVal.showHexVal

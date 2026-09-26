-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, maintainHistory, checkSaveHistory) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.Files as Files
import qualified Util.Log as Log
import qualified App.Path as Path
import qualified Cmd.Clip as Clip
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Cmd.Save as Save
import qualified Cmd.SaveGit as SaveGit
import qualified Cmd.SaveGitT as SaveGitT

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.Transform as Transform
import qualified Ui.Ui as Ui
import qualified Ui.Update as Update

import           Global
import           Types


{- [undo-and-updates]

    Unfortunately updates get in the way of the simple "list of states" model
    and make things a bit hard to understand.

    The head of hist_past is called cur.  It starts with [] updates and when
    a new history is recorded, cur gets its updates.  So each entry has the
    updates that took it to the future, which are also the updates needed to
    move to it from the future:

    load    (a [])  <cur
    +b      (a +b)  (ab []) <cur
    +c      (a +b)  (ab +c) (abc []) <cur

    On undo, the state is set to the state before cur.  The ex-cur gets the
    updates from this previous entry and goes onto the future.  So each entry
    in the future has the updates needed to get to it from the past:

    undo +c (a +b)  (ab +c) <cur    (abc +c)
    undo +b (a +b)  <cur    (ab +b) (abc +c)

    Redo is like a normal history record, except the state and updates are
    taken from the future.  The new cur entry has [] updates, and the prev
    (the old cur) gets its updates:

    redo +b (a +b)  (ab []) <cur    (abc +c)
    redo +c (a +b)  (ab +c) (abc []) <cur
-}

-- * undo / redo

undo :: Cmd.CmdT IO ()
undo = do
    -- If there is a suppressed record, then undo that and put it in the
    -- future.
    recordSuppressed
    hist <- Cmd.gets Cmd.stateHistory
    -- undo is asymmetrical with 'redo' because 'undo' itself is a cmd, and
    -- is happening after the state that is going to be undone.  So the current
    -- state doesn't count (it's the 'undo' cmd), the state I'm coming from is
    -- the previous one, and the one I'm going to is the twice previous one.
    let cur = Cmd.histPresent hist
    case Cmd.histPast hist of
        prev : rest -> doUndo hist cur prev rest
        [] -> do
            repo <- Save.getGitPath
            past <- liftIO $ loadPrev repo cur
            case past of
                [] -> Cmd.throw "no past to undo"
                prev : rest -> doUndo hist cur prev rest
    where
    doUndo hist cur prev rest = do
        Log.notice $ "undo " <> histName cur <> " -> " <> histName prev
        let damage = Cmd.histDamage prev
        Cmd.modify $ \st -> st
            { Cmd.stateHistory = Cmd.History
                { Cmd.histPast = rest
                , Cmd.histPresent = prev
                , Cmd.histFuture = cur { Cmd.histDamage = damage }
                    : Cmd.histFuture hist
                , Cmd.histLastCmd = Just Cmd.UndoRedo
                }
            , Cmd.stateHistoryCollect = Cmd.emptyHistoryCollect
            , Cmd.stateHistoryConfig = (Cmd.stateHistoryConfig st)
                { Cmd.histLastCommit = Cmd.histCommit prev }
            }
        -- This should be safe because these are just saved previous states.
        Ui.unsafeModify $ mergeUndoStates (Cmd.histState prev)
        Ui.damage damage
    loadPrev repo = loadHistory "load_previous_history" $
        SaveGit.loadPreviousHistory repo

redo :: Cmd.CmdT IO ()
redo = do
    hist <- Cmd.gets Cmd.stateHistory
    let cur = Cmd.histPresent hist
    case Cmd.histFuture hist of
        next : rest -> doRedo cur (Cmd.histPast hist) next rest
        [] -> do
            repo <- Save.getGitPath
            future <- liftIO $ loadNext repo cur
            case future of
                [] -> Cmd.throw "no future to redo"
                next : rest -> doRedo cur (Cmd.histPast hist) next rest
    where
    doRedo cur past next rest = do
        Log.notice $ "redo " <> histName cur <> " -> " <> histName next
        Cmd.modify $ \st -> st
            { Cmd.stateHistory = Cmd.History
                { Cmd.histPast =
                    cur { Cmd.histDamage = Cmd.histDamage next } : past
                , Cmd.histPresent = next { Cmd.histDamage = mempty }
                , Cmd.histFuture = rest
                , Cmd.histLastCmd = Just Cmd.UndoRedo
                }
            , Cmd.stateHistoryCollect = Cmd.emptyHistoryCollect
            , Cmd.stateHistoryConfig = (Cmd.stateHistoryConfig st)
                { Cmd.histLastCommit = Cmd.histCommit next }
            }
        -- This should be safe because these are just saved previous states.
        Ui.unsafeModify $ mergeUndoStates (Cmd.histState next)
        Ui.damage $ Cmd.histDamage next
    loadNext repo = loadHistory "load_next_history" $
        SaveGit.loadNextHistory repo

histName :: Cmd.HistoryEntry -> Text
histName hist = "[" <> Text.intercalate ", " (Cmd.histNames hist) <> "] "
    <> pretty (Cmd.histCommit hist)

loadHistory :: Text
    -> (Ui.State -> SaveGit.Commit
        -> IO (Either Text (Maybe SaveGit.LoadHistory)))
     -> Cmd.HistoryEntry -> IO [Cmd.HistoryEntry]
loadHistory name load hist = case Cmd.histCommit hist of
    Nothing -> pure []
    Just commit -> do
        result <- load (Cmd.histState hist) commit
        case result of
            Left err -> do
                Log.error $ name <> ": " <> err
                pure []
            Right Nothing -> pure []
            Right (Just hist) -> pure [entry hist]
    where
    entry (SaveGit.LoadHistory state commit damage names) =
        Cmd.HistoryEntry state damage names (Just commit)

-- | There are certain parts of the state that I don't want to undo, so
-- inherit them from the old state.  It's confusing when undo moves a window,
-- or a selection, or changes the zoom.
mergeUndoStates :: Ui.State -> Ui.State -> Ui.State
mergeUndoStates new old =
    (Transform.replaceNamespace Clip.clipNamespace old new)
    { Ui.stateViews = Map.mapWithKey
        (mergeView (Ui.stateViews old)) (Ui.stateViews new)
    , Ui.stateBlocks = Map.mapWithKey
        (mergeBlock (Ui.stateBlocks old)) (Ui.stateBlocks new)
    -- Previously I would keep the old Ui.state_config.  I don't remember
    -- exactly why, but it turned out to be confusing when I couldn't undo
    -- config changes.
    }

mergeView :: Map ViewId Block.View -> ViewId -> Block.View -> Block.View
mergeView oldViews viewId new = Map.findWithDefault new viewId oldViews

mergeBlock :: Map BlockId Block.Block -> BlockId -> Block.Block
    -> Block.Block
mergeBlock oldBlocks blockId new = case Map.lookup blockId oldBlocks of
    Nothing -> new
    Just old -> new { Block.blockConfig = Block.blockConfig old }

-- | Since 'merge_undo_states' makes sure the clip\/ namespace doesn't
-- participate in undo\/redo, I don't need to record if I have only those kind
-- of updates.
isClipUpdate :: Update.UiUpdate -> Bool
isClipUpdate = maybe False ((==Clip.clipNamespace) . Id.idNamespace)
    . Update.updateId


-- * responder support

-- | This is the toplevel function to record one Cmd's worth of history.
--
-- It's called by the responder after the Cmds are run and the updates are
-- available.
maintainHistory :: Ui.State -> Cmd.State -> [Update.UiUpdate] -> IO Cmd.State
maintainHistory uiState cmdState updates =
    saveHistory cmdState hist collect uncommitted
    where
    (hist, collect, uncommitted) = updateHistory updates uiState cmdState

-- | Save the suppressed history, if there is any.
recordSuppressed :: Cmd.CmdT IO ()
recordSuppressed = do
    cmdState <- Cmd.get
    let uncommitted = Maybe.maybeToList $ Cmd.stateSuppressed $
            Cmd.stateHistoryCollect cmdState
        hist = Cmd.stateHistory cmdState
        collect = Cmd.emptyHistoryCollect
    Cmd.put =<< liftIO (saveHistory cmdState hist collect uncommitted)

-- | Write the given 'SaveGit.SaveHistory's to disk (if I'm recording into
-- git), and update the Cmd.State accordingly.
saveHistory :: Cmd.State -> Cmd.History -> Cmd.HistoryCollect
    -> [SaveGit.SaveHistory] -> IO Cmd.State
saveHistory cmdState hist collect uncommitted = do
    let user = Cmd.configGitUser $ Cmd.stateConfig cmdState
    entries <- case Internal.canCheckpoint cmdState of
        Just (repo, prevCommit) ->
            commitEntries user (Path.toPath repo) prevCommit uncommitted
        _ -> pure $ map (historyEntry Nothing) uncommitted
    let (present, past) = bumpDamage (Cmd.histPresent hist) entries
    pure $ cmdState
        { Cmd.stateHistory = hist
            { Cmd.histPast = take keep (past ++ Cmd.histPast hist)
            , Cmd.histPresent = present
            , Cmd.histFuture = if null entries
                then Cmd.histFuture hist else []
            , Cmd.histLastCmd = Nothing
            }
        , Cmd.stateHistoryCollect = collect
        , Cmd.stateHistoryConfig = (Cmd.stateHistoryConfig cmdState)
            { Cmd.histLastCommit = Cmd.histCommit present
                <|> Cmd.histLastCommit (Cmd.stateHistoryConfig cmdState)
            }
        }
    where
    keep = Cmd.histKeep (Cmd.stateHistoryConfig cmdState)

-- | Only do this if I would have written something.  For that I need the
-- diffs.
checkSaveHistory :: Cmd.State -> IO (Maybe Text)
checkSaveHistory cmdState = case Internal.canCheckpoint cmdState of
    Just (repo, _) -> ifM (Files.writable (Path.toPath repo)) (pure Nothing)
        (pure $ Just $ "repo not writable: " <> showt repo)
    Nothing -> pure Nothing

-- | The present is expected to have no damage, so bump the damage off the
-- new present onto the old present, as described in [undo-and-updates].
bumpDamage :: Cmd.HistoryEntry -> [Cmd.HistoryEntry]
    -> (Cmd.HistoryEntry, [Cmd.HistoryEntry])
bumpDamage oldCur [] = (oldCur, [])
bumpDamage oldCur (newCur : news) =
    -- All I want to do is bump the updates from new_cur to old_cur, but
    -- suppressed records means there can be multiple histories recorded at
    -- once, which makes this a bit more of a hassle.
    (present, zipWith bump (newCur : entries) entries)
    where
    entries = news ++ [oldCur]
    present = newCur { Cmd.histDamage = mempty }
    bump p c = c { Cmd.histDamage = Cmd.histDamage p }

-- | Convert 'SaveGit.SaveHistory's to 'Cmd.HistoryEntry's by writing the
-- commits to disk.
commitEntries :: SaveGitT.User -> SaveGit.Repo -> SaveGit.Commit
    -> [SaveGit.SaveHistory] -> IO [Cmd.HistoryEntry]
commitEntries _ _ _ [] = pure []
commitEntries user repo prevCommit (hist0:hists) = do
    let hist = setCommit prevCommit hist0
    result <- SaveGit.checkpoint user repo hist
    case result of
        Left err -> do
            -- This can happen if the repo is read-only.
            Log.warn $ "error committing history: " <> err
            pure []
        Right commit -> do
            entries <- commitEntries user repo commit hists
            pure $ historyEntry (Just commit) hist : entries
    where
    setCommit commit (SaveGit.SaveHistory state _ updates names) =
        SaveGit.SaveHistory state (Just commit) updates names

-- | Create a 'Cmd.HistoryEntry' from a 'SaveGit.SaveHistory'.
--
-- The SaveHistory has a commit, but it's the commit that this history is
-- relative to (the previous commit), while the commit of the HistoryEntry is
-- the commit that this history was saved as (the current commit).
historyEntry :: Maybe SaveGit.Commit -> SaveGit.SaveHistory -> Cmd.HistoryEntry
historyEntry commit (SaveGit.SaveHistory state _ updates names) =
    -- Recover the CmdUpdates out of the UiUpdates.  I only have to remember
    -- the updates that diff won't recreate for me.
    Cmd.HistoryEntry state (mconcatMap Update.toDamage updates) names commit

-- | Integrate the latest updates into the history.  This could mean
-- accumulating them if history record is suppressed, or putting them into
-- new 'SaveGit.SaveHistory's if it isn't.
updateHistory :: [Update.UiUpdate] -> Ui.State -> Cmd.State
    -> (Cmd.History, Cmd.HistoryCollect, [SaveGit.SaveHistory])
updateHistory updates uiState cmdState
    | Just (Cmd.Load commit names) <- Cmd.histLastCmd hist =
        -- Switching over to someone else's history.  Wipe out existing
        -- history and record the current state as a commit.
        let newHist = Cmd.History
                { Cmd.histPast = []
                , Cmd.histPresent = Cmd.HistoryEntry
                    { histState = uiState
                    , histDamage = mempty
                    , histNames = names
                    , histCommit = commit
                    }
                , Cmd.histFuture = []
                , Cmd.histLastCmd = Nothing
                }
        in (newHist, Cmd.emptyHistoryCollect, [])
    | Just Cmd.UndoRedo <- Cmd.histLastCmd hist =
        -- If I get an undo while a cmd is suppressed, the last state change
        -- will be undone and the suppressed state change will lost entirely.
        -- This seems basically reasonable, since you could see it as an edit
        -- transaction that was cancelled.
        (hist, Cmd.emptyHistoryCollect, [])
    | otherwise =
        let (entries, collect) = recordHistory updates uiState cmdState
        in (hist, collect, entries)
    where hist = Cmd.stateHistory cmdState

-- | Get any history entries that should be saved, and the new HistoryCollect.
-- There may be no saveable history if this cmd doesn't need to be recorded
-- or was suppressed, and there may be more than one if a suppressed history
-- must now be recorded.
recordHistory :: [Update.UiUpdate] -> Ui.State -> Cmd.State
    -> ([SaveGit.SaveHistory], Cmd.HistoryCollect)
recordHistory updates uiState cmdState
    | not isRecordable && Maybe.isNothing suppress =
        ([], (Cmd.stateHistoryCollect cmdState) { Cmd.stateCmdNames = [] })
    | isSuppressed = (,) [] $
        Cmd.emptyHistoryCollect
            { Cmd.stateSuppressed =
                Just $ mergeIntoSuppressed suppressedEntry curEntry
            , Cmd.stateSuppressEdit =
                Cmd.stateSuppressEdit (Cmd.stateHistoryCollect cmdState)
            }
    | otherwise = (entries, Cmd.emptyHistoryCollect)
    where
    isSuppressed =
        suppress == Just (Cmd.stateEditMode (Cmd.stateEdit cmdState))
    isRecordable = shouldRecord updates
    entries = if isRecordable then [curEntry] else []
        ++ Maybe.maybeToList suppressedEntry
    -- Set the commit to Nothing for now, it will be filled in by
    -- 'commit_entries'.
    curEntry = SaveGit.SaveHistory uiState Nothing updates names
    Cmd.HistoryCollect
        { Cmd.stateCmdNames = names
        , Cmd.stateSuppressEdit = suppress
        , Cmd.stateSuppressed = suppressedEntry
        } = Cmd.stateHistoryCollect cmdState

mergeIntoSuppressed :: Maybe SaveGit.SaveHistory -> SaveGit.SaveHistory
    -> SaveGit.SaveHistory
mergeIntoSuppressed Nothing ent = ent
mergeIntoSuppressed (Just (SaveGit.SaveHistory _ _ updates1 names1))
        (SaveGit.SaveHistory state2 commit2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    SaveGit.SaveHistory state2 commit2 (updates1 ++ updates2) names1

-- | True if these updates are interesting enough to record an undo state for.
shouldRecord :: [Update.UiUpdate] -> Bool
shouldRecord = any SaveGit.shouldRecord . filter (not . isClipUpdate)

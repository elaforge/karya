-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, maintain_history, check_save_history) where
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
    record_suppressed
    hist <- Cmd.gets Cmd.state_history
    -- undo is asymmetrical with 'redo' because 'undo' itself is a cmd, and
    -- is happening after the state that is going to be undone.  So the current
    -- state doesn't count (it's the 'undo' cmd), the state I'm coming from is
    -- the previous one, and the one I'm going to is the twice previous one.
    let cur = Cmd.hist_present hist
    case Cmd.hist_past hist of
        prev : rest -> do_undo hist cur prev rest
        [] -> do
            repo <- Save.get_git_path
            past <- liftIO $ load_prev repo cur
            case past of
                [] -> Cmd.throw "no past to undo"
                prev : rest -> do_undo hist cur prev rest
    where
    do_undo hist cur prev rest = do
        Log.notice $ "undo " <> hist_name cur <> " -> " <> hist_name prev
        let damage = Cmd.hist_damage prev
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = rest
                , Cmd.hist_present = prev
                , Cmd.hist_future = cur { Cmd.hist_damage = damage }
                    : Cmd.hist_future hist
                , Cmd.hist_last_cmd = Just Cmd.UndoRedo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            , Cmd.state_history_config = (Cmd.state_history_config st)
                { Cmd.hist_last_commit = Cmd.hist_commit prev }
            }
        -- This should be safe because these are just saved previous states.
        Ui.unsafe_modify $ merge_undo_states (Cmd.hist_state prev)
        Ui.damage damage
    load_prev repo = load_history "load_previous_history" $
        SaveGit.load_previous_history repo

redo :: Cmd.CmdT IO ()
redo = do
    hist <- Cmd.gets Cmd.state_history
    let cur = Cmd.hist_present hist
    case Cmd.hist_future hist of
        next : rest -> do_redo cur (Cmd.hist_past hist) next rest
        [] -> do
            repo <- Save.get_git_path
            future <- liftIO $ load_next repo cur
            case future of
                [] -> Cmd.throw "no future to redo"
                next : rest -> do_redo cur (Cmd.hist_past hist) next rest
    where
    do_redo cur past next rest = do
        Log.notice $ "redo " <> hist_name cur <> " -> " <> hist_name next
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past =
                    cur { Cmd.hist_damage = Cmd.hist_damage next } : past
                , Cmd.hist_present = next { Cmd.hist_damage = mempty }
                , Cmd.hist_future = rest
                , Cmd.hist_last_cmd = Just Cmd.UndoRedo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            , Cmd.state_history_config = (Cmd.state_history_config st)
                { Cmd.hist_last_commit = Cmd.hist_commit next }
            }
        -- This should be safe because these are just saved previous states.
        Ui.unsafe_modify $ merge_undo_states (Cmd.hist_state next)
        Ui.damage $ Cmd.hist_damage next
    load_next repo = load_history "load_next_history" $
        SaveGit.load_next_history repo

hist_name :: Cmd.HistoryEntry -> Text
hist_name hist = "[" <> Text.intercalate ", " (Cmd.hist_names hist) <> "] "
    <> pretty (Cmd.hist_commit hist)

load_history :: Text
    -> (Ui.State -> SaveGit.Commit
        -> IO (Either Text (Maybe SaveGit.LoadHistory)))
     -> Cmd.HistoryEntry -> IO [Cmd.HistoryEntry]
load_history name load hist = case Cmd.hist_commit hist of
    Nothing -> return []
    Just commit -> do
        result <- load (Cmd.hist_state hist) commit
        case result of
            Left err -> do
                Log.error $ name <> ": " <> err
                return []
            Right Nothing -> return []
            Right (Just hist) -> return [entry hist]
    where
    entry (SaveGit.LoadHistory state commit damage names) =
        Cmd.HistoryEntry state damage names (Just commit)

-- | There are certain parts of the state that I don't want to undo, so
-- inherit them from the old state.  It's confusing when undo moves a window,
-- or a selection, or changes the zoom.
merge_undo_states :: Ui.State -> Ui.State -> Ui.State
merge_undo_states new old =
    (Transform.replace_namespace Clip.clip_namespace old new)
    { Ui.state_views = Map.mapWithKey
        (merge_view (Ui.state_views old)) (Ui.state_views new)
    , Ui.state_blocks = Map.mapWithKey
        (merge_block (Ui.state_blocks old)) (Ui.state_blocks new)
    -- Previously I would keep the old Ui.state_config.  I don't remember
    -- exactly why, but it turned out to be confusing when I couldn't undo
    -- config changes.
    }

merge_view :: Map ViewId Block.View -> ViewId -> Block.View -> Block.View
merge_view old_views view_id new = Map.findWithDefault new view_id old_views

merge_block :: Map BlockId Block.Block -> BlockId -> Block.Block
    -> Block.Block
merge_block old_blocks block_id new = case Map.lookup block_id old_blocks of
    Nothing -> new
    Just old -> new { Block.block_config = Block.block_config old }

-- | Since 'merge_undo_states' makes sure the clip\/ namespace doesn't
-- participate in undo\/redo, I don't need to record if I have only those kind
-- of updates.
is_clip_update :: Update.UiUpdate -> Bool
is_clip_update = maybe False ((==Clip.clip_namespace) . Id.id_namespace)
    . Update.update_id


-- * responder support

-- | This is the toplevel function to record one Cmd's worth of history.
--
-- It's called by the responder after the Cmds are run and the updates are
-- available.
maintain_history :: Ui.State -> Cmd.State -> [Update.UiUpdate] -> IO Cmd.State
maintain_history ui_state cmd_state updates =
    save_history cmd_state hist collect uncommitted
    where
    (hist, collect, uncommitted) = update_history updates ui_state cmd_state

-- | Save the suppressed history, if there is any.
record_suppressed :: Cmd.CmdT IO ()
record_suppressed = do
    cmd_state <- Cmd.get
    let uncommitted = Maybe.maybeToList $ Cmd.state_suppressed $
            Cmd.state_history_collect cmd_state
        hist = Cmd.state_history cmd_state
        collect = Cmd.empty_history_collect
    Cmd.put =<< liftIO (save_history cmd_state hist collect uncommitted)

-- | Write the given 'SaveGit.SaveHistory's to disk (if I'm recording into
-- git), and update the Cmd.State accordingly.
save_history :: Cmd.State -> Cmd.History -> Cmd.HistoryCollect
    -> [SaveGit.SaveHistory] -> IO Cmd.State
save_history cmd_state hist collect uncommitted = do
    let user = Cmd.config_git_user $ Cmd.state_config cmd_state
    entries <- case Internal.can_checkpoint cmd_state of
        Just (repo, prev_commit) ->
            commit_entries user (Path.to_path repo) prev_commit uncommitted
        _ -> return $ map (history_entry Nothing) uncommitted
    let (present, past) = bump_damage (Cmd.hist_present hist) entries
    return $ cmd_state
        { Cmd.state_history = hist
            { Cmd.hist_past = take keep (past ++ Cmd.hist_past hist)
            , Cmd.hist_present = present
            , Cmd.hist_future = if null entries
                then Cmd.hist_future hist else []
            , Cmd.hist_last_cmd = Nothing
            }
        , Cmd.state_history_collect = collect
        , Cmd.state_history_config = (Cmd.state_history_config cmd_state)
            { Cmd.hist_last_commit = Cmd.hist_commit present
                <|> Cmd.hist_last_commit (Cmd.state_history_config cmd_state)
            }
        }
    where
    keep = Cmd.hist_keep (Cmd.state_history_config cmd_state)

-- | Only do this if I would have written something.  For that I need the
-- diffs.
check_save_history :: Cmd.State -> IO (Maybe Text)
check_save_history cmd_state = case Internal.can_checkpoint cmd_state of
    Just (repo, _) -> ifM (Files.writable (Path.to_path repo)) (return Nothing)
        (return $ Just $ "repo not writable: " <> showt repo)
    Nothing -> return Nothing

-- | The present is expected to have no damage, so bump the damage off the
-- new present onto the old present, as described in [undo-and-updates].
bump_damage :: Cmd.HistoryEntry -> [Cmd.HistoryEntry]
    -> (Cmd.HistoryEntry, [Cmd.HistoryEntry])
bump_damage old_cur [] = (old_cur, [])
bump_damage old_cur (new_cur : news) =
    -- All I want to do is bump the updates from new_cur to old_cur, but
    -- suppressed records means there can be multiple histories recorded at
    -- once, which makes this a bit more of a hassle.
    (present, zipWith bump (new_cur : entries) entries)
    where
    entries = news ++ [old_cur]
    present = new_cur { Cmd.hist_damage = mempty }
    bump p c = c { Cmd.hist_damage = Cmd.hist_damage p }

-- | Convert 'SaveGit.SaveHistory's to 'Cmd.HistoryEntry's by writing the
-- commits to disk.
commit_entries :: SaveGitT.User -> SaveGit.Repo -> SaveGit.Commit
    -> [SaveGit.SaveHistory] -> IO [Cmd.HistoryEntry]
commit_entries _ _ _ [] = return []
commit_entries user repo prev_commit (hist0:hists) = do
    let hist = set_commit prev_commit hist0
    result <- SaveGit.checkpoint user repo hist
    case result of
        Left err -> do
            -- This can happen if the repo is read-only.
            Log.warn $ "error committing history: " <> err
            return []
        Right commit -> do
            entries <- commit_entries user repo commit hists
            return $ history_entry (Just commit) hist : entries
    where
    set_commit commit (SaveGit.SaveHistory state _ updates names) =
        SaveGit.SaveHistory state (Just commit) updates names

-- | Create a 'Cmd.HistoryEntry' from a 'SaveGit.SaveHistory'.
--
-- The SaveHistory has a commit, but it's the commit that this history is
-- relative to (the previous commit), while the commit of the HistoryEntry is
-- the commit that this history was saved as (the current commit).
history_entry :: Maybe SaveGit.Commit -> SaveGit.SaveHistory -> Cmd.HistoryEntry
history_entry commit (SaveGit.SaveHistory state _ updates names) =
    -- Recover the CmdUpdates out of the UiUpdates.  I only have to remember
    -- the updates that diff won't recreate for me.
    Cmd.HistoryEntry state (mconcatMap Update.to_damage updates) names commit

-- | Integrate the latest updates into the history.  This could mean
-- accumulating them if history record is suppressed, or putting them into
-- new 'SaveGit.SaveHistory's if it isn't.
update_history :: [Update.UiUpdate] -> Ui.State -> Cmd.State
    -> (Cmd.History, Cmd.HistoryCollect, [SaveGit.SaveHistory])
update_history updates ui_state cmd_state
    | Just (Cmd.Load commit names) <- Cmd.hist_last_cmd hist =
        -- Switching over to someone else's history.  Wipe out existing
        -- history and record the current state as a commit.
        let new_hist = Cmd.History
                { Cmd.hist_past = []
                , Cmd.hist_present = Cmd.HistoryEntry
                    { hist_state = ui_state
                    , hist_damage = mempty
                    , hist_names = names
                    , hist_commit = commit
                    }
                , Cmd.hist_future = []
                , Cmd.hist_last_cmd = Nothing
                }
        in (new_hist, Cmd.empty_history_collect, [])
    | Just Cmd.UndoRedo <- Cmd.hist_last_cmd hist =
        -- If I get an undo while a cmd is suppressed, the last state change
        -- will be undone and the suppressed state change will lost entirely.
        -- This seems basically reasonable, since you could see it as an edit
        -- transaction that was cancelled.
        (hist, Cmd.empty_history_collect, [])
    | otherwise =
        let (entries, collect) = record_history updates ui_state cmd_state
        in (hist, collect, entries)
    where hist = Cmd.state_history cmd_state

-- | Get any history entries that should be saved, and the new HistoryCollect.
-- There may be no saveable history if this cmd doesn't need to be recorded
-- or was suppressed, and there may be more than one if a suppressed history
-- must now be recorded.
record_history :: [Update.UiUpdate] -> Ui.State -> Cmd.State
    -> ([SaveGit.SaveHistory], Cmd.HistoryCollect)
record_history updates ui_state cmd_state
    | not is_recordable && Maybe.isNothing suppress =
        ([], (Cmd.state_history_collect cmd_state) { Cmd.state_cmd_names = [] })
    | is_suppressed = (,) [] $
        Cmd.empty_history_collect
            { Cmd.state_suppressed =
                Just $ merge_into_suppressed suppressed_entry cur_entry
            , Cmd.state_suppress_edit =
                Cmd.state_suppress_edit (Cmd.state_history_collect cmd_state)
            }
    | otherwise = (entries, Cmd.empty_history_collect)
    where
    is_suppressed =
        suppress == Just (Cmd.state_edit_mode (Cmd.state_edit cmd_state))
    is_recordable = should_record updates
    entries = if is_recordable then [cur_entry] else []
        ++ Maybe.maybeToList suppressed_entry
    -- Set the commit to Nothing for now, it will be filled in by
    -- 'commit_entries'.
    cur_entry = SaveGit.SaveHistory ui_state Nothing updates names
    Cmd.HistoryCollect
        { Cmd.state_cmd_names = names
        , Cmd.state_suppress_edit = suppress
        , Cmd.state_suppressed = suppressed_entry
        } = Cmd.state_history_collect cmd_state

merge_into_suppressed :: Maybe SaveGit.SaveHistory -> SaveGit.SaveHistory
    -> SaveGit.SaveHistory
merge_into_suppressed Nothing ent = ent
merge_into_suppressed (Just (SaveGit.SaveHistory _ _ updates1 names1))
        (SaveGit.SaveHistory state2 commit2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    SaveGit.SaveHistory state2 commit2 (updates1 ++ updates2) names1

-- | True if these updates are interesting enough to record an undo state for.
should_record :: [Update.UiUpdate] -> Bool
should_record = any SaveGit.should_record . filter (not . is_clip_update)

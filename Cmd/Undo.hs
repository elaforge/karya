-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, maintain_history) where
import qualified Control.Monad.Trans as Trans
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Git.Git as Git
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.SaveGit as SaveGit
import qualified App.Config as Config
import Types


{-
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
    hist <- Cmd.gets Cmd.state_history
    -- undo is asymmetrical with 'redo' because 'undo' itself is a cmd, and
    -- is happening after the state that is going to be undone.  So the current
    -- state doesn't count (it's the 'undo' cmd), the state I'm coming from is
    -- the previous one, and the one I'm going to is the twice previous one.
    case Cmd.hist_past hist of
        cur : prev : rest -> do_undo hist cur prev rest
        [cur] -> do
            repo <- State.gets SaveGit.save_repo
            past <- Trans.liftIO $ load_prev repo cur
            case past of
                [] -> Cmd.throw "no past to undo"
                prev : rest -> do_undo hist cur prev rest
        [] -> Cmd.throw "no past at all (this sholudn't happen)"
        -- TODO can I make past a non-empty list?
    where
    do_undo hist cur prev rest = do
        Log.notice $ "undo -> " ++ hist_name prev
        let updates = Cmd.hist_updates prev
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = prev : rest
                , Cmd.hist_future = cur { Cmd.hist_updates = updates }
                    : Cmd.hist_future hist
                , Cmd.hist_last_cmd = Just Cmd.UndoRedo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
        State.modify $ merge_undo_states (Cmd.hist_state prev)
        mapM_ State.update updates
    load_prev repo = load_history "load_previous_history" $
        SaveGit.load_previous_history repo

redo :: Cmd.CmdT IO ()
redo = do
    hist <- Cmd.gets Cmd.state_history
    cur <- Cmd.require_msg "redo: no past at all (this shouldn't happen)" $
        Seq.head (Cmd.hist_past hist)
    case Cmd.hist_future hist of
        next : rest -> do_redo hist next rest
        [] -> do
            repo <- State.gets SaveGit.save_repo
            future <- Trans.liftIO $ load_next repo cur
            case future of
                [] -> Cmd.throw "no future to redo"
                next : rest -> do_redo hist next rest
    where
    do_redo hist next rest = do
        Log.notice $ "redo -> " ++ hist_name next
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = next { Cmd.hist_updates = [] }
                    : Cmd.hist_past hist
                , Cmd.hist_future = rest
                , Cmd.hist_last_cmd = Just Cmd.UndoRedo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
        State.modify $ merge_undo_states (Cmd.hist_state next)
        mapM_ State.update (Cmd.hist_updates next)
    load_next repo = load_history "load_next_history" $
        SaveGit.load_next_history repo

hist_name :: Cmd.HistoryEntry -> String
hist_name hist = '[' : Seq.join ", " (Cmd.hist_names hist) ++ "] "
    ++ Pretty.pretty (Cmd.hist_commit hist)

load_history :: String
    -> (State.State -> Git.Commit
        -> IO (Either String (Maybe (SaveGit.LoadHistory, Git.Commit))))
     -> Cmd.HistoryEntry -> IO [Cmd.HistoryEntry]
load_history name load hist = case Cmd.hist_commit hist of
    Nothing -> return []
    Just commit -> do
        result <- load (Cmd.hist_state hist) commit
        case result of
            Left err -> do
                Log.error $ name ++ ": " ++ err
                return []
            Right Nothing -> return []
            Right (Just (hist, commit)) -> return [entry commit hist]
    where
    entry commit (SaveGit.History state updates names) =
        Cmd.HistoryEntry state updates names (Just commit)

-- | There are certain parts of the state that I don't want to undo, so
-- inherit them from the old state.  It's confusing when undo moves a window,
-- or a selection, or changes the zoom.
merge_undo_states :: State.State -> State.State -> State.State
merge_undo_states new old = new
    { State.state_views = clip (State.state_views old) $ Map.mapWithKey
        (merge_view (State.state_views old)) (State.state_views new)
    , State.state_blocks = clip (State.state_blocks old) $ Map.mapWithKey
        (merge_block (State.state_blocks old)) (State.state_blocks new)
    , State.state_tracks = clip
        (State.state_tracks old) (State.state_tracks new)
    , State.state_config =
        merge_config (State.state_config new) (State.state_config old)
    }
    where
    clip :: (Id.Ident k, Ord k) => Map.Map k a -> Map.Map k a -> Map.Map k a
    clip = keep_clip Config.clip_namespace

merge_config :: State.Config -> State.Config -> State.Config
merge_config new old = new
    { State.config_namespace = State.config_namespace old
    , State.config_project_dir = State.config_project_dir old
    , State.config_midi = State.config_midi old
    }

merge_view :: Map.Map ViewId Block.View -> ViewId -> Block.View -> Block.View
merge_view old_views view_id new = Map.findWithDefault new view_id old_views

merge_block :: Map.Map BlockId Block.Block
    -> BlockId -> Block.Block -> Block.Block
merge_block old_blocks block_id new = case Map.lookup block_id old_blocks of
    Nothing -> new
    Just old -> new { Block.block_config = Block.block_config old }

-- | The contents of the clipboard should be preserved across undo and redo.
keep_clip :: (Id.Ident k, Ord k) => Id.Namespace -> Map.Map k a
    -> Map.Map k a -> Map.Map k a
keep_clip clip_ns old new = Map.union new (Map.filterWithKey (\k _ -> ns k) old)
    where ns = (==clip_ns) . Id.ident_namespace


-- * responder support

-- Undo has some hooks directly in the responder, since it needs to be run
-- after cmds.

maintain_history :: State.State -> Cmd.State -> [Update.UiUpdate]
    -> IO Cmd.State
maintain_history ui_state cmd_state updates = do
    entries <- if has_saved then mapM commit_entry uncommitted
        else return $ map (history_entry Nothing) uncommitted
    return $ cmd_state
        { Cmd.state_history = hist
            { Cmd.hist_past = take (max 1 keep) $
                bump_updates entries (Cmd.hist_past hist)
            , Cmd.hist_last_cmd = Nothing
            }
        , Cmd.state_history_collect = collect
        }
    where
    (hist, collect, uncommitted) =
        record_history updates ui_state cmd_state
    has_saved = Maybe.isJust $ Cmd.hist_last_save $
        Cmd.state_history_config cmd_state
    keep = Cmd.hist_keep (Cmd.state_history_config cmd_state)

bump_updates :: [Cmd.HistoryEntry] -> [Cmd.HistoryEntry] -> [Cmd.HistoryEntry]
bump_updates [] past = past
bump_updates (new_cur : news) (old_cur : past) =
    -- All I want to do is bump the updates from new_cur to old_cur, but
    -- suppressed records means there can be multiple histories recorded at
    -- once, which makes this a bit more of a hassle.
    new_cur { Cmd.hist_updates = [] }
        : map bump (zip_next new_cur (news ++ [old_cur]))
        ++ past
    where bump (p, c) = c { Cmd.hist_updates = Cmd.hist_updates p }
bump_updates new [] = new -- This should never happen.

zip_next :: a -> [a] -> [(a, a)]
zip_next _ [] = []
zip_next prev (x : xs) = (prev, x) : zip_next x xs

commit_entry :: SaveGit.SaveHistory -> IO Cmd.HistoryEntry
commit_entry hist@(SaveGit.History state _ _) = do
    result <- SaveGit.checkpoint (SaveGit.save_repo state) hist
    commit <- case result of
        Left err -> do
            Log.error $ "error committing history: " ++ err
            return Nothing
        Right commit -> return (Just commit)
    return $ history_entry commit hist

history_entry :: Maybe Git.Commit -> SaveGit.SaveHistory -> Cmd.HistoryEntry
history_entry maybe_commit (SaveGit.History state updates names) =
    -- Recover the CmdUpdates out of the UiUpdates.  I only have to remember
    -- the updates diff won't recreate for me.
    Cmd.HistoryEntry state (Maybe.mapMaybe Update.to_cmd updates)
        names maybe_commit

-- | Record 'Cmd.HistoryEntry's, if required.
record_history :: [Update.UiUpdate] -> State.State -> Cmd.State
    -> (Cmd.History, Cmd.HistoryCollect, [SaveGit.SaveHistory])
record_history updates ui_state cmd_state
    | Just (Cmd.Load commit names) <- Cmd.hist_last_cmd hist =
        -- Switching over to someone else's history.  Wipe out existing
        -- history and record the current state as a commit.
        let new_hist = Cmd.History
                { Cmd.hist_past =
                    [Cmd.HistoryEntry ui_state [] names (Just commit)]
                , Cmd.hist_future = []
                , Cmd.hist_last_cmd = Nothing
                }
        in (new_hist, empty_collect, [])
    | Just Cmd.UndoRedo <- Cmd.hist_last_cmd hist =
        -- If I get an undo while a cmd is suppressed, the last state change
        -- will be undone and the suppressed state change will lost entirely.
        -- This seems basically reasonable, since you could see it as an edit
        -- transaction that was cancelled.
        (hist, empty_collect, [])
    | otherwise = (hist, collect, entries)
    where
    hist = Cmd.state_history cmd_state
    empty_collect = Cmd.empty_history_collect
    (entries, collect) = pure_record_history updates ui_state cmd_state

-- | Get any history entries that should be saved, and the new HistoryCollect.
pure_record_history :: [Update.UiUpdate] -> State.State -> Cmd.State
    -> ([SaveGit.SaveHistory], Cmd.HistoryCollect)
pure_record_history updates ui_state cmd_state
    | not is_recordable && Maybe.isNothing suppress =
        ([], (Cmd.state_history_collect cmd_state) { Cmd.state_cmd_names = [] })
    | is_suppressed = ((,) []) $
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
    cur_entry = SaveGit.History ui_state updates names
    Cmd.HistoryCollect names suppress suppressed_entry =
        Cmd.state_history_collect cmd_state

merge_into_suppressed :: Maybe SaveGit.SaveHistory -> SaveGit.SaveHistory
    -> SaveGit.SaveHistory
merge_into_suppressed Nothing ent = ent
merge_into_suppressed (Just (SaveGit.History _ updates1 names1))
        (SaveGit.History state2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    SaveGit.History state2 (updates1 ++ updates2) names1

should_record :: [Update.UiUpdate] -> Bool
should_record = any (not . Update.is_view_update)

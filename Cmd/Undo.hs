-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, maintain_history) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Git as Git
import qualified Util.Lens as Lens
import qualified Util.Log as Log

import qualified Ui.Block as Block
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.SaveGit as SaveGit
import qualified App.Config as Config
import Types


-- * undo / redo

undo :: (Cmd.M m) => m ()
undo = do
    hist <- Cmd.gets Cmd.state_history
    -- undo is asymmetrical with 'redo' because 'undo' itself is a cmd, and
    -- is happening after the state that is going to be undone.  So the current
    -- state doesn't count (it's the 'undo' cmd), the state I'm coming from is
    -- the previous one, and the one I'm going to is the twice previous one.
    case Cmd.hist_past hist of
        cur : prev : rest -> do_undo hist cur prev rest
        _ -> Cmd.throw "no past to undo"
    where
    do_undo hist cur prev rest = do
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = prev : rest
                , Cmd.hist_future = cur : Cmd.hist_future hist
                , Cmd.hist_undo_redo = Just Cmd.Undo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
        State.modify $ merge_undo_states (Cmd.hist_state prev)
        mapM_ State.update (Cmd.hist_updates cur)

redo :: (Cmd.M m) => m ()
redo = do
    hist <- Cmd.gets Cmd.state_history
    case Cmd.hist_future hist of
        next : rest -> do_redo hist next rest
        [] -> Log.warn "no future to redo"
    where
    do_redo hist next rest = do
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = next : Cmd.hist_past hist
                , Cmd.hist_future = rest
                , Cmd.hist_undo_redo = Just Cmd.Redo
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
        State.modify $ merge_undo_states (Cmd.hist_state next)
        mapM_ State.update (Cmd.hist_updates next)

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
maintain_history ui_state cmd_state updates =
    record_history updates ui_state (discard cmd_state)
    where
    discard = history#past %= take (max 1 keep)
    keep = Cmd.hist_keep (Cmd.state_history_config cmd_state)

-- | Record 'Cmd.HistoryEntry's, if required.
record_history :: [Update.UiUpdate] -> State.State -> Cmd.State -> IO Cmd.State
record_history updates ui_state cmd_state
    -- If I get an undo while a cmd is suppressed, the last state change will
    -- be undone and the suppressed state change will lost entirely.  This
    -- seems basically reasonable, since you could see it as an edit
    -- transaction that was cancelled.
    | Just undo_redo <- Cmd.hist_undo_redo hist = do
        past <- case (undo_redo, Cmd.hist_past hist) of
            (Cmd.Undo, [cur]) -> load_previous_history ui_state cur
            _ -> return []
        future <- case (undo_redo, Cmd.hist_future hist, Cmd.hist_past hist) of
            (Cmd.Redo, [], cur : _) -> do
                load_next_history ui_state cur
            _ -> return []
        return $ cmd_state
            { Cmd.state_history = hist
                { Cmd.hist_undo_redo = Nothing
                , Cmd.hist_past = Cmd.hist_past hist ++ past
                , Cmd.hist_future = future ++ Cmd.hist_future hist
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
    | otherwise = case maybe_entries of
        Nothing -> return $ collect_history collect cmd_state
        Just entries -> do
            entries <- if has_saved then mapM (commit_entry updates) entries
                else return $ map (history_entry Nothing) entries
            return $ cmd_state
                { Cmd.state_history = Cmd.History
                    { Cmd.hist_past = entries ++ Cmd.hist_past hist
                    , Cmd.hist_future = []
                    , Cmd.hist_undo_redo = Nothing
                    }
                , Cmd.state_history_collect = collect
                }
    where
    (maybe_entries, collect) = pure_record_history updates ui_state cmd_state
    has_saved = Maybe.isJust $ Cmd.hist_last_save $
        Cmd.state_history_config cmd_state
    hist = Cmd.state_history cmd_state

collect_history :: Cmd.HistoryCollect -> Cmd.State -> Cmd.State
collect_history collect cmd_state = cmd_state
    { Cmd.state_history = (Cmd.state_history cmd_state)
        { Cmd.hist_undo_redo = Nothing }
    , Cmd.state_history_collect = collect
    }

load_previous_history :: State.State -> Cmd.HistoryEntry
    -> IO [Cmd.HistoryEntry]
load_previous_history =
    load_history "load_previous_history" SaveGit.load_previous_history

load_next_history :: State.State -> Cmd.HistoryEntry -> IO [Cmd.HistoryEntry]
load_next_history = load_history "load_next_history" SaveGit.load_next_history

load_history :: String
    -> (Git.Repo -> State.State -> Git.Commit
        -> IO (Either String (Maybe (SaveGit.History, Git.Commit))))
     -> State.State -> Cmd.HistoryEntry -> IO [Cmd.HistoryEntry]
load_history name load ui_state cur = case Cmd.hist_commit cur of
    Nothing -> return []
    Just commit -> do
        result <- load (SaveGit.save_repo ui_state) ui_state commit
        case result of
            Left err -> do
                Log.error $ name ++ ": " ++ err
                return []
            Right Nothing -> return []
            Right (Just (hist, commit)) ->
                return [history_entry (Just commit) hist]

commit_entry :: [Update.UiUpdate] -> SaveGit.History -> IO Cmd.HistoryEntry
commit_entry updates history@(SaveGit.History state _ _) = do
    result <- SaveGit.checkpoint (SaveGit.save_repo state) history updates
    commit <- case result of
        Left err -> do
            Log.error $ "error committing history: " ++ err
            return Nothing
        Right commit -> return (Just commit)
    return $ history_entry commit history

history_entry :: Maybe Git.Commit -> SaveGit.History -> Cmd.HistoryEntry
history_entry maybe_commit (SaveGit.History state updates names) =
    Cmd.HistoryEntry state updates names maybe_commit

-- | Get any history entries that should be saved, and the new HistoryCollect.
pure_record_history :: [Update.UiUpdate] -> State.State -> Cmd.State
    -> (Maybe [SaveGit.History], Cmd.HistoryCollect)
pure_record_history updates ui_state cmd_state
    | not is_recordable && Maybe.isNothing suppress = (Nothing,
        (Cmd.state_history_collect cmd_state) { Cmd.state_cmd_names = [] })
    | is_suppressed = ((,) Nothing) $
        Cmd.empty_history_collect
            { Cmd.state_suppressed =
                Just $ merge_into_suppressed suppressed_entry cur_entry
            , Cmd.state_suppress_edit =
                Cmd.state_suppress_edit (Cmd.state_history_collect cmd_state)
            }
    | otherwise = (Just entries, Cmd.empty_history_collect)
    where
    is_suppressed =
        suppress == Just (Cmd.state_edit_mode (Cmd.state_edit cmd_state))
    is_recordable = should_record updates

    entries = if is_recordable then [cur_entry] else []
        ++ Maybe.maybeToList suppressed_entry
    cur_entry = SaveGit.History ui_state cmd_updates names
    Cmd.HistoryCollect cmd_updates names suppress suppressed_entry =
        Cmd.state_history_collect cmd_state

merge_into_suppressed :: Maybe SaveGit.History -> SaveGit.History
    -> SaveGit.History
merge_into_suppressed Nothing ent = ent
merge_into_suppressed (Just (SaveGit.History _ updates1 names1))
        (SaveGit.History state2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    SaveGit.History state2 (updates1 ++ updates2) names1

should_record :: [Update.UiUpdate] -> Bool
should_record = any (not . Update.is_view_update)


--

history = Lens.lens Cmd.state_history (\v r -> r { Cmd.state_history = v })
past = Lens.lens Cmd.hist_past (\v r -> r { Cmd.hist_past = v })
-- future = Lens.lens Cmd.hist_future (\v r -> r { Cmd.hist_future = v })

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
import qualified Ui.SaveGit as SaveGit
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
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
                , Cmd.hist_undo_redo = True
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
                , Cmd.hist_undo_redo = True
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

maintain_history :: State.State -> Cmd.State -> IO Cmd.State
maintain_history ui_state cmd_state =
    record_history ui_state (discard cmd_state)
    where
    discard = history#past %= take (max 1 keep)
    keep = Cmd.hist_keep (Cmd.state_history_config cmd_state)

-- | Record 'Cmd.HistoryEntry's, if required.
record_history :: State.State -> Cmd.State -> IO Cmd.State
record_history ui_state cmd_state
    | is_undo = case Cmd.hist_past (Cmd.state_history cmd_state) of
        [cur] -> load_previous_history cmd_state ui_state cur
        _ -> return $ reset_history cmd_state
    | otherwise = case maybe_entries of
        Nothing -> return $ collect_history collect cmd_state
        Just entries -> do
            entries <- if has_saved then mapM commit_entry entries
                else return $ map (history_entry Nothing) entries
            return $ cmd_state
                { Cmd.state_history = Cmd.History
                    { Cmd.hist_past = entries
                        ++ Cmd.hist_past (Cmd.state_history cmd_state)
                    , Cmd.hist_future = []
                    , Cmd.hist_undo_redo = False
                    }
                , Cmd.state_history_collect = collect
                }
    where
    (maybe_entries, collect) = pure_record_history ui_state cmd_state
    is_undo = Cmd.hist_undo_redo (Cmd.state_history cmd_state)
    has_saved = Maybe.isJust $ Cmd.hist_last_save $
        Cmd.state_history_config cmd_state

collect_history :: Cmd.HistoryCollect -> Cmd.State -> Cmd.State
collect_history collect cmd_state = cmd_state
    { Cmd.state_history = (Cmd.state_history cmd_state)
        { Cmd.hist_undo_redo = False }
    , Cmd.state_history_collect = collect
    }

reset_history :: Cmd.State -> Cmd.State
reset_history = collect_history Cmd.empty_history_collect

load_previous_history :: Cmd.State -> State.State -> Cmd.HistoryEntry
    -> IO Cmd.State
load_previous_history cmd_state ui_state cur = do
    result <- maybe (return $ Right Nothing)
        (SaveGit.load_previous_history (SaveGit.save_repo ui_state) ui_state)
        (Cmd.hist_commit cur)
    case result of
        Left err -> do
            Log.error $ "load_previous_history: " ++ err
            return $ reset_history cmd_state
        Right Nothing -> return $ reset_history cmd_state
        Right (Just (hist, commit)) ->
            return $ update_history cur (history_entry (Just commit) hist)
    where
    update_history cur prev = cmd_state
        { Cmd.state_history = (Cmd.state_history cmd_state)
            { Cmd.hist_undo_redo = False
            , Cmd.hist_past = [cur, prev]
            }
        , Cmd.state_history_collect = Cmd.empty_history_collect
        }

history = Lens.lens Cmd.state_history (\v r -> r { Cmd.state_history = v })
past = Lens.lens Cmd.hist_past (\v r -> r { Cmd.hist_past = v })
-- future = Lens.lens Cmd.hist_future (\v r -> r { Cmd.hist_future = v })

{-
    The updates are a bit confusing.  I could not save them and do a full diff.
    That would probably make undo and redo slow on a large score.  How about
    make a new kind of update that only has track ranges, and remove inversion
    since it's broken anyway.

    History:
    ([], a)     (a->b, b)   (b->c, c)   (c->d, d)

    Drop entries 'a' and 'b'.
    Undo reverses updates in last entry:
                            (b->c, c)   |cur        (d->c, d)
    Only 1 past remains, load another one, updates are the wrong direction:
                (c->b, b)   (b->c, c)   |cur        (d->c, d)
    Undo again:
                (c->b, b)   |cur        (b->c, c)   (c->d, d)
    1 past remains, load another one:
    (b->a, a)   (c->b, b)   |cur        (b->c, c)   (c->d, d)
-}

commit_entry :: SaveGit.History -> IO Cmd.HistoryEntry
commit_entry history@(SaveGit.History state _ _) = do
    result <- SaveGit.checkpoint (SaveGit.save_repo state) history
    commit <- case result of
        Left err -> do
            Log.error $ "error committing history: " ++ err
            return Nothing
        Right commit -> return (Just commit)
    putStrLn $ "commit entry: " ++ show commit
    return $ history_entry commit history

history_entry :: Maybe Git.Commit -> SaveGit.History -> Cmd.HistoryEntry
history_entry maybe_commit (SaveGit.History state updates names) =
    Cmd.HistoryEntry state updates names maybe_commit

-- | Get any history entries that should be saved, and the new HistoryCollect.
pure_record_history :: State.State -> Cmd.State
    -> (Maybe [SaveGit.History], Cmd.HistoryCollect)
pure_record_history ui_state cmd_state
    -- If I get an undo while a cmd is suppressed, the last state change will
    -- be undone and the suppressed state change will lost entirely.  This
    -- seems basically reasonable, since you could see it as an edit
    -- transaction that was cancelled.
    | is_undo = (Nothing, Cmd.empty_history_collect)
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
    -- Don't record history if I just did an undo or redo.
    is_undo = Cmd.hist_undo_redo (Cmd.state_history cmd_state)
    is_suppressed =
        suppress == Just (Cmd.state_edit_mode (Cmd.state_edit cmd_state))
    is_recordable = should_record updates

    Cmd.HistoryCollect updates names suppress suppressed_entry =
        Cmd.state_history_collect cmd_state

    entries = if is_recordable then [cur_entry] else []
        ++ Maybe.maybeToList suppressed_entry
    cur_entry = SaveGit.History ui_state updates names

merge_into_suppressed :: Maybe SaveGit.History -> SaveGit.History
    -> SaveGit.History
merge_into_suppressed Nothing ent = ent
merge_into_suppressed (Just (SaveGit.History _ updates1 names1))
        (SaveGit.History state2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    SaveGit.History state2 (updates1 ++ updates2) names1

should_record :: [Update.CmdUpdate] -> Bool
should_record = any (not . Update.is_view_update)

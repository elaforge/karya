-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, record_history) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.SaveGit as SaveGit
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
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
        _ -> Log.warn "no past to undo"
    where
    do_undo hist cur prev rest = do
        Cmd.modify $ \st -> st
            { Cmd.state_history = Cmd.History
                { Cmd.hist_past = prev : rest
                , Cmd.hist_future = invert_hist cur : Cmd.hist_future hist
                , Cmd.hist_undo_redo = True
                }
            , Cmd.state_history_collect = Cmd.empty_history_collect
            }
        State.modify $ merge_undo_states (Cmd.hist_state prev)
        mapM_ State.update $ invert prev cur
    -- hist_to and hist_from is the same because I want to invert the updates
    -- in place, so that the updates that used to be relative to the previous
    -- state are now relative to the current one.
    invert_hist hist = hist { Cmd.hist_updates = invert hist hist }
    invert hist_to hist_from =
        Maybe.mapMaybe (invert_update (Cmd.hist_state hist_to))
            (Cmd.hist_updates hist_from)

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
    { State.state_views = Map.mapWithKey
        (merge_view (State.state_views old)) (State.state_views new)
    , State.state_blocks = Map.mapWithKey
        (merge_block (State.state_blocks old)) (State.state_blocks new)
    , State.state_config =
        merge_config (State.state_config new) (State.state_config old)
    }

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


-- * responder support

-- Undo has some hooks directly in the responder, since it needs to be run
-- after cmds.

-- | Record 'Cmd.HistoryEntry's, if required.
record_history :: State.State -> Cmd.State -> IO Cmd.State
record_history ui_state cmd_state = do
    let (maybe_entries, collect) = pure_record_history ui_state cmd_state
    case maybe_entries of
        Nothing -> return $ cmd_state
            { Cmd.state_history = (Cmd.state_history cmd_state)
                { Cmd.hist_undo_redo = False
                }
            , Cmd.state_history_collect = collect
            }
        Just entries -> do
            -- TODO check if there is a SavePoint, don't commit if there isn't
            -- entries <- mapM commit_entry entries
            entries <- return $ map uncommitted_entry entries
            return $ cmd_state
                { Cmd.state_history = Cmd.History
                    { Cmd.hist_past = entries
                        ++ Cmd.hist_past (Cmd.state_history cmd_state)
                    , Cmd.hist_future = []
                    , Cmd.hist_undo_redo = False
                    }
                , Cmd.state_history_collect = collect
                }

uncommitted_entry :: Cmd.UncommittedHistoryEntry -> Cmd.HistoryEntry
uncommitted_entry (Cmd.UncommittedHistoryEntry state updates names) =
    Cmd.HistoryEntry state updates names Nothing

commit_entry :: Cmd.UncommittedHistoryEntry -> IO Cmd.HistoryEntry
commit_entry (Cmd.UncommittedHistoryEntry state updates names) = do
    result <- SaveGit.checkpoint (State.config#State.project_dir #$ state)
        state updates
    commit <- case result of
        Left err -> do
            Log.error $ "error committing history: " ++ err
            return Nothing
        Right commit -> return (Just commit)
    return $ Cmd.HistoryEntry state updates names commit

-- | Get any history entries that should be saved, and the new HistoryCollect.
pure_record_history :: State.State -> Cmd.State
    -> (Maybe [Cmd.UncommittedHistoryEntry], Cmd.HistoryCollect)
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
    cur_entry = Cmd.UncommittedHistoryEntry ui_state updates names

merge_into_suppressed :: Maybe Cmd.UncommittedHistoryEntry
    -> Cmd.UncommittedHistoryEntry -> Cmd.UncommittedHistoryEntry
merge_into_suppressed Nothing ent = ent
merge_into_suppressed (Just (Cmd.UncommittedHistoryEntry _ updates1 names1))
        (Cmd.UncommittedHistoryEntry state2 updates2 _) =
    -- Keep the name of the first suppressed cmd.  The rest are likely to be
    -- either duplicates or unrecorded cmds like selection setting.
    Cmd.UncommittedHistoryEntry state2 (updates1 ++ updates2) names1

should_record :: [Update.CmdUpdate] -> Bool
should_record = any (not . Update.is_view_update)

-- | Most updates need not be stored since they can be generated by 'diff'
-- given two States.  However, some are too expensive for that.
--
-- When a HistoryEntry is regenerated after an undo or redo, the update
-- needs to be inverted from its previous meaning.
--
-- Actually, I think the inversion is unnecessary since the update contents
-- are only looked at by incremental save, and undos are not saved.  But I
-- have to put something there, so I might as well put the right thing.
invert_update :: State.State -> Update.CmdUpdate -> Maybe Update.CmdUpdate
invert_update state (Update.TrackUpdate tid update) =
    Update.TrackUpdate tid <$> do
        track <- Map.lookup tid (State.state_tracks state)
        case update of
            Update.TrackEvents s e _ -> Just $ Update.TrackEvents s e
                (Events.in_range s e (Track.track_events track))
            Update.TrackAllEvents _ -> Just $ Update.TrackAllEvents
                (Track.track_events track)
            _ -> Nothing
invert_update _ _ = Nothing

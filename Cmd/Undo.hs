-- | Undo and redo cmds and support.
module Cmd.Undo (undo, redo, record_history) where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Ui.Block as Block
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import Types


-- * undo / redo

undo :: (Cmd.M m) => m ()
undo = do
    hist <- Cmd.gets Cmd.state_history
    -- 'undo' is asymmetrical with 'redo' because 'undo' itself is a cmd, and
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
                , Cmd.hist_serial = Cmd.hist_serial hist - 1
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
                , Cmd.hist_serial = Cmd.hist_serial hist + 1
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

-- | Record a 'Cmd.HistoryEntry'.
--
-- I do the traditional thing where an action deletes the redo buffer.  At some
-- point I could think about a real branching history.
record_history :: State.State -> Cmd.State -> Cmd.State
record_history ui_to cmd_state
    | not skip && should_record updates = cmd_state
        { Cmd.state_history = cur
        , Cmd.state_history_collect = Cmd.empty_history_collect
        }
    | skip = cmd_state { Cmd.state_history_collect = Cmd.empty_history_collect }
        -- Be careful to not modify it when I don't need to, otherwise this
        -- can build up unevaluated thunks until the history is forced.
    | otherwise = cmd_state
        { Cmd.state_history_collect = (Cmd.state_history_collect cmd_state)
            { Cmd.state_cmd_names = [] }
        }
    where
    -- Don't record history if I just did an undo or redo.
    skip = Cmd.hist_undo_redo $ Cmd.state_history cmd_state
    updates = Cmd.state_updates (Cmd.state_history_collect cmd_state)
    names = Cmd.state_cmd_names (Cmd.state_history_collect cmd_state)
    prev = Cmd.state_history cmd_state
    cur = Cmd.History
        { Cmd.hist_past = Cmd.HistoryEntry ui_to updates names
            : Cmd.hist_past prev
        , Cmd.hist_future = []
        , Cmd.hist_undo_redo = False
        , Cmd.hist_serial = Cmd.hist_serial prev + 1
        }

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

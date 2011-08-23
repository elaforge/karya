{- | Undo and redo cmds and support.

    - record only diffs, so I can save the history along with Ui.State

    - this undo ignores view-only changes, but it would be nice to have
    separate undo for view changes, or at least zoom / scroll settings

    - undo within selected area

    - if Cmds have a name return, history entries could have names

    - branching history so I can back out of changes?
-}
module Cmd.Undo (undo, redo, record_history) where
import qualified Data.Map as Map

import qualified Util.Log as Log
import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd


-- * undo / redo

undo :: (Cmd.M m) => m ()
undo = do
    (past, future) <- Cmd.gets Cmd.state_history
    now <- State.get
    case past of
        prev : rest -> do
            Cmd.modify_state $ \st -> st
                { Cmd.state_history = (rest,
                    history_entry now (Cmd.hist_updates prev) : future)
                , Cmd.state_skip_history_record = True
                }
            State.modify $ merge_undo_states (Cmd.hist_state prev)
            mapM_ State.update (Cmd.hist_updates prev)
            -- Edit.initialize_state
        [] -> Log.warn "no past to undo"

redo :: (Cmd.M m) => m ()
redo = do
    (past, future) <- Cmd.gets Cmd.state_history
    now <- State.get
    case future of
        next : rest -> do
            Cmd.modify_state $ \st -> st
                { Cmd.state_history =
                    (history_entry now (Cmd.hist_updates next) : past, rest)
                , Cmd.state_skip_history_record = True }
            State.modify $ merge_undo_states (Cmd.hist_state next)
            mapM_ State.update (Cmd.hist_updates next)
            -- Edit.initialize_state
        [] -> Log.warn "no future to redo"

-- | There are certain parts of the state that I don't want to undo, so
-- inherit them from the old state.  It's confusing when undo moves a window,
-- or a selection, or changes the zoom.
merge_undo_states :: State.State -> State.State -> State.State
merge_undo_states new old = new {
    State.state_namespace = State.state_namespace old
    , State.state_project_dir = State.state_project_dir old
    , State.state_views = Map.mapWithKey
        (merge_view (State.state_views old)) (State.state_views new)
    , State.state_blocks = Map.mapWithKey
        (merge_block (State.state_blocks old)) (State.state_blocks new)
    , State.state_midi_config = State.state_midi_config old
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
-- after cmds and needs access to the old state.

-- | Record a 'Cmd.HistoryEntry'.
--
-- Do the traditional thing where an action deletes the redo buffer.  At some
-- point I could think about a real branching history, but not now.
--
-- The distinction between cmd_updates and all_updates is icky.  Undo has to
-- invert updates for them to be usable in undo, e.g. ViewSize should record
-- the previous ViewSize rather than the current one.  However, it happens
-- that the updates recorded in "State.State" are only TrackEvents or TrackAll
-- which don't mention any data so they're the same inverted.  The other
-- updates are all cheaply obtainable from diff, so those don't go in the
-- HistoryEntry.
--
-- TODO another source of problems is if a cmd skips history record and has
-- a cmd_update, that will be lost.  That can't happen currently but could
-- in the future.
record_history :: [Update.Update] -> [Update.Update]
    -> State.State -> Cmd.State -> Cmd.State
record_history cmd_updates all_updates old_state cmd_state
    | not skip && should_record_history all_updates = cmd_state
        { Cmd.state_history = new_hist, Cmd.state_skip_history_record = False }
    | skip = cmd_state { Cmd.state_skip_history_record = False }
        -- Be careful to not modify it when I don't need to, otherwise this
        -- can build up unevaluated thunks until the history is forced.
    | otherwise = cmd_state
    where
    skip = Cmd.state_skip_history_record cmd_state
    hist = fst (Cmd.state_history cmd_state)
    new_hist = (history_entry old_state cmd_updates : hist, [])

should_record_history :: [Update.Update] -> Bool
should_record_history = any (not . Update.is_view_update)

history_entry :: State.State -> [Update.Update] -> Cmd.HistoryEntry
history_entry state updates
    -- Serious error, an internal invariant has been violated.
    -- Should have been enforced in State.update.
    | not (all Update.invertable updates) = error $
        "non-invertable update can't go in HistoryEntry: " ++ show updates
    | otherwise = Cmd.HistoryEntry state updates


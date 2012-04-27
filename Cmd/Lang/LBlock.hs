module Cmd.Lang.LBlock where
import qualified Data.Map as Map

import Util.Control
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import Types


-- | Rename a block and all occurrances in the current block.
--
-- It doesn't update TrackIds so they may still be named under their old block,
-- but track id names aren't supposed to carry meaning anyway.
rename :: BlockId -> BlockId -> Cmd.CmdL ()
rename from to = Create.rename_block from to

-- | Rename block calls in a single block.
replace :: BlockId -> BlockId -> Cmd.CmdL ()
replace from to = do
    block_id <- Cmd.get_focused_block
    ModifyEvents.block_tracks block_id $ \_ events ->
        return $ Just $ map (replace_block_call from to) events

replace_block_call :: BlockId -> BlockId -> Events.PosEvent -> Events.PosEvent
replace_block_call from to pevent@(pos, evt)
    | Event.event_string evt == Id.ident_name from =
        (pos, Event.set_string (Id.ident_name to) evt)
    | Event.event_string evt == Id.ident_string from =
        (pos, Event.set_string (Id.ident_string to) evt)
    | otherwise = pevent


-- * create

-- | If the events under the cursor are a block calls, create blocks that don't
-- already exist.  Optionally use a model block.
block_for_event :: Maybe BlockId -> Cmd.CmdL ()
block_for_event model = mapM_ make =<< Selection.events
    where
    make (_, _, events) = mapM_ (make_named model . Event.event_string . snd)
        events

make_named :: Maybe BlockId -> String -> Cmd.CmdL ()
make_named template name = whenM (can_create name) $ case template of
    Nothing -> do
        template_id <- Cmd.get_focused_block
        Create.block_from_template False template_id
    Just template_id -> Create.block_from_template True template_id

can_create :: (State.M m) => String -> m Bool
can_create "" = return False
can_create name = do
    ns <- State.get_namespace
    case Types.BlockId <$> Id.make ns name of
        Just block_id -> not . Map.member block_id
            <$> State.gets State.state_blocks
        Nothing -> return False

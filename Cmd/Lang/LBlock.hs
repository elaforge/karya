module Cmd.Lang.LBlock where
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Id as Id

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.ModifyEvents as ModifyEvents

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

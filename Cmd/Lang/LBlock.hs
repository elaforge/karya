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
--
-- TODO throughout the entire piece?
rename :: BlockId -> BlockId -> Cmd.CmdL ()
rename from to = do
    Create.rename_block from to
    block_id <- Cmd.get_focused_block
    ModifyEvents.map_note_tracks block_id (Just . replace_block_call from to)

replace_block_call :: BlockId -> BlockId -> Events.PosEvent -> Events.PosEvent
replace_block_call from to pevent@(pos, evt)
    | Event.event_string evt == Id.ident_name from =
        (pos, Event.set_string (Id.ident_name to) evt)
    | Event.event_string evt == Id.ident_string from =
        (pos, Event.set_string (Id.ident_string to) evt)
    | otherwise = pevent

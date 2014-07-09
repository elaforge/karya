-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for talams for Carnatic music.
module Cmd.Repl.LTala where
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Repl.LEvent as LEvent
import qualified Cmd.Repl.LRuler as LRuler
import qualified Cmd.Selection as Selection
import qualified Cmd.Tala as Tala


-- | Convert the selection from chatusram nadai to tisram nadai.
chatusram_to_tisram :: Cmd.CmdL ()
chatusram_to_tisram = do
    (block_id, _, _, start, end) <- Selection.tracks
    ModifyEvents.selection $
        ModifyEvents.event $ LEvent.stretch_event start (2/3)
    let dur = (end - start) * (2/3)
    LRuler.local
        (block_id, LRuler.replace_range start (start + dur) (Tala.adi3 10))
    -- Delete final 1/3.
    Edit.delete_block_time block_id (start + dur) end

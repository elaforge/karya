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
import qualified Cmd.Ruler.Tala as Tala
import qualified Cmd.Selection as Selection

import qualified Ui.Events as Events
import qualified Ui.Meter.Meter as Meter


-- TODO broken:
-- takes 4 to 2 2/3, instead of 3
-- I get a fragment section, and following sections are at .99999

-- | Convert the selection from chatusram nadai to tisram nadai.
chatusram_to_tisram :: Cmd.CmdL ()
chatusram_to_tisram = do
    (block_id, _, _, range) <- Selection.tracks
    let (start, end) = Events.range_times range
    ModifyEvents.selection $
        ModifyEvents.event $ LEvent.stretch_event start (2/3)
    let dur = (end - start) * (2/3)
    LRuler.local $ LRuler.modify_selected $
        LRuler.replace_range start (start + dur) $
        Meter.meter_sections (Tala.make_until Tala.adi_tala 3 1 dur)
    -- Delete final 1/3.
    Edit.delete_block_time block_id (start + dur) (end - (start+dur))

-- | Create adi tala in chatusram-tisram.  Assuming 1t per aksharam, there
-- is 3/4 per c-t aksharam.  So 8 of them fits in 6t, so each one is 6/8t.
chatis :: Tala.Avartanams -> Tala.Nadai -> Meter.Meter
chatis avartanams nadai =
    -- TODO I think originally the 6/8 was akshara dur, not avartanam dur
    Tala.make Tala.adi_tala nadai (6/8) avartanams 1

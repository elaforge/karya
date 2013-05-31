-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Keymap cmds for a NoteTrack.  These apply regardless of the edit mode.
module Cmd.NoteTrackKeymap where
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Info as Info
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (plain_command_char, command_char)
import qualified Cmd.Selection as Selection


make_keymap :: (Cmd.M m) => (Keymap.CmdMap m, [String])
make_keymap = Keymap.make_cmd_map $ concat
    [ command_char 'm' "toggle merged" cmd_toggle_merged
    , plain_command_char '.' "dur * 1.5" (Edit.modify_dur (*1.5))
    , plain_command_char ',' "dur / 1.5" (Edit.modify_dur (/1.5))
    ]

cmd_toggle_merged :: (Cmd.M m) => m ()
cmd_toggle_merged = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    pitch <- Cmd.require =<< Info.pitch_of_note block_id tracknum
    btrack <- State.get_block_track_at block_id tracknum
    if null (Block.track_merged btrack)
        then State.merge_track block_id tracknum (State.track_tracknum pitch)
        else State.unmerge_track block_id tracknum

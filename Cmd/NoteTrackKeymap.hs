-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Keymap cmds for a NoteTrack.  These apply regardless of the edit mode.
module Cmd.NoteTrackKeymap where
import qualified Data.Set as Set

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.BlockConfig as BlockConfig
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (command_char)
import qualified Cmd.ModifyEvents as ModifyEvents
import qualified Cmd.Selection as Selection

import qualified Derive.ParseTitle as ParseTitle
import Global


make_keymap :: Cmd.M m => (Keymap.CmdMap m, [Text])
make_keymap = Keymap.make_cmd_map $ concat
    [ command_char 'm' "toggle merged" toggle_merged
    , command_char '.' "add ." (add_transform_generator ".")
    ]

-- | Add a call that works both as a transformer and generator, as long as
-- it isn't already there.
add_transform_generator :: Cmd.M m => Text -> m ()
add_transform_generator text =
    ModifyEvents.selection_advance $
    ModifyEvents.tracks_named ParseTitle.is_note_track $
    ModifyEvents.text $ ModifyEvents.pipeline add
    where
    add [] = [[text]]
    add calls
        | [text] `elem` calls = calls
        | otherwise = [text] : calls

toggle_merged :: Cmd.M m => m ()
toggle_merged = do
    (block_id, tracknum, _, _) <- Selection.get_insert
    btrack <- Cmd.abort_unless =<< State.block_track_at block_id tracknum
    if Set.null (Block.track_merged btrack)
        then whenM (BlockConfig.is_control_or_pitch block_id (tracknum + 1)) $
            State.merge_track block_id tracknum (tracknum + 1)
        else State.unmerge_track block_id tracknum

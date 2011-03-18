-- | Keymap cmds for a NoteTrack.  These apply regardless of the edit mode.
module Cmd.NoteTrackKeymap where

import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (command_char, command_only, bind_char)

import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection


make_keymap :: (Cmd.M m) => NoteTrack.PitchTrack -> (Keymap.CmdMap m, [String])
make_keymap pitch_track = Keymap.make_cmd_map $ concat
    [ command_only 'm' "toggle merged" (cmd_toggle_merged pitch_track)

    , command_char 's' "set dur" Edit.cmd_set_duration
    , command_char '.' "dur * 1.5" (Edit.cmd_modify_dur (*1.5))
    , command_char ',' "dur / 1.5" (Edit.cmd_modify_dur (/1.5))

    , bind_char '_' "insert track end" cmd_insert_track_end
    ]

-- | This can be used to extend the length of a block so when it is subderived
-- it has the right length.
--
-- If it's more convenient, I could remove any existing "--" events before
-- inserting the new one.
cmd_insert_track_end :: (Cmd.M m) => m ()
cmd_insert_track_end = Edit.insert_event "--" 0

cmd_toggle_merged :: (Cmd.M m) => NoteTrack.PitchTrack -> m ()
cmd_toggle_merged (NoteTrack.CreateTrack {}) =
    Cmd.throw "no pitch track to collapse"
cmd_toggle_merged (NoteTrack.ExistingTrack pitch_tracknum _) = do
    (block_id, note_tracknum, _, _) <- Selection.get_insert
    btrack <- State.get_block_track block_id note_tracknum
    if null (Block.track_merged btrack)
        then State.merge_track block_id note_tracknum pitch_tracknum
        else State.unmerge_track block_id note_tracknum

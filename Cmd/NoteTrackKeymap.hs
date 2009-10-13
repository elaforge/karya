-- | Keymap cmds for a NoteTrack.  These apply regardless of the edit mode.
module Cmd.NoteTrackKeymap where

import qualified Ui.Block as Block
import qualified Ui.Key as Key
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Edit as Edit
import qualified Cmd.Keymap as Keymap
import Cmd.Keymap (bind_mod, command_char)
import Cmd.Keymap (SimpleMod(..))

import qualified Cmd.NoteTrack as NoteTrack
import qualified Cmd.Selection as Selection


make_keymap :: (Monad m) => NoteTrack.PitchTrack -> (Keymap.CmdMap m, [String])
make_keymap pitch_track = Keymap.make_cmd_map $ concat
    [ bind_mod [Shift, PrimaryCommand] (Key.KeyChar 'm')
        "toggle merged" (cmd_toggle_merged pitch_track)

    , command_char 's' "set dur" Edit.cmd_set_duration
    , command_char '.' "dur * 1.5" (Edit.cmd_modify_dur (*1.5))
    , command_char ',' "dur / 1.5" (Edit.cmd_modify_dur (/1.5))
    , command_char 'p' "paste events" Edit.cmd_paste_events
    ]

cmd_toggle_merged :: (Monad m) => NoteTrack.PitchTrack -> Cmd.CmdT m ()
cmd_toggle_merged (NoteTrack.CreateTrack _ _ _) =
    Cmd.throw "no pitch track to collapse"
cmd_toggle_merged (NoteTrack.ExistingTrack pitch_tracknum) = do
    (block_id, note_tracknum, _, _) <- Selection.get_insert
    btrack <- State.get_block_track block_id note_tracknum
    if null (Block.track_merged btrack)
        then State.merge_track block_id note_tracknum pitch_tracknum
        else State.unmerge_track block_id note_tracknum


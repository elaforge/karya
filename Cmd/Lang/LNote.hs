-- | Utilities that use "Cmd.ModifyNote" to do higher-level transformations.
module Cmd.Lang.LNote where
import Util.Control
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection


-- | Merge the selected note tracks into one.
merge :: Cmd.CmdL ()
merge = ModifyNotes.selection $ ModifyNotes.modify_note $ \note ->
    note { ModifyNotes.note_index = 0 }

-- | Distribute the notes among the given number of tracks, round-robin.  Since
-- only each note only carries over the controls in its extend, if there are
-- notes that rely on control values carried forward, the values will be
-- different in the new tracks.
distribute_n :: Int -> Cmd.CmdL ()
distribute_n tracks = ModifyNotes.selection $ \note_envs -> return $
        zipWith (modify tracks) [0..] (map fst note_envs)
    where
    modify tracks n note = note { ModifyNotes.note_index = n `mod` tracks }

-- | Like 'distribute_n', but use only the selected tracks.
distribute :: Cmd.CmdL ()
distribute = do
    (block_id, _, track_ids, _, _) <- Selection.tracks
    tracks <- length <$> ModifyNotes.extract_note_trees block_id track_ids
    distribute_n tracks

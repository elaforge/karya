-- | Utilities that use "Cmd.ModifyNote" to do higher-level transformations.
module Cmd.Lang.LNote where
import qualified Data.List as List

import Util.Control
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import Types


-- * query

notes :: Cmd.CmdL [(ModifyNotes.Note, TrackId)]
notes = ModifyNotes.selection_notes

note_controls :: Cmd.CmdL
    [(ModifyNotes.Note, (Maybe PitchSignal.Pitch, PitchSignal.Controls))]
note_controls = do
    block_id <- Cmd.get_focused_block
    ModifyNotes.annotate_controls block_id =<< notes

-- * modify

-- | Merge the selected note tracks into one.
merge :: Cmd.CmdL ()
merge = ModifyNotes.selection $ ModifyNotes.modify_note $ set_index 0

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

-- | Try to compact non-overlapping notes to use the least number of tracks
-- possible.
compact :: Cmd.CmdL ()
compact = ModifyNotes.selection $
        return . snd . List.mapAccumL allocate [] . map fst
    where
    allocate state note = (next, set_index i note)
        where
        (i, next) = find_index (ModifyNotes.note_start note)
            (ModifyNotes.note_end note) state

-- | Find the lowest index that a note will fit.  Search the list of end times
-- for one at or before the given start, and return that index and upate the
-- list with the new end.
find_index :: ScoreTime -> ScoreTime -> [ScoreTime] -> (Int, [ScoreTime])
find_index start end = go 0
    where
    go i [] = (i, [end])
    go i (t:ts)
        | t <= start = (i, end : ts)
        | otherwise = second (t:) $ go (i+1) ts

-- | If it's above the nn, compact starting at the high_index, otherwise
-- compact starting at index 0.
split_on_pitch :: ModifyNotes.Index -> Pitch.NoteNumber -> Cmd.CmdL ()
split_on_pitch high_index break_nn = do
    block_id <- Cmd.get_focused_block
    ModifyNotes.selection $ \notes -> do
        notes <- ModifyNotes.annotate_nns block_id notes
        return $ split notes
    where
    split xs = snd (List.mapAccumL allocate ([], []) xs)
    allocate (low_alloc, high_alloc) (note, maybe_nn)
        | maybe True (<=break_nn) maybe_nn =
            let (i, next) = find low_alloc
            in ((next, high_alloc), set_index i note)
        | otherwise =
            let (i, next) = find high_alloc
            in ((low_alloc, next), set_index (high_index + i) note)
        where
        find = find_index (ModifyNotes.note_start note)
            (ModifyNotes.note_end note)

set_index :: ModifyNotes.Index -> ModifyNotes.Note -> ModifyNotes.Note
set_index i note = note { ModifyNotes.note_index = i }

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that use "Cmd.ModifyNotes" to do higher-level transformations.
module Cmd.Repl.LNote where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Map
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import Global
import Types


-- * query

notes :: Cmd.CmdL [(ModifyNotes.Note, TrackId)]
notes = ModifyNotes.selected_notes

note_controls :: Cmd.CmdL
    [(ModifyNotes.Note, (Maybe PSignal.Transposed, Score.ControlValMap))]
note_controls = do
    block_id <- Cmd.get_focused_block
    events <- Cmd.perf_events <$> Cmd.get_performance block_id
    note_track_ids <- notes
    return $ ModifyNotes.find_controls note_track_ids events

-- * modify

-- | Merge the selected note tracks into one.
merge :: Cmd.CmdL ()
merge = ModifyNotes.selection $ ModifyNotes.note $ ModifyNotes.index #= 0

-- | Distribute the notes among the given number of tracks, round-robin.  Since
-- only each note only carries over the controls in its extent, if there are
-- notes that rely on control values carried forward, the values will be
-- different in the new tracks.
distribute_n :: Int -> Cmd.CmdL ()
distribute_n tracks = ModifyNotes.selection $ \_ notes -> return $
        zipWith (modify tracks) [0..] (map fst notes)
    where
    modify tracks n = ModifyNotes.index #= n `mod` tracks

-- | Like 'distribute_n', but use only the selected tracks.
distribute :: Cmd.CmdL ()
distribute = do
    (block_id, _, track_ids, _) <- Selection.tracks
    tracks <- length <$> ModifyNotes.extract_note_trees block_id track_ids
    distribute_n tracks

-- | Try to compact non-overlapping notes to use the least number of tracks
-- possible.
compact :: Cmd.CmdL ()
compact =
    ModifyNotes.selection $ const $
        return . snd . List.mapAccumL allocate [] . map fst
    where
    allocate state note = (next, ModifyNotes.index #= i $ note)
        where
        (i, next) = find_index (ModifyNotes.note_start note)
            (ModifyNotes.note_end note) state

-- | Find the lowest index that a note will fit.  Search the list of end times
-- for one at or before the given start, and return that index and update the
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
--
-- For example, to split on middle C, assuming only dyads in the left hand:
--
-- > LNote.split_on_pitch 2 NN.c4
split_on_pitch :: ModifyNotes.Index -> Pitch.NoteNumber -> Cmd.CmdL ()
split_on_pitch high_index break_nn =
    ModifyNotes.selection $ ModifyNotes.annotate_nns $ \notes ->
        return $ split notes
    where
    split = snd . List.mapAccumL allocate ([], [])
    allocate (low_alloc, high_alloc) (note, maybe_nn)
        | maybe True (<=break_nn) maybe_nn =
            let (i, next) = find low_alloc
            in ((next, high_alloc), ModifyNotes.index #= i $ note)
        | otherwise =
            let (i, next) = find high_alloc
            in ((low_alloc, next), ModifyNotes.index #= (high_index + i) $ note)
        where
        find = find_index (ModifyNotes.note_start note)
            (ModifyNotes.note_end note)

-- | Sort by pitch and compact.
sort_on_pitch :: Cmd.M m => Bool -> m ()
sort_on_pitch high_left = ModifyNotes.selection $ ModifyNotes.annotate_nns $
    return . extract . foldl' insert mempty
    where
    insert state note =
        insert_ordered (\n1 n2 -> cmp (snd n1) (snd n2)) note state
    cmp = if high_left then (<) else (>)

-- | Find the last index with an overlapping note that isn't place_before,
-- and put the note on index+1.  If it it overlaps, make a space by bumping
-- tracks up by one.
insert_ordered :: ((ModifyNotes.Note, a) -> (ModifyNotes.Note, a) -> Bool)
    -> (ModifyNotes.Note, a) -> State a -> State a
insert_ordered place_before note state = case Map.lookup index state of
    Just (n : _) | overlap note n ->
        Map.insert index [note] $ bump_index index state
    _ -> insert_cons index note state
    where
    index = maybe 0 ((+1) . fst) $ Seq.last $
        takeWhile (place_before note . snd) overlapping
    overlapping = [(i, n) | (i, n : _) <- Map.toAscList state, overlap note n]
    overlap n1 n2 = ModifyNotes.notes_overlap (fst n1) (fst n2)

bump_index :: Int -> Map Int a -> Map Int a
bump_index index m =
    pre <> Map.fromAscList (map (first (+1)) (Map.toAscList post))
    where (pre, post) = Util.Map.split2 index m

extract :: State a -> [ModifyNotes.Note]
extract state = concat $
    [ map ((ModifyNotes.index #= i) . fst) (reverse notes)
    | (i, notes) <- Map.toAscList state
    ]

-- | From track index to notes in reverse order.
type State annot = Map ModifyNotes.Index [(ModifyNotes.Note, annot)]

insert_cons :: Ord k => k -> a -> Map k [a] -> Map k [a]
insert_cons k a = Map.alter (Just . maybe [a] (a:)) k

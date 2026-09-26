-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities that use "Cmd.ModifyNotes" to do higher-level transformations.
module Cmd.Repl.LNote where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import qualified Util.Maps as Maps
import qualified Cmd.Cmd as Cmd
import qualified Cmd.ModifyNotes as ModifyNotes
import qualified Cmd.Selection as Selection

import qualified Derive.PSignal as PSignal
import qualified Derive.ScoreT as ScoreT
import qualified Perform.Pitch as Pitch

import           Global
import           Types


-- * query

notes :: Cmd.CmdL [(ModifyNotes.Note, TrackId)]
notes = ModifyNotes.selectedNotes

noteControls :: Cmd.CmdL
    [(ModifyNotes.Note, (Maybe PSignal.Transposed, ScoreT.ControlValMap))]
noteControls = do
    blockId <- Cmd.getFocusedBlock
    events <- Cmd.perfEvents <$> Cmd.getPerformance blockId
    noteTrackIds <- notes
    pure $ ModifyNotes.findControls noteTrackIds events

-- * modify

-- | Merge the selected note tracks into one.
merge :: Cmd.CmdL ()
merge = ModifyNotes.selection $ ModifyNotes.note $ ModifyNotes.index #= 0

-- | Distribute the notes among the given number of tracks, round-robin.  Since
-- only each note only carries over the controls in its extent, if there are
-- notes that rely on control values carried forward, the values will be
-- different in the new tracks.
distributeN :: Int -> Cmd.CmdL ()
distributeN tracks = ModifyNotes.selection $ \_ notes -> pure $
        zipWith (modify tracks) [0..] (map fst notes)
    where
    modify tracks n = ModifyNotes.index #= n `mod` tracks

-- | Like 'distribute_n', but use only the selected tracks.
distribute :: Cmd.CmdL ()
distribute = do
    (blockId, _, trackIds, _) <- Selection.tracks
    tracks <- length <$> ModifyNotes.extractNoteTrees blockId trackIds
    distributeN tracks

-- | Try to compact non-overlapping notes to use the least number of tracks
-- possible.
compact :: Cmd.CmdL ()
compact =
    ModifyNotes.selection $ const $
        pure . snd . List.mapAccumL allocate [] . map fst
    where
    allocate state note = (next, ModifyNotes.index #= i $ note)
        where
        (i, next) = findIndex (ModifyNotes.noteStart note)
            (ModifyNotes.noteEnd note) state

-- | Find the lowest index that a note will fit.  Search the list of end times
-- for one at or before the given start, and return that index and update the
-- list with the new end.
findIndex :: ScoreTime -> ScoreTime -> [ScoreTime] -> (Int, [ScoreTime])
findIndex start end = go 0
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
splitOnPitch :: ModifyNotes.Index -> Pitch.NoteNumber -> Cmd.CmdL ()
splitOnPitch highIndex breakNn =
    ModifyNotes.selection $ ModifyNotes.annotateNns $ \notes ->
        pure $ split notes
    where
    split = snd . List.mapAccumL allocate ([], [])
    allocate (lowAlloc, highAlloc) (note, maybeNn)
        | maybe True (<=breakNn) maybeNn =
            let (i, next) = find lowAlloc
            in ((next, highAlloc), ModifyNotes.index #= i $ note)
        | otherwise =
            let (i, next) = find highAlloc
            in ((lowAlloc, next), ModifyNotes.index #= (highIndex + i) $ note)
        where
        find = findIndex (ModifyNotes.noteStart note)
            (ModifyNotes.noteEnd note)

-- | Sort by pitch and compact.
sortOnPitch :: Cmd.M m => Bool -> m ()
sortOnPitch highLeft = ModifyNotes.selection $ ModifyNotes.annotateNns $
    pure . extract . foldl' insert mempty
    where
    insert state note =
        insertOrdered (\n1 n2 -> cmp (snd n1) (snd n2)) note state
    cmp = if highLeft then (<) else (>)

-- | Find the last index with an overlapping note that isn't place_before,
-- and put the note on index+1.  If it it overlaps, make a space by bumping
-- tracks up by one.
insertOrdered :: ((ModifyNotes.Note, a) -> (ModifyNotes.Note, a) -> Bool)
    -> (ModifyNotes.Note, a) -> State a -> State a
insertOrdered placeBefore note state = case Map.lookup index state of
    Just (n : _) | overlap note n ->
        Map.insert index [note] $ bumpIndex index state
    _ -> insertCons index note state
    where
    index = maybe 0 ((+1) . fst) $ Lists.last $
        takeWhile (placeBefore note . snd) overlapping
    overlapping = [(i, n) | (i, n : _) <- Map.toAscList state, overlap note n]
    overlap n1 n2 = ModifyNotes.notesOverlap (fst n1) (fst n2)

bumpIndex :: Int -> Map Int a -> Map Int a
bumpIndex index m =
    pre <> Map.fromAscList (map (first (+1)) (Map.toAscList post))
    where (pre, post) = Maps.split2 index m

extract :: State a -> [ModifyNotes.Note]
extract state = concat $
    [ map ((ModifyNotes.index #= i) . fst) (reverse notes)
    | (i, notes) <- Map.toAscList state
    ]

-- | From track index to notes in reverse order.
type State annot = Map ModifyNotes.Index [(ModifyNotes.Note, annot)]

insertCons :: Ord k => k -> a -> Map k [a] -> Map k [a]
insertCons k a = Map.alter (Just . maybe [a] (a:)) k

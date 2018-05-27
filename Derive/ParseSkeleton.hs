-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton where
import qualified Data.Tree as Tree

import qualified Util.Seq as Seq
import qualified Util.Tree
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Ui as Ui
import qualified Derive.ParseTitle as ParseTitle


-- | A parser figures out a skeleton based on track titles and position.
--
-- Tracks starting with '>' are instrument tracks, the rest are control tracks.
-- A track titled \"tempo\" scopes over all tracks to its right.
-- Below that, tracks scope left to right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- TODO do something special with embedded rulers and dividers
default_parser :: [Ui.TrackInfo] -> Skeleton.Skeleton
default_parser = make_skeleton . parse_to_tree False

-- | The note-bottom parser puts note tracks at the bottom:
--
-- @[tempo c1 i1 c2 i2] -> [tempo1 (c1 i1) (c2 i2)]@
--
-- This is useful when you don't want to invoke slicing.
note_bottom_parser :: [Ui.TrackInfo] -> Skeleton.Skeleton
note_bottom_parser = make_skeleton . parse_to_tree True

make_skeleton :: Tree.Forest Ui.TrackInfo -> Skeleton.Skeleton
make_skeleton =
    Skeleton.make . Util.Tree.edges . map (fmap Ui.track_tracknum)

-- | [c0 tempo1 i1 c1 tempo2 c2 i2 c3] ->
-- [c0, tempo1 (i1 c1), tempo2 (c2 c2 c3)]
parse_to_tree :: Bool -> [Ui.TrackInfo] -> Tree.Forest Ui.TrackInfo
parse_to_tree reversed tracks = concatMap parse groups
    where
    groups = Seq.split_before (ParseTitle.is_tempo_track . Ui.track_title)
        tracks
    parse = if reversed then reverse_tempo_group else parse_tempo_group

parse_tempo_group :: [Ui.TrackInfo] -> Tree.Forest Ui.TrackInfo
parse_tempo_group tracks = case groups of
        [] -> []
        non_note : ngroups ->
            descend non_note (concatMap parse_note_group ngroups)
    where
    groups = Seq.split_before (ParseTitle.is_note_track . Ui.track_title)
        tracks

reverse_tempo_group :: [Ui.TrackInfo] -> Tree.Forest Ui.TrackInfo
reverse_tempo_group [] = []
reverse_tempo_group (track:tracks) =
    [Tree.Node track $ concatMap parse_note_group (shift groups)]
    where
    groups = Seq.split_before (ParseTitle.is_note_track . Ui.track_title)
        tracks
    shift (group : (note : rest) : gs) = (group ++ [note]) : shift (rest : gs)
    shift gs = gs

parse_note_group :: [Ui.TrackInfo] -> Tree.Forest Ui.TrackInfo
parse_note_group tracks = descend tracks []

descend :: [a] -> Tree.Forest a -> Tree.Forest a
descend [] bottom = bottom
descend (track:tracks) bottom = [Tree.Node track (descend tracks bottom)]

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton (
    Track(..)
    , default_parser, note_bottom_parser
) where
import qualified Data.Text as Text
import qualified Data.Tree as Tree

import qualified Util.Lists as Lists
import qualified Util.Trees as Trees
import qualified Derive.ParseTitle as ParseTitle
import qualified Ui.Skeleton as Skeleton

import           Global


-- TODO ParseTitle pulls in Derive.Parse when it only needs is_note_track and
-- is_tempo_track.  But everyone likely already needs Derive.Parse.

data Track = Track {
    _tracknum :: !Int
    , _title :: !Title
    } deriving (Show)
type Title = Text

-- | A parser figures out a skeleton based on track titles and position.
--
-- Tracks starting with '>' are instrument tracks, the rest are control tracks.
-- A track titled \"tempo\" scopes over all tracks to its right.
-- Below that, tracks scope left to right.
--
-- This should take arguments to apply to instrument and control tracks.
--
-- TODO do something special with embedded rulers and dividers
default_parser :: [Track] -> Skeleton.Skeleton
default_parser = make_skeleton . parse_to_tree False

-- | The note-bottom parser puts note tracks at the bottom:
--
-- @[tempo c1 i1 c2 i2] -> [tempo1 (c1 i1) (c2 i2)]@
--
-- This is useful when you don't want to invoke slicing.
note_bottom_parser :: [Track] -> Skeleton.Skeleton
note_bottom_parser = make_skeleton . parse_to_tree True

make_skeleton :: Tree.Forest Track -> Skeleton.Skeleton
make_skeleton = Skeleton.make . Trees.edges . map (fmap _tracknum)

-- | [c0 tempo1 i1 c1 tempo2 c2 i2 c3] ->
-- c0 (tempo1 (i1 (c1)) (tempo2 (c2 (i2 (c3)))))
--
-- [i1, c1, i2] -> (i1 c1) (i2)
-- [i1, c1 -->, i2] -> i1 (c1 (i2))
parse_to_tree :: Bool -> [Track] -> Tree.Forest Track
parse_to_tree reversed tracks = concatMap parse groups
    where
    groups = split_title ParseTitle.is_tempo_track tracks
    parse = if reversed then reverse_tempo_group else parse_tempo_group

parse_tempo_group :: [Track] -> Tree.Forest Track
parse_tempo_group = concatMap parse_control_group . split_title is_cgroup
    where
    is_cgroup t = ParseTitle.is_control_track t && "-->" `Text.isSuffixOf` t
    -- Use a --> "pragma" comment to make a control track associate to the
    -- right.

parse_control_group :: [Track] -> Tree.Forest Track
parse_control_group tracks = case split_title ParseTitle.is_note_track tracks of
    [] -> []
    non_note : ngroups -> descend non_note (concatMap parse_note_group ngroups)

split_title :: (Title -> Bool) -> [Track] -> [[Track]]
split_title f = Lists.splitBefore (f . _title)

reverse_tempo_group :: [Track] -> Tree.Forest Track
reverse_tempo_group [] = []
reverse_tempo_group (track:tracks) =
    [Tree.Node track $ concatMap parse_note_group (shift groups)]
    where
    groups = split_title ParseTitle.is_note_track tracks
    shift (group : (note : rest) : gs) = (group ++ [note]) : shift (rest : gs)
    shift gs = gs

parse_note_group :: [a] -> Tree.Forest a
parse_note_group tracks = descend tracks []

descend :: [a] -> Tree.Forest a -> Tree.Forest a
descend [] bottom = bottom
descend (track:tracks) bottom = [Tree.Node track (descend tracks bottom)]

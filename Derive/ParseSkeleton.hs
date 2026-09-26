-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.ParseSkeleton (
    Track(..)
    , defaultParser, noteBottomParser
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

data Track = Track
    { _tracknum :: !Int
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
defaultParser :: [Track] -> Skeleton.Skeleton
defaultParser = makeSkeleton . parseToTree False

-- | The note-bottom parser puts note tracks at the bottom:
--
-- @[tempo c1 i1 c2 i2] -> [tempo1 (c1 i1) (c2 i2)]@
--
-- This is useful when you don't want to invoke slicing.
noteBottomParser :: [Track] -> Skeleton.Skeleton
noteBottomParser = makeSkeleton . parseToTree True

makeSkeleton :: Tree.Forest Track -> Skeleton.Skeleton
makeSkeleton = Skeleton.make . Trees.edges . map (fmap _tracknum)

-- | [c0 tempo1 i1 c1 tempo2 c2 i2 c3] ->
-- c0 (tempo1 (i1 (c1)) (tempo2 (c2 (i2 (c3)))))
--
-- [i1, c1, i2] -> (i1 c1) (i2)
-- [i1, c1 -->, i2] -> i1 (c1 (i2))
parseToTree :: Bool -> [Track] -> Tree.Forest Track
parseToTree reversed tracks = concatMap parse groups
    where
    groups = splitTitle ParseTitle.isTempoTrack tracks
    parse = if reversed then reverseTempoGroup else parseTempoGroup

parseTempoGroup :: [Track] -> Tree.Forest Track
parseTempoGroup = concatMap parseControlGroup . splitTitle isCgroup
    where
    isCgroup t = ParseTitle.isControlTrack t && "-->" `Text.isSuffixOf` t
    -- Use a --> "pragma" comment to make a control track associate to the
    -- right.

parseControlGroup :: [Track] -> Tree.Forest Track
parseControlGroup tracks = case splitTitle ParseTitle.isNoteTrack tracks of
    [] -> []
    nonNote : ngroups -> descend nonNote (concatMap parseNoteGroup ngroups)

splitTitle :: (Title -> Bool) -> [Track] -> [[Track]]
splitTitle f = Lists.splitBefore (f . _title)

reverseTempoGroup :: [Track] -> Tree.Forest Track
reverseTempoGroup [] = []
reverseTempoGroup (track:tracks) =
    [Tree.Node track $ concatMap parseNoteGroup (shift groups)]
    where
    groups = splitTitle ParseTitle.isNoteTrack tracks
    shift (group : (note : rest) : gs) = (group ++ [note]) : shift (rest : gs)
    shift gs = gs

parseNoteGroup :: [a] -> Tree.Forest a
parseNoteGroup tracks = descend tracks []

descend :: [a] -> Tree.Forest a -> Tree.Forest a
descend [] bottom = bottom
descend (track:tracks) bottom = [Tree.Node track (descend tracks bottom)]

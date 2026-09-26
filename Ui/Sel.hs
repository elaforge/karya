-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The selection type.
module Ui.Sel (
    Ui.Sel.Num
    , Selection(..)
    , Orientation(..)
    , point, isPoint
    , modifyTracks
    , expandTracks
    , trackRange
    , tracknums
    , min, max
    , range
    , eventOrientation
    , duration
    , merge
    , union
    , move
) where
import qualified Prelude
import           Prelude hiding (min, max)
import qualified Data.Tuple as Tuple

import qualified Util.Num as Num
import qualified Ui.Types as Types

import           Global
import           Types


-- | Index into the the selection list.
type Num = Int

data Selection = Selection
    -- | The position the selection was established at.  Since a selection can
    -- logically go off the edge of a block, this is not necessarily a valid
    -- TrackNum!
    { startTrack :: !TrackNum
    , startPos :: !TrackTime

    -- | The position the selection is now at.  The tracks are an inclusive
    -- range, the pos are half-open.  This is because these pairs are meant to
    -- be symmetrical, but the c++ layer only supports half-open pos ranges.
    -- I don't think there's much I can do about this.
    --
    -- Unlike 'start_track', this should be a valid TrackNum, because cmds want
    -- to use it as the focused track.
    , curTrack :: !TrackNum
    , curPos :: !TrackTime
    , orientation :: !Orientation
    } deriving (Eq, Ord, Show, Read)

instance Pretty Selection where
    pretty (Selection strack spos ctrack cpos orientation) =
        "Selection" <> o <> pretty (strack, spos) <> "--"
            <> pretty (ctrack, cpos)
        where
        o = case orientation of
            None -> "_"
            Positive -> "+"
            Negative -> "-"

-- | None is used for display selections, which don't need arrows on them.
data Orientation = None | Positive | Negative
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | A point is a selection with no duration.
point :: TrackNum -> TrackTime -> Orientation -> Selection
point tracknum pos orientation = Selection
    { startTrack = tracknum, startPos = pos
    , curTrack = tracknum, curPos = pos
    , orientation = orientation
    }

isPoint :: Selection -> Bool
isPoint sel = startPos sel == curPos sel

modifyTracks :: (TrackNum -> TrackNum) -> Selection -> Selection
modifyTracks f sel = sel
    { startTrack = f (startTrack sel)
    , curTrack = f (curTrack sel)
    }

expandTracks :: TrackNum -> Selection -> Selection
expandTracks n sel
    | cur > start = sel { curTrack = cur + n }
    | otherwise = sel { startTrack = start + n }
    where
    start = startTrack sel
    cur = curTrack sel

-- | Start and end tracks, from small to large.
trackRange :: Selection -> (TrackNum, TrackNum)
trackRange sel = (Prelude.min track0 track1, Prelude.max track0 track1)
    where (track0, track1) = (startTrack sel, curTrack sel)

-- | TrackNums covered by the selection.  Since Selections may have out of
-- range tracks, I need the number of tracks to generate a list of valid
-- TrackNums.
tracknums :: TrackNum -> Selection -> [TrackNum]
tracknums tracks sel
    | tracks <= 0 = []
    | otherwise = [Num.clamp 0 (tracks-1) start .. Num.clamp 0 (tracks-1) end]
    where (start, end) = trackRange sel

min :: Selection -> TrackTime
min sel = Prelude.min (startPos sel) (curPos sel)

max :: Selection -> TrackTime
max sel = Prelude.max (startPos sel) (curPos sel)

-- | Start and end points, from small to large.
range :: Selection -> (TrackTime, TrackTime)
range sel = (min sel, max sel)

eventOrientation :: Selection -> Types.Orientation
eventOrientation sel = case orientation sel of
    Negative -> Types.Negative
    _ -> Types.Positive

duration :: Selection -> TrackTime
duration sel = abs (startPos sel - curPos sel)

-- | Extend the current track and pos, but keep the start track and pos the
-- same.
merge :: Selection -> Selection -> Selection
merge (Selection strack spos _ _ _) (Selection _ _ ctrack cpos orient) =
    Selection strack spos ctrack cpos orient

-- | Make a selection that covers both the given selections.  It tries to set
-- start and cur values based on the direction of the merge, assuming you are
-- starting with the first selection and adding the second.
union :: Selection -> Selection -> Selection
union sel1 sel2 = Selection strack spos ctrack cpos (orientation sel2)
    where
    (strack, ctrack) =
        if curTrack sel2 >= curTrack sel1 then se else Tuple.swap se
        where
        se = (Prelude.min s1 s2, Prelude.max e1 e2)
        (s1, e1) = trackRange sel1
        (s2, e2) = trackRange sel2
    (spos, cpos) = if curPos sel2 >= curPos sel1 then se else Tuple.swap se
        where
        se = (Prelude.min s1 s2, Prelude.max e1 e2)
        (s1, e1) = range sel1
        (s2, e2) = range sel2

move :: TrackTime -> Selection -> Selection
move t sel = sel { startPos = startPos sel + t, curPos = curPos sel + t }

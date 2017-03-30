-- Copyright 2015 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | The selection type.
module Ui.Sel where
import qualified Prelude
import Prelude hiding (min, max)
import qualified Data.Tuple as Tuple

import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Ui.Event as Event
import Global
import Types


-- | Index into the the selection list.
type Num = Int

data Selection = Selection {
    -- | The position the selection was established at.  Since a selection can
    -- logically go off the edge of a block, this is not necessarily a valid
    -- TrackNum!
    start_track :: !TrackNum
    , start_pos :: !TrackTime

    -- | The position the selection is now at.  The tracks are an inclusive
    -- range, the pos are half-open.  This is because these pairs are meant to
    -- be symmetrical, but the c++ layer only supports half-open pos ranges.
    -- I don't think there's much I can do about this.
    --
    -- Unlike 'start_track', this should be a valid TrackNum, because cmds want
    -- to use it as the focused track.
    , cur_track :: !TrackNum
    , cur_pos :: !TrackTime
    , orientation :: !Orientation
    } deriving (Eq, Ord, Show, Read)

instance Pretty.Pretty Selection where
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

selection :: TrackNum -> TrackTime -> TrackNum -> TrackTime -> Orientation
    -> Selection
selection start_track start_pos cur_track cur_pos orientation = Selection
    { start_track = start_track
    , start_pos = start_pos
    , cur_track = cur_track
    , cur_pos = cur_pos
    , orientation = orientation
    }

-- | A point is a selection with no duration.
point :: TrackNum -> TrackTime -> Orientation -> Selection
point tracknum pos orientation = selection tracknum pos tracknum pos orientation

is_point :: Selection -> Bool
is_point sel = start_pos sel == cur_pos sel

modify_tracks :: (TrackNum -> TrackNum) -> Selection -> Selection
modify_tracks f sel = sel
    { start_track = f (start_track sel)
    , cur_track = f (cur_track sel)
    }

expand_tracks :: TrackNum -> Selection -> Selection
expand_tracks n sel
    | cur > start = sel { cur_track = cur + n }
    | otherwise = sel { start_track = start + n }
    where
    start = start_track sel
    cur = cur_track sel

-- | Start and end tracks, from small to large.
track_range :: Selection -> (TrackNum, TrackNum)
track_range sel = (Prelude.min track0 track1, Prelude.max track0 track1)
    where (track0, track1) = (start_track sel, cur_track sel)

-- | TrackNums covered by the selection.  Since Selections may have out of
-- range tracks, I need the number of tracks to generate a list of valid
-- TrackNums.
tracknums :: TrackNum -> Selection -> [TrackNum]
tracknums tracks sel
    | tracks <= 0 = []
    | otherwise = [Num.clamp 0 (tracks-1) start .. Num.clamp 0 (tracks-1) end]
    where (start, end) = track_range sel

min :: Selection -> TrackTime
min sel = Prelude.min (start_pos sel) (cur_pos sel)

max :: Selection -> TrackTime
max sel = Prelude.max (start_pos sel) (cur_pos sel)

-- | Start and end points, from small to large.
range :: Selection -> (TrackTime, TrackTime)
range sel = (min sel, max sel)

event_orientation :: Selection -> Event.Orientation
event_orientation sel = case orientation sel of
    Negative -> Event.Negative
    _ -> Event.Positive

duration :: Selection -> TrackTime
duration sel = abs (start_pos sel - cur_pos sel)

set_duration :: TrackTime -> Selection -> Selection
set_duration dur sel
    | cur > start = sel { cur_pos = start + Prelude.max 0 dur }
    | otherwise = sel { start_pos = cur + Prelude.max 0 dur }
    where
    start = start_pos sel
    cur = cur_pos sel

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
        if cur_track sel2 >= cur_track sel1 then se else Tuple.swap se
        where
        se = (Prelude.min s1 s2, Prelude.max e1 e2)
        (s1, e1) = track_range sel1
        (s2, e2) = track_range sel2
    (spos, cpos) = if cur_pos sel2 >= cur_pos sel1 then se else Tuple.swap se
        where
        se = (Prelude.min s1 s2, Prelude.max e1 e2)
        (s1, e1) = range sel1
        (s2, e2) = range sel2

move :: TrackTime -> Selection -> Selection
move t sel = sel { start_pos = start_pos sel + t, cur_pos = cur_pos sel + t }

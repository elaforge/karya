-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- | This is a stubbed out version of BlockC.  The real BlockC will use this
-- when TESTING is defined, since they don't work from ghci if they have
-- a C dependency.
--
-- It's just a big copy-paste from BlockC, but should be ok since that changes
-- very rarely.
module Ui.BlockCStub where
import qualified Util.Rect as Rect
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import Ui.Fltk (Fltk, fltk)
import qualified Ui.Events as Events
import qualified Ui.Sel as Sel
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import Global
import Types


-- * view creation

create_view :: ViewId -> Text -> Rect.Rect -> Block.Config -> Fltk ()
create_view view_id window_title rect block_config = fltk $ return ()

destroy_view :: ViewId -> Fltk ()
destroy_view view_id = fltk $ return ()

get_view_status :: ViewId -> Fltk (Rect.Rect, Types.Zoom, Int, Int)
get_view_status view_id = fltk $ errorIO "unimplemented"

set_size :: ViewId -> Rect.Rect -> Fltk ()
set_size view_id rect = fltk $ return ()

set_zoom :: ViewId -> Types.Zoom -> Fltk ()
set_zoom view_id zoom = fltk $ return ()

set_track_scroll :: ViewId -> Types.Width -> Fltk ()
set_track_scroll view_id offset = fltk $ return ()

set_selection :: ViewId -> Sel.Num -> [TrackNum] -> [Selection] -> Fltk ()
set_selection view_id selnum tracknums sels = fltk $ return ()

bring_to_front :: ViewId -> Fltk ()
bring_to_front view_id = fltk $ return ()

-- * Block operations

set_config :: ViewId -> Block.Config -> Fltk ()
set_config view_id config = fltk $ return ()

set_skeleton :: ViewId -> Skeleton.Skeleton
    -> [(Color.Color, [(TrackNum, TrackNum)])] -> [Block.Status] -> Fltk ()
set_skeleton view_id skel integrate_edges statuses = fltk $ return ()

set_title :: ViewId -> Text -> Fltk ()
set_title view_id title = fltk $ return ()

set_status :: ViewId -> Text -> Color.Color -> Fltk ()
set_status view_id status color = fltk $ return ()

set_display_track :: ViewId -> TrackNum -> Block.DisplayTrack -> Fltk ()
set_display_track view_id tracknum dtrack = fltk $ return ()

floating_open :: ViewId -> TrackNum -> ScoreTime -> Text -> (Int, Int)
    -> Fltk ()
floating_open view_id tracknum pos text (sel_start, sel_end) = fltk $ return ()

floating_insert :: [ViewId] -> Text -> Fltk ()
floating_insert view_ids text = fltk $ return ()

tracks :: ViewId -> Fltk TrackNum
tracks view_id = fltk $ return 0

insert_track :: ViewId -> TrackNum -> Block.Tracklike -> [Events.Events]
    -> Track.SetStyle -> Types.Width -> Fltk ()
insert_track view_id tracknum tracklike merged set_style width =
    fltk $ return ()

remove_track :: ViewId -> TrackNum -> Fltk ()
remove_track view_id tracknum = fltk $ return ()

update_track :: Bool -- ^ True if the ruler has changed and should be copied
    -- over.  It's a bit of a hack to be a separate flag, but rulers are
    -- updated rarely and copied over entirely for efficiency.
    -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> ScoreTime -> ScoreTime -> Fltk ()
update_track update_ruler view_id tracknum tracklike merged set_style start
        end = fltk $ return ()

-- | Like 'update_track' except update everywhere.
update_entire_track :: Bool -> ViewId -> TrackNum -> Block.Tracklike
    -> [Events.Events] -> Track.SetStyle -> Fltk ()
update_entire_track update_ruler view_id tracknum tracklike merged set_style =
    -- -1 is special cased in c++.
    update_track update_ruler view_id tracknum tracklike merged set_style
        (-1) (-1)

-- | Unlike other Fltk functions, this doesn't throw if the ViewId is not
-- found.  That's because it's called asynchronously when derivation is
-- complete.
set_track_signal :: ViewId -> TrackNum -> Track.TrackSignal -> Fltk ()
set_track_signal view_id tracknum tsig = fltk $ return ()

set_track_title :: ViewId -> TrackNum -> Text -> Fltk ()
set_track_title view_id tracknum title = fltk $ return ()

set_track_title_focus :: ViewId -> TrackNum -> Fltk ()
set_track_title_focus view_id tracknum = fltk $ return ()

set_block_title_focus :: ViewId -> Fltk ()
set_block_title_focus view_id = fltk $ return ()


-- ** debugging

show_children :: ViewId -> IO String
show_children view_id = return ""

dump :: IO [(ViewId, String)]
dump = return []

-- ** selection

-- | This is the low level version of 'Sel.Selection'.  It only applies to
-- a single track, and has an explicit color.
data Selection = Selection {
    sel_color :: !Color.Color
    , sel_start :: !TrackTime
    , sel_cur :: !TrackTime
    , sel_draw_arrow :: !Bool
    }
    deriving (Eq, Ord, Show)

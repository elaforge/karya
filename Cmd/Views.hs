-- Copyright 2016 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Lower level 'Block.View' utilities.  Specifically, this has support for
-- "Cmd.ViewConfig" and "Cmd.Create", so they don't have to import each other.
module Cmd.Views where
import qualified Util.Num as Num
import qualified Util.Rect as Rect
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Ui as Ui
import qualified Ui.Zoom as Zoom

import           Global
import           Types


maximize_and_zoom :: Cmd.M m => ViewId -> m ()
maximize_and_zoom view_id = do
    resize_to_fit True view_id
    zoom_to_ruler view_id

-- * zoom

-- | Set zoom on the given view to make the entire block visible.
zoom_to_ruler :: Cmd.M m => ViewId -> m ()
zoom_to_ruler view_id = do
    block_end <- block_end view_id
    factor <- zoom_factor view_id block_end
    set_zoom view_id $ Zoom.Zoom { offset = 0, factor = factor }

-- | Figure out the zoom factor to display the given amount of TrackTime.
zoom_factor :: Ui.M m => ViewId -> TrackTime -> m Double
zoom_factor view_id dur
    | dur == 0 = return 1
    | otherwise = do
        view <- Ui.get_view view_id
        let pixels = Block.view_visible_time view
        return $ fromIntegral pixels / ScoreTime.to_double dur

set_zoom :: Cmd.M m => ViewId -> Zoom.Zoom -> m ()
set_zoom view_id = modify_zoom view_id . const

-- | Set time scroll, clipping so it doesn't scroll past 'Block.block_end'.
set_time_offset :: Cmd.M m => ViewId -> TrackTime -> m ()
set_time_offset view_id offset = do
    view <- Ui.get_view view_id
    let visible = Block.visible_time view
    end <- Ui.block_end $ Block.view_block view
    modify_zoom view_id $ \zoom -> zoom
        { Zoom.offset = Num.clamp 0 (end - visible) offset }

modify_zoom :: Cmd.M m => ViewId -> (Zoom.Zoom -> Zoom.Zoom) -> m ()
modify_zoom view_id modify = do
    Ui.modify_zoom view_id modify
    Internal.sync_zoom_status view_id

-- * size

-- | Resize a window to fit its tracks.
resize_to_fit :: Cmd.M m => Bool -- ^ maximize the window vertically
    -> ViewId -> m ()
resize_to_fit maximize view_id = do
    view <- Ui.get_view view_id
    screen <- Cmd.get_screen (Rect.upper_left (Block.view_rect view))
    rect <- contents_rect view
    Ui.set_view_rect view_id $ Rect.intersection screen $ scootch screen $
        if maximize then max_height screen rect else rect
    where
    -- Move the rect over so it fits on the screen.
    scootch screen r = Rect.place
        (Num.clamp (Rect.x screen) (Rect.r screen - Rect.w r) (Rect.x r))
        (Num.clamp (Rect.y screen) (Rect.b screen - Rect.h r) (Rect.y r))
        r
    max_height screen r = Rect.xywh
        (Rect.x r) (Rect.y screen)
        (Rect.w r) (Rect.h screen - Config.window_decoration_h)

-- | Get the View's Rect, resized to fit its contents at its current zoom.  Its
-- position is unchanged.
contents_rect :: Ui.M m => Block.View -> m Rect.Rect
contents_rect view = do
    block_end <- Ui.block_end (Block.view_block view)
    block <- Ui.get_block (Block.view_block view)
    let (x, y) = Rect.upper_left (Block.track_rect view)
        w = Num.sum $ map Block.display_track_width (Block.block_tracks block)
        h = Zoom.to_pixels (Block.view_zoom view) block_end
    return $ Block.set_track_rect view $ Rect.xywh x y (max w 40) (max h 40)

set_track_rect :: Ui.M m => ViewId -> Rect.Rect -> m ()
set_track_rect view_id rect = do
    view <- Ui.get_view view_id
    Ui.set_view_rect view_id $ Block.set_track_rect view rect

-- * util

block_end :: Ui.M m => ViewId -> m TrackTime
block_end = Ui.block_end . Block.view_block <=< Ui.get_view

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
import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Ui as Ui
import qualified Ui.Zoom as Zoom

import           Global
import           Types


maximizeAndZoom :: Cmd.M m => ViewId -> m ()
maximizeAndZoom viewId = do
    resizeToFit True viewId
    zoomToRuler viewId

-- * zoom

-- | Set zoom on the given view to make the entire block visible.
zoomToRuler :: Cmd.M m => ViewId -> m ()
zoomToRuler viewId = do
    blockEnd <- blockEnd viewId
    factor <- zoomFactor viewId blockEnd
    setZoom viewId $ Zoom.Zoom { offset = 0, factor = factor }

-- | Figure out the zoom factor to display the given amount of TrackTime.
zoomFactor :: Ui.M m => ViewId -> TrackTime -> m Double
zoomFactor viewId dur
    | dur == 0 = pure 1
    | otherwise = do
        view <- Ui.getView viewId
        let pixels = Block.viewVisibleTime view
        pure $ fromIntegral pixels / ScoreTime.toDouble dur

setZoom :: Cmd.M m => ViewId -> Zoom.Zoom -> m ()
setZoom viewId = modifyZoom viewId . const

-- | Set time scroll, clipping so it doesn't scroll past 'Block.block_end'.
setTimeOffset :: Cmd.M m => ViewId -> TrackTime -> m ()
setTimeOffset viewId offset = do
    view <- Ui.getView viewId
    let visible = Block.visibleTime view
    end <- Ui.blockEnd $ Block.viewBlock view
    modifyZoom viewId $ \zoom -> zoom
        { Zoom.offset = Num.clamp 0 (end - visible) offset }

modifyZoom :: Cmd.M m => ViewId -> (Zoom.Zoom -> Zoom.Zoom) -> m ()
modifyZoom viewId modify = do
    Ui.modifyZoom viewId modify
    -- it's a no-op and Internal adds many deps
    -- Internal.sync_zoom_status view_id

-- * size

-- | Resize a window to fit its tracks.
resizeToFit :: Cmd.M m => Bool -- ^ maximize the window vertically
    -> ViewId -> m ()
resizeToFit maximize viewId = do
    view <- Ui.getView viewId
    screen <- Cmd.getScreen $ Just (Rect.upperLeft (Block.viewRect view))
    rect <- contentsRect view
    Ui.setViewRect viewId $ Rect.intersection screen $ scootch screen $
        if maximize then maxHeight screen rect else rect
    where
    -- Move the rect over so it fits on the screen.
    scootch screen r = Rect.place
        (Num.clamp (Rect.x screen) (Rect.r screen - Rect.w r) (Rect.x r))
        (Num.clamp (Rect.y screen) (Rect.b screen - Rect.h r) (Rect.y r))
        r
    maxHeight screen r = Rect.xywh
        (Rect.x r) (Rect.y screen)
        (Rect.w r) (Rect.h screen - Config.windowDecorationH)

-- | Get the View's Rect, resized to fit its contents at its current zoom.  Its
-- position is unchanged.
contentsRect :: Ui.M m => Block.View -> m Rect.Rect
contentsRect view = do
    blockEnd <- Ui.blockEnd (Block.viewBlock view)
    block <- Ui.getBlock (Block.viewBlock view)
    let (x, y) = Rect.upperLeft (Block.trackRect view)
        w = Num.sum $ map Block.displayTrackWidth (Block.blockTracks block)
        h = Zoom.toPixels (Block.viewZoom view) blockEnd
    pure $ Block.setTrackRect view $ Rect.xywh x y (max w 40) (max h 40)

setTrackRect :: Ui.M m => ViewId -> Rect.Rect -> m ()
setTrackRect viewId rect = do
    view <- Ui.getView viewId
    Ui.setViewRect viewId $ Block.setTrackRect view rect

-- * util

blockEnd :: Ui.M m => ViewId -> m TrackTime
blockEnd = Ui.blockEnd . Block.viewBlock <=< Ui.getView

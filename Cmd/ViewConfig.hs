-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds related to view level state.
module Cmd.ViewConfig where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

import qualified Util.Control as Control
import qualified Util.Lens as Lens
import qualified Util.Lists as Lists
import qualified Util.Num as Num
import qualified Util.Rect as Rect

import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep
import qualified Cmd.Views as Views

import qualified Ui.Block as Block
import qualified Ui.Meter.Meter as Meter
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import           Global
import           Types


-- * zoom

cmdZoomAroundInsert :: Cmd.M m => (Double -> Double) -> m ()
cmdZoomAroundInsert f = do
    (viewId, (_, _, pos)) <- Selection.getAnyInsert
    cmdZoomAround viewId pos f

cmdZoomAround :: Cmd.M m => ViewId -> ScoreTime -> (Double -> Double) -> m ()
cmdZoomAround viewId pos f = do
    -- Zoom by the given factor, but try to keep pos in the same place on the
    -- screen.
    zoom <- Ui.getZoom viewId
    Views.setZoom viewId (zoomAround zoom pos f)

zoomAround :: Zoom.Zoom -> ScoreTime -> (Double -> Double) -> Zoom.Zoom
zoomAround (Zoom.Zoom offset factor) pos f = Zoom.Zoom
    { offset = zoomPos offset pos (ScoreTime.fromDouble factor)
        (ScoreTime.fromDouble newf)
    , factor = newf
    }
    where newf = f factor

zoomPos :: ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime
zoomPos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

modifyFactor :: Cmd.M m => ViewId -> (Double -> Double) -> m ()
modifyFactor viewId f = do
    zoom <- Ui.getZoom viewId
    Views.setZoom viewId $ zoom { Zoom.factor = f (Zoom.factor zoom) }

-- | Zoom to the ruler duration if the selection is a point, or zoom to the
-- selection if it's not.
zoomToRulerOrSelection :: Cmd.M m => m ()
zoomToRulerOrSelection = do
    (viewId, sel) <- Selection.getViewSel
    if Sel.isPoint sel
        then Views.zoomToRuler viewId
        else uncurry (zoomTo viewId) (Sel.range sel)

zoomTo :: Cmd.M m => ViewId -> TrackTime -> TrackTime -> m ()
zoomTo viewId start end =
    Views.setZoom viewId . Zoom.Zoom start
        =<< Views.zoomFactor viewId (end - start)

-- | Go through zoom factors for timesteps at the current point, and pick the
-- next larger or smaller one.
zoomByRank :: Cmd.M m => TimeStep.Direction -> m ()
zoomByRank direction = do
    (viewId, (blockId, tracknum, pos)) <- Selection.getAnyInsert
    factor <- Zoom.factor <$> Ui.getZoom viewId
    let durAt = TimeStep.durationAt blockId tracknum pos . TimeStep.rank
    mbFactor <- Control.loop1 ranks $ \loop -> \case
        rank : ranks -> durAt rank >>= \case
            Just dur
                | cmp f factor -> pure $ Just f
                | otherwise -> loop ranks
                where
                cmp = case direction of
                    TimeStep.Advance -> (>)
                    TimeStep.Rewind -> (<)
                f = timeToZoom dur
            Nothing -> pure Nothing
        [] -> pure Nothing
    whenJust mbFactor $ cmdZoomAround viewId pos . const
    where
    ranks = case direction of
        TimeStep.Advance -> rs
        TimeStep.Rewind -> reverse rs
        where rs = [Meter.W .. maxBound]

-- | Set zoom to where text at the given timestep should be visible.
-- This means the timestep amount of score should get Config.event_text_height
-- pixels.
zoomToRank :: Cmd.M m => Meter.Rank -> m ()
zoomToRank rank = do
    (viewId, (blockId, tracknum, pos)) <- Selection.getAnyInsert
    whenJustM (TimeStep.durationAt blockId tracknum pos tstep) $ \dur ->
        cmdZoomAround viewId pos (const (timeToZoom dur))
    where tstep = TimeStep.rank rank

timeToZoom :: TrackTime -> Double
timeToZoom step = text / ScoreTime.toDouble step
    where text = fromIntegral Config.eventTextHeight

-- * scroll

-- | Scroll by the number of pages, where a page is a fraction of the score
-- visible at the current zoom.
scrollPages :: Cmd.M m => TrackTime -> m ()
scrollPages pages = do
    viewId <- Cmd.getFocusedView
    view <- Ui.getView viewId
    let visible = Block.visibleTime view
        offset = Zoom.offset $ Block.viewZoom view
    Views.setTimeOffset viewId (offset + pages * visible)

scrollToEnd :: Cmd.M m => m ()
scrollToEnd = do
    viewId <- Cmd.getFocusedView
    Views.setTimeOffset viewId =<< Views.blockEnd viewId

scrollToHome :: Cmd.M m => m ()
scrollToHome = flip Views.setTimeOffset 0 =<< Cmd.getFocusedView

-- * resize

resizeAll :: Cmd.M m => m ()
resizeAll = mapM_ (Views.resizeToFit False) =<< Ui.allViewIds

setSuggestedTrackWidths :: Cmd.M m => ViewId -> m ()
setSuggestedTrackWidths viewId = do
    blockId <- Ui.blockIdOf viewId
    tracks <- Ui.blockTracknums blockId
    let changes = do
            (track, tracknum) <- tracks
            let suggested = Block.trackSuggestedWidth track
            guard (suggested /= Block.trackWidth track)
            pure (tracknum, suggested)
    unless (null changes) $ do
        mapM_ (uncurry (Ui.setTrackWidth blockId)) changes
        Views.resizeToFit False viewId

-- * window management

-- If a window significantly overlaps its left neighbor, and is a certain
-- distance below it, then shorten the neighbor and line up to the neigbor's
-- left edge.
--
-- Or, try to tile, but if a window winds up going off the screen, then shorten
-- everything in that column until they fit.  So I can do 'fit_rects', and then
-- just shrink horizontally and vertically.
--
-- In general I don't want to expand windows because I want to leave space for
-- new ones.  Or maybe I could expand vertically, but leave horizontal space
-- open.
--
-- Use cases: put a window halfway down another one and expect them to tile
-- vertically.

-- -- | Infer a tiling layout based on current window position, and move and
-- -- resize them to fit.
-- auto_tile :: Cmd.M m => m ()
-- auto_tile = do

-- | Fit rectangles into a tiling pattern.  The algorithm is to sort them by
-- X and Y, and place the first rectangle at (0, 0).  Then try to fit each
-- rectangle to the below or the right of each already placed rectangle,
-- filtering out the positions that would cause an overlap, and pick the spot
-- closest to the rectangle's original position.  The whole process is started
-- again with any rectangles that wind up totally outside the screen.
fitRects :: Rect.Rect -> [(ViewId, Rect.Rect)] -> [(ViewId, Rect.Rect)]
fitRects screen =
    redoOutside . foldl' fit []
        . Lists.sortOn (\(_, r) -> (Rect.x r, Rect.y r))
    where
    fit windows (viewId, rect) = case Lists.head (sort rect corners) of
        Just (x, y) -> (viewId, Rect.place x y rect) : windows
        -- Shouldn't happen since you can always place to the right or
        -- below the rightmost or bottom rectangle.
        -- Nothing -> error $ "no corners for " <> show rect
        Nothing -> (viewId, rect) : windows
        where
        rects = map snd windows
        corners =
            filter (not . wouldOverlap rects rect) (cornersOf screen rects)

    sort rect = Lists.sortOn $ Rect.pointDistance (Rect.upperLeft rect)
    -- If there are rects outside the screen, fit them again into an empty
    -- screen.  I should run out eventually.
    redoOutside rects = rects
        -- - | null outside = inside
        -- - | otherwise = fit_rects screen outside ++ inside
        -- where
        -- (inside, outside) = List.partition
        --     (Rect.contains_point screen . Rect.upper_left . snd) rects

cornersOf :: Rect.Rect -> [Rect.Rect] -> [(Int, Int)]
cornersOf screen [] = [Rect.upperLeft screen]
cornersOf _ rects =
    filter (\p -> not $ pointAbove p || pointLeft p) notTouching
    where
    notTouching = filter (not . touches) $
        map Rect.lowerLeft rects ++ map Rect.upperRight rects
    pointAbove (x, y) = any (\(x1, y1) -> x1 == x && y1 < y) notTouching
    pointLeft (x, y) = any (\(x1, y1) -> x1 < x && y1 == y) notTouching
    touches p = any (\r -> Rect.containsPoint r p) rects

-- If I move the rect to the point, will it overlap with anything in the
-- list?
wouldOverlap :: [Rect.Rect] -> Rect.Rect -> (Int, Int) -> Bool
wouldOverlap rects rect (x, y) =
    any (Rect.overlaps (Rect.place x y rect)) rects

-- | Arrange views horizontally on each screen.  They'll overlap if there isn't
-- room for all of them.
horizontalTile :: Cmd.M m => m ()
horizontalTile = mapM_ (uncurry tileScreen) =<< windowsByScreen
    where
    tileScreen screen viewRects =
        zipWithM_ Ui.setViewRect viewIds $
            horizontalTileRects screen rects
        where (viewIds, rects) = unzip (Lists.sortOn (Rect.x . snd) viewRects)

windowsByScreen :: Cmd.M m => m [(Rect.Rect, [(ViewId, Rect.Rect)])]
windowsByScreen = do
    viewRects <- Ui.gets $
        map (second Block.viewRect) . Map.toList . Ui.stateViews
    screens <- Cmd.gets Cmd.stateScreens
    let (screenViews, orphaned) = groupWith
            (\s -> Rect.overlaps s . snd) screens viewRects
    mapM_ (Ui.destroyView . fst) orphaned
    pure screenViews

horizontalTileRects :: Rect.Rect -> [Rect.Rect] -> [Rect.Rect]
horizontalTileRects screen rects = zipWith place rects xs
    where
    place rect x = Rect.place x (Rect.y screen) rect
    xs = scanl (+) (Rect.x screen) (map (subtract overlap . Rect.w) rects)
    overlap = case rects of
        _ : _ : _ -> max 0 $
            (Num.sum (map Rect.w rects) - Rect.w screen)
                `div` (length rects - 1)
        -- 0 or 1 rects are not going to have any overlap.
        _ -> 0

groupWith :: (key -> val -> Bool) -> [key] -> [val] -> ([(key, [val])], [val])
groupWith cmp keys vals = Tuple.swap $ List.mapAccumL go vals keys
    where
    go vals key = (out, (key, inside))
        where (inside, out) = List.partition (cmp key) vals

-- ** focus

cycleFocus :: Cmd.M m => Bool -> m ()
cycleFocus forward = do
    focused <- Cmd.getFocusedView
    viewIds <- (if forward then id else reverse) <$> Ui.allViewIds
    case drop 1 $ dropWhile (/=focused) viewIds of
        next : _ -> Cmd.focus next
        [] -> whenJust (Lists.head viewIds) Cmd.focus

-- | Right and Left would clash with Either.
data Direction = North | South | East | West deriving (Show)

moveFocus :: Cmd.M m => Direction -> m ()
moveFocus dir = do
    rects <- Ui.gets $
        map (second Block.viewRect) . Map.toList . Ui.stateViews
    focused <- Block.viewRect <$> (Ui.getView =<< Cmd.getFocusedView)
    let getRects cmp f = Lists.sortOn snd $ filter ((`cmp` f focused) . snd) $
            map (second f) rects
    let next = case dir of
            East -> Lists.head $ getRects (>) Rect.x
            West -> Lists.last $ getRects (<) Rect.x
            South -> Lists.head $ getRects (>) Rect.y
            North -> Lists.last $ getRects (<) Rect.y
    whenJust next $ Cmd.focus . fst

-- * create views

-- | For the current window, open enough views at the current zoom to see the
-- score from the current time until the end of the block.
viewsCovering :: Cmd.M m => ViewId -> m [ViewId]
viewsCovering viewId = do
    view <- Ui.getView viewId
    blockDur <- Ui.blockEnd $ Block.viewBlock view
    forM (viewsCoveringStarts blockDur view) $ \start -> do
        viewId <- Create.view (Block.viewBlock view)
        Views.modifyZoom viewId $ const $ Zoom.Zoom
            { factor = Zoom.factor (Block.viewZoom view)
            , offset = start
            }
        pure viewId

viewsCoveringStarts :: TrackTime -> Block.View -> [TrackTime]
viewsCoveringStarts blockDur view =
    -- drop 1 to exclude the given view.
    drop 1 $ take needed $ Lists.range_ offset (blockDur / fromIntegral needed)
    where
    offset = Zoom.offset (Block.viewZoom view)
    visible = Block.visibleTime view - offset
    needed = ceiling (blockDur / visible)

-- * saved views

-- | Save the current views under the given name.
saveViews :: Cmd.M m => Text -> m ()
saveViews name = do
    views <- Ui.gets Ui.stateViews
    focused <- Cmd.lookupFocusedView
    Ui.modifyConfig $ UiConfig.savedViews %= Map.insert name (views, focused)

-- | Replace the current views with the saved ones.  The current one is first
-- saved as \"prev\".
restoreViews :: Cmd.M m => Text -> m ()
restoreViews name = do
    (views, focused) <- Cmd.require ("no saved views named: " <> name)
        =<< Ui.config#UiConfig.savedViews # Lens.map name <#> Ui.get
    saveViews "prev"
    Ui.putViews views
    whenJust focused Cmd.focus

removeViews :: Ui.M m => Text -> m ()
removeViews name = Ui.modifyConfig $ UiConfig.savedViews %= Map.delete name

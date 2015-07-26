-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Cmds related to view level state.
module Cmd.ViewConfig where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

import qualified Util.Lens as Lens
import qualified Util.Num as Num
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Cmd.Selection as Selection

import qualified App.Config as Config
import Global
import Types


-- * zoom

cmd_zoom_around_insert :: Cmd.M m => (Double -> Double) -> m ()
cmd_zoom_around_insert f = do
    (view_id, (_, _, pos)) <- Selection.get_any_insert
    cmd_zoom_around view_id pos f

cmd_zoom_around :: Cmd.M m => ViewId -> ScoreTime -> (Double -> Double) -> m ()
cmd_zoom_around view_id pos f = do
    -- Zoom by the given factor, but try to keep pos in the same place on the
    -- screen.
    zoom <- State.get_zoom view_id
    set_zoom view_id (zoom_around zoom pos f)

zoom_around :: Types.Zoom -> ScoreTime -> (Double -> Double) -> Types.Zoom
zoom_around (Types.Zoom offset factor) pos f =
    Types.Zoom (zoom_pos offset pos (ScoreTime.double factor)
        (ScoreTime.double newf)) newf
    where newf = f factor

zoom_pos :: ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime
zoom_pos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

set_zoom :: Cmd.M m => ViewId -> Types.Zoom -> m ()
set_zoom view_id zoom = do
    State.modify_zoom view_id (const zoom)
    Internal.sync_zoom_status view_id

modify_factor :: Cmd.M m => ViewId -> (Double -> Double) -> m ()
modify_factor view_id f = do
    zoom <- State.get_zoom view_id
    set_zoom view_id (zoom { Types.zoom_factor = f (Types.zoom_factor zoom) })

-- | Zoom to the ruler duration if the selection is a point, or zoom to the
-- selection if it's not.
zoom_to_ruler_or_selection :: Cmd.M m => m ()
zoom_to_ruler_or_selection = do
    (view_id, sel) <- Selection.get
    if Sel.is_point sel
        then zoom_to_ruler view_id
        else uncurry (zoom_to view_id) (Sel.range sel)

zoom_to :: Cmd.M m => ViewId -> TrackTime -> TrackTime -> m ()
zoom_to view_id start end =
    set_zoom view_id . Types.Zoom start =<< zoom_factor view_id (end - start)

-- | Set zoom on the given view to make the entire block visible.
zoom_to_ruler :: Cmd.M m => ViewId -> m ()
zoom_to_ruler view_id = do
    view <- State.get_view view_id
    block_end <- State.block_end (Block.view_block view)
    factor <- zoom_factor view_id block_end
    set_zoom view_id $ Types.Zoom 0 factor

-- | Figure out the zoom factor to display the given amount of TrackTime.
zoom_factor :: State.M m => ViewId -> TrackTime -> m Double
zoom_factor view_id dur
    | dur == 0 = return 1
    | otherwise = do
        view <- State.get_view view_id
        let pixels = Block.view_visible_time view
        return $ fromIntegral pixels / ScoreTime.to_double dur

-- * scroll

scroll_pages :: Cmd.M m => TrackTime -> m ()
scroll_pages pages = do
    view_id <- Cmd.get_focused_view
    view <- State.get_view view_id
    let visible = Block.visible_time view
        offset = Types.zoom_offset $ Block.view_zoom view
    end <- State.block_end $ Block.view_block view
    State.modify_zoom view_id $ \zoom -> zoom
        { Types.zoom_offset =
            Num.clamp 0 (end - visible) $ offset + pages * visible
        }

-- * resize

resize_to_fit :: Cmd.M m => Bool -- ^ maximize the window vertically
    -> ViewId -> m ()
resize_to_fit maximize view_id = do
    view <- State.get_view view_id
    screen <- Cmd.get_screen (Rect.upper_left (Block.view_rect view))
    rect <- contents_rect view
    State.set_view_rect view_id $ Rect.intersection screen $
        scootch screen $ Block.set_visible_rect view $
        if maximize then max_height view screen rect else rect
    where
    -- Move the rect over so it fits on the screen.
    scootch screen r = Rect.place
        (Num.clamp (Rect.rx screen) (Rect.rr screen - Rect.rw r) (Rect.rx r))
        (Num.clamp (Rect.ry screen) (Rect.rb screen - Rect.rh r) (Rect.ry r))
        r
    max_height view screen r = Rect.xywh (Rect.rx r) (Rect.ry screen)
        (Rect.rw r) (Rect.rh screen - Block.view_time_padding view
            - Config.window_decoration_h)

resize_all :: Cmd.M m => m ()
resize_all = mapM_ (resize_to_fit False) =<< State.all_view_ids

-- | Get the View's Rect, resized to fit its contents.  Its position is
-- unchanged.
contents_rect :: State.M m => Block.View -> m Rect.Rect
contents_rect view = do
    block_end <- State.block_end (Block.view_block view)
    block <- State.get_block (Block.view_block view)
    let (x, y) = Rect.upper_left (Block.view_rect view)
        w = sum $ map Block.display_track_width (Block.block_tracks block)
        h = Types.zoom_to_pixels (Block.view_zoom view) block_end
    return $ Rect.xywh x y (max w 40) (max h 40)

-- * window management

-- | Infer a tiling layout based on current window position, and move and
-- resize them to fit.

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

-- auto_tile :: Cmd.M m => m ()
-- auto_tile = do

-- | Fit rectangles into a tiling pattern.  The algorithm is to sort them by
-- X and Y, and place the first rectangle at (0, 0).  Then try to fit each
-- rectangle to the below or the right of each already placed rectangle,
-- filtering out the positions that would cause an overlap, and pick the spot
-- closest to the rectangle's original position.  The whole process is started
-- again with any rectangles that wind up totally outside the screen.
fit_rects :: Rect.Rect -> [(ViewId, Rect.Rect)] -> [(ViewId, Rect.Rect)]
fit_rects screen =
    redo_outside . List.foldl' fit []
        . Seq.sort_on (\(_, r) -> (Rect.rx r, Rect.ry r))
    where
    fit windows (view_id, rect) = case Seq.head (sort rect corners) of
        Just (x, y) -> (view_id, Rect.place x y rect) : windows
        -- Shouldn't happen since you can always place to the right or
        -- below the rightmost or bottom rectangle.
        -- Nothing -> error $ "no corners for " <> show rect
        Nothing -> (view_id, rect) : windows
        where
        rects = map snd windows
        corners =
            filter (not . would_overlap rects rect) (corners_of screen rects)

    sort rect = Seq.sort_on $ Rect.point_distance (Rect.upper_left rect)
    -- If there are rects outside the screen, fit them again into an empty
    -- screen.  I should run out eventually.
    redo_outside rects = rects
        -- - | null outside = inside
        -- - | otherwise = fit_rects screen outside ++ inside
        -- where
        -- (inside, outside) = List.partition
        --     (Rect.contains_point screen . Rect.upper_left . snd) rects

corners_of :: Rect.Rect -> [Rect.Rect] -> [(Int, Int)]
corners_of screen [] = [Rect.upper_left screen]
corners_of _ rects =
    filter (\p -> not $ point_above p || point_left p) not_touching
    where
    not_touching = filter (not . touches) $
        map Rect.lower_left rects ++ map Rect.upper_right rects
    point_above (x, y) = any (\(x1, y1) -> x1 == x && y1 < y) not_touching
    point_left (x, y) = any (\(x1, y1) -> x1 < x && y1 == y) not_touching
    touches p = any (\r -> Rect.contains_point r p) rects

-- If I move the rect to the point, will it overlap with anything in the
-- list?
would_overlap :: [Rect.Rect] -> Rect.Rect -> (Int, Int) -> Bool
would_overlap rects rect (x, y) =
    any (Rect.overlaps (Rect.place x y rect)) rects

-- | Arrange views horizontally on each screen.  They'll overlap if there isn't
-- room for all of them.
horizontal_tile :: Cmd.M m => m ()
horizontal_tile = mapM_ (uncurry tile_screen) =<< windows_by_screen
    where
    tile_screen screen view_rects =
        zipWithM_ State.set_view_rect view_ids $
            horizontal_tile_rects screen rects
        where (view_ids, rects) = unzip (Seq.sort_on (Rect.rx . snd) view_rects)

windows_by_screen :: Cmd.M m => m [(Rect.Rect, [(ViewId, Rect.Rect)])]
windows_by_screen = do
    view_rects <- State.gets $
        map (second Block.view_rect) . Map.toList . State.state_views
    screens <- Cmd.gets Cmd.state_screens
    let (screen_views, orphaned) = group_with
            (\s -> Rect.overlaps s . snd) screens view_rects
    mapM_ (State.destroy_view . fst) orphaned
    return screen_views

horizontal_tile_rects :: Rect.Rect -> [Rect.Rect] -> [Rect.Rect]
horizontal_tile_rects screen rects = zipWith place rects xs
    where
    place rect x = Rect.place x (Rect.ry screen) rect
    xs = scanl (+) (Rect.rx screen) (map (subtract overlap . Rect.rw) rects)
    overlap = case rects of
        _ : _ : _ -> max 0 $
            (sum (map Rect.rw rects) - Rect.rw screen) `div` (length rects - 1)
        -- 0 or 1 rects are not going to have any overlap.
        _ -> 0

group_with :: (key -> val -> Bool) -> [key] -> [val] -> ([(key, [val])], [val])
group_with cmp keys vals = Tuple.swap $ List.mapAccumL go vals keys
    where
    go vals key = (out, (key, inside))
        where (inside, out) = List.partition (cmp key) vals

-- ** focus

-- | Right and Left would clash with Either.
data Direction = North | South | East | West deriving (Show)

move_focus :: Cmd.M m => Direction -> m ()
move_focus dir = do
    rects <- State.gets $
        map (second Block.view_rect) . Map.toList . State.state_views
    focused <- Block.view_rect <$> (State.get_view =<< Cmd.get_focused_view)
    let get_rects cmp f = Seq.sort_on snd $ filter ((`cmp` f focused) . snd) $
            map (second f) rects
    let next = case dir of
            East -> Seq.head $ get_rects (>) Rect.rx
            West -> Seq.last $ get_rects (<) Rect.rx
            South -> Seq.head $ get_rects (>) Rect.ry
            North -> Seq.last $ get_rects (<) Rect.ry
    whenJust next $ \(view_id, _) ->
        State.update $ Update.CmdBringToFront view_id

-- * saved views

-- | Save the current views under the given name.
save_views :: Cmd.M m => Text -> m ()
save_views name = do
    views <- State.gets State.state_views
    focused <- Cmd.lookup_focused_view
    State.modify_config $ State.saved_views %= Map.insert name (views, focused)

-- | Replace the current views with the saved ones.  The current one is first
-- saved as \"prev\".
restore_views :: Cmd.M m => Text -> m ()
restore_views name = do
    (views, focused) <- Cmd.require ("no saved views named: " <> name)
        =<< State.config#State.saved_views # Lens.map name <#> State.get
    save_views "prev"
    State.put_views views
    whenJust focused bring_to_front

-- * misc

maximize_and_zoom :: Cmd.M m => ViewId -> m ()
maximize_and_zoom view_id = do
    resize_to_fit True view_id
    zoom_to_ruler view_id

bring_to_front :: Cmd.M m => ViewId -> m ()
bring_to_front view_id = do
    view <- State.lookup_view view_id
    case view of
        Nothing -> Cmd.throw $ "can't bring to front non-existent view "
            <> showt view_id
        _ -> return ()
    State.update $ Update.CmdBringToFront view_id

-- | Cmds related to view level state.
module Cmd.ViewConfig where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple

import Util.Control
import qualified Util.Num as Num
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Cmd.Selection as Selection

import qualified App.Config as Config
import Types


-- * zoom

cmd_zoom_around_insert :: (Cmd.M m) => (Double -> Double) -> m ()
cmd_zoom_around_insert f = do
    (view_id, (_, _, pos)) <- Selection.get_any_insert
    cmd_zoom_around view_id pos f

cmd_zoom_around :: (Cmd.M m) =>
    ViewId -> ScoreTime -> (Double -> Double) -> m ()
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

set_zoom :: (Cmd.M m) => ViewId -> Types.Zoom -> m ()
set_zoom view_id zoom = do
    State.set_zoom view_id zoom
    Internal.sync_zoom_status view_id

modify_factor :: (Cmd.M m) => ViewId -> (Double -> Double) -> m ()
modify_factor view_id f = do
    zoom <- State.get_zoom view_id
    set_zoom view_id (zoom { Types.zoom_factor = f (Types.zoom_factor zoom) })

-- | Set zoom on the given view to make the entire block visible.
zoom_to_ruler :: (Cmd.M m) => ViewId -> m ()
zoom_to_ruler view_id = do
    view <- State.get_view view_id
    block_end <- State.block_ruler_end (Block.view_block view)
    let pixels = Block.view_visible_time view
    let factor = fromIntegral pixels / ScoreTime.to_double block_end
    set_zoom view_id (Types.Zoom 0 factor)

-- * resize

resize_to_fit :: (Cmd.M m) => Bool -- ^ maximize the window vertically
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

resize_all :: (Cmd.M m) => m ()
resize_all = mapM_ (resize_to_fit False) =<< State.all_view_ids

-- | Get the View's Rect, resized to fit its contents.  Its position is
-- unchanged.
contents_rect :: (State.M m) => Block.View -> m Rect.Rect
contents_rect view = do
    block_end <- State.block_ruler_end (Block.view_block view)
    block <- State.get_block (Block.view_block view)
    let (x, y) = Rect.upper_left (Block.view_rect view)
        w = sum $ map Block.display_track_width (Block.block_tracks block)
        h = Types.zoom_to_pixels (Block.view_zoom view) block_end
    return $ Rect.xywh x y (max w 40) (max h 40)

-- * window management

-- | Arrange views horizontally on each screen.  They'll overlap if there isn't
-- room for all of them.
horizontal_tile :: (Cmd.M m) => m ()
horizontal_tile = do
    view_rects <- State.gets $
        map (second Block.view_rect) . Map.toList . State.state_views
    screens <- Cmd.gets Cmd.state_screens
    let (screen_views, orphaned) = group_with
            (\s -> Rect.overlapping s . snd) screens view_rects
    mapM_ (State.destroy_view . fst) orphaned
    mapM_ (uncurry tile_screen) screen_views
    where
    tile_screen screen view_rects =
        zipWithM_ State.set_view_rect view_ids $
            horizontal_tile_rects screen rects
        where (view_ids, rects) = unzip (Seq.sort_on (Rect.rx . snd) view_rects)

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

-- * misc

maximize_and_zoom :: (Cmd.M m) => ViewId -> m ()
maximize_and_zoom view_id = do
    resize_to_fit True view_id
    zoom_to_ruler view_id

bring_to_front :: (Cmd.M m) => ViewId -> m ()
bring_to_front view_id = do
    view <- State.lookup_view view_id
    case view of
        Nothing -> Cmd.throw $ "can't bring to front non-existent view "
            ++ show view_id
        _ -> return ()
    State.update $ Update.CmdBringToFront view_id

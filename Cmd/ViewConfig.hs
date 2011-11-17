-- | Cmds related to view level state.
module Cmd.ViewConfig where
import qualified Util.Rect as Rect
import qualified Ui.Block as Block
import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Internal as Internal
import qualified Cmd.Selection as Selection

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
zoom_to_fit :: (Cmd.M m) => ViewId -> m ()
zoom_to_fit view_id = do
    view <- State.get_view view_id
    block_end <- State.block_event_end (Block.view_block view)
    let pixels = Block.view_visible_time view
    let factor = fromIntegral pixels / ScoreTime.to_double block_end
    set_zoom view_id (Types.Zoom 0 factor)

-- * resize

resize_to_fit :: (Cmd.M m) => ViewId -> m ()
resize_to_fit view_id = do
    view <- State.get_view view_id
    screen <- Cmd.get_screen (Rect.upper_left (Block.view_rect view))
    rect <- view_rect view
    State.set_view_rect view_id $ Rect.intersection screen $
        scootch screen $ Block.set_visible_rect view rect
    where
    -- Move the rect over so it fits on the screen.
    scootch screen r =
        Rect.move (min (Rect.rx r) (Rect.rr screen - Rect.rw r)) (Rect.ry r) r

-- | Get the View's Rect, resized to fit its contents.  Its position is
-- unchanged.
view_rect :: (State.M m) => Block.View -> m Rect.Rect
view_rect view = do
    block_end <- State.block_event_end (Block.view_block view)
    block <- State.get_block (Block.view_block view)
    let (x, y) = Rect.upper_left (Block.view_rect view)
        w = sum $ map Block.display_track_width (Block.block_tracks block)
        h = Types.zoom_to_pixels (Block.view_zoom view) block_end
    return $ Rect.xywh x y (max w 40) (max h 40)

-- * misc

bring_to_front :: (Cmd.M m) => ViewId -> m ()
bring_to_front view_id =
    State.update $ Update.ViewUpdate view_id Update.BringToFront

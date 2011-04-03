-- | Cmds related to view level state.
module Cmd.ViewConfig where
import qualified Util.Rect as Rect

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.Update as Update

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


-- * zoom

cmd_modify_zoom :: (Cmd.M m) => (Double -> Double) -> ViewId -> m ()
cmd_modify_zoom f view_id = do
    zoom <- State.get_zoom view_id
    set_zoom view_id (zoom { Types.zoom_factor = f (Types.zoom_factor zoom) })

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

zoom_around :: Types.Zoom -> Types.ScoreTime -> (Double -> Double) -> Types.Zoom
zoom_around (Types.Zoom offset factor) pos f =
    Types.Zoom (zoom_pos offset pos (Types.ScoreTime factor)
        (Types.ScoreTime newf)) newf
    where newf = f factor

zoom_pos :: ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime -> ScoreTime
zoom_pos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

set_zoom :: (Cmd.M m) => ViewId -> Types.Zoom -> m ()
set_zoom view_id zoom = do
    State.set_zoom view_id zoom
    Cmd.sync_zoom_status view_id

-- * resize

resize_to_fit :: (Cmd.M m) => ViewId -> m ()
resize_to_fit view_id = do
    view <- State.get_view view_id
    screen <- Cmd.get_screen (Rect.upper_left (Block.view_rect view))
    rect <- view_rect view
    State.set_view_rect view_id $ Rect.intersection screen $
        Block.set_visible_rect view rect

-- | Get the View's Rect, resized to fit its contents.  Its position is
-- unchanged.
view_rect :: (State.M m) => Block.View -> m Rect.Rect
view_rect view = do
    block_end <- State.event_end (Block.view_block view)
    block <- State.get_block (Block.view_block view)
    let (x, y) = Rect.upper_left (Block.view_rect view)
        w = sum $ drop 1 (Block.visible_track_widths block view)
        h = Types.zoom_to_pixels (Block.view_zoom view) block_end
    return $ Rect.xywh x y w h

-- * misc

bring_to_front :: (Cmd.M m) => ViewId -> m ()
bring_to_front view_id =
    State.update $ Update.ViewUpdate view_id Update.BringToFront

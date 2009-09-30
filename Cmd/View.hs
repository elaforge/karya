-- | Cmds related to view level state.
module Cmd.View where

import Ui
import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


-- * zoom

set_zoom view_id zoom = do
    State.set_zoom view_id zoom
    Cmd.sync_zoom_status view_id

cmd_modify_zoom :: (Monad m) =>
    (Double -> Double) -> ViewId -> Cmd.CmdT m ()
cmd_modify_zoom f view_id = do
    zoom <- get_zoom view_id
    set_zoom view_id (zoom { Types.zoom_factor = f (Types.zoom_factor zoom) })

cmd_zoom_around_insert :: (Monad m) => (Double -> Double) -> Cmd.CmdT m ()
cmd_zoom_around_insert f = do
    view_id <- Cmd.get_focused_view
    (_, _, pos) <- Selection.get_insert_pos
    cmd_zoom_around view_id pos f

cmd_zoom_around :: (Monad m) =>
    ViewId -> TrackPos -> (Double -> Double) -> Cmd.CmdT m ()
cmd_zoom_around view_id pos f = do
    -- Zoom by the given factor, but try to keep pos in the same place on the
    -- screen.
    zoom <- get_zoom view_id
    set_zoom view_id (zoom_around zoom pos f)

zoom_around (Types.Zoom offset factor) pos f =
    Types.Zoom (zoom_pos offset pos (TrackPos factor) (TrackPos newf)) newf
    where
    newf = f factor

zoom_pos :: TrackPos -> TrackPos -> TrackPos -> TrackPos -> TrackPos
zoom_pos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

get_zoom view_id = fmap Block.view_zoom (State.get_view view_id)

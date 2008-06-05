{- | Block, track, and event related cmds.

An abstraction layer in between the Block that's stored in the state, and the
Block that's visible in the UI.  This is so you can collapse / expand tracks,
mute / unmute tracks, manage deriver views, ...

- Create new view, destroy view.

- Create / destroy blocks.

- Add track, remove track.

- Collapse / expand track into a divider.

    Ways to display a derivation

reorder tracks, without messing up the derivation rules

A neighboring track in the same block.

Another block view.

"Hidden" behind the current block / track, press a key to "descend" one
level.

Not displayed at all, but played of course.

-}
module Cmd.View where

import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


-- * zoom

set_zoom view_id zoom = do
    Log.notice $ "set zoom: " ++ show view_id ++ ": " ++ show zoom
    State.set_zoom view_id zoom
    Cmd.sync_zoom_status view_id

cmd_modify_zoom :: (Double -> Double) -> Block.ViewId -> Cmd.CmdId
cmd_modify_zoom f view_id = do
    zoom <- get_zoom view_id
    set_zoom view_id (zoom { Block.zoom_factor = f (Block.zoom_factor zoom) })
    return Cmd.Done

cmd_zoom_around_insert :: (Double -> Double) -> Cmd.CmdId
cmd_zoom_around_insert f = do
    view_id <- Cmd.get_focused_view
    (pos, _, _) <- Selection.get_insert_pos
    cmd_zoom_around view_id pos f

cmd_zoom_around :: Block.ViewId -> TrackPos -> (Double -> Double) -> Cmd.CmdId
cmd_zoom_around view_id pos f = do
    -- Zoom by the given factor, but try to keep pos in the same place on the
    -- screen.
    zoom <- get_zoom view_id
    set_zoom view_id (zoom_around zoom pos f)
    return Cmd.Done

zoom_around (Block.Zoom offset factor) pos f =
    Block.Zoom (floor (zoom_pos (i offset) (i pos) factor newf)) newf
    where
    i = fromIntegral
    newf = f factor

zoom_pos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

get_zoom view_id = fmap Block.view_zoom (State.get_view view_id)

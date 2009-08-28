{- | Block, track, and event related cmds.

    An abstraction layer in between the Block that's stored in the state, and
    the Block that's visible in the UI.  This is so you can collapse / expand
    tracks, mute / unmute tracks, manage deriver views, ...

    - Collapse / expand track into a divider.

        Ways to display a derivation

    - reorder tracks, without messing up the derivation rules

    - A neighboring track in the same block.

    - Another block view.

    - "Hidden" behind the current block / track, press a key to "descend" one
    level.

    - Not displayed at all, but played of course.
-}
module Cmd.View where

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Selection as Selection


{-
-- These are combined with Block.View to produce the actual visible Block.View.

data View = View {
    view_view :: Block.ViewId
    , view_tracks :: [Track]
    } deriving (Show)

data Track = Track {
    track_collapsed :: Maybe (Block.Width, Color.Color)
    , track_muted :: Bool
    } deriving (Show)
empty_track = Track Nothing False

combine :: (State.UiStateMonad m) => View -> m (View, Block.View)
combine view = do
    view <- State.get_view (view_view view)
    block <- State.get_block (Block.view_block view)
    let (block_tracks, view_tracks) = unzip $  map combine_track $
        Seq.padded_zip empty_track (Block.block_tracks block) (view_tracks view)

-- modify track_bg, block_tracks, view TrackView width
-- convert the block
-}

-- * zoom

set_zoom view_id zoom = do
    State.set_zoom view_id zoom
    Cmd.sync_zoom_status view_id

cmd_modify_zoom :: (Monad m) =>
    (Double -> Double) -> Block.ViewId -> Cmd.CmdT m ()
cmd_modify_zoom f view_id = do
    zoom <- get_zoom view_id
    set_zoom view_id (zoom { Block.zoom_factor = f (Block.zoom_factor zoom) })

cmd_zoom_around_insert :: (Monad m) => (Double -> Double) -> Cmd.CmdT m ()
cmd_zoom_around_insert f = do
    view_id <- Cmd.get_focused_view
    (_, _, pos) <- Selection.get_insert_pos
    cmd_zoom_around view_id pos f

cmd_zoom_around :: (Monad m) =>
    Block.ViewId -> TrackPos -> (Double -> Double) -> Cmd.CmdT m ()
cmd_zoom_around view_id pos f = do
    -- Zoom by the given factor, but try to keep pos in the same place on the
    -- screen.
    zoom <- get_zoom view_id
    set_zoom view_id (zoom_around zoom pos f)

zoom_around (Block.Zoom offset factor) pos f =
    Block.Zoom (zoom_pos offset pos (track_pos factor) (track_pos newf)) newf
    where
    newf = f factor

zoom_pos :: TrackPos -> TrackPos -> TrackPos -> TrackPos -> TrackPos
zoom_pos offset pos oldf newf = (offset - pos) * (oldf/newf) + pos

get_zoom view_id = fmap Block.view_zoom (State.get_view view_id)

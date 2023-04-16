-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Repl.LView where
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Pretty as Pretty
import qualified Util.Rect as Rect
import qualified Util.Lists as Lists

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Create as Create
import qualified Cmd.NoteTrackParse as NoteTrackParse
import qualified Cmd.ViewConfig as ViewConfig
import qualified Cmd.Views as Views

import qualified Ui.Block as Block
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.Zoom as Zoom

import           Global
import           Types


list :: Cmd.CmdL [ViewId]
list = Ui.all_view_ids

-- * create

create :: BlockId -> Cmd.CmdL ViewId
create = Create.view

-- | Create a view from a BlockId inferred like a block call would, from the
-- selected block.
create_infer :: Text -> Cmd.CmdL ViewId
create_infer name = do
    blocks <- Ui.gets Ui.state_blocks
    ns <- Ui.get_namespace
    caller <- Cmd.lookup_focused_block
    case NoteTrackParse.to_block_id blocks ns caller name of
        Just block_id -> Create.view block_id
        Nothing -> Cmd.throw "blah"

-- | For the current window, open enough views at the current zoom to see the
-- score from the current time until the end of the block.
cover :: Cmd.M m => m [ViewId]
cover = ViewConfig.views_covering =<< Cmd.get_focused_view

-- * arrange

-- | Crunch all the views up against each other.
arrange :: Cmd.CmdL ()
arrange = do
    screens <- Cmd.gets Cmd.state_screens
    mapM_ arrange_screen [(Rect.x r, Rect.y r) | r <- screens]

arrange_screen :: (Int, Int) -> Cmd.CmdL ()
arrange_screen point = do
    screen <- Cmd.get_screen $ Just point
    view_rects <- filter (Rect.overlaps screen . snd)
        . map (second Block.view_rect) . Map.toList . Ui.state_views
        <$> Ui.get
    mapM_ (uncurry Ui.set_view_rect) $
        compact screen (Lists.sortOn snd view_rects)

compact :: Rect.Rect -> [(a, Rect.Rect)] -> [(a, Rect.Rect)]
compact screen =
    snd . List.mapAccumL go (Rect.x screen, Rect.y screen, Rect.h screen)
    where
    go (x, y, min_h) (view_id, rect) = (next, (view_id, Rect.place x y rect))
        where
        next
            | x + Rect.w rect < Rect.r screen =
                (x + Rect.w rect, y, min min_h (Rect.h rect))
            | otherwise = (Rect.x screen, y + min_h, Rect.h screen)

-- * save and load

-- | Show the list of saved views, with a star on the focused one.
saved :: Cmd.CmdL Text
saved = do
    saved <- Ui.config#UiConfig.saved_views <#> Ui.get
    return $ Pretty.formatted $ Map.map pretty saved
    where
    pretty (views, focused) =
        map (\view_id -> Pretty.text $
                (if Just view_id == focused then "*" else "") <> showt view_id)
            (Map.keys views)

-- | Save the current view layout.
save :: Text -> Cmd.CmdL ()
save = ViewConfig.save_views

load :: Text -> Cmd.CmdL ()
load = ViewConfig.restore_views

remove :: Ui.M m => Text -> m ()
remove = ViewConfig.remove_views

prev :: Cmd.CmdL ()
prev = load "prev"

-- * zoom

-- | Copy the zoom factor from the given view to the selected one.  This is
-- useful when lining up parts.
zoom_from :: Cmd.M m => ViewId -> m ()
zoom_from from = do
    from_factor <- Zoom.factor . Block.view_zoom <$> Ui.get_view from
    to <- Cmd.get_focused_view
    Views.modify_zoom to (\z -> z { Zoom.factor = from_factor })

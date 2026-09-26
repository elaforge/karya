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
list = Ui.allViewIds

-- * create

create :: BlockId -> Cmd.CmdL ViewId
create = Create.view

-- | Create a view from a BlockId inferred like a block call would, from the
-- selected block.
createInfer :: Text -> Cmd.CmdL ViewId
createInfer name = do
    blocks <- Ui.gets Ui.stateBlocks
    ns <- Ui.getNamespace
    caller <- Cmd.lookupFocusedBlock
    case NoteTrackParse.toBlockId blocks ns caller name of
        Just blockId -> Create.view blockId
        Nothing -> Cmd.throw "blah"

-- | For the current window, open enough views at the current zoom to see the
-- score from the current time until the end of the block.
cover :: Cmd.M m => m [ViewId]
cover = ViewConfig.viewsCovering =<< Cmd.getFocusedView

-- * arrange

-- | Crunch all the views up against each other.
arrange :: Cmd.CmdL ()
arrange = do
    screens <- Cmd.gets Cmd.stateScreens
    mapM_ arrangeScreen [(Rect.x r, Rect.y r) | r <- screens]

arrangeScreen :: (Int, Int) -> Cmd.CmdL ()
arrangeScreen point = do
    screen <- Cmd.getScreen $ Just point
    viewRects <- filter (Rect.overlaps screen . snd)
        . map (second Block.viewRect) . Map.toList . Ui.stateViews
        <$> Ui.get
    mapM_ (uncurry Ui.setViewRect) $
        compact screen (Lists.sortOn snd viewRects)

compact :: Rect.Rect -> [(a, Rect.Rect)] -> [(a, Rect.Rect)]
compact screen =
    snd . List.mapAccumL go (Rect.x screen, Rect.y screen, Rect.h screen)
    where
    go (x, y, minH) (viewId, rect) = (next, (viewId, Rect.place x y rect))
        where
        next
            | x + Rect.w rect < Rect.r screen =
                (x + Rect.w rect, y, min minH (Rect.h rect))
            | otherwise = (Rect.x screen, y + minH, Rect.h screen)

-- * save and load

-- | Show the list of saved views, with a star on the focused one.
saved :: Cmd.CmdL Text
saved = do
    saved <- Ui.config#UiConfig.savedViews <#> Ui.get
    pure $ Pretty.formatted $ Map.map pretty saved
    where
    pretty (views, focused) =
        map (\viewId -> Pretty.text $
                (if Just viewId == focused then "*" else "") <> showt viewId)
            (Map.keys views)

-- | Save the current view layout.
save :: Text -> Cmd.CmdL ()
save = ViewConfig.saveViews

load :: Text -> Cmd.CmdL ()
load = ViewConfig.restoreViews

remove :: Ui.M m => Text -> m ()
remove = ViewConfig.removeViews

prev :: Cmd.CmdL ()
prev = load "prev"

-- * zoom

-- | Copy the zoom factor from the given view to the selected one.  This is
-- useful when lining up parts.
zoomFrom :: Cmd.M m => ViewId -> m ()
zoomFrom from = do
    fromFactor <- Zoom.factor . Block.viewZoom <$> Ui.getView from
    to <- Cmd.getFocusedView
    Views.modifyZoom to (\z -> z { Zoom.factor = fromFactor })

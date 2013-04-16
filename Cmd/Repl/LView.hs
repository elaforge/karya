{-# LANGUAGE NoMonomorphismRestriction #-}
module Cmd.Repl.LView where
import qualified Data.List as List
import qualified Data.Map as Map

import Util.Control
import qualified Util.Rect as Rect
import qualified Util.Seq as Seq

import qualified Ui.Block as Block
import qualified Ui.State as State
import qualified Cmd.Cmd as Cmd


-- | Crunch all the views up against each other.
arrange :: Cmd.CmdL ()
arrange = do
    screens <- Cmd.gets Cmd.state_screens
    mapM_ arrange_screen [(Rect.rx r, Rect.ry r) | r <- screens]

arrange_screen :: (Int, Int) -> Cmd.CmdL ()
arrange_screen point = do
    screen <- Cmd.get_screen point
    view_rects <- filter (Rect.overlapping screen . snd)
        . map (second Block.view_rect) . Map.toList . State.state_views
        <$> State.get
    mapM_ (uncurry State.set_view_rect) $
        compact screen (Seq.sort_on snd view_rects)

compact :: Rect.Rect -> [(a, Rect.Rect)] -> [(a, Rect.Rect)]
compact screen =
    snd . List.mapAccumL go (Rect.rx screen, Rect.ry screen, Rect.rh screen)
    where
    go (x, y, min_h) (view_id, rect) = (next, (view_id, Rect.place x y rect))
        where
        next
            | x + Rect.rw rect < Rect.rr screen =
                (x + Rect.rw rect, y, min min_h (Rect.rh rect))
            | otherwise = (Rect.rx screen, y + min_h, Rect.rh screen)

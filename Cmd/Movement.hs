{- | Commands dealing with selection / cursor movement.


-}
module Cmd.Movement where

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Color as Color
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State

import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep


data Direction = Advance | Rewind deriving (Eq, Ord, Show)

-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.
step_selection :: Block.SelNum -> Direction -> Cmd.CmdM
step_selection selnum dir = do
    -- there must be a way to shorten this
    view_id <- Cmd.get_active_view
    view <- State.get_view view_id
    block <- State.get_block (Block.view_block view)
    step <- Cmd.get_current_step
    sel <- Cmd.require =<< State.get_selection view_id selnum
    ruler_id <- Cmd.require (relevant_ruler block (Block.sel_start_track sel))
    ruler <- State.get_ruler ruler_id

    let pos = Block.sel_start_pos sel
        next = if dir == Advance then TimeStep.advance else TimeStep.rewind
    case next step (Ruler.ruler_marklists ruler) pos of
        Nothing -> Log.notice $
            "can't advance to " ++ show step ++ " from " ++ show pos
        Just next_pos -> State.set_selection view_id selnum
            (select_point (Block.sel_start_track sel) next_pos)
    return Cmd.Done

cmd_drag_selection :: Block.SelNum -> Int -> Cmd.Cmd
cmd_drag_selection selnum mouse_btn = undefined

-- * util

-- | Select a "point" at the given @pos@.
select_point tracknum pos = Block.Selection Color.blue
    tracknum pos 1 (TrackPos 0)

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler.
relevant_ruler :: Block.Block -> Block.TrackNum -> Maybe Ruler.RulerId
relevant_ruler block tracknum = case (ruler, Block.block_ruler_track block) of
        (Block.R ruler_id : _, _) -> Just ruler_id
        (_, Block.R ruler_id) -> Just ruler_id
        _ -> Nothing
    where
    is_ruler (Block.R _) = True
    is_ruler _ = False
    tracks = (reverse . Seq.enumerate . map fst) (Block.block_tracks block)
    ruler = map snd $ dropWhile (not . is_ruler . snd) $
        dropWhile ((/=tracknum) . fst) tracks

{- | Commands dealing with selection / cursor movement.


-}
module Cmd.Selection where
import Control.Monad
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import qualified Util.Log as Log

import Ui.Types
import qualified Ui.Block as Block
import qualified Ui.Ruler as Ruler
import qualified Ui.State as State
import qualified Ui.UiMsg as UiMsg

import Cmd.Types
import qualified Cmd.Msg as Msg
import qualified Cmd.Cmd as Cmd
import qualified Cmd.TimeStep as TimeStep

-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.
cmd_step_selection :: (Monad m) =>
    Block.SelNum -> TimeDirection -> Cmd.CmdT m Cmd.Status
cmd_step_selection selnum dir = do
    -- there must be a way to shorten this
    view_id <- Cmd.get_active_view
    block <- get_view_block view_id
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
            (Block.point_selection (Block.sel_start_track sel) next_pos)
    return Cmd.Done

-- | Move the selection across tracks by @nshift@.
cmd_shift_selection :: Block.SelNum -> Int -> Cmd.CmdM
cmd_shift_selection selnum nshift = do
    view_id <- Cmd.get_active_view
    block <- get_view_block view_id
    sel <- Cmd.require =<< State.get_selection view_id selnum
    let sel' = shift_selection nshift (length (Block.block_tracks block)) sel
    State.set_selection view_id selnum (Just sel')
    return Cmd.Done

-- | Set the selection based on a click or drag.
--
-- TODO: support snap
cmd_mouse_selection :: Int -> Block.SelNum -> Cmd.Cmd
cmd_mouse_selection btn selnum msg = do
    mod <- Cmd.require (mouse_mod msg)
    msg_btn <- Cmd.require (Cmd.mouse_mod_btn mod)
    keys_down <- Cmd.keys_down
    Log.debug $ "mod btn " ++ show mod ++ " in " ++ show (Map.elems keys_down)
    when (msg_btn /= btn) Cmd.abort

    down_at <- Cmd.require $
        case Map.lookup (Cmd.modifier_key mod) keys_down of
            Just (Cmd.MouseMod _btn (Just down_at)) -> Just down_at
            _ -> Nothing

    view_id <- Cmd.get_active_view
    mouse_at <- Cmd.require $ case mod of
        Cmd.MouseMod _ (Just at) -> Just at
        _ -> Nothing
    let sel = selection_from_mouse down_at mouse_at
    Log.debug $ "drag sel from " ++ show down_at ++ " --> " ++ show mouse_at
    State.set_selection view_id selnum (Just sel)
    return Cmd.Done

mouse_mod msg = do
    mouse <- Msg.mouse msg
    btn <- case UiMsg.mouse_state mouse of
        UiMsg.MouseDown btn -> Just btn
        UiMsg.MouseDrag btn -> Just btn
        UiMsg.MouseUp btn -> Just btn
        _ -> Nothing
    track_pos <- Msg.context_track_pos msg
    return $ Cmd.MouseMod btn (Just track_pos)

-- | Create a selection between the two points.
selection_from_mouse ::
    (Block.TrackNum, TrackPos) -> (Block.TrackNum, TrackPos) -> Block.Selection
selection_from_mouse (track1, pos1) (track2, pos2) =
    Block.Selection (min track1 track2) (min pos1 pos2)
        (abs (track1 - track2) + 1) (abs (pos1 - pos2))

-- | Shift the selection to the right or left, clipping it if it hits the edges
-- of the displayed tracks.
shift_selection :: Block.TrackNum -> Block.TrackNum -> Block.Selection
    -> Block.Selection
shift_selection nshift ntracks sel = sel
        { Block.sel_start_track = start'
        , Block.sel_tracks = min (Block.sel_tracks sel) (ntracks - start')
        }
    where
    start = Block.sel_start_track sel
    max_track = ntracks - 1
    start' = between 0 max_track (nshift + start)

get_view_block view_id = do
    view <- State.get_view view_id
    State.get_block (Block.view_block view)

between low high n = min high (max low n)

-- * util

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler.
relevant_ruler :: Block.Block -> Block.TrackNum -> Maybe Ruler.RulerId
relevant_ruler block tracknum = case (ruler, Block.block_ruler_track block) of
        (Block.RId ruler_id : _, _) -> Just ruler_id
        (_, Block.RId ruler_id) -> Just ruler_id
        _ -> Nothing
    where
    is_ruler (Block.RId _) = True
    is_ruler _ = False
    tracks = (reverse . Seq.enumerate . map fst) (Block.block_tracks block)
    ruler = map snd $ dropWhile (not . is_ruler . snd) $
        dropWhile ((/=tracknum) . fst) tracks

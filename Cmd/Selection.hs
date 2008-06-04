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

import qualified App.Config as Config


-- | Advance the given selection by the current step.
-- Require: active block, insert_selection is set
--
-- The selection will maintain its current track span, be set to a point, and
-- advance to the next relevant mark.  "next relevant mark" is the next visible
-- mark in the ruler to the left.
cmd_step_selection :: Block.SelNum -> TimeStep.TimeDirection -> Cmd.CmdId
cmd_step_selection selnum dir = do
    view_id <- Cmd.get_focused_view
    sel <- Cmd.require =<< State.get_selection view_id selnum

    new_pos <- step_from
        (Block.sel_start_track sel) (Block.sel_start_pos sel) dir
    State.set_selection view_id selnum
        (Block.point_selection (Block.sel_start_track sel) new_pos)
    return Cmd.Done

-- | Advance the insert selection by the current step, which is a popular thing
-- to do.
cmd_advance_insert :: Cmd.CmdId
cmd_advance_insert = cmd_step_selection Config.insert_selnum TimeStep.Advance

-- | Move the selection across tracks by @nshift@.
cmd_shift_selection :: Block.SelNum -> Int -> Cmd.CmdId
cmd_shift_selection selnum nshift = do
    view_id <- Cmd.get_focused_view
    block <- State.block_of_view view_id
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

    view_id <- Cmd.get_focused_view
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

between low high n = min high (max low n)

-- * util

step_from :: (Monad m) => Block.TrackNum -> TrackPos -> TimeStep.TimeDirection
    -> Cmd.CmdT m TrackPos
step_from tracknum pos direction = do
    block <- State.block_of_view =<< Cmd.get_focused_view
    step <- Cmd.get_current_step
    ruler_id <- Cmd.require (relevant_ruler block tracknum)
    ruler <- State.get_ruler ruler_id
    let msg = case direction of
            TimeStep.Advance -> "advance to "
            TimeStep.Rewind -> "rewind from "
    case TimeStep.stepper direction step (Ruler.ruler_marklists ruler) pos of
        Nothing -> do
            Log.notice $ "can't " ++ msg ++ show step ++ " from " ++ show pos
            Cmd.abort
        Just next_pos -> return next_pos

-- | Get the ruler that applies to the given track.  Search left for the
-- closest ruler that has all the given marklist names.  This includes ruler
-- tracks and the rulers of event tracks.
relevant_ruler :: Block.Block -> Block.TrackNum -> Maybe Ruler.RulerId
relevant_ruler block tracknum =
    Seq.first_just (map rid_of in_order)
    where
    in_order = map snd $ dropWhile ((/=tracknum) . fst) $ reverse $
        zip [-1..] tracks
    tracks = Block.block_ruler_track block : map fst (Block.block_tracks block)
    rid_of (Block.RId rid) = Just rid
    rid_of (Block.TId _ rid) = Just rid
    rid_of _ = Nothing

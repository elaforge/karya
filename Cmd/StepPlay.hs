{- | A non-realtime play.  The idea is to manually step note-by-note.

    I'm not sure how practical this will be at getting expected notes to
    sound given the complexities of midi state, but it's a starting point.
-}
module Cmd.StepPlay where
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Midi.Midi as Midi
import Ui
import qualified Ui.State as State
import qualified Ui.Types as Types

import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.LEvent as LEvent
import qualified App.Config as Config


selnum :: Types.SelNum
selnum = Config.step_play_selnum

cmd_set_or_advance :: (Cmd.M m) => m ()
cmd_set_or_advance =
    maybe (cmd_set >> cmd_advance) (const cmd_advance)
        =<< Selection.lookup_selnum selnum

-- | Place the play step position at the 'Cmd.state_play_step' before the
-- insert point and prepare the performance.
cmd_set :: (Cmd.M m) => m ()
cmd_set = do
    step <- Cmd.gets (Cmd.state_play_step . Cmd.state_play)
    (block_id, tracknum, track_id, sel_pos) <- Selection.get_insert
    view_id <- Cmd.get_focused_view
    -- TODO need a lax rewind that will go to the earliest point instead of
    -- failing
    start <- Maybe.fromMaybe sel_pos <$>
        TimeStep.rewind step block_id tracknum sel_pos
    start <- TimeStep.snap play_step block_id tracknum Nothing start
    cmd_set_at view_id tracknum track_id start

cmd_set_at :: (Cmd.M m) => ViewId -> TrackNum -> TrackId -> ScoreTime -> m ()
cmd_set_at view_id tracknum track_id pos = do
    State.set_selection view_id selnum (Just (Types.selection 0 pos 999 pos))
    -- put the performance into the state
    perf <- Perf.get_root
    block_id <- State.block_id_of_view view_id
    start_real <- Perf.find_realtime perf block_id (Just track_id) pos
    msgs <- PlayUtil.absolute_perform_from start_real perf
    -- Log.debug $ "putting a step at " ++ show (pos, start_real)
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Just $
        Cmd.StepState tracknum view_id pos perf [] (LEvent.events_of msgs) }

cmd_clear :: (Cmd.M m) => m ()
cmd_clear = do
    view_ids <- Map.keys . State.state_views <$> State.get
    forM_ view_ids $ \view_id ->
        State.set_selection view_id selnum Nothing
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Nothing }
    Cmd.all_notes_off

cmd_advance :: (Cmd.M m) => m ()
cmd_advance = do
    (start, block_id) <- get_selection
    step_state <- Cmd.require =<< get
    when (start < Cmd.step_beginning step_state) $
        reset step_state block_id start
    move True

cmd_rewind :: (Cmd.M m) => m ()
cmd_rewind = move False

-- | If I rewind past the the beginning, I need to regenerate the performance.
-- I could avoid this by always performing from the beginning, but that seems
-- wasteful when the beginning may be far away and those notes will probably
-- not be played.
reset :: (Cmd.M m) => Cmd.StepState -> BlockId -> ScoreTime -> m ()
reset step_state block_id pos = do
    let tracknum = Cmd.step_tracknum step_state
    track_id <- State.get_event_track_at "StepPlay.reset" block_id tracknum
    cmd_set_at (Cmd.step_view_id step_state) tracknum track_id pos

move :: (Cmd.M m) => Bool -> m ()
move forward = do
    (start, block_id) <- get_selection
    step_state <- Cmd.require =<< get
    let msg = "can't " ++ (if forward then "advance" else "rewind")
            ++ " for step play"
    end <- Cmd.require_msg msg
        =<< (if forward then TimeStep.advance else TimeStep.rewind)
            play_step block_id (Cmd.step_tracknum step_state) start
    let err = "step play initiated for track "
            ++ show (Cmd.step_tracknum step_state)
            ++ ", which is evidentally no longer an event track"
    track_id <- Cmd.require_msg err
        =<< State.event_track_at block_id (Cmd.step_tracknum step_state)

    -- Map to RealTime, and then map back to ScoreTime to find all the play
    -- positions, not just the one on this track.
    let perf = Cmd.step_performance step_state
    start_real <- Perf.find_realtime perf block_id (Just track_id) start
    end_real <- Perf.find_realtime perf block_id (Just track_id) end

    -- let inv = Cmd.perf_inv_tempo perf end_real
    -- Log.debug $ "score: " ++ show (start, end)
    --     ++ " real: " ++ show (start_real, end_real)
    --     ++ " inv: " ++ show inv

    -- Cmd.Play.updater_loop keeps track of the previous selections so it
    -- can emit the right clears, but I take the easy way out here.  When I
    -- support discontiguous selections I might have to do the same here.
    view_ids <- State.get_all_view_ids
    clear_selections view_ids
    set_selections view_ids end
    -- If I want to get accurate playback positions, I need to call
    -- find_play_pos on the RealTime.  However, converting ScoreTime ->
    -- RealTime -> ScoreTime loses information since they are different types,
    -- and the inaccuracy messes up time step.  In any case, I don't support
    -- discontiguous play selections yet, so I don't need to get this right.
    -- set_selections =<< Perf.find_play_pos (Cmd.perf_inv_tempo perf) end_real
    let (msgs, step_state2) =
            (if forward then advance_state_step else rewind_state_step)
                start_real end_real step_state
    modify (const (Just step_state2))
    -- TODO You can't just play midi backwards and expect to get a sensible
    -- result.  The proper thing to do would be to record the midi state at
    -- each point and then emit the msgs necessary to restore that state,
    -- I'll implement that if it turns out I want it.
    when forward (play_msgs msgs)

play_step :: TimeStep.TimeStep
play_step = TimeStep.merge 0 (TimeStep.EventEnd TimeStep.AllTracks) $
    TimeStep.step (TimeStep.EventStart TimeStep.AllTracks)

get_selection :: (Cmd.M m) => m (ScoreTime, BlockId)
get_selection = do
    (view_id, sel) <- Selection.get_selnum selnum
    block_id <- State.block_id_of_view view_id
    return (Selection.selection_point sel, block_id)

play_msgs :: (Cmd.M m) => [Midi.WriteMessage] -> m ()
play_msgs = mapM_ $ \wmsg ->
    Cmd.midi (Midi.wmsg_dev wmsg) (Midi.wmsg_msg wmsg)

clear_selections :: (State.M m) => [ViewId] -> m ()
clear_selections = mapM_ $
    \view_id -> State.set_selection view_id selnum Nothing

set_selections :: (State.M m) => [ViewId] -> ScoreTime -> m ()
set_selections view_ids pos = sequence_
    [State.set_selection view_id selnum (sel pos) | view_id <- view_ids]
    where sel pos = Just $ Types.selection 0 pos 999 pos

{-
set_selections :: (State.M m) => [(ViewId, [(TrackNum, ScoreTime)])] -> m ()
set_selections view_sels =
    sequence_ [State.set_selection view_id selnum (sel tracknum pos)
        | ((view_id, tracknum), pos) <- sels]
    where
    -- Since I don't yet support multiple play positions per block, display
    -- only the highest one.  For example, this can happen when asking for the
    -- spot in between two events calling the same block.  It will want to put
    -- a selection both at the top and bottom, and it looks strange to have
    -- the selection jump to the top when you move to the end.
    -- TODO hopefully I can implement multiple positions and get rid of this
    sels = Map.toList $ Map.fromListWith max [((view_id, tracknum), pos)
        | (view_id, ts) <- view_sels, (tracknum, pos) <- ts]
    sel _tracknum pos = Just $ Types.selection 0 pos 999 pos
    -- TODO for this to work right I need per-track selections.  Ui.State
    -- doesn't support this since playback goes through Sync directly.
-}

advance_state_step :: RealTime -> RealTime -> Cmd.StepState
    -> ([Midi.WriteMessage], Cmd.StepState)
advance_state_step start end state =
    (msgs, state { Cmd.step_before = before, Cmd.step_after = after })
    where
    -- Play the msgs up to but not including the end point.
    (all_msgs, before, after) = zip_forward ((>=end) . Midi.wmsg_ts)
        (Cmd.step_before state) (Cmd.step_after state)
    msgs = dropWhile ((<start) . Midi.wmsg_ts) all_msgs

rewind_state_step :: RealTime -> RealTime -> Cmd.StepState
    -> ([Midi.WriteMessage], Cmd.StepState)
rewind_state_step start end state =
    (msgs, state { Cmd.step_before = before, Cmd.step_after = after })
    where
    -- Msgs including the end but not the start.
    (all_msgs, before, after) = zip_backward ((<end) . Midi.wmsg_ts)
        (Cmd.step_before state) (Cmd.step_after state)
    msgs = dropWhile ((>=start) . Midi.wmsg_ts) all_msgs

zip_forward :: (a -> Bool) -- ^ when this is True, stop advancing
    -> [a] -- ^ previous elements, descending
    -> [a] -- ^ following elements, ascending
    -> ([a], [a], [a]) -- ^ (collected, previous, following)
zip_forward f pre post = go [] pre post
    where
    go accum pre (x:xs)
        | f x = (reverse accum, pre, x:xs)
        | otherwise = go (x:accum) (x:pre) xs
    go accum pre [] = (reverse accum, pre, [])

zip_backward :: (a -> Bool) -> [a] -> [a] -> ([a], [a], [a])
zip_backward f pre post = go [] pre post
    where
    go accum (x:xs) post
        | f x = (reverse accum, x:xs, post)
        | otherwise = go (x:accum) xs (x:post)
    go accum [] post = (reverse accum, [], post)

get :: (Cmd.M m) => m (Maybe Cmd.StepState)
get = Cmd.gets (Cmd.state_step . Cmd.state_play)

modify :: (Cmd.M m) => (Maybe Cmd.StepState -> Maybe Cmd.StepState) -> m ()
modify f = Cmd.modify_play_state $ \st ->
    st { Cmd.state_step = f (Cmd.state_step st) }

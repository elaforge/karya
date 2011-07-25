{-# LANGUAGE CPP #-}
{- | A non-realtime play.  The idea is to manually step note-by-note.

    I'm not sure how practical this will be at getting expected notes to
    sound given the complexities of midi state, but it's a starting point.
-}
module Cmd.StepPlay (
    cmd_set_or_advance, cmd_set, cmd_clear, cmd_advance, cmd_rewind

#ifdef TESTING
    , selnum, make_states
#endif
) where
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Midi.State
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
    maybe cmd_set (const cmd_advance) =<< Selection.lookup_selnum selnum

-- | Place the play step position at the 'Cmd.state_play_step' before the
-- insert point and prepare the performance.
cmd_set :: (Cmd.M m) => m ()
cmd_set = do
    (block_id, tracknum, track_id, sel_pos) <- Selection.get_insert
    view_id <- Cmd.get_focused_view
    initialize view_id block_id tracknum track_id sel_pos
    step <- Cmd.gets (Cmd.state_play_step . Cmd.state_play)
    start <- Maybe.fromMaybe sel_pos <$>
        TimeStep.rewind step block_id tracknum sel_pos
    move_to block_id start


-- | Puts MIDI states at every step point along the block.  Then set will zip
-- forward to a certain one and diff it with empty to start the process.
--
-- This may be inefficient if the user only wants to play the previous few
-- notes, but it makes rewinding without a limit simple.  Otherwise I have
-- to detect when the selection has moved before the starting point and
-- reinitialize from there.  I'll do that only if this simpler approach has
-- problems.
initialize :: (Cmd.M m) => ViewId -> BlockId -> TrackNum -> TrackId
    -> ScoreTime -> m ()
initialize view_id block_id tracknum track_id pos = do
    perf <- Perf.get_root
    steps <- Cmd.require_msg "can't get event starts for step play"
        =<< TimeStep.get_points play_step block_id tracknum pos
    let (score_steps, real_steps) = unzip $
            Perf.find_realtimes perf block_id track_id steps
    start <- Cmd.require_msg "no valid step points" (Seq.head real_steps)
    msgs <- LEvent.events_of <$> PlayUtil.perform_from start perf
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Just $ Cmd.StepState
        view_id [] (zip score_steps (make_states real_steps msgs)) }
    where
    play_step = TimeStep.merge 0 (TimeStep.EventEnd TimeStep.AllTracks) $
        TimeStep.step (TimeStep.EventStart TimeStep.AllTracks)

make_states :: [RealTime] -> [Midi.WriteMessage] -> [Midi.State.State]
make_states ts msgs = snd $ List.mapAccumL go (Midi.State.empty, msgs) ts
    where
    go (prev_state, msgs) t = ((state, post), state)
        where
        (pre, post) = break ((>t) . Midi.wmsg_ts) msgs
        state = Midi.State.play (map Midi.State.convert pre) prev_state

cmd_clear :: (Cmd.M m) => m ()
cmd_clear = do
    view_ids <- Map.keys . State.state_views <$> State.get
    forM_ view_ids $ \view_id ->
        State.set_selection view_id selnum Nothing
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Nothing }
    Cmd.all_notes_off

cmd_advance :: (Cmd.M m) => m ()
cmd_advance = move True

cmd_rewind :: (Cmd.M m) => m ()
cmd_rewind = move False

move :: (Cmd.M m) => Bool -> m ()
move forward = do
    let msg = "can't " ++ (if forward then "advance" else "rewind")
            ++ " for step play"
    (view_id, prev_state, pos, state) <- Cmd.require_msg msg
        =<< zip_state forward
    block_id <- State.block_id_of view_id
    -- If I want to get accurate playback positions, I need to call
    -- find_play_pos on the RealTime.  However, converting ScoreTime ->
    -- RealTime -> ScoreTime loses information since they are different types,
    -- and the inaccuracy messes up time step.  In any case, I don't support
    -- discontiguous play selections yet, so I don't need to get this right.
    view_ids <- Map.keys <$> State.get_views_of block_id
    set_selections view_ids pos
    let msgs = Midi.State.diff prev_state state
    mapM_ (uncurry Cmd.midi) msgs

-- | Move to the midi state at the given time and play it.  If there is no
-- exact match for the time, pick the previous one.
move_to :: (Cmd.M m) => BlockId -> ScoreTime -> m ()
move_to block_id pos = do
    step_state <- Cmd.require =<< get
    let (before, after) = zip_backward $ zip_until ((>pos) . fst)
            (Cmd.step_before step_state, Cmd.step_after step_state)
    (pos, mstate) <- Cmd.require $ zip_head (before, after)
    put $ Just $
        step_state { Cmd.step_before = before, Cmd.step_after = after }
    view_ids <- Map.keys <$> State.get_views_of block_id
    set_selections view_ids pos
    mapM_ (uncurry Cmd.midi) $ Midi.State.diff Midi.State.empty mstate

zip_state :: (Cmd.M m) => Bool ->
    m (Maybe (ViewId, Midi.State.State, ScoreTime, Midi.State.State))
zip_state forward = do
    step_state <- Cmd.require =<< get
    let zipper = (Cmd.step_before step_state, Cmd.step_after step_state)
        (before, after) = if forward
            then zip_forward zipper else zip_backward zipper
    when (not (null after)) $
        put $ Just $
            step_state { Cmd.step_before = before, Cmd.step_after = after }
    return $ if forward
        then do
            (_, prev) <- Seq.head before
            (p, next) <- Seq.head after
            return (Cmd.step_view_id step_state, prev, p, next)
        else do
            (p, next) <- Seq.head (fst zipper)
            (_, prev) <- Seq.head (snd zipper)
            return (Cmd.step_view_id step_state, prev, p, next)

zip_forward :: ([a], [a]) -> ([a], [a])
zip_forward (before, []) = (before, [])
zip_forward (before, x : xs) = (x : before, xs)

zip_backward :: ([a], [a]) -> ([a], [a])
zip_backward ([], after) = ([], after)
zip_backward (x : xs, after) = (xs, x : after)

zip_until :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
zip_until f (before, after@(x:xs))
    | f x = (before, after)
    | otherwise = zip_until f (x:before, xs)
zip_until _ (before, []) = (before, [])

zip_head :: ([a], [a]) -> Maybe a
zip_head = Seq.head . snd

set_selections :: (State.M m) => [ViewId] -> ScoreTime -> m ()
set_selections view_ids pos = sequence_
    [State.set_selection view_id selnum (sel pos) | view_id <- view_ids]
    where sel pos = Just $ Types.selection 0 pos 999 pos

get :: (Cmd.M m) => m (Maybe Cmd.StepState)
get = Cmd.gets (Cmd.state_step . Cmd.state_play)

put :: (Cmd.M m) => (Maybe Cmd.StepState) -> m ()
put step_state = Cmd.modify_play_state $ \st ->
    st { Cmd.state_step = step_state }

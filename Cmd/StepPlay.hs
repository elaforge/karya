{-# LANGUAGE CPP #-}
{- | A non-realtime play.  The idea is to manually step note-by-note.

    I'm not sure how practical this will be at getting expected notes to
    sound given the complexities of midi state, but it's a starting point.
-}
module Cmd.StepPlay (
    cmd_set_or_advance, cmd_set, cmd_here, cmd_clear, cmd_advance, cmd_rewind

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
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Cmd.Cmd as Cmd
import qualified Cmd.Perf as Perf
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified App.Config as Config
import Types


selnum :: Types.SelNum
selnum = Config.step_play_selnum

cmd_set_or_advance :: (Cmd.M m) => Bool -> m ()
cmd_set_or_advance play_selected_tracks =
    maybe (cmd_set play_selected_tracks) (const cmd_advance)
        =<< Selection.lookup_selnum selnum

-- | Place the play step position at the 'Cmd.state_play_step' before the
-- insert point and prepare the performance.
cmd_set :: (Cmd.M m) => Bool -> m ()
cmd_set = set True

cmd_here :: (Cmd.M m) => Bool -> m ()
cmd_here = set False

-- | Prepare the step play performance and emit MIDI for the initial position.
set :: (Cmd.M m) => Bool -- ^ Rewind from the selection pos by the play step.
    -> Bool -- ^ Filter events to include only the ones on the selected
    -- tracks.  This is like rederiving with those tracks soloed but doesn't
    -- require a full rederive.
    -> m ()
set step_back play_selected_tracks = do
    (block_id, tracknum, track_id, sel_pos) <- Selection.get_insert
    view_id <- Cmd.get_focused_view
    play_tracks <- if play_selected_tracks
        then Types.sel_tracknums . snd <$> Selection.get
        else return []
    initialize view_id block_id tracknum track_id sel_pos play_tracks
    start <- if step_back
        then do
            step <- Cmd.gets (Cmd.state_play_step . Cmd.state_play)
            Maybe.fromMaybe sel_pos <$>
                TimeStep.rewind step block_id tracknum sel_pos
        else return sel_pos
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
    -> ScoreTime -> [TrackNum] -> m ()
initialize view_id block_id tracknum track_id pos play_tracks = do
    perf <- Perf.get_root
    steps <- Cmd.require_msg "can't get event starts for step play"
        =<< TimeStep.get_points play_step block_id tracknum pos
    let (score_steps, real_steps) = unzip $
            Perf.get_realtimes perf block_id track_id steps
    start <- Cmd.require_msg "no valid step points" (Seq.head real_steps)

    filter_tracks <- if null play_tracks then return id
        else do
            play_ids <- Maybe.catMaybes <$>
                mapM (State.event_track_at block_id) play_tracks
            return $ filter $ LEvent.either (from_track play_ids) (const False)
    msgs <- fmap LEvent.events_of $ PlayUtil.perform_events $ filter_tracks $
        PlayUtil.events_from start $ Cmd.perf_events perf

    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Just $
        Cmd.StepState view_id play_tracks []
            (zip score_steps (make_states real_steps msgs)) }
    where
    play_step = TimeStep.merge 0 (TimeStep.EventEnd step_tracks) $
        TimeStep.step (TimeStep.EventStart step_tracks)
    step_tracks = if null play_tracks then TimeStep.AllTracks
        else TimeStep.TrackNums play_tracks

-- | True if the event was from one of these tracks.
from_track :: [TrackId] -> Score.Event -> Bool
from_track track_ids event = any (`elem` track_ids) $
    Maybe.mapMaybe track_of $ Stack.innermost (Score.event_stack event)
    where
    track_of (Stack.Track tid) = Just tid
    track_of _ = Nothing

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
    forM_ view_ids $ \view_id -> Selection.set view_id selnum Nothing
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Nothing }
    Cmd.all_notes_off

cmd_advance :: (Cmd.M m) => m ()
cmd_advance = move True

cmd_rewind :: (Cmd.M m) => m ()
cmd_rewind = move False

move :: (Cmd.M m) => Bool -> m ()
move forward = do
    step_state <- Cmd.require =<< get
    let msg = "can't " ++ (if forward then "advance" else "rewind")
            ++ " for step play"
    (view_id, prev_state, pos, state) <- Cmd.require_msg msg
        =<< zip_state step_state forward
    block_id <- State.block_id_of view_id
    -- If I want to get accurate playback positions, I need to call
    -- find_play_pos on the RealTime.  However, converting ScoreTime ->
    -- RealTime -> ScoreTime loses information since they are different types,
    -- and the inaccuracy messes up time step.  In any case, I don't support
    -- discontiguous play selections yet, so I don't need to get this right.
    view_ids <- Map.keys <$> State.get_views_of block_id
    set_selections view_ids pos (Cmd.step_tracknums step_state)
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
    set_selections view_ids pos (Cmd.step_tracknums step_state)
    mapM_ (uncurry Cmd.midi) $ Midi.State.diff Midi.State.empty mstate

zip_state :: (Cmd.M m) => Cmd.StepState -> Bool ->
    m (Maybe (ViewId, Midi.State.State, ScoreTime, Midi.State.State))
zip_state step_state forward = do
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

set_selections :: (Cmd.M m) => [ViewId] -> ScoreTime -> [TrackNum] -> m ()
set_selections view_ids pos tracks = sequence_
    [Selection.set view_id selnum (sel pos) | view_id <- view_ids]
    where
    -- I can't display disjoint selections so assume the tracks are
    -- contiguous.
    sel pos = Just $ if null tracks then Types.selection 0 pos 999 pos
        else Types.selection (minimum tracks) pos (maximum tracks) pos

get :: (Cmd.M m) => m (Maybe Cmd.StepState)
get = Cmd.gets (Cmd.state_step . Cmd.state_play)

put :: (Cmd.M m) => (Maybe Cmd.StepState) -> m ()
put step_state = Cmd.modify_play_state $ \st ->
    st { Cmd.state_step = step_state }

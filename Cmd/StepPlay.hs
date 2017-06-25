-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | A non-realtime play.  The idea is to manually step note-by-note.

    This gets the performance for the current block and creates a series of
    MIDI states at each event start which you can then scrub through.

    It uses the starts of the notes in the performance, with a bit of eta
    to account for start randomization.
-}
module Cmd.StepPlay (
    cmd_set_or_advance, cmd_set, cmd_here, cmd_clear, cmd_advance, cmd_rewind

#ifdef TESTING
    , selnum, make_states
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified Util.Seq as Seq
import qualified Midi.Midi as Midi
import qualified Midi.State
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import qualified App.Config as Config
import Global
import Types


selnum :: Sel.Num
selnum = Config.step_play_selnum

cmd_set_or_advance :: Cmd.M m => Bool -> m ()
cmd_set_or_advance play_selected_tracks =
    ifM (Maybe.isJust <$> get)
        cmd_advance (cmd_set play_selected_tracks)

-- | Place the play step position at the 'Cmd.state_play_step' before the
-- insert point and prepare the performance.
cmd_set :: Cmd.M m => Bool -> m ()
cmd_set = set True

cmd_here :: Cmd.M m => Bool -> m ()
cmd_here = set False

-- | Prepare the step play performance and emit MIDI for the initial position.
set :: Cmd.M m => Bool -- ^ Rewind from the selection pos by the play step.
    -> Bool -- ^ Filter events to include only the ones on the selected
    -- tracks.
    -> m ()
set step_back play_selected_tracks = do
    (block_id, tracknum, _, sel_pos) <- Selection.get_insert
    view_id <- Cmd.get_focused_view
    tracks <- Ui.track_count block_id
    play_tracks <- if play_selected_tracks
        then Sel.tracknums tracks <$> Selection.get
        else return []
    initialize view_id block_id play_tracks
    start <- if step_back
        then do
            step <- Cmd.gets (Cmd.state_play_step . Cmd.state_play)
            fromMaybe sel_pos <$>
                TimeStep.rewind step block_id tracknum sel_pos
        else return sel_pos
    move_to block_id start

make_states :: [RealTime] -> [Midi.WriteMessage] -> [Midi.State.State]
make_states ts msgs = snd $ List.mapAccumL go (Midi.State.empty, msgs) ts
    where
    go (prev_state, msgs) t = ((state, post), state)
        where
        (pre, post) = break ((>t) . Midi.wmsg_ts) msgs
        state = Midi.State.play (map Midi.State.convert pre) prev_state

-- ** initialize

-- | Puts MIDI states at every step point along the block.  Then set will zip
-- forward to a certain one and diff it with empty to start the process.
--
-- This may be inefficient if the user only wants to play the previous few
-- notes, but it makes rewinding without a limit simple.  Otherwise I have
-- to detect when the selection has moved before the starting point and
-- reinitialize from there.  I'll do that only if this simpler approach has
-- problems.
--
-- This places step points by looking for event edges, within an eta value.
-- Previously, I placed points based on score positions of event starts and
-- ends, but that doesn't work when the events don't line up with the score.
-- This happens with tuplets, or even if events are a bit randomized.
initialize :: Cmd.M m => ViewId -> BlockId -> [TrackNum] -> m ()
initialize view_id block_id play_tracks = do
    play_track_ids <- Set.fromList <$>
        mapMaybeM (Ui.event_track_at block_id) play_tracks
    perf <- Cmd.get_performance block_id
    let events = filter_tracks play_track_ids $ Cmd.perf_events perf
        reals = group_edges eta events
        scores = real_to_score block_id (Cmd.perf_inv_tempo perf) reals
        steps = [(s, r) | (Just s, r) <- zip scores reals]
    msgs <- LEvent.events_of <$> PlayUtil.perform_events events
    Cmd.modify_play_state $ \st -> st
        { Cmd.state_step = Just $ Cmd.StepState
            { Cmd.step_view_id = view_id
            , Cmd.step_tracknums = play_tracks
            , Cmd.step_before = []
            , Cmd.step_after =
                zip (map fst steps)
                    (make_states (map ((+eta) . snd) steps) msgs)
            }
        }
    where eta = RealTime.seconds 0.01

real_to_score :: BlockId -> Transport.InverseTempoFunction -> [RealTime]
    -> [Maybe ScoreTime]
real_to_score block_id inv = map $ \t ->
    case Seq.head $ filter ((==block_id) . fst) (inv t) of
        -- If this block is being played multiple times then just pick the
        -- first one and the first track.  That's basically what the playback
        -- monitor does anyway.
        Just (_, (_, score) : _) -> Just score
        _ -> Nothing

filter_tracks :: Set TrackId -> Vector.Vector Score.Event
    -> Vector.Vector Score.Event
filter_tracks track_ids
    | Set.null track_ids = id
    | otherwise = Vector.filter (from_track track_ids)

group_edges :: RealTime -> Vector.Vector Score.Event -> [RealTime]
group_edges eta = group . edges . Vector.toList
    where
    edges events = Seq.merge (map Score.event_start events)
        -- The starts should be in order, but the ends have no such guarantee.
        (List.sort (map Score.event_end events))
    group [] = []
    group (t : ts) = t : group (dropWhile (<= t + eta) ts)

-- | True if the event was from one of these tracks.
from_track :: Set TrackId -> Score.Event -> Bool
from_track track_ids event = any (`Set.member` track_ids) $
    mapMaybe track_of $ Stack.innermost (Score.event_stack event)
    where
    track_of (Stack.Track tid) = Just tid
    track_of _ = Nothing

-- * movement

cmd_clear :: Cmd.M m => m ()
cmd_clear = do
    view_ids <- Map.keys . Ui.state_views <$> Ui.get
    forM_ view_ids $ \view_id -> Selection.set_selnum view_id selnum Nothing
    Cmd.modify_play_state $ \st -> st { Cmd.state_step = Nothing }
    Cmd.all_notes_off

cmd_advance :: Cmd.M m => m ()
cmd_advance = move True

cmd_rewind :: Cmd.M m => m ()
cmd_rewind = move False

move :: Cmd.M m => Bool -> m ()
move forward = do
    step_state <- Cmd.abort_unless =<< get
    let msg = "can't " <> (if forward then "advance" else "rewind")
            <> " for step play"
    (view_id, prev_state, pos, state) <- Cmd.require msg
        =<< zip_state step_state forward
    block_id <- Ui.block_id_of view_id
    -- If I want to get accurate playback positions, I need to call
    -- find_play_pos on the RealTime.  However, converting ScoreTime ->
    -- RealTime -> ScoreTime loses information since they are different types,
    -- and the inaccuracy messes up time step.  In any case, I don't support
    -- discontiguous play selections yet, so I don't need to get this right.
    view_ids <- Map.keys <$> Ui.views_of block_id
    set_selections view_ids pos (Cmd.step_tracknums step_state)
    let msgs = Midi.State.diff prev_state state
    mapM_ (uncurry Cmd.midi) msgs

-- | Move to the midi state at the given time and play it.  If there is no
-- exact match for the time, pick the previous one.
move_to :: Cmd.M m => BlockId -> ScoreTime -> m ()
move_to block_id pos = do
    step_state <- Cmd.abort_unless =<< get
    let (before, after) = zip_backward $ zip_until ((>pos) . fst)
            (Cmd.step_before step_state, Cmd.step_after step_state)
    (pos, mstate) <- Cmd.abort_unless $ zip_head (before, after)
    put $ Just $
        step_state { Cmd.step_before = before, Cmd.step_after = after }
    view_ids <- Map.keys <$> Ui.views_of block_id
    set_selections view_ids pos (Cmd.step_tracknums step_state)
    mapM_ (uncurry Cmd.midi) $ Midi.State.diff Midi.State.empty mstate

zip_state :: Cmd.M m => Cmd.StepState -> Bool ->
    m (Maybe (ViewId, Midi.State.State, ScoreTime, Midi.State.State))
zip_state step_state forward = do
    let zipper = (Cmd.step_before step_state, Cmd.step_after step_state)
        (before, after) = if forward
            then zip_forward zipper else zip_backward zipper
    unless (null after) $
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

set_selections :: Cmd.M m => [ViewId] -> ScoreTime -> [TrackNum] -> m ()
set_selections view_ids pos tracks = sequence_
    [Selection.set_selnum view_id selnum (sel pos) | view_id <- view_ids]
    where
    -- I can't display disjoint selections so assume the tracks are
    -- contiguous.
    sel pos = Just $ if null tracks
        then Sel.Selection
            { start_track = 0, start_pos = pos
            , cur_track = 999, cur_pos = pos
            , orientation = Sel.Positive
            }
        else Sel.Selection
            { start_track = minimum tracks, start_pos = pos
            , cur_track = maximum tracks, cur_pos = pos
            , orientation = Sel.Positive
            }

get :: Cmd.M m => m (Maybe Cmd.StepState)
get = Cmd.gets (Cmd.state_step . Cmd.state_play)

put :: Cmd.M m => Maybe Cmd.StepState -> m ()
put step_state = Cmd.modify_play_state $ \st ->
    st { Cmd.state_step = step_state }

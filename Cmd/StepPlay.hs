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
    cmdSetOrAdvance, cmdSet, cmdHere, cmdClear, cmdAdvance, cmdRewind

#ifdef TESTING
    , selnum, makeStates
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector

import qualified Util.Lists as Lists
import qualified App.Config as Config
import qualified Cmd.Cmd as Cmd
import qualified Cmd.PlayUtil as PlayUtil
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Midi.Midi as Midi
import qualified Midi.State
import qualified Perform.RealTime as RealTime
import qualified Perform.Transport as Transport
import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui

import           Global
import           Types


selnum :: Sel.Num
selnum = Config.stepPlaySelnum

cmdSetOrAdvance :: Cmd.M m => Bool -> m ()
cmdSetOrAdvance playSelectedTracks =
    ifM (Maybe.isJust <$> get)
        cmdAdvance (cmdSet playSelectedTracks)

-- | Place the play step position at the 'Cmd.state_play_step' before the
-- insert point and prepare the performance.
cmdSet :: Cmd.M m => Bool -> m ()
cmdSet = set True

cmdHere :: Cmd.M m => Bool -> m ()
cmdHere = set False

-- | Prepare the step play performance and emit MIDI for the initial position.
set :: Cmd.M m => Bool -- ^ Rewind from the selection pos by the play step.
    -> Bool -- ^ Filter events to include only the ones on the selected
    -- tracks.
    -> m ()
set stepBack playSelectedTracks = do
    (blockId, tracknum, _, selPos) <- Selection.getInsert
    viewId <- Cmd.getFocusedView
    tracks <- Ui.trackCount blockId
    playTracks <- if playSelectedTracks
        then Sel.tracknums tracks <$> Selection.get
        else pure []
    initialize viewId blockId playTracks
    start <- if stepBack
        then do
            step <- Cmd.gets (Cmd.statePlayStep . Cmd.statePlay)
            fromMaybe selPos <$>
                TimeStep.rewind step blockId tracknum selPos
        else pure selPos
    moveTo blockId start

makeStates :: [RealTime] -> [Midi.WriteMessage] -> [Midi.State.State]
makeStates ts msgs = snd $ List.mapAccumL go (Midi.State.empty, msgs) ts
    where
    go (prevState, msgs) t = ((state, post), state)
        where
        (pre, post) = break ((>t) . Midi.wmsgTs) msgs
        state = Midi.State.play (map Midi.State.convert pre) prevState

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
initialize viewId blockId playTracks = do
    playTrackIds <- Set.fromList <$>
        mapMaybeM (Ui.eventTrackAt blockId) playTracks
    perf <- Cmd.getPerformance blockId
    let events = filterTracks playTrackIds $ Cmd.perfEvents perf
        reals = groupEdges eta events
        scores = realToScore blockId (Cmd.perfInvTempo perf) reals
        steps = [(s, r) | (Just s, r) <- zip scores reals]
    msgs <- LEvent.eventsOf <$> PlayUtil.performRaw (Vector.toList events)
    Cmd.modifyPlayState $ \st -> st
        { Cmd.stateStep = Just $ Cmd.StepState
            { Cmd.stepViewId = viewId
            , Cmd.stepTracknums = playTracks
            , Cmd.stepBefore = []
            , Cmd.stepAfter =
                zip (map fst steps)
                    (makeStates (map ((+eta) . snd) steps) msgs)
            }
        }
    where eta = RealTime.seconds 0.01

realToScore :: BlockId -> Transport.InverseTempoFunction -> [RealTime]
    -> [Maybe ScoreTime]
realToScore blockId inv = map $ \t ->
    case Lists.head $ filter ((==blockId) . fst) (inv Transport.StopAtEnd t) of
        -- If this block is being played multiple times then just pick the
        -- first one and the first track.  That's basically what the playback
        -- monitor does anyway.
        Just (_, (_, score) : _) -> Just score
        _ -> Nothing

filterTracks :: Set TrackId -> Vector.Vector Score.Event
    -> Vector.Vector Score.Event
filterTracks trackIds
    | Set.null trackIds = id
    | otherwise = Vector.filter (fromTrack trackIds)

groupEdges :: RealTime -> Vector.Vector Score.Event -> [RealTime]
groupEdges eta = group . edges . Vector.toList
    where
    edges events = Lists.merge (map Score.eventStart events)
        -- The starts should be in order, but the ends have no such guarantee.
        (List.sort (map Score.eventEnd events))
    group [] = []
    group (t : ts) = t : group (dropWhile (<= t + eta) ts)

-- | True if the event was from one of these tracks.
fromTrack :: Set TrackId -> Score.Event -> Bool
fromTrack trackIds event = any (`Set.member` trackIds) $
    mapMaybe trackOf $ Stack.innermost (Score.eventStack event)
    where
    trackOf (Stack.Track tid) = Just tid
    trackOf _ = Nothing

-- * movement

cmdClear :: Cmd.M m => m ()
cmdClear = do
    viewIds <- Map.keys . Ui.stateViews <$> Ui.get
    forM_ viewIds $ \viewId -> Selection.setSelnum viewId selnum Nothing
    Cmd.modifyPlayState $ \st -> st { Cmd.stateStep = Nothing }
    Cmd.allNotesOff

cmdAdvance :: Cmd.M m => m ()
cmdAdvance = move True

cmdRewind :: Cmd.M m => m ()
cmdRewind = move False

move :: Cmd.M m => Bool -> m ()
move forward = do
    stepState <- Cmd.abortUnless =<< get
    let msg = "can't " <> (if forward then "advance" else "rewind")
            <> " for step play"
    (viewId, prevState, pos, state) <- Cmd.require msg
        =<< zipState stepState forward
    blockId <- Ui.blockIdOf viewId
    -- If I want to get accurate playback positions, I need to call
    -- find_play_pos on the RealTime.  However, converting ScoreTime ->
    -- RealTime -> ScoreTime loses information since they are different types,
    -- and the inaccuracy messes up time step.  In any case, I don't support
    -- discontiguous play selections yet, so I don't need to get this right.
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    setSelections viewIds pos (Cmd.stepTracknums stepState)
    let msgs = Midi.State.diff prevState state
    mapM_ (uncurry Cmd.midi) msgs

-- | Move to the midi state at the given time and play it.  If there is no
-- exact match for the time, pick the previous one.
moveTo :: Cmd.M m => BlockId -> ScoreTime -> m ()
moveTo blockId pos = do
    stepState <- Cmd.abortUnless =<< get
    let (before, after) = zipBackward $ zipUntil ((>pos) . fst)
            (Cmd.stepBefore stepState, Cmd.stepAfter stepState)
    (pos, mstate) <- Cmd.abortUnless $ zipHead (before, after)
    put $ Just $
        stepState { Cmd.stepBefore = before, Cmd.stepAfter = after }
    viewIds <- Map.keys <$> Ui.viewsOf blockId
    setSelections viewIds pos (Cmd.stepTracknums stepState)
    mapM_ (uncurry Cmd.midi) $ Midi.State.diff Midi.State.empty mstate

zipState :: Cmd.M m => Cmd.StepState -> Bool ->
    m (Maybe (ViewId, Midi.State.State, ScoreTime, Midi.State.State))
zipState stepState forward = do
    let zipper = (Cmd.stepBefore stepState, Cmd.stepAfter stepState)
        (before, after) = if forward
            then zipForward zipper else zipBackward zipper
    unless (null after) $
        put $ Just $
            stepState { Cmd.stepBefore = before, Cmd.stepAfter = after }
    pure $ if forward
        then do
            (_, prev) <- Lists.head before
            (p, next) <- Lists.head after
            pure (Cmd.stepViewId stepState, prev, p, next)
        else do
            (p, next) <- Lists.head (fst zipper)
            (_, prev) <- Lists.head (snd zipper)
            pure (Cmd.stepViewId stepState, prev, p, next)

zipForward :: ([a], [a]) -> ([a], [a])
zipForward (before, []) = (before, [])
zipForward (before, x : xs) = (x : before, xs)

zipBackward :: ([a], [a]) -> ([a], [a])
zipBackward ([], after) = ([], after)
zipBackward (x : xs, after) = (xs, x : after)

zipUntil :: (a -> Bool) -> ([a], [a]) -> ([a], [a])
zipUntil f (before, after@(x:xs))
    | f x = (before, after)
    | otherwise = zipUntil f (x:before, xs)
zipUntil _ (before, []) = (before, [])

zipHead :: ([a], [a]) -> Maybe a
zipHead = Lists.head . snd

setSelections :: Cmd.M m => [ViewId] -> ScoreTime -> [TrackNum] -> m ()
setSelections viewIds pos tracks = sequence_
    [Selection.setSelnum viewId selnum (sel pos) | viewId <- viewIds]
    where
    -- I can't display disjoint selections so assume the tracks are
    -- contiguous.
    sel pos = Just $ if null tracks
        then Sel.Selection
            { startTrack = 0, startPos = pos
            , curTrack = 999, curPos = pos
            , orientation = Sel.Positive
            }
        else Sel.Selection
            { startTrack = minimum tracks, startPos = pos
            , curTrack = maximum tracks, curPos = pos
            , orientation = Sel.Positive
            }

get :: Cmd.M m => m (Maybe Cmd.StepState)
get = Cmd.gets (Cmd.stateStep . Cmd.statePlay)

put :: Cmd.M m => Maybe Cmd.StepState -> m ()
put stepState = Cmd.modifyPlayState $ \st ->
    st { Cmd.stateStep = stepState }

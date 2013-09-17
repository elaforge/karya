-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Post-processing utils.  These are transformers that directly modify the
-- output of a deriver, as opposed to simply modifying the 'Derive.Dynamic'.
module Derive.Call.Post where
import qualified Data.FixedList as FixedList
import Data.FixedList (Nil(..))
import qualified Data.Monoid as Monoid
import qualified Data.Traversable as Traversable

import Util.Control
import qualified Util.Log as Log
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Signal as Signal
import Types


-- Functions here force a Deriver into its LEvent.LEvents and process them
-- directly, and then repackage them as a Deriver.  This can accomplish
-- concrete post-processing type effects but has the side-effect of collapsing
-- the Deriver, which will no longer respond to the environment.

-- Generators can mostly forget about LEvents and emit plain Events since
-- 'Derive.generator' applies the fmap.  Unfortunately the story is not so
-- simple for transformers.  Hopefully functions here can mostly hide LEvents
-- from transformers.

map_around :: ([Score.Event] -> Score.Event -> [Score.Event] -> Score.Event)
    -> Derive.Events -> Derive.Events
map_around f events = go [] events
    where
    go prev (event : events) = case event of
        LEvent.Log log -> LEvent.Log log : go prev events
        LEvent.Event event ->
            let out = f prev event (LEvent.events_of events)
            in LEvent.Event out : go (out : prev) events
    go _ [] = []

-- | Apply a function on the first Event of an LEvent stream.
map_first :: (a -> Derive.Deriver a) -> LEvent.LEvents a -> Derive.LogsDeriver a
map_first f events = event_head events $ \e es -> do
    e <- f e
    return $ LEvent.Event e : es

event_head :: LEvent.LEvents d
    -> (d -> LEvent.LEvents d -> Derive.Deriver (LEvent.LEvents d))
    -> Derive.LogsDeriver d
event_head [] _ = return []
event_head (log@(LEvent.Log _) : rest) f = (log:) <$> event_head rest f
event_head (LEvent.Event event : rest) f = f event rest

map_events_asc :: state
    -> (state -> Score.Event -> Derive.Deriver (state, [Score.Event]))
    -> Derive.NoteDeriver -> Derive.NoteDeriver
map_events_asc state f deriver = do
    (_, result) <- map_controls Nil state (\Nil -> f) =<< deriver
    return $ Derive.merge_asc_events result

-- | Specialization of 'map_controls' where the transformation will return
-- events in ascending order.
map_controls_asc :: (FixedList.FixedList cs) => cs TrackLang.ValControl
    -> state
    -> (cs Score.TypedVal -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.NoteDeriver -> Derive.NoteDeriver
map_controls_asc controls state f deriver = do
    (_, result) <- map_controls controls state f =<< deriver
    return $ Derive.merge_asc_events result

-- | Specialization of 'map_controls_pitches' with no pitch signals.  Also,
-- the mapped function is not in Deriver since you are expected to be
-- depending only on the control values.
map_controls :: (FixedList.FixedList cs) => cs TrackLang.ValControl
    -> state
    -> (cs Score.TypedVal -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.Events -> Derive.Deriver (state, [Derive.Events])
map_controls controls state f =
    map_controls_pitches controls Nil state (\cs Nil -> f cs)

-- | Map a function with state over events and lookup pitch and controls vals
-- for each event.  Exceptions are caught and logged.
--
-- This is the most general transformer map over events.
map_controls_pitches :: (FixedList.FixedList cs, FixedList.FixedList ps) =>
    cs TrackLang.ValControl -> ps TrackLang.PitchControl
    -> state
    -> (cs Score.TypedVal -> ps PitchSignal.Pitch -> state -> Score.Event
        -> Derive.Deriver (state, [Score.Event]))
    -> Derive.Events -> Derive.Deriver (state, [Derive.Events])
map_controls_pitches controls pitch_controls state f events = go state events
    where
    go state [] = return (state, [])
    go state (log@(LEvent.Log _) : rest) = do
        (final_state, rest_vals) <- go state rest
        return (final_state, [log] : rest_vals)
    go state (LEvent.Event event : rest) = do
        let pos = Score.event_start event
        control_vals <- Traversable.mapM
            (flip Util.typed_control_at pos) controls
        pitch_vals <- Traversable.mapM (Util.pitch_at pos) pitch_controls
        result <- Derive.with_event event $
            f control_vals pitch_vals state event
        (final_state, rest_vals) <- go (maybe state fst result) rest
        let vals = maybe rest_vals
                ((:rest_vals) . map LEvent.Event . snd) result
        return (final_state, vals)

-- * next in track

-- | Return only the events that follow the given event on its track.
filter_next_in_track :: Score.Event -> [Score.Event] -> [Score.Event]
filter_next_in_track event = filter (next_in_track (stack event) . stack)
    where stack = Stack.to_ui . Score.event_stack

-- | Is the second stack from an event that occurs later on the same track as
-- the first?  This is more complicated than it may seem at first because the
-- second event could come from a different deriver.  So it should look like
-- @same ; same ; bid same / tid same / range higher ; *@.
next_in_track :: [Stack.UiFrame] -> [Stack.UiFrame] -> Bool
next_in_track (s1@(bid1, tid1, r1) : stack1) (s2@(bid2, tid2, r2) : stack2)
    | s1 == s2 = next_in_track stack1 stack2
    | bid1 == bid2 && tid1 == tid2 && r1 `before` r2 = True
    | otherwise = False
    where
    before (Just (s1, _)) (Just (s2, _)) = s1 < s2
    before _ _ = False
next_in_track _ _ = True


-- * signal

control_range :: Derive.ControlDeriver
    -> Derive.Deriver (Signal.Control, (RealTime, RealTime), [Log.Msg])
control_range deriver = do
    (sig, logs) <- first mconcat . LEvent.partition <$> deriver
    let range = case (Signal.head sig, Signal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    return (sig, range, logs)

pitch_range :: Derive.PitchDeriver
    -> Derive.Deriver (PitchSignal.Signal, (RealTime, RealTime), [Log.Msg])
pitch_range deriver = do
    (sig, logs) <- first mconcat . LEvent.partition <$> deriver
    let range = case (PitchSignal.head sig, PitchSignal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    return (sig, range, logs)

-- | Transform a pitch or control signal.
signal :: (Monoid.Monoid sig) => (sig -> sig)
    -> Derive.LogsDeriver sig -> Derive.LogsDeriver sig
signal f deriver = do
    (chunks, logs) <- LEvent.partition <$> deriver
    return $ LEvent.Event (f (mconcat chunks)) : map LEvent.Log logs

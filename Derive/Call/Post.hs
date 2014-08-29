-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Post-processing utils.  These are transformers that directly modify the
    output of a deriver, as opposed to simply modifying the 'Derive.Dynamic'.

    Unfortunately things are complicated by the presence of 'LEvent.Log's in
    the output stream.  I haven't been able to figure out how to cleanly
    abstract that away, so I wind up with a collection of functions to handle
    specific kinds of maps.

    This is a bit of a mess.  The problem is that there are multiple axes:

    - output is ascending vs. unordered

    - monadic vs. pure

    - state vs. stateless

    Additionally, there are several kinds of state I'd like to factor out, e.g.
    sampling controls and previous and next events.  Unfortunately I haven't
    been able to come up with a nice composable way to do this, so I'm stuck
    with a million separate functions.

    The _asc functions require the mapped function to emit sorted lists of
    sorted events.  This is pretty terrible, because it's a somewhat subtle
    property, and if you break it you get events out of order which makes
    other functions down the line buggy.  But on the other hand, I'm reluctant
    to abandon it because many transformations do have this property and
    the efficiency difference seems compelling.
-}
module Derive.Call.Post where
import qualified Prelude
import Prelude hiding (map, mapM)
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq

import qualified Derive.Call.Note as Note
import qualified Derive.Call.Util as Util
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.TrackLang as TrackLang

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Types


-- * map events

-- ** non-monadic

-- | Apply a function to the events as a list.
apply :: ([a] -> [b]) -> [LEvent.LEvent a] -> [LEvent.LEvent b]
apply f = merge . first f . LEvent.partition
    where
    -- Prepend logs because there should be fewer logs than events.
    merge (events, logs) =
        Prelude.map LEvent.Log logs ++ Prelude.map LEvent.Event events

-- | Non-monadic map without state.
map :: (a -> b) -> [LEvent.LEvent a] -> [LEvent.LEvent b]
map = Prelude.map . fmap

-- | Non-monadic map with state.
map_events :: (state -> a -> (state, [b])) -> state
    -> [LEvent.LEvent a] -> (state, [[LEvent.LEvent b]])
map_events f = List.mapAccumL go
    where
    go state (LEvent.Log log) = (state, [LEvent.Log log])
    go state (LEvent.Event event) = Prelude.map LEvent.Event <$> f state event

map_events_ :: (a -> [b]) -> [LEvent.LEvent a] -> [[LEvent.LEvent b]]
map_events_ f = snd . map_events (\() event -> ((), f event)) ()

map_events_asc :: (state -> event -> (state, [Score.Event])) -> state
    -> [LEvent.LEvent event] -> (state, Derive.Events)
map_events_asc f state = second Derive.merge_asc_events . map_events f state

-- | 'map_events_asc' without state.
map_events_asc_ :: (event -> [Score.Event]) -> [LEvent.LEvent event]
    -> Derive.Events
map_events_asc_ f = snd . map_events_asc (\() event -> ((), f event)) ()

-- ** monadic

-- | Monadic map without state.  Exceptions are not caught.
-- TODO use Derive.with_event_stack
mapM :: Monad m => (a -> m b) -> [LEvent.LEvent a] -> m [LEvent.LEvent b]
mapM _ [] = return []
mapM f (LEvent.Event e : es) = do
    e <- f e
    es <- mapM f es
    return (LEvent.Event e : es)
mapM f (LEvent.Log e : es) = (LEvent.Log e :) `liftM` mapM f es

-- | Monadic map with state and annotations.  Annotations are are also state,
-- but unthreaded, which makes them simpler.  You construct them with 'control'
-- and 'nexts' and the like, and then they get zipped up with the input events.
map_events_m ::
    (state -> annot -> Score.Event -> Derive.Deriver (state, [Score.Event]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> [annot] -> Derive.Events
    -> Derive.Deriver (state, [Derive.Events])
    -- ^ events are return as unmerged chunks, so the caller can merge
map_events_m f state annots = go state . LEvent.zip annots
    where
    go state [] = return (state, [])
    go state (LEvent.Log log : events) =
        fmap ([LEvent.Log log] :) <$> go state events
    go state (LEvent.Event (annot, event) : events) = do
        (state, output) <-
            fromMaybe (state, []) <$> Derive.catch True
                (Derive.with_event_stack event (f state annot event))
        (final, outputs) <- go state events
        return (final, Prelude.map LEvent.Event output : outputs)

-- | Like 'map_events_m', but assume the function returns sorted chunks in
-- increasing order, so they can be merged efficiently.
map_events_asc_m ::
    (state -> annot -> Score.Event -> Derive.Deriver (state, [Score.Event]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> [annot] -> Derive.Events
    -> Derive.Deriver (state, Derive.Events)
map_events_asc_m f state annots =
    fmap (second Derive.merge_asc_events) . map_events_m f state annots

-- ** unthreaded state

control :: (Score.TypedVal -> a) -> TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [a]
control f c events = do
    sig <- Util.to_typed_function c
    return $ Prelude.map (f . sig . Score.event_start) (LEvent.events_of events)

time_control :: TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [RealTime]
time_control = control (RealTime.seconds . Score.typed_val)

-- | Extract subsequent events.
nexts :: [LEvent.LEvent e] -> [[e]]
nexts = drop 1 . List.tails . LEvent.events_of

-- | Extract previous events.
prevs :: [LEvent.LEvent e] -> [[e]]
prevs = scanl (flip (:)) [] . LEvent.events_of

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

-- ** next in track

-- | Return only the events that follow the given event on its track.
next_in_track :: Score.Event -> [Score.Event] -> [Score.Event]
next_in_track event = filter $ next_prev_in_track True (stack event) . stack
    where stack = Stack.to_ui . Score.event_stack

prev_in_track :: Score.Event -> [Score.Event] -> [Score.Event]
prev_in_track event = filter $ next_prev_in_track False (stack event) . stack
    where stack = Stack.to_ui . Score.event_stack

-- | Is the second stack from an event that occurs later (or earlier) on the
-- same track as the first?  This is more complicated than it may seem at first
-- because the second event could come from a different block.  So it should
-- look like @same ; same ; bid same / tid same / range higher or lower ; *@.
next_prev_in_track :: Bool -> [Stack.UiFrame] -> [Stack.UiFrame] -> Bool
next_prev_in_track next
        (f1@(bid1, tid1, r1) : stack1) (f2@(bid2, tid2, r2) : stack2)
    | f1 == f2 = next_prev_in_track next stack1 stack2
    | bid1 == bid2 && tid1 == tid2 && r1 `before_after` r2 = True
    | otherwise = False
    where
    before_after (Just (s1, _)) (Just (s2, _)) =
        if next then s1 < s2 else s1 > s2
    before_after _ _ = False
next_prev_in_track _ _ _ = True -- TODO why true?

-- | If the given event has a hand, return only events with the same hand.
-- Filter for the same instrument regardless.
same_hand :: Score.Event -> [Score.Event] -> [Score.Event]
same_hand event = filter ((== inst event) . inst) .  case lookup event of
    Nothing -> id
    Just hand -> filter $ (== Just hand) . lookup
    where
    inst = Score.event_instrument
    lookup :: Score.Event -> Maybe Text
    lookup = TrackLang.maybe_val Environ.hand . Score.event_environ

-- ** misc maps

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
    (sig, logs) <- derive_signal deriver
    return $ LEvent.Event (f sig) : Prelude.map LEvent.Log logs

derive_signal :: (Monoid.Monoid sig) => Derive.LogsDeriver sig
    -> Derive.Deriver (sig, [Log.Msg])
derive_signal deriver = do
    (chunks, logs) <- LEvent.partition <$> deriver
    return (mconcat chunks, logs)

-- * delayed events

{- | Make a delayed event.

    A delayed event should be realized by an accompanying postproc call. It has
    an 'Environ.args', which are the arguments to the postproc call, and so
    it's a little bit like a closure or a delayed thunk.

    It's awkward because you have to manually call the postproc, which then has
    to extract the args and re-typecheck them.  I considered storing actual
    thunks as functions, and running a generic postproc which forces them, but
    I think each one is likely to require a different context.  E.g. previous
    and next events for the same instrument, or with the same hand, or map
    over groups of events, etc.  TODO wait until I have more experience.
-}
make_delayed :: Derive.PassedArgs a -> RealTime -> RealTime -> [TrackLang.Val]
    -> Derive.NoteDeriver
make_delayed args start next event_args = do
    dyn <- Internal.get_dynamic id
    control_vals <- Derive.controls_at start
    let event = delayed_event event_args $
            Note.make_event args dyn control_vals start 0 next
    return [LEvent.Event event]

delayed_event :: [TrackLang.Val] -> Score.Event -> Score.Event
delayed_event args = Score.modify_environ $
    TrackLang.insert_val Environ.args (TrackLang.VList args)

-- | Return the args if this is a delayed event created by the given call.
delayed_args :: TrackLang.CallId -> Score.Event -> Maybe [TrackLang.Val]
delayed_args (TrackLang.Symbol call) event
    | Seq.head (Stack.innermost (Score.event_stack event))
            == Just (Stack.Call call) =
        TrackLang.maybe_val Environ.args (Score.event_environ event)
    | otherwise = Nothing

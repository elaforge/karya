-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE ScopedTypeVariables #-}
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

    TODO on the other hand, sorting is fast.  Now that I don't care about
    deriving the score lazily, it may actually be cheaper to remove the ordered
    constraint and do one sort at the end.
-}
module Derive.Call.Post where
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

-- 'emap' is kind of ugly, but at least it's consistent and short.
-- I previously used 'map', but it turns out replacing the Prelude map is
-- really confusing.

-- ** non-monadic

-- | 1:1 non-monadic map without state.
emap1 :: (a -> b) -> [LEvent.LEvent a] -> [LEvent.LEvent b]
emap1 = map . fmap

-- | 'Data.Maybe.catMaybes' for LEvents.
cat_maybes :: [LEvent.LEvent (Maybe a)] -> [LEvent.LEvent a]
cat_maybes [] = []
cat_maybes (x : xs) = case x of
    LEvent.Log log -> LEvent.Log log : cat_maybes xs
    LEvent.Event (Just e) -> LEvent.Event e : cat_maybes xs
    LEvent.Event Nothing -> cat_maybes xs

-- | 1:n non-monadic map with state.
emap :: (state -> a -> (state, [b])) -> state
    -> [LEvent.LEvent a] -> (state, [[LEvent.LEvent b]])
emap f = List.mapAccumL go
    where
    go state (LEvent.Log log) = (state, [LEvent.Log log])
    go state (LEvent.Event event) = map LEvent.Event <$> f state event

-- | 'emap' without state.
emap_ :: (a -> [b]) -> [LEvent.LEvent a] -> [[LEvent.LEvent b]]
emap_ f = map flatten . emap1 f
    where
    flatten (LEvent.Log log) = [LEvent.Log log]
    flatten (LEvent.Event events) = map LEvent.Event events

emap_asc :: (state -> event -> (state, [Score.Event])) -> state
    -> [LEvent.LEvent event] -> (state, Derive.Events)
emap_asc f state = second Derive.merge_asc_events . emap f state

-- | 'emap_asc' without state.
emap_asc_ :: (event -> [Score.Event]) -> [LEvent.LEvent event] -> Derive.Events
emap_asc_ f = Derive.merge_asc_events . emap_ f

-- ** monadic

-- | Apply a function to the non-log events.
apply :: Functor m => ([a] -> m [b]) -> [LEvent.LEvent a] -> m [LEvent.LEvent b]
apply f levents = (map LEvent.Log logs ++) . map LEvent.Event <$> f events
    where (events, logs) = LEvent.partition levents

-- | Like 'Derive.with_event_stack', but directly add the event's innermost
-- stack to a log msg.
add_event_stack :: Score.Event -> Log.Msg -> Log.Msg
add_event_stack =
    maybe id with_stack . Stack.block_track_region_of . Score.event_stack
    where
    with_stack (block_id, track_id, (s, e)) msg =
        msg { Log.msg_stack = add_stack msg }
        where
        add_stack = Just . add . fromMaybe Stack.empty . Log.msg_stack
        add = Stack.add (Stack.Region s e) . Stack.add (Stack.Track track_id)
            . Stack.add (Stack.Block block_id)

-- | Monadic map with state.  The event type is polymorphic, so you can use
-- 'LEvent.zip' and co. to zip up unthreaded state, constructed with 'control'
-- and 'nexts' and such.
emap_m :: (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [b]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> [LEvent.LEvent a] -> Derive.Deriver (state, [[LEvent.LEvent b]])
    -- ^ events are return as unmerged chunks, so the caller can merge
emap_m event_of f state = go state
    where
    go state [] = return (state, [])
    go state (LEvent.Log log : events) =
        fmap ([LEvent.Log log] :) <$> go state events
    go state (LEvent.Event event : events) = do
        (state, output) <- fromMaybe (state, []) <$>
            Derive.with_event (event_of event) (f state event)
        (final, outputs) <- go state events
        return (final, map LEvent.Event output : outputs)
    -- TODO this could also take [(a, LEvent Score.Event)] and omit 'event_of'
    -- since it's always 'snd', but this is basically the same as the separate
    -- annots approach I had earlier, and forces you to have a () annotation
    -- if you don't want one.

-- | 'emap_m' without the state.
emap_m_ :: (a -> Score.Event)
    -> (a -> Derive.Deriver [b]) -> [LEvent.LEvent a]
    -> Derive.Deriver [[LEvent.LEvent b]]
emap_m_ event_of f = fmap snd . emap_m event_of (\() e -> (,) () <$> f e) ()

-- | Like 'emap_m', but assume the function returns sorted chunks in
-- increasing order, so they can be merged efficiently.
emap_asc_m ::
    (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [Score.Event]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> [LEvent.LEvent a]
    -> Derive.Deriver (state, Derive.Events)
emap_asc_m event_of f state =
    fmap (second Derive.merge_asc_events) . emap_m event_of f state

-- ** unthreaded state

control :: (Score.TypedVal -> a) -> TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [a]
control f c events = do
    sig <- Util.to_typed_function c
    return $ Prelude.map (f . sig . Score.event_start) (LEvent.events_of events)

time_control :: TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [RealTime]
time_control = control (RealTime.seconds . Score.typed_val)

-- | Zip each event up with its neighbors.
neighbors :: [LEvent.LEvent a] -> [LEvent.LEvent ([a], a, [a])]
neighbors events = emap1 (\(ps, ns, e) -> (ps, e, ns)) $
    LEvent.zip3 (prevs events) (nexts events) events

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
map_first :: (a -> Derive.Deriver a) -> [LEvent.LEvent a]
    -> Derive.LogsDeriver a
map_first f events = event_head events $ \e es -> do
    e <- f e
    return $ LEvent.Event e : es

event_head :: [LEvent.LEvent d]
    -> (d -> [LEvent.LEvent d] -> Derive.Deriver [LEvent.LEvent d])
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
    return $ LEvent.Event (f sig) : map LEvent.Log logs

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

    TODO this stuff is now unused, but maybe I'll find a use for it again some
    day.
-}
make_delayed :: Derive.PassedArgs a -> RealTime -> [TrackLang.Val]
    -> Derive.NoteDeriver
make_delayed args start event_args = do
    dyn <- Internal.get_dynamic id
    control_vals <- Derive.controls_at start
    let event = delayed_event event_args $
            Note.make_event args dyn control_vals start 0 mempty
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

-- | Typecheck a single Val.
typecheck :: forall a. TrackLang.Typecheck a => TrackLang.Val -> Either Text a
typecheck val = case TrackLang.from_val val of
    Nothing -> Left $ "expected "
        <> prettyt (TrackLang.to_type (Proxy :: Proxy a)) <> " but got "
        <> prettyt (TrackLang.type_of val)
    Just a -> Right a

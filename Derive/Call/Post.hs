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

    There are variants for each axis:

    - monadic vs. pure

    - state vs. stateless
-}
module Derive.Call.Post where
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Monoid as Monoid

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.Prelude.Note as Note
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import Derive.Stream (Stream)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import Global
import Types


-- * map events

-- 'emap' is kind of an ugly name, but at least it's consistent and short.
-- I previously used 'map', but it turns out replacing the Prelude map is
-- really confusing.

-- ** non-monadic

-- | 1:1 non-monadic map without state.
--
-- TODO this is expected to not destroy the order, but that isn't checked.
-- That means either the event doesn't move, or it doesn't move past its
-- neighbors.
emap1_ :: (a -> b) -> Stream a -> Stream b
emap1_ = fmap

-- | Map on Score.Events.  The function is allowed to move the events, since it
-- sorts them afterwards.
emap1_ord_ :: (a -> Score.Event) -> Stream a -> Stream Score.Event
emap1_ord_ f = Stream.sort . fmap f

-- | 1:1 non-monadic map with state.
emap1 :: (state -> a -> (state, b)) -> state -> Stream a -> (state, Stream b)
emap1 f state =
    second Stream.from_sorted_list . List.mapAccumL go state . Stream.to_list
    where
    go state (LEvent.Log log) = (state, LEvent.Log log)
    go state (LEvent.Event event) = LEvent.Event <$> f state event

-- | 1:n non-monadic map with state.
emap :: (state -> a -> (state, [Score.Event])) -> state
    -> Stream a -> (state, Stream Score.Event)
emap f state = second Stream.merge_asc_lists . emap_groups f state
    where
    emap_groups :: (state -> a -> (state, [b])) -> state
        -> Stream a -> (state, [Stream b])
    emap_groups f state = List.mapAccumL go state . Stream.to_list
        where
        go state (LEvent.Log log) = (state, Stream.from_logs [log])
        go state (LEvent.Event event) =
            Stream.from_sorted_events <$> f state event

emap_asc :: (state -> a -> (state, [Score.Event])) -> state
    -> Stream a -> (state, Stream Score.Event)
emap_asc = emap

-- | 'emap' without state.
emap_ :: (a -> [Score.Event]) -> Stream a -> Stream Score.Event
emap_ f = Stream.merge_asc_lists . map flatten . Stream.to_list . fmap f
    where
    flatten (LEvent.Log log) = Stream.from_logs [log]
    flatten (LEvent.Event events) = Stream.from_sorted_events events

emap_asc_ :: (a -> [Score.Event]) -> Stream a -> Stream Score.Event
emap_asc_ = emap_

-- ** monadic

-- | Apply a function to the non-log events.
-- TODO assumes the function doesn't destroy the order.
apply :: Functor f => ([a] -> f [b]) -> Stream.Stream a -> f (Stream.Stream b)
apply f stream = Stream.merge_logs logs . Stream.from_sorted_events <$> f events
    where (events, logs) = Stream.partition stream

-- | Monadic map with state.  The event type is polymorphic, so you can use
-- 'LEvent.zip' and co. to zip up unthreaded state, constructed with 'control'
-- and 'nexts' and such.
emap_m :: (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [b]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> Stream a -> Derive.Deriver (state, Stream b)
emap_m event_of f state =
    fmap (second (Stream.from_sorted_list . DList.toList)) . go state
        . Stream.to_list
    where
    go state [] = return (state, mempty)
    go state (LEvent.Log log : events) =
        fmap (DList.cons (LEvent.Log log)) <$> go state events
    go state (LEvent.Event event : events) = do
        (state, output) <- fromMaybe (state, []) <$>
            Derive.with_event (event_of event) (f state event)
        (final, outputs) <- go state events
        return (final, DList.fromList (map LEvent.Event output) <> outputs)
    -- TODO this could also take [(a, LEvent Score.Event)] and omit 'event_of'
    -- since it's always 'snd', but this is basically the same as the separate
    -- annots approach I had earlier, and forces you to have a () annotation
    -- if you don't want one.

emap_asc_m :: (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [Score.Event]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> Stream a -> Derive.Deriver (state, Stream Score.Event)
emap_asc_m event_of f state =
    fmap (second merge) . Seq.mapAccumLM go state . Stream.to_list
    where
    merge = Stream.merge_asc_lists . map Stream.from_sorted_list
    go state (LEvent.Event event) =
        maybe (state, []) (second (map LEvent.Event)) <$>
            Derive.with_event (event_of event) (f state event)
    go state (LEvent.Log log) = return (state, [LEvent.Log log])

-- | 'emap_m' without the state.
emap_m_ :: (a -> Score.Event) -> (a -> Derive.Deriver [b]) -> Stream a
    -> Derive.Deriver (Stream b)
emap_m_ event_of f = fmap snd . emap_m event_of (\() e -> (,) () <$> f e) ()

emap_asc_m_ :: (a -> Score.Event) -> (a -> Derive.Deriver [Score.Event])
    -> Stream a -> Derive.Deriver (Stream Score.Event)
emap_asc_m_ event_of f =
    fmap snd <$> emap_asc_m event_of (\() e -> (,) () <$> f e) ()

-- ** unthreaded state

control :: (Score.TypedVal -> a) -> BaseTypes.ControlRef
    -> Stream Score.Event -> Derive.Deriver [a]
control f c events = do
    sig <- Typecheck.to_typed_function c
    return $ map (f . sig . Score.event_start) (Stream.events_of events)

time_control :: BaseTypes.ControlRef -> Stream Score.Event
    -> Derive.Deriver [RealTime]
time_control = control (RealTime.seconds . Score.typed_val)

-- | Zip each event up with its neighbors.
neighbors :: Stream a -> Stream ([a], a, [a])
neighbors events = emap1_ (\(ps, ns, e) -> (ps, e, ns)) $
    Stream.zip3_on prevs nexts events

-- | Zip each event with its nearest same-instrument same-hand neighbor.
neighbors_same_hand :: (a -> Score.Event) -> Stream a
    -> Stream (Maybe a, a, Maybe a)
neighbors_same_hand event_of = emap1_ extract . neighbors
    where
    extract (ps, e, ns) = (same ps, e, same ns)
        where same = Seq.head . same_hand (event_of e) event_of

-- | Like 'neighbors_same_hand', but only the next neighbor.
nexts_same_hand :: (a -> Score.Event) -> Stream a -> Stream (a, Maybe a)
nexts_same_hand event_of = fmap extract . neighbors_same_hand event_of
    where extract (_, e, n) = (e, n)

prevs_same_hand :: (a -> Score.Event) -> Stream a -> Stream (Maybe a, a)
prevs_same_hand event_of = fmap extract . neighbors_same_hand event_of
    where extract (p, e, _) = (p, e)

-- | Extract subsequent events.
nexts :: [a] -> [[a]]
nexts = drop 1 . List.tails

-- | Extract previous events.
prevs :: [a] -> [[a]]
prevs = scanl (flip (:)) []

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
same_hand :: Score.Event -> (a -> Score.Event) -> [a] -> [a]
same_hand event event_of =
    filter ((== inst_of event) . inst_of . event_of) .  case hand_of event of
        Nothing -> id
        Just hand -> filter $ (== Just hand) . hand_of . event_of
    where
    inst_of = Score.event_instrument
    hand_of :: Score.Event -> Maybe Text
    hand_of = Env.maybe_val EnvKey.hand . Score.event_environ

hand_key :: Score.Event -> (Score.Instrument, Maybe Text)
hand_key e = (Score.event_instrument e,
    Env.maybe_val EnvKey.hand $ Score.event_environ e)

-- ** misc maps

-- | Apply a function on the first Event of an LEvent stream.
-- TODO this shouldn't destroy the order, but it isn't checkded.
map_first :: (a -> Derive.Deriver a) -> Stream a
    -> Derive.Deriver (Stream.Stream a)
map_first f events = event_head events $ \e es -> do
    e <- f e
    return $ Stream.from_sorted_list $ LEvent.Event e : Stream.to_list es

-- | Transform the first event and the rest of the events.
-- TODO weird function with crummy name.
event_head :: Stream a
    -> (a -> Stream.Stream a -> Derive.Deriver (Stream.Stream a))
    -> Derive.Deriver (Stream.Stream a)
event_head stream f = go (Stream.to_list stream)
    where
    go [] = return Stream.empty
    go (LEvent.Log log : rest) = Stream.merge_log log <$> go rest
    go (LEvent.Event event : rest) = f event (Stream.from_sorted_list rest)

-- * signal

control_range :: Derive.ControlDeriver
    -> Derive.Deriver (Signal.Control, (RealTime, RealTime), [Log.Msg])
control_range deriver = do
    (sig, logs) <- first mconcat . Stream.partition <$> deriver
    let range = case (Signal.head sig, Signal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    return (sig, range, logs)

pitch_range :: Derive.PitchDeriver
    -> Derive.Deriver (PSignal.PSignal, (RealTime, RealTime), [Log.Msg])
pitch_range deriver = do
    (sig, logs) <- first mconcat . Stream.partition <$> deriver
    let range = case (PSignal.head sig, PSignal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    return (sig, range, logs)

-- | Transform a pitch or control signal.
signal :: Monoid.Monoid sig => (sig -> sig)
    -> Derive.Deriver (Stream.Stream sig) -> Derive.Deriver (Stream.Stream sig)
signal f deriver = do
    (sig, logs) <- derive_signal deriver
    return $ Stream.from_sorted_list $
        LEvent.Event (f sig) : map LEvent.Log logs

derive_signal :: Monoid.Monoid sig => Derive.Deriver (Stream.Stream sig)
    -> Derive.Deriver (sig, [Log.Msg])
derive_signal deriver = do
    (chunks, logs) <- Stream.partition <$> deriver
    return (mconcat chunks, logs)

-- * delayed events

{- | Make a delayed event.

    A delayed event should be realized by an accompanying postproc call. It has
    an 'EnvKey.args', which are the arguments to the postproc call, and so
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
make_delayed :: Derive.PassedArgs a -> RealTime -> [BaseTypes.Val]
    -> Derive.NoteDeriver
make_delayed args start event_args = do
    dyn <- Internal.get_dynamic id
    let event = delayed_event event_args $
            Note.make_event args dyn start 0 mempty
    return $! Stream.from_event event

delayed_event :: [BaseTypes.Val] -> Score.Event -> Score.Event
delayed_event args = Score.modify_environ $
    Env.insert_val EnvKey.args (BaseTypes.VList args)

-- | Return the args if this is a delayed event created by the given call.
delayed_args :: BaseTypes.CallId -> Score.Event -> Maybe [BaseTypes.Val]
delayed_args (BaseTypes.Symbol call) event
    | Seq.head (Stack.innermost (Score.event_stack event))
            == Just (Stack.Call call) =
        Env.maybe_val EnvKey.args (Score.event_environ event)
    | otherwise = Nothing

-- * modify events

-- | Like 'add_environ', but check the type.
put_environ :: (Typecheck.Typecheck a, Typecheck.ToVal a) => Env.Key -> a
    -> Score.Event -> Either Text Score.Event
put_environ name val event =
    case Env.put_val_error name val (Score.event_environ event) of
        Left err -> Left err
        Right env -> Right $ event { Score.event_environ = env }

add_environ :: (Typecheck.Typecheck a, Typecheck.ToVal a) => Env.Key -> a
    -> Score.Event -> Score.Event
add_environ name val = Score.modify_environ $ Env.insert_val name val

-- | Set the instrument on an event, and also update its pitches based on
-- the instrument's environ.
set_instrument :: (Score.Instrument, Derive.Instrument)
    -- ^ unaliased instrument name, this should come from Derive.get_instrument
    -> Score.Event -> Score.Event
set_instrument (score_inst, inst) event = event
    { Score.event_instrument = score_inst
    , Score.event_untransformed_pitch = PSignal.apply_environ env $
        Score.event_untransformed_pitch event
    , Score.event_untransformed_pitches = PSignal.apply_environ env <$>
        Score.event_untransformed_pitches event
    }
    where env = Derive.inst_environ inst

-- * misc

-- | Like 'Derive.with_event_stack', but directly add the event's innermost
-- stack to a log msg.
-- TODO unused
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

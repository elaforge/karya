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
import Global
import Types


-- * map events

-- 'emap' is kind of an ugly name, but at least it's consistent and short.
-- I previously used 'map', but it turns out replacing the Prelude map is
-- really confusing.

-- ** non-monadic

-- | 1:1 non-monadic map without state.
emap1_ :: (a -> b) -> [LEvent.LEvent a] -> [LEvent.LEvent b]
emap1_ = map . fmap

-- | 1:1 non-monadic map with state.
emap1 :: (state -> a -> (state, b)) -> state -> [LEvent.LEvent a]
    -> (state, [LEvent.LEvent b])
emap1 f state = List.mapAccumL go state
    where
    go state (LEvent.Log log) = (state, LEvent.Log log)
    go state (LEvent.Event event) = LEvent.Event <$> f state event

-- | 'Data.Maybe.catMaybes' for LEvents.
cat_maybes :: [LEvent.LEvent (Maybe a)] -> [LEvent.LEvent a]
cat_maybes [] = []
cat_maybes (x : xs) = case x of
    LEvent.Log log -> LEvent.Log log : cat_maybes xs
    LEvent.Event (Just e) -> LEvent.Event e : cat_maybes xs
    LEvent.Event Nothing -> cat_maybes xs

-- | 1:n non-monadic map with state.
emap :: (state -> a -> (state, [b])) -> state
    -> [LEvent.LEvent a] -> (state, [LEvent.LEvent b])
emap f state = second concat . List.mapAccumL go state
    where
    go state (LEvent.Log log) = (state, [LEvent.Log log])
    go state (LEvent.Event event) = map LEvent.Event <$> f state event

-- | 'emap' without state.
emap_ :: (a -> [b]) -> [LEvent.LEvent a] -> [LEvent.LEvent b]
emap_ f = concatMap flatten . emap1_ f
    where
    flatten (LEvent.Log log) = [LEvent.Log log]
    flatten (LEvent.Event events) = map LEvent.Event events

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
    -> state -> [LEvent.LEvent a] -> Derive.Deriver (state, [LEvent.LEvent b])
emap_m event_of f state = fmap (second DList.toList) . go state
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

-- | 'emap_m' without the state.
emap_m_ :: (a -> Score.Event) -> (a -> Derive.Deriver [b]) -> [LEvent.LEvent a]
    -> Derive.Deriver [LEvent.LEvent b]
emap_m_ event_of f = fmap snd . emap_m event_of (\() e -> (,) () <$> f e) ()

-- ** unthreaded state

zip_on :: ([a] -> [b]) -> [LEvent.LEvent a] -> [LEvent.LEvent (b, a)]
zip_on f xs = LEvent.zip (f (LEvent.events_of xs)) xs

zip3_on :: ([a] -> [b]) -> ([a] -> [c]) -> [LEvent.LEvent a]
    -> [LEvent.LEvent (b, c, a)]
zip3_on f g xs =
    LEvent.zip3 (f (LEvent.events_of xs)) (g (LEvent.events_of xs)) xs

control :: (Score.TypedVal -> a) -> TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [a]
control f c events = do
    sig <- Util.to_typed_function c
    return $ map (f . sig . Score.event_start) (LEvent.events_of events)

time_control :: TrackLang.ValControl -> Derive.Events
    -> Derive.Deriver [RealTime]
time_control = control (RealTime.seconds . Score.typed_val)

-- | Zip each event up with its neighbors.
neighbors :: [LEvent.LEvent a] -> [LEvent.LEvent ([a], a, [a])]
neighbors events = emap1_ (\(ps, ns, e) -> (ps, e, ns)) $
    zip3_on prevs nexts events

-- | Zip each event with its nearest same-hand neighbor.
neighbors_same_hand :: (a -> Score.Event) -> [LEvent.LEvent a]
    -> [LEvent.LEvent (Maybe a, a, Maybe a)]
neighbors_same_hand event_of = emap1_ extract . neighbors
    where
    extract (ps, e, ns) = (same ps, e, same ns)
        where same = Seq.head . same_hand (event_of e) event_of

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
    hand_of = TrackLang.maybe_val Environ.hand . Score.event_environ

hand_key :: Score.Event -> (Score.Instrument, Maybe Text)
hand_key e = (Score.event_instrument e,
    TrackLang.maybe_val Environ.hand $ Score.event_environ e)

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
signal :: Monoid.Monoid sig => (sig -> sig)
    -> Derive.LogsDeriver sig -> Derive.LogsDeriver sig
signal f deriver = do
    (sig, logs) <- derive_signal deriver
    return $ LEvent.Event (f sig) : map LEvent.Log logs

derive_signal :: Monoid.Monoid sig => Derive.LogsDeriver sig
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
    let event = delayed_event event_args $
            Note.make_event args dyn start 0 mempty
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

-- * modify events

-- | Like 'add_environ', but check the type.
put_environ :: TrackLang.Typecheck a => TrackLang.ValName -> a
    -> Score.Event -> Either Text Score.Event
put_environ name val event =
    case TrackLang.put_val_error name val (Score.event_environ event) of
        Left err -> Left err
        Right env -> Right $ event { Score.event_environ = env }

add_environ :: TrackLang.Typecheck a => TrackLang.ValName -> a
    -> Score.Event -> Score.Event
add_environ name val = Score.modify_environ $ TrackLang.insert_val name val

-- | Set the instrument on an event, and also update its pitches based on
-- the instrument's environ.
set_instrument :: (Score.Instrument, Derive.Instrument)
    -- ^ unaliased instrument name, this should come from Derive.get_instrument
    -> Score.Event -> Score.Event
set_instrument (score_inst, inst) event = event
    { Score.event_instrument = score_inst
    , Score.event_untransformed_pitch = PitchSignal.apply_environ env $
        Score.event_untransformed_pitch event
    , Score.event_untransformed_pitches = PitchSignal.apply_environ env <$>
        Score.event_untransformed_pitches event
    }
    where env = Derive.inst_environ inst

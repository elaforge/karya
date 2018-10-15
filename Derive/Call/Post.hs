-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Post-processing utils.  These are transformers that directly modify the
    output of a deriver, as opposed to simply modifying the 'Derive.Dynamic'.

    Unfortunately things are complicated by the presence of 'LEvent.Log's in
    the output stream.  I haven't been able to figure out how to cleanly
    abstract that away, so I wind up with a collection of functions to handle
    specific kinds of maps.

    There are variants for each axis:

    - monadic vs. pure

    - state vs. stateless

    - 1:1 vs. 1:many

    - preserves order vs. doesn't preserve order

    TODO

    One big problem with this is the permutations.  Another is that I should be
    able to fuse composed maps, but I think it'll mostly be defeated by the
    monadic bits, and maybe state.  But even monadic bits should be
    theoretically fusible since I don't mind if the effects (i.e. exceptions)
    are interleaved.  A job for pipes maybe?
-}
module Derive.Call.Post where
import qualified Data.DList as DList
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Util.Log as Log
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.NoteUtil as NoteUtil
import qualified Derive.Derive as Derive
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
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
-- neighbors.  Way back when events didn't have their start times, I could
-- express this by only mapping over the event, but I'm not sure how to do it
-- now.  And in any case, "don't destroy order" is more permissive than "don't
-- move."
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

-- | This is 'emap', but it promises to emit events in sorted order.
-- TODO except that's not enforced, and maybe I should just always sort.
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
apply :: ([a] -> [b]) -> Stream.Stream a -> Stream.Stream b
apply f stream = Stream.merge_logs logs $ Stream.from_sorted_events (f events)
    where (events, logs) = Stream.partition stream

apply_m :: Functor f => ([a] -> f [b]) -> Stream.Stream a -> f (Stream.Stream b)
apply_m f stream =
    Stream.merge_logs logs . Stream.from_sorted_events <$> f events
    where (events, logs) = Stream.partition stream

-- | 1:1 monadic map without state.
emap1m_ :: (a -> Score.Event) -> (a -> Derive.Deriver b) -> Stream a
    -> Derive.Deriver (Stream b)
emap1m_ event_of f =
    fmap Stream.from_sorted_list . mapMaybeM process . Stream.to_list
    where
    process (LEvent.Log log) = return $ Just $ LEvent.Log log
    process (LEvent.Event a) =
        Derive.with_event (event_of a) $ LEvent.Event <$> f a

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

-- * only

-- | Only process the events that match, otherwise pass unchanged.
only :: (a -> event) -> (event -> Bool) -> (a -> event) -> a -> event
only event_of match f a = if match (event_of a) then f a else event_of a

has_instrument :: [Score.Instrument] -> Score.Event -> Bool
has_instrument wanted = (`Set.member` set) . Score.event_instrument
    where set = Set.fromList wanted

-- ** unthreaded state

control :: (Score.Typed Signal.Y -> a) -> BaseTypes.ControlRef
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

-- | Zip each event with its nearest neighbor with the same key.  A key might
-- be 'Score.event_instrument', 'hand_key', or 'voice_key'.
--
-- TODO it's awkward how calls that are not instrument-specific still have to
-- choose between hand or voice when they want the next \"relevant\" note.
-- Perhaps hand and voice should be merged into a single concept.  They have to
-- be distinct for the lilypond backend though.
neighbors_by :: Eq key => (a -> key) -> Stream a -> Stream (Maybe a, a, Maybe a)
neighbors_by key = emap1_ extract . neighbors
    where
    extract (ps, e, ns) = (same ps, e, same ns)
        where same = Seq.head . filter ((== key e) . key)

nexts_by :: Eq key => (a -> key) -> Stream a -> Stream (a, [a])
nexts_by key = emap1_ extract . Stream.zip_on nexts
    where
    extract (ns, e) = (e, same ns)
        where same = filter ((== key e) . key)

-- | Like 'neighbors_by', but only the next neighbor.
next_by :: Eq key => (a -> key) -> Stream a -> Stream (a, Maybe a)
next_by key = emap1_ extract . neighbors_by key
    where extract (_, e, n) = (e, n)

prev_by :: Eq key => (a -> key) -> Stream a -> Stream (Maybe a, a)
prev_by key = emap1_ extract . neighbors_by key
    where extract (p, e, _) = (p, e)

hand_key :: Score.Event -> (Score.Instrument, Maybe Text)
hand_key e =
    ( Score.event_instrument e
    , Env.maybe_val EnvKey.hand $ Score.event_environ e
    )

voice_key :: Score.Event -> (Score.Instrument, Int)
voice_key e =
    ( Score.event_instrument e
    , fromMaybe 0 $ Env.maybe_val EnvKey.voice $ Score.event_environ e
    )

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

-- ** misc maps

-- | Apply a function on the first Event of an LEvent stream.
-- TODO this shouldn't destroy the order, but it isn't checkded.
map_first :: (a -> Derive.Deriver a) -> Stream a
    -> Derive.Deriver (Stream.Stream a)
map_first f = map_head_tail $ \e es -> do
    e <- f e
    return $ Stream.from_sorted_list $ LEvent.Event e : Stream.to_list es

-- | Transform the first event and the rest of the events.
map_head_tail :: (a -> Stream.Stream a -> Derive.Deriver (Stream.Stream a))
    -> Stream a
    -> Derive.Deriver (Stream.Stream a)
map_head_tail f = go . Stream.to_list
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
signal :: Monoid sig => (sig -> sig)
    -> Derive.Deriver (Stream.Stream sig) -> Derive.Deriver (Stream.Stream sig)
signal f deriver = do
    (sig, logs) <- derive_signal deriver
    return $ Stream.from_sorted_list $
        LEvent.Event (f sig) : map LEvent.Log logs

derive_signal :: Monoid sig => Derive.Deriver (Stream.Stream sig)
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
    Stream.from_event . delayed_event event_args <$>
        NoteUtil.make_event args dyn start 0 mempty

delayed_event :: [BaseTypes.Val] -> Score.Event -> Score.Event
delayed_event args = Score.modify_environ $
    Env.insert_val EnvKey.args (BaseTypes.VList args)

-- | Return the args if this is a delayed event created by the given call.
delayed_args :: Expr.Symbol -> Score.Event -> Maybe [BaseTypes.Val]
delayed_args (Expr.Symbol call) event
    | Seq.head (Stack.innermost (Score.event_stack event))
            == Just (Stack.Call call) =
        Env.maybe_val EnvKey.args (Score.event_environ event)
    | otherwise = Nothing

-- * modify events

-- | Like 'add_environ', but check the type.
put_environ :: Typecheck.ToVal a => Env.Key -> a -> Score.Event
    -> Either Text Score.Event
put_environ name val event =
    case Env.put_val_error name val (Score.event_environ event) of
        Left err -> Left err
        Right env -> Right $ event { Score.event_environ = env }

add_environ :: Typecheck.ToVal a => Env.Key -> a -> Score.Event -> Score.Event
add_environ name val = Score.modify_environ $ Env.insert_val name val

set_instrument :: (Score.Instrument, Derive.Instrument)
    -- ^ unaliased instrument name, from 'Derive.get_instrument'
    -> Score.Event -> Score.Event
set_instrument (score_inst, inst) =
    Score.set_instrument score_inst (Derive.inst_environ inst)

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

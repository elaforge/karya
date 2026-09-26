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

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Derive.Call as Call
import qualified Derive.Call.NoteUtil as NoteUtil
import qualified Derive.Derive as Derive
import qualified Derive.DeriveT as DeriveT
import qualified Derive.Deriver.Internal as Internal
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.Expr as Expr
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.Stack as Stack
import qualified Derive.Stream as Stream
import           Derive.Stream (Stream)
import qualified Derive.Typecheck as Typecheck

import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Ui.ScoreTime as ScoreTime

import           Global
import           Types


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
emap1Ord_ :: (a -> Score.Event) -> Stream a -> Stream Score.Event
emap1Ord_ f = Stream.sort . fmap f

-- | 1:1 non-monadic map with state.  This is like mapAccumL.
emap1 :: (state -> a -> (state, b)) -> state -> Stream a -> (state, Stream b)
emap1 f state =
    second Stream.fromSortedList . List.mapAccumL go state . Stream.toList
    where
    go state (LEvent.Log log) = (state, LEvent.Log log)
    go state (LEvent.Event event) = LEvent.Event <$> f state event

-- | 1:n non-monadic map with state.
emap :: (state -> a -> (state, [Score.Event])) -> state
    -> Stream a -> (state, Stream Score.Event)
emap f state = second Stream.mergeAscLists . emapGroups f state
    where
    emapGroups :: (state -> a -> (state, [b])) -> state
        -> Stream a -> (state, [Stream b])
    emapGroups f state = List.mapAccumL go state . Stream.toList
        where
        go state (LEvent.Log log) = (state, Stream.fromLogs [log])
        go state (LEvent.Event event) =
            Stream.fromSortedEvents <$> f state event

-- | This is 'emap', but it promises to emit events in sorted order.
-- TODO except that's not enforced, and maybe I should just always sort.
emapAsc :: (state -> a -> (state, [Score.Event])) -> state
    -> Stream a -> (state, Stream Score.Event)
emapAsc = emap

-- | 'emap' without state.
emap_ :: (a -> [Score.Event]) -> Stream a -> Stream Score.Event
emap_ f = Stream.mergeAscLists . map flatten . Stream.toList . fmap f
    where
    flatten (LEvent.Log log) = Stream.fromLogs [log]
    flatten (LEvent.Event events) = Stream.fromSortedEvents events

emapAsc_ :: (a -> [Score.Event]) -> Stream a -> Stream Score.Event
emapAsc_ = emap_

-- ** monadic

-- | Apply a function to the non-log events.
-- TODO assumes the function doesn't destroy the order.
apply :: ([a] -> [b]) -> Stream.Stream a -> Stream.Stream b
apply f stream = Stream.mergeLogs logs $ Stream.fromSortedEvents (f events)
    where (events, logs) = Stream.partition stream

applyM :: Functor f => ([a] -> f [b]) -> Stream.Stream a -> f (Stream.Stream b)
applyM f stream =
    Stream.mergeLogs logs . Stream.fromSortedEvents <$> f events
    where (events, logs) = Stream.partition stream

-- | 1:1 monadic map without state.
emap1m_ :: (a -> Score.Event) -> (a -> Derive.Deriver b) -> Stream a
    -> Derive.Deriver (Stream b)
emap1m_ eventOf f =
    fmap Stream.fromSortedList . mapMaybeM process . Stream.toList
    where
    process (LEvent.Log log) = pure $ Just $ LEvent.Log log
    process (LEvent.Event a) =
        Derive.withEvent (eventOf a) $ LEvent.Event <$> f a

-- | Monadic map with state.  The event type is polymorphic, so you can use
-- 'LEvent.zip' and co. to zip up unthreaded state, constructed with 'control'
-- and 'nexts' and such.
emapM :: (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [b]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> Stream a -> Derive.Deriver (state, Stream b)
emapM eventOf f state =
    fmap (second (Stream.fromSortedList . DList.toList)) . go state
        . Stream.toList
    where
    go state [] = pure (state, mempty)
    go state (LEvent.Log log : events) =
        fmap (DList.cons (LEvent.Log log)) <$> go state events
    go state (LEvent.Event event : events) = do
        (state, output) <- fromMaybe (state, []) <$>
            Derive.withEvent (eventOf event) (f state event)
        (final, outputs) <- go state events
        pure (final, DList.fromList (map LEvent.Event output) <> outputs)
    -- TODO this could also take [(a, LEvent Score.Event)] and omit 'event_of'
    -- since it's always 'snd', but this is basically the same as the separate
    -- annots approach I had earlier, and forces you to have a () annotation
    -- if you don't want one.

emapAscM :: (a -> Score.Event)
    -> (state -> a -> Derive.Deriver (state, [Score.Event]))
    -- ^ Process an event. Exceptions are caught and logged.
    -> state -> Stream a -> Derive.Deriver (state, Stream Score.Event)
emapAscM eventOf f state =
    fmap (second merge) . Lists.mapAccumLM go state . Stream.toList
    where
    merge = Stream.mergeAscLists . map Stream.fromSortedList
    go state (LEvent.Event event) =
        maybe (state, []) (second (map LEvent.Event)) <$>
            Derive.withEvent (eventOf event) (f state event)
    go state (LEvent.Log log) = pure (state, [LEvent.Log log])

-- | 'emap_m' without the state.
emapM_ :: (a -> Score.Event) -> (a -> Derive.Deriver [b]) -> Stream a
    -> Derive.Deriver (Stream b)
emapM_ eventOf f = fmap snd . emapM eventOf (\() e -> (,) () <$> f e) ()

emapAscM_ :: (a -> Score.Event) -> (a -> Derive.Deriver [Score.Event])
    -> Stream a -> Derive.Deriver (Stream Score.Event)
emapAscM_ eventOf f =
    fmap snd <$> emapAscM eventOf (\() e -> (,) () <$> f e) ()

-- | Postprocess each event with a NoteDeriver.  This is necessary if you need
-- to generate more notes, e.g. with 'Call.note'.
emapS_ :: (a -> Score.Event) -> (a -> Derive.NoteDeriver) -> Stream a
    -> Derive.NoteDeriver
emapS_ eventOf f = concatMapM go . Stream.toList
    where
    go (LEvent.Log log) = pure $ Stream.fromLogs [log]
    go (LEvent.Event a) = fromMaybe mempty <$>
        Derive.withEvent (eventOf a) (f a)

mergeAsc :: [[Score.Event]] -> [Score.Event]
mergeAsc = Lists.mergeAscLists Score.eventStart

-- * only

-- | Only process the events that match, otherwise pass unchanged.
only :: (a -> event) -> (event -> Bool) -> (a -> event) -> a -> event
only eventOf match f a = if match (eventOf a) then f a else eventOf a

hasInstrument :: [ScoreT.Instrument] -> Score.Event -> Bool
hasInstrument wanted = (`Set.member` set) . Score.eventInstrument
    where set = Set.fromList wanted

-- ** unthreaded state

control :: ScoreT.Function -> Stream Score.Event -> [Signal.Y]
control f = map (f . Score.eventStart) . Stream.eventsOf

realTimeControl :: ScoreT.Function -> Stream Score.Event -> [RealTime]
realTimeControl f = map RealTime.seconds . control f

-- | Take a typed signal to RealTime durations.
durationControl :: ScoreT.TimeT -> ScoreT.Function
    -> Stream Score.Event -> Derive.Deriver [RealTime]
durationControl timeT f events = do
    let starts = map Score.eventStart (Stream.eventsOf events)
    case timeT of
        ScoreT.TReal -> pure $ map (RealTime.seconds . f) starts
        ScoreT.TScore ->
            mapM (\t -> Call.realDuration t (ScoreTime.fromDouble (f t)))
                starts

-- | Zip each event up with its neighbors.
neighbors :: Stream a -> Stream ([a], a, [a])
neighbors events = emap1_ (\(ps, ns, e) -> (ps, e, ns)) $
    Stream.zip3On prevs nexts events

-- | Zip each event with its nearest neighbor with the same key.  A key might
-- be 'Score.event_instrument', 'hand_key', or 'voice_key'.
--
-- TODO it's awkward how calls that are not instrument-specific still have to
-- choose between hand or voice when they want the next \"relevant\" note.
-- Perhaps hand and voice should be merged into a single concept.  They have to
-- be distinct for the lilypond backend though.
neighborsBy :: Eq key => (a -> key) -> Stream a -> Stream (Maybe a, a, Maybe a)
neighborsBy key = emap1_ extract . neighbors
    where
    extract (ps, e, ns) = (same ps, e, same ns)
        where same = Lists.head . filter ((== key e) . key)

nextsBy :: Eq key => (a -> key) -> Stream a -> Stream (a, [a])
nextsBy key = emap1_ extract . Stream.zipOn nexts
    where
    extract (ns, e) = (e, same ns)
        where same = filter ((== key e) . key)

-- | Like 'neighbors_by', but only the next neighbor.
nextBy :: Eq key => (a -> key) -> Stream a -> Stream (a, Maybe a)
nextBy key = emap1_ extract . neighborsBy key
    where extract (_, e, n) = (e, n)

prevBy :: Eq key => (a -> key) -> Stream a -> Stream (Maybe a, a)
prevBy key = emap1_ extract . neighborsBy key
    where extract (p, e, _) = (p, e)

handKey :: Score.Event -> (ScoreT.Instrument, Maybe Text)
handKey e =
    ( Score.eventInstrument e
    , Env.maybeVal EnvKey.hand $ Score.eventEnviron e
    )

voiceKey :: Score.Event -> (ScoreT.Instrument, Int)
voiceKey e =
    ( Score.eventInstrument e
    , fromMaybe 0 $ Env.maybeVal EnvKey.voice $ Score.eventEnviron e
    )

-- | Extract subsequent events.
nexts :: [a] -> [[a]]
nexts = drop 1 . List.tails

-- | Extract previous events.
prevs :: [a] -> [[a]]
prevs = scanl (flip (:)) []

-- ** misc maps

-- | Apply a function on the first Event of an LEvent stream.
-- TODO this shouldn't destroy the order, but it isn't checkded.
mapFirst :: (a -> Derive.Deriver a) -> Stream a
    -> Derive.Deriver (Stream.Stream a)
mapFirst f = mapHeadTail $ \e es -> do
    e <- f e
    pure $ Stream.fromSortedList $ LEvent.Event e : Stream.toList es

-- | Transform the first event and the rest of the events.
mapHeadTail :: (a -> Stream.Stream a -> Derive.Deriver (Stream.Stream a))
    -> Stream a
    -> Derive.Deriver (Stream.Stream a)
mapHeadTail f = go . Stream.toList
    where
    go [] = pure Stream.empty
    go (LEvent.Log log : rest) = Stream.mergeLog log <$> go rest
    go (LEvent.Event event : rest) = f event (Stream.fromSortedList rest)

-- * signal

controlRange :: Derive.ControlDeriver
    -> Derive.Deriver (Signal.Control, (RealTime, RealTime), [Log.Msg])
controlRange deriver = do
    (sig, logs) <- first mconcat . Stream.partition <$> deriver
    let range = case (Signal.head sig, Signal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    pure (sig, range, logs)

pitchRange :: Derive.PitchDeriver
    -> Derive.Deriver (PSignal.PSignal, (RealTime, RealTime), [Log.Msg])
pitchRange deriver = do
    (sig, logs) <- first mconcat . Stream.partition <$> deriver
    let range = case (PSignal.head sig, PSignal.last sig) of
            (Just (s, _), Just (e, _)) -> (s, e)
            _ -> (0, 0)
    pure (sig, range, logs)

-- | Transform a pitch or control signal.
signal :: Monoid sig => (sig -> sig)
    -> Derive.Deriver (Stream.Stream sig) -> Derive.Deriver (Stream.Stream sig)
signal f deriver = do
    (sig, logs) <- deriveSignal deriver
    pure $ Stream.fromSortedList $
        LEvent.Event (f sig) : map LEvent.Log logs

deriveSignal :: Monoid sig => Derive.Deriver (Stream.Stream sig)
    -> Derive.Deriver (sig, [Log.Msg])
deriveSignal deriver = do
    (chunks, logs) <- Stream.partition <$> deriver
    pure (mconcat chunks, logs)

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
makeDelayed :: Derive.PassedArgs a -> RealTime -> [DeriveT.Val]
    -> Derive.NoteDeriver
makeDelayed args start eventArgs = do
    dyn <- Internal.getDynamic id
    Stream.fromEvent . delayedEvent eventArgs <$>
        NoteUtil.makeEvent args dyn start 0 "" mempty

delayedEvent :: [DeriveT.Val] -> Score.Event -> Score.Event
delayedEvent args = Score.modifyEnviron $
    Env.insertVal EnvKey.args (DeriveT.VList args)

-- | Return the args if this is a delayed event created by the given call.
delayedArgs :: Expr.Symbol -> Score.Event -> Maybe [DeriveT.Val]
delayedArgs (Expr.Symbol call) event
    | Lists.head (Stack.innermost (Score.eventStack event))
            == Just (Stack.Call call) =
        Env.maybeVal EnvKey.args (Score.eventEnviron event)
    | otherwise = Nothing

-- * modify events

-- | Like 'add_environ', but check the type.
putEnviron :: Typecheck.ToVal a => Env.Key -> a -> Score.Event
    -> Either Text Score.Event
putEnviron name val event =
    case Env.putVal name val (Score.eventEnviron event) of
        Left err -> Left err
        Right env -> Right $ event { Score.eventEnviron = env }

addEnviron :: Typecheck.ToVal a => Env.Key -> a -> Score.Event -> Score.Event
addEnviron name val = Score.modifyEnviron $ Env.insertVal name val

setInstrument :: (ScoreT.Instrument, Derive.Instrument)
    -- ^ unaliased instrument name, from 'Derive.get_instrument'
    -> Score.Event -> Score.Event
setInstrument (scoreInst, inst) =
    Score.setInstrument scoreInst (Derive.instEnviron inst)

-- * misc

-- | Like 'Derive.with_event_stack', but directly add the event's innermost
-- stack to a log msg.
-- TODO unused
addEventStack :: Score.Event -> Log.Msg -> Log.Msg
addEventStack =
    maybe id withStack . Stack.blockTrackRegionOf . Score.eventStack
    where
    withStack (blockId, trackId, (s, e)) msg =
        msg { Log.msgStack = addStack msg }
        where
        addStack = Just . add . fromMaybe Stack.empty . Log.msgStack
        add = Stack.add (Stack.Region s e) . Stack.add (Stack.Track trackId)
            . Stack.add (Stack.Block blockId)

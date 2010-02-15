{- | Utilities for writing calls.  This is higher-level than TrackLang, so
it can import "Derive.Derive".

Calls are evaluated in normalized time, which means that they start at
@score_to_real 0@ and end at @score to real 1@.  The events passed to the
generator are also in this time.
-}
module Derive.Call where
-- import qualified Data.DList as DList
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Util.Log as Log
-- import qualified Util.Map as Map
import qualified Util.Pretty as Pretty

import Ui
import qualified Ui.Event as Event
import qualified Ui.Id as Id
import qualified Ui.State as State
import qualified Ui.Track as Track
import qualified Ui.Types as Types

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
-- import qualified Derive.Score as Score
import qualified Derive.Call.Basic as Basic

import qualified Perform.Signal as Signal


-- * signals

with_controls :: [TrackLang.Control] -> ([Signal.Y] -> Derive.Deriver a)
    -> Derive.Deriver a
with_controls controls f = f =<< mapM (control_at 0) controls

control_at :: ScoreTime -> TrackLang.Control -> Derive.Deriver Signal.Y
control_at pos control = case control of
    TrackLang.ConstantControl deflt -> return deflt
    TrackLang.DefaultedControl cont deflt ->
        Derive.control_at cont (Just deflt) pos
    TrackLang.Control cont -> Derive.control_at cont Nothing pos

-- | Convert a 'TrackLang.Control' to a signal.
to_signal :: TrackLang.Control -> Derive.Deriver Signal.Control
to_signal control = case control of
    TrackLang.ConstantControl deflt -> return $ Signal.constant deflt
    TrackLang.DefaultedControl cont deflt -> do
        sig <- Derive.get_control cont
        return $ maybe (Signal.constant deflt) id sig
    TrackLang.Control cont ->
        maybe (Derive.throw $ "not found: " ++ show cont) return
            =<< Derive.get_control cont

-- * util

one_note :: Either TrackLang.TypeError Derive.EventDeriver
    -> Either TrackLang.TypeError (Derive.EventDeriver, Int)
one_note = fmap $ \d -> (d, 1)

skip_event :: GeneratorReturn
skip_event = return (Derive.empty_deriver, 1)

-- * eval

type GeneratorReturn = Derive.Deriver (Derive.EventDeriver, Int)

-- | Evaluate a single note as a generator.  Fake up an event with no prev or
-- next lists.
eval_one :: String -> ScoreTime -> ScoreTime -> TrackLang.Expr
    -> Derive.EventDeriver
eval_one caller start dur expr = do
    -- Since the event was fake, I don't care if it wants to consume.
    (deriver, _) <- eval_generator caller expr []
        (event start dur ("expr: " ++ show expr)) []
    deriver

event :: ScoreTime -> ScoreTime -> String -> Track.PosEvent
event start dur text = (start, Event.event text dur)

eval_generator :: String -> TrackLang.Expr -> [Track.PosEvent]
    -> Track.PosEvent -> [Track.PosEvent] -> GeneratorReturn
eval_generator caller (TrackLang.Call call_id args : rest) prev cur
        next = do
    let msg = "eval_generator " ++ show caller ++ ": "
    call <- lookup_note_call call_id
    env <- Derive.gets Derive.state_environ
    let passed = TrackLang.PassedArgs args env call_id stretch
    case Derive.call_generator call of
        Nothing -> do
            Derive.warn $ msg ++ "non-generator " ++ show call_id
                ++ " in generator position"
            skip_event
        Just c -> case c passed (map warp prev) evt0 (map warp next) of
            Left err -> do
                Derive.warn $ msg ++ Pretty.pretty err
                skip_event
            Right (deriver, consumed) -> do
                deriver <- eval_transformer caller stretch rest
                    (handle_exc "generator" call_id deriver)
                return (place deriver, consumed)
    where
    -- Derivation happens according to the extent of the note, not the
    -- duration.  This is how negative duration events begin deriving before
    -- arriving at the trigger.  Note generating calls that wish to actually
    -- generate negative duration may check the event_duration and reverse this
    -- by looking at a (start, end) of (1, 0) instead of (0, 1).
    place = Derive.d_at start . Derive.d_stretch stretch
    (start, end) = (Track.event_min cur, Track.event_max cur)
    -- A 0 dur event can't be normalized, so don't try.
    stretch = if start == end then 1 else end - start
    -- Warp all the events to be local to the warp established by 'place'.
    warp (pos, evt) =
        ((pos-start) / stretch, Event.modify_duration (/stretch) evt)
    evt0 = Event.modify_duration (/stretch) (snd cur)

eval_generator _ [] _ cur _ = Derive.throw $
    "event with no calls at all (this shouldn't happen): " ++ show cur

eval_transformer :: String -> ScoreTime -> TrackLang.Expr
    -> Derive.EventDeriver -> Derive.Deriver Derive.EventDeriver
eval_transformer caller stretch (TrackLang.Call call_id args : rest) deriver= do
    let msg = "eval_transformer " ++ show caller ++ ": "
    call <- lookup_note_call call_id
    env <- Derive.gets Derive.state_environ
    let passed = TrackLang.PassedArgs args env call_id stretch
    case Derive.call_transformer call of
        Nothing -> do
            Derive.warn $ msg ++ "non-transformer " ++ show call_id
                ++ " in transformer position"
            return Derive.empty_deriver
        Just c -> case c passed deriver of
            Left err -> do
                Derive.warn $ msg ++ Pretty.pretty err
                return Derive.empty_deriver
            Right deriver ->
                eval_transformer caller stretch rest
                    (handle_exc "transformer" call_id deriver)
eval_transformer _ _ [] deriver = return deriver

handle_exc :: String -> TrackLang.CallId -> Derive.EventDeriver
    -> Derive.EventDeriver
handle_exc call_type call_id deriver = fmap (maybe Derive.no_events id) $
    Derive.catch_warn ("exception: "++) (Derive.with_msg msg deriver)
    where
    msg = call_type ++ " " ++ Pretty.pretty call_id

-- * lookup_note_call

-- | This is here instead of Derive because note calls first look at the block
-- ids to derive a block.
lookup_note_call :: TrackLang.CallId -> Derive.Deriver Derive.Call
lookup_note_call call_id = do
    st <- Derive.get
    let default_ns = State.state_project (Derive.state_ui st)
        block_id = Types.BlockId (make_id default_ns call_id)
    let call_map = Derive.calls_note (Derive.state_call_map st)
    if block_id `Map.member` State.state_blocks (Derive.state_ui st)
        then return $ Basic.c_block block_id
        else case Map.lookup call_id call_map of
            Nothing -> return (c_not_found call_id)
            Just call -> return call

-- | I don't want to abort all of derivation by throwing, but I do want to
-- abort evaluation of this expression, so consider this a kind of type error,
-- which does just that.
c_not_found :: TrackLang.CallId -> Derive.Call
c_not_found call_id = Derive.Call
    (Just $ \_ _ _ _ -> err) (Just $ \_ _ -> err)
    where err = Left (TrackLang.CallNotFound call_id)

-- | Make an Id from a string, relative to the current ns if it doesn't already
-- have one.
--
-- TODO move this to a more generic place since LanguageCmds may want it to?
make_id :: String -> TrackLang.CallId -> Id.Id
make_id default_ns (TrackLang.Symbol ident_str) = Id.id ns ident
    where
    (w0, w1) = break (=='/') ident_str
    (ns, ident) = if null w1 then (default_ns, w0) else (w0, drop 1 w1)


-- * map score events

-- Functions here force a Deriver into its Score.Events and process them
-- directly, and then repackage them as a Deriver.  This can accomplish
-- concrete post-processing type effects but has the side-effect of collapsing
-- the Deriver, which will no longer respond to the environment.
--
-- The warp behaviour is reinstated though.

{-
-- | Transform an event list.  As a convenience, you can optionally pass a list
-- of signals which will be looked up at each event start.
--
-- The iteratee can return any number of events in any order.  This is flexible
-- but destroys laziness.
map_any :: (Monad m) => [Score.Event] -> st
    -> (EventContext st -> Result m st) -> Derive.DeriveT m [Score.Event]
map_any events st f = do
    (emap, _) <- fold_events go (Map.empty, st) events
    return (Map.elems emap)
    where
    go (emap, st) prev event next = do
        (new_st, new_events) <- f (st, prev, event, next)
        let epos = [(Score.start e, e) | e <- new_events]
        return (Map.insert_list epos emap, new_st)

-- | This is the same as 'map_any' except that the iteratee promises that
-- @last r0 <= head r1@ where @r0@ and @r1@ are consecutive result lists.  This
-- is less flexible but preserves laziness.
map_asc :: (Monad m) => [Score.Event] -> st
    -> (EventContext st -> Result m st) -> Derive.DeriveT m [Score.Event]
map_asc events st f = do
    (new_events, _) <- fold_events go (DList.empty, st) events
    return (DList.toList new_events)
    where
    go (collect, st) prev event next = do
        (new_st, new_events) <- f (st, prev, event, next)
        return (collect `DList.append` (DList.fromList new_events), new_st)

-- TODO: a variant map that promises to return ascending lists of events that
-- can overlap, e.g. 'head r0 <= head r1'.  If I can write a lazy
-- merge_sublists function...  I could use this one to make c_echo lazy.

type EventContext st = (st, [Score.Event], Score.Event, [Score.Event])
type Result m st = Derive.DeriveT m (st, [Score.Event])

with_directive :: (Monad m) => (TrackLang.CallId -> Bool)
    -> (Note.Call -> EventContext st -> Result m st)
    -> EventContext st -> Result m st
with_directive is_dir f context@(st, _, event, _) =
    case Note.parse_directive event of
        Nothing -> return (st, [event])
        Just (Left msg) -> do
            Log.warn $ "with_directive: error parsing directive: " ++ msg
            return (st, [event])
        Just (Right call)
            | is_dir (Note.call_id call) -> f call context
            | otherwise -> return (st, [event])

with_directive_calls call_ids = with_directive (is_call call_ids)

is_call :: [String] -> TrackLang.CallId -> Bool
is_call call_id_strs call_id = Set.member call_id call_ids
    where call_ids = Set.fromList (map TrackLang.Symbol call_id_strs)

-- * util

-- TODO merge this with Derive.map_events, which is doing the same kind of
-- thing

fold_events :: (Monad m, Score.Eventlike e) =>
    (st -> [e] -> e -> [e] -> Derive.DeriveT m st)
    -> st -> [e] -> Derive.DeriveT m st
fold_events f st events = foldM_neighbors go st events
    where go st prev event next = Derive.with_event event (f st prev event next)

-- | This is like 'foldM', but additionally pass the iteratee a list of
-- previous events in reverse order and a list of following events.
foldM_neighbors :: (Monad m) =>
    (st -> [a] -> a -> [a] -> m st) -> st -> [a] -> m st
foldM_neighbors f st xs = go st [] xs
    where
    go st _ [] = return st
    go st prev (x:xs) = do
        new_st <- f st prev x xs
        go new_st (x:prev) xs
-}

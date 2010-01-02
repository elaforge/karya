-- | Utilities for writing calls.  This is higher-level than TrackLang, so
-- it can import "Derive.Derive".
module Derive.Call where
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Util.Log as Log
import qualified Util.Map as Map

import qualified Derive.Derive as Derive
import qualified Derive.TrackLang as TrackLang
import qualified Derive.Score as Score
import qualified Derive.Note as Note

import qualified Perform.Signal as Signal


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

with_signals :: (Monad m) => [TrackLang.Signal]
    -> ([Signal.Y] -> EventContext st -> Result m st)
    -> EventContext st -> Result m st
with_signals sigs f context@(_, _, event, _) = do
    vals <- event_signals event sigs
    f vals context

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
    where call_ids = Set.fromList (map TrackLang.CallId call_id_strs)

get_signal :: (Monad m) => Score.Event -> TrackLang.Signal
    -> Derive.DeriveT m Signal.Y
get_signal event (TrackLang.Signal (deflt, control)) = case control of
    Nothing -> maybe (Derive.throw $ "TrackLang.Signal with no control and no "
        ++ "default value is silly") return deflt
    Just cont -> maybe
        (Derive.throw $ "get_signal: not in environment and no default given: "
            ++ show cont)
        return (Score.control_at (Score.start event) cont deflt event)
    -- TODO hspp screws up \ syntax
    -- Nothing -> maybe (Derive.throw $ "TrackLang.Signal with no control and \
    --     \no default value is silly") return deflt

event_signals :: (Monad m) => Score.Event -> [TrackLang.Signal]
    -> Derive.DeriveT m [Signal.Y]
event_signals event = mapM (get_signal event)

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

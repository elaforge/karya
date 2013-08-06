-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.LEvent where
import Prelude hiding (length, either)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import Util.Pretty ((<+>))
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos

import qualified Derive.Stack as Stack


-- * LEvent

data LEvent a = Event !a | Log !Log.Msg
    deriving (Read, Show)

instance Functor LEvent where
    fmap f (Event a) = Event (f a)
    fmap _ (Log a) = Log a

instance (Pretty.Pretty d) => Pretty.Pretty (LEvent d) where
    format (Log msg) = format_log msg
    format (Event event) = Pretty.format event

instance (Eq d) => Eq (LEvent d) where
    Log e1 == Log e2 = e1 == e2
    Event e1 == Event e2 = e1 == e2
    Log _ == Event _ = False
    Event _ == Log _ = False

-- | A variation on 'Log.format_msg', except this can format the stack nicely.
format_log :: Log.Msg -> Pretty.Doc
format_log msg = Pretty.fsep
    [Pretty.text stars <+> Pretty.text srcpos <+> Pretty.format stack,
        Pretty.nest 2 $ Pretty.text (Log.msg_string msg)]
    where
    stars = replicate (fromEnum (Log.msg_prio msg)) '*'
    srcpos = maybe "" ((++": ") . SrcPos.show_srcpos . Just)
        (Log.msg_caller msg)
    stack = case Log.msg_stack msg of
        Nothing -> Pretty.text "[]"
        Just stack -> Stack.format_ui (Stack.from_strings stack)

event :: LEvent a -> Maybe a
event (Event d) = Just d
event _ = Nothing

-- | Always true for logs.  Useful for take and drop on events.
log_or :: (d -> Bool) -> LEvent d -> Bool
log_or f = either f (const True)

either :: (d -> a) -> (Log.Msg -> a) -> LEvent d -> a
either f1 _ (Event event) = f1 event
either _ f2 (Log log) = f2 log

find_event :: (a -> Bool) -> [LEvent a] -> Maybe a
find_event _ [] = Nothing
find_event f (Log _ : rest) = find_event f rest
find_event f (Event event : rest)
    | f event = Just event
    | otherwise = find_event f rest

events_of :: [LEvent d] -> [d]
events_of [] = []
events_of (Event e : rest) = e : events_of rest
events_of (Log _ : rest) = events_of rest

logs_of :: [LEvent d] -> [Log.Msg]
logs_of [] = []
logs_of (Event _ : rest) = logs_of rest
logs_of (Log log : rest) = log : logs_of rest

write_logs :: (Log.LogMonad m) => [LEvent d] -> m [d]
write_logs events = mapM_ Log.write logs >> return vals
    where (vals, logs) = partition events

partition :: Stream (LEvent d) -> ([d], [Log.Msg])
partition = Seq.partition_either . map to_either
    where
    to_either (Event d) = Left d
    to_either (Log msg) = Right msg

map_state :: state -> (state -> a -> (b, state)) -> [LEvent a] -> [LEvent b]
map_state _ _ [] = []
map_state state f (Log log : rest) = Log log : map_state state f rest
map_state state f (Event event : rest) = Event event2 : map_state state2 f rest
    where (event2, state2) = f state event

instance (DeepSeq.NFData a) => DeepSeq.NFData (LEvent a) where
    rnf (Event event) = DeepSeq.rnf event
    rnf (Log msg) = DeepSeq.rnf msg


-- | This is similar to 'List.mapAccumL', but lifted into LEvents.  It also
-- passes future events to the function.
map_accum :: (state -> a -> [a] -> (state, [b])) -> state -> [LEvent a]
    -> (state, [[LEvent b]])
map_accum f state events =
    List.mapAccumL process state (zip events (drop 1 (List.tails events)))
    where
    process st (Event event, future_events) =
        second (map Event) (f st event (events_of future_events))
    process st (Log log, _) = (st, [Log log])

-- | Like 'map_accum', but provide past and future events to the function.
map_around :: ([a] -> a -> [a] -> [b]) -> [LEvent a] -> [[LEvent b]]
map_around f =
    snd . map_accum (\prev event next -> (event : prev, f prev event next)) []

-- * stream

type Stream a = [a]

empty_stream :: Stream a
empty_stream = []

length :: Stream a -> Int
length = List.length

type LEvents d = Stream (LEvent d)

one :: a -> Stream a
one x = x `seq` [x]

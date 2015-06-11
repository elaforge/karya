-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.LEvent where
import Prelude hiding (length, either, log, zip, zip3)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import Util.Pretty ((<+>))
import qualified Util.Seq as Seq
import qualified Util.SrcPos as SrcPos

import qualified Derive.Stack as Stack
import Global


-- * LEvent

data LEvent a = Event !a | Log !Log.Msg
    deriving (Eq, Show)

log :: Log.Msg -> LEvent a
log = Log

instance Functor LEvent where
    fmap f (Event a) = Event (f a)
    fmap _ (Log a) = Log a

instance Pretty.Pretty d => Pretty.Pretty (LEvent d) where
    format = either Pretty.format format_log

-- | A variation on 'Log.format_msg', except this can format the stack nicely.
format_log :: Log.Msg -> Pretty.Doc
format_log msg =
    Pretty.text stars <+> Pretty.text srcpos <+> Pretty.format stack
        <> Pretty.indent_ (Pretty.text (Log.msg_text msg))
    where
    stars = Text.replicate (fromEnum (Log.msg_priority msg)) "*"
    srcpos = maybe "" ((<>": ") . txt . SrcPos.show_srcpos . Just)
        (Log.msg_caller msg)
    stack = case Log.msg_stack msg of
        Nothing -> Pretty.text "[]"
        Just stack -> Stack.format_ui stack

event :: LEvent a -> Maybe a
event (Event d) = Just d
event _ = Nothing

is_event :: LEvent a -> Bool
is_event (Event {}) = True
is_event _ = False

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
logs_of (Log log : rest) = log : logs_of rest
logs_of (_ : rest) = logs_of rest

write_logs :: Log.LogMonad m => [LEvent d] -> m [d]
write_logs events = mapM_ Log.write logs >> return vals
    where (vals, logs) = partition events

write_snd :: Log.LogMonad m => (a, [Log.Msg]) -> m a
write_snd (result, logs) = mapM_ Log.write logs >> return result

write_snd_prefix :: Log.LogMonad m => Text -> (a, [Log.Msg]) -> m a
write_snd_prefix prefix (result, logs) =
    mapM_ Log.write (Log.add_prefix prefix logs) >> return result

partition :: [LEvent d] -> ([d], [Log.Msg])
partition = Seq.partition_either . map to_either
    where
    to_either (Event d) = Left d
    to_either (Log msg) = Right msg

instance DeepSeq.NFData a => DeepSeq.NFData (LEvent a) where
    rnf = either DeepSeq.rnf DeepSeq.rnf

-- | This is similar to 'List.mapAccumL', but lifted into LEvents.  It also
-- passes future events to the function.
map_accum :: (state -> a -> [a] -> (state, [b])) -> state -> [LEvent a]
    -> (state, [[LEvent b]])
map_accum f state events =
    List.mapAccumL process state (List.zip events (drop 1 (List.tails events)))
    where
    process st (Event event, future_events) =
        second (map Event) (f st event (events_of future_events))
    process st (Log log, _) = (st, [Log log])

-- | Like 'map_accum', but provide past and future events to the function.
map_around :: ([a] -> a -> [a] -> [b]) -> [LEvent a] -> [[LEvent b]]
map_around f =
    snd . map_accum (\prev event next -> (event : prev, f prev event next)) []

zip :: [a] -> [LEvent x] -> [LEvent (a, x)]
zip as (Log x : xs) = Log x : zip as xs
zip (a:as) (Event x : xs) = Event (a, x) : zip as xs
zip _ _ = []

zip3 :: [a] -> [b] -> [LEvent x] -> [LEvent (a, b, x)]
zip3 as bs (Log x : xs) = Log x : zip3 as bs xs
zip3 (a:as) (b:bs) (Event x : xs) = Event (a, b, x) : zip3 as bs xs
zip3 _ _ _ = []

zip4 :: [a] -> [b] -> [c] -> [LEvent x] -> [LEvent (a, b, c, x)]
zip4 as bs cs (Log x : xs) = Log x : zip4 as bs cs xs
zip4 (a:as) (b:bs) (c:cs) (Event x : xs) = Event (a, b, c, x) : zip4 as bs cs xs
zip4 _ _ _ _ = []

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Derive.LEvent where
import Prelude hiding (either, length, log, takeWhile, dropWhile, zip, zip3)
import qualified Prelude
import qualified Control.DeepSeq as DeepSeq
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import Util.Pretty ((<+>))

import qualified Derive.Stack as Stack
import Global


-- * LEvent

data LEvent a = Event !a | Log !Log.Msg
    deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty d => Pretty (LEvent d) where
    format = either Pretty.format formatLog

instance DeepSeq.NFData a => DeepSeq.NFData (LEvent a) where
    rnf = either DeepSeq.rnf DeepSeq.rnf

-- | A variation on 'Log.format_msg', except this can format the stack nicely.
formatLog :: Log.Msg -> Pretty.Doc
formatLog msg =
    Pretty.text stars <+> Pretty.text caller <+> Pretty.format stack
        <> Pretty.indent_ (Pretty.text (Log.msgText msg))
    where
    stars = Text.replicate (fromEnum (Log.msgPriority msg)) "*"
    caller = CallStack.showCaller (Log.msgCaller msg) <> ": "
    stack = case Log.msgStack msg of
        Nothing -> Pretty.text "[]"
        Just stack -> Stack.formatUi stack

event :: LEvent a -> Maybe a
event (Event d) = Just d
event _ = Nothing

isEvent :: LEvent a -> Bool
isEvent (Event {}) = True
isEvent _ = False

isLog :: LEvent a -> Bool
isLog (Log {}) = True
isLog _ = False

-- | Always true for logs.  Useful for take and drop on events.
logOr :: (d -> Bool) -> LEvent d -> Bool
logOr f = either f (const True)

eventOr :: (Log.Msg -> Bool) -> LEvent d -> Bool
eventOr = either (const True)

-- | Drop while the predicate is true, but keep preceding logs.
dropWhile :: (a -> Bool) -> [LEvent a] -> [LEvent a]
dropWhile f = go []
    where
    go accum [] = reverse accum
    go accum es@(Event event : rest)
        | f event = go [] rest
        | otherwise = reverse accum ++ es
    go accum (log@(Log _) : rest) = go (log:accum) rest

takeWhile :: (a -> Bool) -> [LEvent a] -> [LEvent a]
takeWhile f = Prelude.takeWhile (logOr f)

either :: (d -> a) -> (Log.Msg -> a) -> LEvent d -> a
either f1 _ (Event event) = f1 event
either _ f2 (Log log) = f2 log

mapLog :: (Log.Msg -> Log.Msg) -> LEvent a -> LEvent a
mapLog f (Log log) = Log (f log)
mapLog _ event = event

findEvent :: (a -> Bool) -> [LEvent a] -> Maybe a
findEvent _ [] = Nothing
findEvent f (Log _ : rest) = findEvent f rest
findEvent f (Event event : rest)
    | f event = Just event
    | otherwise = findEvent f rest

eventsOf :: [LEvent d] -> [d]
eventsOf [] = []
eventsOf (Event e : rest) = e : eventsOf rest
eventsOf (Log _ : rest) = eventsOf rest

logsOf :: [LEvent d] -> [Log.Msg]
logsOf [] = []
logsOf (Log log : rest) = log : logsOf rest
logsOf (_ : rest) = logsOf rest

writeLogs :: Log.LogMonad m => [LEvent d] -> m [d]
writeLogs events = mapM_ Log.write logs >> pure vals
    where (vals, logs) = partition events

writeSnd :: Log.LogMonad m => (a, [Log.Msg]) -> m a
writeSnd (result, logs) = mapM_ Log.write logs >> pure result

writeSndPrefix :: Log.LogMonad m => Text -> (a, [Log.Msg]) -> m a
writeSndPrefix prefix (result, logs) =
    mapM_ Log.write (map (Log.addPrefix prefix) logs) >> pure result

partition :: [LEvent d] -> ([d], [Log.Msg])
partition = Either.partitionEithers . map toEither
    where
    toEither (Event d) = Left d
    toEither (Log msg) = Right msg

-- | This is similar to 'List.mapAccumL', but lifted into LEvents.  It also
-- passes future events to the function.
mapAccum :: (state -> a -> [a] -> (state, [b])) -> state -> [LEvent a]
    -> (state, [[LEvent b]])
mapAccum f state events =
    List.mapAccumL process state (List.zip events (drop 1 (List.tails events)))
    where
    process st (Event event, futureEvents) =
        second (map Event) (f st event (eventsOf futureEvents))
    process st (Log log, _) = (st, [Log log])

-- | Like 'map_accum', but provide past and future events to the function.
mapAround :: ([a] -> a -> [a] -> [b]) -> [LEvent a] -> [[LEvent b]]
mapAround f =
    snd . mapAccum (\prev event next -> (event : prev, f prev event next)) []

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

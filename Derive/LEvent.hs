module Derive.LEvent where
import Prelude hiding (length, either)
import qualified Data.List as List
import qualified Control.DeepSeq as DeepSeq

import qualified Util.Log as Log
import qualified Util.Seq as Seq


-- * LEvent

data LEvent derived = Event !derived | Log !Log.Msg
    deriving (Show)

instance Functor LEvent where
    fmap f (Event a) = Event (f a)
    fmap _ (Log a) = Log a

event :: LEvent derived -> Maybe derived
event (Event d) = Just d
event _ = Nothing

is_event :: LEvent d -> Bool
is_event (Event _) = True
is_event _ = False

either :: (d -> a) -> (Log.Msg -> a) -> LEvent d -> a
either f _ (Event event) = f event
either _ f (Log log) = f log

extract_events :: Stream (LEvent d) -> ([d], [LEvent d2])
extract_events [] = ([], [])
extract_events (x:xs) =
    let (ds, logs) = extract_events xs
    in case x of
        Event d -> (d : ds, logs)
        -- TODO is there a way to do this without making a new Log?
        Log msg -> (ds, Log msg : logs)

events_of :: [LEvent d] -> [d]
events_of [] = []
events_of (Event e : rest) = e : events_of rest
events_of (Log _ : rest) = events_of rest

logs_of :: [LEvent d] -> [Log.Msg]
logs_of [] = []
logs_of (Event _ : rest) = logs_of rest
logs_of (Log log : rest) = log : logs_of rest


partition :: Stream (LEvent d) -> ([d], [Log.Msg])
partition = Seq.partition_either . map to_either
    where
    to_either (Event d) = Left d
    to_either (Log msg) = Right msg

instance (DeepSeq.NFData derived) => DeepSeq.NFData (LEvent derived) where
    rnf (Event event) = DeepSeq.rnf event
    rnf (Log msg) = DeepSeq.rnf msg


-- * stream

type Stream a = [a]

empty_stream :: Stream a
empty_stream = []

length :: Stream a -> Int
length = List.length

type LEvents d = Stream (LEvent d)

one :: a -> Stream a
one x = x `seq` [x]

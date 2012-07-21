{-# LANGUAGE OverloadedStrings #-}
module Cmd.Integrate.Merge_test where
import Util.Control
import Util.Test
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Cmd.Integrate.Merge as Merge
import qualified Derive.Stack as Stack
import Types


test_diff = do
    let f old new = extract $ Merge.diff index (1, mkevent new)
            where index = Merge.make_index [mkevent old]
        extract (Merge.Add tracknum event) =
            Left (tracknum, extract_event event)
        extract (Merge.Edit stack tracknum mods) =
            Right (extract_stack stack, tracknum, mods)
    -- no mods
    equal (f (0, 1, "a", Just 'a') (0, 1, "a", Just 'a')) $
        Right ('a', 1, [])
    -- moved
    equal (f (0, 1, "a", Just 'a') (1, 1, "a", Just 'a')) $
        Right ('a', 1, [Merge.Position 1])
    -- A new event has an unknown stack, so it's considered an add and the
    -- stack is deleted.
    equal (f (0, 1, "a", Just 'a') (1, 1, "b", Just 'b')) $
        Left (1, (1, 1, "b", Nothing))

test_diff_event = do
    let f old new = Merge.diff_event (mk old) (mk new)
        mk (start, dur, text) = mkevent (start, dur, text, Nothing)
    equal (f (0, 1, "a") (1, 2, "b"))
        [Merge.Position 1, Merge.Duration 2, Merge.Set "b"]
    -- Detect a Prefix but only on | boundaries.
    equal (f (0, 1, "a") (0, 1, "x a")) [Merge.Set "x a"]
    equal (f (0, 1, "a") (0, 1, "x | a")) [Merge.Prefix "x | "]

test_reintegrate = do
    -- This also tests Merge.apply
    let f last integrated events = reintegrate last integrated events
    -- Event with a stack.
    let stack start text stack = (1, (start, 1, text, Just stack))
        added start text = (1, (start, 1, text, Nothing))
    equal (f [] [] []) []
    -- no changes
    equal (f [stack 0 "a" 'a'] [stack 0 "a" 'a'] [stack 0 "a" 'a'])
        [(1, [(0, 1, "a", Just 'a')])]
    -- no edits, integrated changed to "b"
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 0 "a" 'a'])
        [(1, [(0, 1, "b", Just 'a')])]
    -- event deleted
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] []) []
    -- event moved
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 1 "a" 'a'])
        [(1, [(1, 1, "b", Just 'a')])]
    -- one event of 3 deleted
    equal (f [stack 0 "a" 'a', stack 1 "b" 'b', stack 2 "c" 'c']
             [stack 0 "x" 'a', stack 1 "y" 'b', stack 2 "z" 'c']
             [stack 0 "a" 'a', stack 2 "c" 'c'])
        [(1, [(0, 1, "x", Just 'a'), (2, 1, "z", Just 'c')])]
    -- add new event
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 0 "a" 'a', added 1 "q"])
        [(1, [(0, 1, "b", Just 'a'), (1, 1, "q", Nothing)])]
    -- new event replaces generated one
    equal (f [stack 0 "a" 'a'] [stack 1 "b" 'a'] [stack 0 "a" 'a', added 1 "q"])
        [(1, [(1, 1, "q", Nothing)])]

    -- if an event is edited it keeps its tracknum
    let tnum tracknum start text stack =
            (tracknum, (start, 1, text, Just stack))
    equal (f [tnum 0 0 "a" 'a'] [tnum 1 0 "b" 'a'] [tnum 0 1 "a" 'a'])
        -- integrate says a -> b, user says 0 -> 1
        [(0, [(1, 1, "b", Just 'a')])]
    -- even if not edited, it keeps the old tracknum
    -- TODO even though it might be confusing if events move, it might be more
    -- confusing if they get stack on a track and subsequent integrates are all
    -- on a different track.  Wait until I have some experience to see which
    -- works.
    equal (f [tnum 0 0 "a" 'a'] [tnum 1 0 "b" 'a'] [tnum 0 0 "a" 'a'])
        [(0, [(0, 1, "b", Just 'a')])]

reintegrate :: [(TrackNum, Event)]
    -- ^ this doesn't need TrackNum, but takes it anyway for consistency
    -> [(TrackNum, Event)] -> [(TrackNum, Event)] -> [(TrackNum, [Event])]
reintegrate last_integrate integrated events = map extract $
    Merge.apply deletes edits (mkstack_events integrated)
    where
    (deletes, edits) = Merge.diff_events index (mkevents events)
    extract (tracknum, events) =
        (tracknum, map extract_event (Events.ascending events))
    index = Merge.make_index (map (mkevent . snd) last_integrate)

-- * util

mkevents :: [(TrackNum, Event)] -> [(TrackNum, Events.PosEvent)]
mkevents = map (second mkevent)

mkstack_events :: [(TrackNum, Event)]
    -> [(Event.Stack, TrackNum, Events.PosEvent)]
mkstack_events events =
    [(stack, tracknum, event) | (tracknum, event) <- mkevents events,
        Just stack <- [Event.event_stack (snd event)]]

type Event = (ScoreTime, ScoreTime, String, Maybe Char)

mkevent :: Event -> Events.PosEvent
mkevent (start, dur, text, call) =
    (start, (Event.event text dur) { Event.event_stack = mkstack <$> call })

mkstack :: Char -> Event.Stack
mkstack c = Event.Stack (Stack.from_innermost [Stack.Call (c:"")]) "tag" 0

extract_event :: Events.PosEvent -> Event
extract_event (pos, event) =
    (pos, Event.event_duration event, Event.event_string event,
        extract_stack <$> Event.event_stack event)

extract_stack :: Event.Stack -> Char
extract_stack stack = case Stack.innermost (Event.stack_stack stack) of
    [Stack.Call [c]] -> c
    _ -> error $ "un-extractable stack: " ++ show stack

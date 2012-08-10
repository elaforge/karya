{-# LANGUAGE OverloadedStrings #-}
module Cmd.Integrate.Merge_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge

import qualified Derive.Stack as Stack
import Types


test_diff = do
    let f old new = extract $ Merge.diff index (mkevent new)
            where index = mkindex [old]
        extract (Merge.Add event) = Left (extract_event event)
        extract (Merge.Edit stack mods) = Right (extract_stack stack, mods)
    -- no mods
    equal (f (0, 1, "a", Just 'a') (0, 1, "a", Just 'a')) $
        Right ('a', [])
    -- moved
    equal (f (0, 1, "a", Just 'a') (1, 1, "a", Just 'a')) $
        Right ('a', [Merge.Position 1])
    -- A new event has an unknown stack, so it's considered an add and the
    -- stack is deleted.
    equal (f (0, 1, "a", Just 'a') (1, 1, "b", Just 'b')) $
        Left (1, 1, "b", Nothing)

test_diff_event = do
    let f old new = Merge.diff_event (mk old) (mk new)
        mk (start, dur, text) = mkevent (start, dur, text, Nothing)
    equal (f (0, 1, "a") (1, 2, "b"))
        [Merge.Position 1, Merge.Duration 2, Merge.Set "b"]
    -- Detect a Prefix but only on | boundaries.
    equal (f (0, 1, "a") (0, 1, "x a")) [Merge.Set "x a"]
    equal (f (0, 1, "a") (0, 1, "x | a")) [Merge.Prefix "x | "]

test_apply = do
    let f = apply
    -- Event with a stack.
    let stack start text stack = (start, 1, text, Just stack)
        added start text = (start, 1, text, Nothing)
    equal (f [] [] []) []
    -- no changes
    equal (f [stack 0 "a" 'a'] [stack 0 "a" 'a'] [stack 0 "a" 'a'])
        [(0, 1, "a", Just 'a')]
    -- no edits, integrated changed to "b"
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 0 "a" 'a'])
        [(0, 1, "b", Just 'a')]
    -- event deleted
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] []) []
    -- event moved
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 1 "a" 'a'])
        [(1, 1, "b", Just 'a')]
    -- one event of 3 deleted
    equal (f [stack 0 "a" 'a', stack 1 "b" 'b', stack 2 "c" 'c']
             [stack 0 "x" 'a', stack 1 "y" 'b', stack 2 "z" 'c']
             [stack 0 "a" 'a', stack 2 "c" 'c'])
        [(0, 1, "x", Just 'a'), (2, 1, "z", Just 'c')]
    -- add new event
    equal (f [stack 0 "a" 'a'] [stack 0 "b" 'a'] [stack 0 "a" 'a', added 1 "q"])
        [(0, 1, "b", Just 'a'), (1, 1, "q", Nothing)]
    -- new event replaces generated one
    equal (f [stack 0 "a" 'a'] [stack 1 "b" 'a'] [stack 0 "a" 'a', added 1 "q"])
        [(1, 1, "q", Nothing)]

apply :: [Event] -> [Event] -> [Event] -> [Event]
apply last_integrate integrated events = map extract_event $ Events.ascending $
    Merge.apply deletes edits (mkstack_events integrated)
    where
    (deletes, edits) = Merge.diff_events index (map mkevent events)
    index = mkindex last_integrate

test_integrate = do
    let f state integrated = return $ integrate state integrated
    let events cs = [(n, 1, c:"") | (n, c) <- zip (Seq.range_ 0 2) cs]
        extract = UiTest.extract_tracks

    -- New tracks are appended.
    state <- f (mkblock [(">", events "ab")]) [((">a", events "cd"), [])]
    equal (extract state) [(">", events "ab"), (">a", events "cd")]
    -- Added events are merged into the reintegration.
    state <- f (modify (insert_event 2 "z" 1) state) [((">a", events "cf"), [])]
    equal (extract state)
        [ (">", events "ab")
        , (">a", [(0, 1, "c"), (1, 1, "z"), (2, 1, "f")])
        ]

    -- Added and removed control tracks.
    state <- f (mkblock [])
        [((">a", []), [("ca", events "12")]), ((">b", []), [])]
    equal (extract state)
        [(">a", []), ("ca", events "12"), (">b", [])]
    equal (UiTest.extract_skeleton state) [(1, 2)]
    state <- f
        (modify (Create.track UiTest.default_block_id 4 ">z" mempty) state)
        [((">a", []), []), ((">b", []), [("ba", [])])]
    -- It won't delete a track, but will clear the events out.
    equal (extract state)
        [(">a", []), ("ca", []), (">b", []), ("ba", []), (">z", [])]
    equal (UiTest.extract_skeleton state) [(1, 2), (3, 4)]

    -- Only non-generated events are cleared.
    state <- f (mkblock []) [((">a", events "ab"), [])]
    state <- f (modify (insert_event 1 "z" 1) state) []
    equal (extract state) [(">a", [(1, 1, "z")])]

insert_event :: TrackNum -> String -> ScoreTime -> State.StateId ()
insert_event tracknum text pos =
    State.insert_event (UiTest.mk_tid tracknum) pos (Event.event text 1)

mkblock :: [UiTest.TrackSpec] -> State.State
mkblock = snd . UiTest.run_mkblock

integrate :: State.State -> [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> State.State
integrate state integrated = UiTest.exec state $ do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    dests <- Merge.merge_tracks block_id (mktracks integrated)
        (maybe [] snd (Seq.head itracks))
    State.modify_block block_id $ \block -> block
        { Block.block_integrated_tracks = [(UiTest.tid "source", dests)] }
    where block_id = UiTest.default_block_id

modify :: State.StateId a -> State.State -> State.State
modify action state = UiTest.exec state action

mktracks :: [(UiTest.TrackSpec, [UiTest.TrackSpec])] -> Convert.Tracks
mktracks = map $ \(note, controls) -> (convert note, map convert controls)
    where
    convert (title, events) = Convert.Track title
        (map (add_stack title) $ zip [0..] $ map UiTest.make_event events)
    add_stack title (n, (pos, event)) = (pos, event
        { Event.event_stack = Just (Event.Stack Stack.empty title n) })

mkindex :: [Event] -> Block.EventIndex
mkindex = Merge.make_index . map mkevent

-- * util

mkstack_events :: [Event] -> [(Event.Stack, Events.PosEvent)]
mkstack_events events =
    [(stack, event) | event <- map mkevent events,
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

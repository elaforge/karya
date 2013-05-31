-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Merge_test where
import qualified Data.List.NonEmpty as NonEmpty

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
        extract (Merge.Edit key mods) = Right (key, mods)
    -- no mods
    equal (f (0, 1, "a", Just 0) (0, 1, "a", Just 0)) $
        Right (0, [])
    -- moved
    equal (f (0, 1, "a", Just 0) (1, 1, "a", Just 0)) $
        Right (0, [Merge.Position 1])
    -- A new event has an unknown stack, so it's considered an add and the
    -- stack is deleted.
    equal (f (0, 1, "a", Just 0) (1, 1, "b", Just 1)) $
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
    equal (f [stack 0 "a" 0] [stack 0 "a" 0] [stack 0 "a" 0])
        [(0, 1, "a", Just 0)]
    -- no edits, integrated changed to "b"
    equal (f [stack 0 "a" 0] [stack 0 "b" 0] [stack 0 "a" 0])
        [(0, 1, "b", Just 0)]
    -- event deleted
    equal (f [stack 0 "a" 0] [stack 0 "b" 0] []) []
    -- event moved
    equal (f [stack 0 "a" 0] [stack 0 "b" 0] [stack 1 "a" 0])
        [(1, 1, "b", Just 0)]
    -- one event of 3 deleted
    equal (f [stack 0 "a" 0, stack 1 "b" 1, stack 2 "c" 2]
             [stack 0 "x" 0, stack 1 "y" 1, stack 2 "z" 2]
             [stack 0 "a" 0, stack 2 "c" 2])
        [(0, 1, "x", Just 0), (2, 1, "z", Just 2)]
    -- add new event
    equal (f [stack 0 "a" 0] [stack 0 "b" 0] [stack 0 "a" 0, added 1 "q"])
        [(0, 1, "b", Just 0), (1, 1, "q", Nothing)]
    -- new event replaces generated one
    equal (f [stack 0 "a" 0] [stack 1 "b" 0] [stack 0 "a" 0, added 1 "q"])
        [(1, 1, "q", Nothing)]

apply :: [Event] -> [Event] -> [Event] -> [Event]
apply last_integrate integrated events = map extract_event $ Events.ascending $
    Merge.apply deletes edits (map mkevent integrated)
    where
    (deletes, edits) = Merge.diff_events index (map mkevent events)
    index = mkindex last_integrate

test_integrate = do
    let f state integrated = return $ integrate state integrated
    let events cs = [(n, 1, c:"") | (n, c) <- zip (Seq.range_ 0 2) cs]
        extract = UiTest.extract_tracks

    -- New tracks are appended.
    state <- f (mkblock [(">", events "ab")]) [((">a", events "cd"), [])]
    equal (extract state)
        [(">source", []), (">", events "ab"), (">a", events "cd")]
    -- Added events are merged into the reintegration.
    state <- f (modify (UiTest.insert_event 3 (1, 1, "z")) state)
        [((">a", events "cf"), [])]
    equal (extract state)
        [ (">source", [])
        , (">", events "ab")
        , (">a", [(0, 1, "c"), (1, 1, "z"), (2, 1, "f")])
        ]

    -- Added and removed control tracks.
    state <- f (mkblock [])
        [((">a", []), [("ca", events "12")]), ((">b", []), [])]
    equal (extract state)
        [(">source", []), (">a", []), ("ca", events "12"), (">b", [])]
    equal (UiTest.extract_skeleton state) [(2, 3)]
    state <- f
        (modify (Create.track UiTest.default_block_id 5 ">z" mempty) state)
        [((">a", []), []), ((">b", []), [("ba", [])])]
    -- It won't delete a track, but will clear the events out.
    equal (extract state)
        [ (">source", [])
        , (">a", []), ("ca", []), (">b", []), ("ba", []), (">z", [])
        ]
    equal (UiTest.extract_skeleton state) [(2, 3), (4, 5)]

    -- Only non-generated events are cleared.
    state <- f (mkblock []) [((">a", events "ab"), [])]
    state <- f (modify (UiTest.insert_event 2 (1, 1, "z")) state) []
    equal (extract state) [(">source", []), (">a", [(1, 1, "z")])]

    -- Interacts properly with aded events.
    -- state <- f (mkblock []) [((">a", events "ab"), [])]

mkblock :: [UiTest.TrackSpec] -> State.State
mkblock = snd . UiTest.run_mkblock . ((">source", []):)
    -- Add a track that 'integrate' can set as the source since the integrated
    -- track source is required to exist.

integrate :: State.State -> [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> State.State
integrate state integrated = UiTest.exec state $ do
    itracks <- Block.block_integrated_tracks <$> State.get_block block_id
    dests <- Merge.merge_tracks block_id (mktracks integrated)
        (maybe [] (NonEmpty.toList . snd) (Seq.head itracks))
    when_just (NonEmpty.nonEmpty dests) $ \dests ->
        State.set_integrated_tracks block_id [(UiTest.mk_tid 1, dests)]
    where block_id = UiTest.default_block_id

modify :: State.StateId a -> State.State -> State.State
modify action state = UiTest.exec state action

mktracks :: [(UiTest.TrackSpec, [UiTest.TrackSpec])] -> Convert.Tracks
mktracks = map $ \(note, controls) -> (convert note, map convert controls)
    where
    convert (title, events) = Convert.Track (txt title)
        (map (add_stack title) $ map UiTest.make_event events)
    add_stack title event = Event.set_stack
        (Event.Stack (Stack.call (txt title)) (Event.start event)) event

mkindex :: [Event] -> Block.EventIndex
mkindex = Merge.make_index . map mkevent

-- * util

type Event = (ScoreTime, ScoreTime, String, Maybe ScoreTime)

mkevent :: Event -> Event.Event
mkevent (start, dur, text, mb_stack) = add_stack (Event.event start dur text)
    where
    add_stack = case mb_stack of
        Nothing -> id
        Just pos ->
            Event.set_stack (Event.Stack (Stack.call (showt pos)) pos)

extract_event :: Event.Event -> Event
extract_event event =
    (Event.start event, Event.duration event, Event.event_string event,
        Event.stack_key <$> Event.stack event)

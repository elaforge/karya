-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Merge_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified App.Config as Config
import qualified Cmd.Create as Create
import qualified Cmd.Integrate.Convert as Convert
import qualified Cmd.Integrate.Merge as Merge

import qualified Derive.Stack as Stack
import qualified Ui.Block as Block
import qualified Ui.Color as Color
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Style as Style
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_diff :: Test
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

test_diff_event :: Test
test_diff_event = do
    let f old new = Merge.diff_event (mk old) (mk new)
        mk (start, dur, text) = mkevent (start, dur, text, Nothing)
    equal (f (0, 1, "a") (1, 2, "b"))
        [Merge.Position 1, Merge.Duration 2, Merge.Set "b"]
    -- Detect a Prefix but only on | boundaries.
    equal (f (0, 1, "a") (0, 1, "x a")) [Merge.Set "x a"]
    equal (f (0, 1, "a") (0, 1, "x | a")) [Merge.Prefix "x | "]

test_apply :: Test
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

-- * derive integrate

test_derive_integrate :: Test
test_derive_integrate = do
    let f state integrated = return $ derive_integrate state integrated
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Seq.range_ 0 2) cs]
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

mkblock :: [UiTest.TrackSpec] -> Ui.State
mkblock = snd . UiTest.run_mkblock . ((">source", []):)
    -- Add a track that 'derive_integrate' can set as the source since the
    -- integrated track source is required to exist.

derive_integrate :: Ui.State -> [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> Ui.State
derive_integrate state integrated = UiTest.exec state $ do
    itracks <- Block.block_integrated_tracks <$> Ui.get_block block_id
    let derive_itracks =
            [dests | (_, Block.DeriveDestinations dests) <- itracks]
    dests <- Merge.merge_tracks Merge.KeepTitles block_id
        (make_convert_tracks integrated)
        (fromMaybe [] $ Seq.head derive_itracks)
    Ui.modify_integrated_tracks block_id $
        const [(UiTest.mk_tid 1, Block.DeriveDestinations dests)]
    where block_id = UiTest.default_block_id

make_convert_tracks :: [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> Convert.Tracks
make_convert_tracks =
    map $ \(note, controls) -> (convert note, map convert controls)
    where
    convert (title, events) = Convert.Track title
        (map (add_stack title) $ map UiTest.make_event events)
    add_stack title event = Event.stack_ #= Just stack $ event
        where stack = Event.Stack (Stack.call title) (Event.start event)

-- * score integrate

test_score_integrate_delete :: Test
test_score_integrate_delete = do
    let run state m = return $ run_score_integrate 1 (modify m state)
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Seq.range_ 0 2) cs]
    state <- run Ui.empty $ UiTest.mkblock
        [(">", events "ab"), ("c1", events "12")]
    equal (UiTest.extract_tracks state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extract_skeleton state) [(1, 2), (3, 4)]
    equal (lookup (UiTest.mk_tid 3) (e_event_styles state)) $
        Just [("a", Style.StyleId 1), ("b", Style.StyleId 1)]

    -- Deleting the source note track disowns the destinations.
    state <- return $ UiTest.exec state $
        mapM_ (Ui.remove_track UiTest.default_block_id) [2, 1]
    equal (UiTest.extract_tracks state)
        [ (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extract_skeleton state) [(1, 2)]
    equal (e_integrate_skeleton state) []
    equal (lookup (UiTest.mk_tid 3) (e_event_styles state)) $
        Just [("a", Style.StyleId 0), ("b", Style.StyleId 0)]

e_event_styles :: Ui.State -> [(TrackId, [(Text, Style.StyleId)])]
e_event_styles =
    map (second (map (\e -> (Event.text e, Event.style e))
        . Events.ascending . Track.track_events))
    . Map.toList . Ui.state_tracks

test_score_integrate :: Test
test_score_integrate = do
    -- make a block with the source, then modify
    let run state m = return $ run_score_integrate 1 (modify m state)
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Seq.range_ 0 2) cs]
        extract = UiTest.extract_tracks

    state <- run Ui.empty $ UiTest.mkblock
        [(">", events "ab"), ("c1", events "12")]
    equal (extract state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extract_skeleton state) [(1, 2), (3, 4)]

    equal (e_integrate_skeleton state)
        [(Config.score_integrate_skeleton, [(1, 3)])]

    -- Ensure a merge is happening.
    state <- run state $ UiTest.insert_event 3 (4, 1, "z")
    equal (extract state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "abz"), ("c1", events "12")
        ]
    state <- run state $ UiTest.insert_event 1 (2, 1, "x")
    equal (extract state)
        [ (">", events "ax"), ("c1", events "12")
        , (">", events "axz"), ("c1", events "12")
        ]
    equal (UiTest.extract_skeleton state) [(1, 2), (3, 4)]

    -- TODO score integrate is a bit broken, so the below doesn't work.
    -- let block_id = UiTest.default_block_id
    -- -- Add a new control and an integrated version is also created.
    -- state <- run state $ do
    --     Create.empty_track block_id 2
    --     Ui.splice_skeleton_below block_id 2 1
    -- equal (extract state)
    --     [ (">", events "ax"), ("", []), ("c1", events "12")
    --     , (">", events "axz"), ("", []), ("c1", events "12")
    --     ]
    -- equal (UiTest.extract_skeleton state) [(1, 2), (2, 3), (4, 5), (5, 6)]
    -- equal (e_integrate_skeleton state)
    --     [(Config.score_integrate_skeleton, [(1, 4)])]
    --
    -- -- The new destination should be linked to the new source.
    -- state <- run state $ do
    --     t1 <- Ui.require "track" =<< Ui.event_track_at block_id 2
    --     Ui.set_track_title t1 "new"
    --     Ui.insert_event t1 (Event.event 0 0 "1.5")
    -- equal (extract state)
    --     [ (">", events "ax"), ("new", [(0, 0, "1.5")]), ("c1", events "12")
    --     , (">", events "axz"), ("new", [(0, 0, "1.5")]), ("c1", events "12")
    --     ]
    --
    -- -- Deleting the control deletes the destination.
    -- state <- run state $ Ui.remove_track block_id 2
    -- equal (extract state)
    --     [ (">", events "ax"), ("c1", events "12")
    --     , (">", events "axz"), ("c1", events "12")
    --     ]
    -- equal (UiTest.extract_skeleton state) [(1, 2), (3, 4)]

e_integrate_skeleton :: Ui.State -> [(Color.Color, [(TrackNum, TrackNum)])]
e_integrate_skeleton =
    concatMap Block.integrate_skeleton . Map.elems . Ui.state_blocks

-- | Reproduce enough of the respond cycle for score integration to work.
-- Hard coded to integrate track 1.
run_score_integrate :: TrackNum -> Ui.State -> Ui.State
run_score_integrate tracknum state = UiTest.exec state $ do
    itracks <- Block.block_integrated_tracks <$> Ui.get_block block_id
    let score_itracks = [dests | (_, Block.ScoreDestinations dests) <- itracks]
    dests <- Merge.score_merge_tracks block_id (UiTest.mk_tid tracknum)
        (fromMaybe [] $ Seq.head score_itracks)
    Ui.modify_integrated_tracks block_id $
        const [(UiTest.mk_tid 1, Block.ScoreDestinations dests)]
    where block_id = UiTest.default_block_id

-- * util

modify :: Ui.StateId a -> Ui.State -> Ui.State
modify action state = UiTest.exec state action

mkindex :: [Event] -> Block.EventIndex
mkindex = Merge.make_index . map mkevent

type Event = (ScoreTime, ScoreTime, Text, Maybe ScoreTime)

mkevent :: Event -> Event.Event
mkevent (start, dur, text, mb_stack) = add_stack (Event.event start dur text)
    where
    add_stack = case mb_stack of
        Nothing -> id
        Just pos ->
            Event.stack_ #= Just (Event.Stack (Stack.call (showt pos)) pos)

extract_event :: Event.Event -> Event
extract_event event =
    (Event.start event, Event.duration event, Event.text event,
        Event.stack_key <$> Event.stack event)

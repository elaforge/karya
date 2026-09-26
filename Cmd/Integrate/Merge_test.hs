-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Integrate.Merge_test where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Lists as Lists
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
        extract (Merge.Add event) = Left (extractEvent event)
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
    let f old new = Merge.diffEvent (mk old) (mk new)
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
apply lastIntegrate integrated events = map extractEvent $ Events.ascending $
    Merge.apply deletes edits (map mkevent integrated)
    where
    (deletes, edits) = Merge.diffEvents index (map mkevent events)
    index = mkindex lastIntegrate

-- * derive integrate

test_derive_integrate :: Test
test_derive_integrate = do
    let f state integrated = pure $ deriveIntegrate state integrated
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Lists.range_ 0 2) cs]
        extract = UiTest.extractTracks

    -- New tracks are appended.
    state <- f (mkblock [(">", events "ab")]) [((">a", events "cd"), [])]
    equal (extract state)
        [(">source", []), (">", events "ab"), (">a", events "cd")]
    -- Added events are merged into the reintegration.
    state <- f (modify (UiTest.insertEvent 3 (1, 1, "z")) state)
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
    equal (UiTest.extractSkeleton state) [(2, 3)]
    state <- f
        (modify (Create.track UiTest.defaultBlockId 5 ">z" mempty) state)
        [((">a", []), []), ((">b", []), [("ba", [])])]
    -- It won't delete a track, but will clear the events out.
    equal (extract state)
        [ (">source", [])
        , (">a", []), ("ca", []), (">b", []), ("ba", []), (">z", [])
        ]
    equal (UiTest.extractSkeleton state) [(2, 3), (4, 5)]

    -- Only non-generated events are cleared.
    state <- f (mkblock []) [((">a", events "ab"), [])]
    state <- f (modify (UiTest.insertEvent 2 (1, 1, "z")) state) []
    equal (extract state) [(">source", []), (">a", [(1, 1, "z")])]

    -- Interacts properly with aded events.
    -- state <- f (mkblock []) [((">a", events "ab"), [])]

mkblock :: [UiTest.TrackSpec] -> Ui.State
mkblock = snd . UiTest.runMkblock . ((">source", []):)
    -- Add a track that 'derive_integrate' can set as the source since the
    -- integrated track source is required to exist.

deriveIntegrate :: Ui.State -> [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> Ui.State
deriveIntegrate state integrated = UiTest.exec state $ do
    itracks <- Block.blockIntegratedTracks <$> Ui.getBlock blockId
    let deriveItracks =
            [dests | (_, Block.DeriveDestinations dests) <- itracks]
    dests <- Merge.mergeTracks Merge.KeepTitles blockId
        (makeConvertTracks integrated)
        (fromMaybe [] $ Lists.head deriveItracks)
    Ui.modifyIntegratedTracks blockId $
        const [(UiTest.mkTid 1, Block.DeriveDestinations dests)]
    where blockId = UiTest.defaultBlockId

makeConvertTracks :: [(UiTest.TrackSpec, [UiTest.TrackSpec])]
    -> Convert.Tracks
makeConvertTracks =
    map $ \(note, controls) -> (convert note, map convert controls)
    where
    convert (title, events) = Convert.Track title
        (map (addStack title) $ map UiTest.makeEvent events)
    addStack title event = Event.stack_ #= Just stack $ event
        where stack = Event.Stack (Stack.call title) (Event.start event)

-- * score integrate

test_score_integrate_delete :: Test
test_score_integrate_delete = do
    let run state m = pure $ runScoreIntegrate 1 (modify m state)
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Lists.range_ 0 2) cs]
    state <- run Ui.empty $ UiTest.mkblock
        [(">", events "ab"), ("c1", events "12")]
    equal (UiTest.extractTracks state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extractSkeleton state) [(1, 2), (3, 4)]
    equal (lookup (UiTest.mkTid 3) (eEventStyles state)) $
        Just [("a", Style.StyleId 1), ("b", Style.StyleId 1)]

    -- Deleting the source note track disowns the destinations.
    state <- pure $ UiTest.exec state $
        mapM_ (Ui.removeTrack UiTest.defaultBlockId) [2, 1]
    equal (UiTest.extractTracks state)
        [ (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extractSkeleton state) [(1, 2)]
    equal (eIntegrateSkeleton state) []
    equal (lookup (UiTest.mkTid 3) (eEventStyles state)) $
        Just [("a", Style.StyleId 0), ("b", Style.StyleId 0)]

eEventStyles :: Ui.State -> [(TrackId, [(Text, Style.StyleId)])]
eEventStyles =
    map (second (map (\e -> (Event.text e, Event.style e))
        . Events.ascending . Track.trackEvents))
    . Map.toList . Ui.stateTracks

test_score_integrate :: Test
test_score_integrate = do
    -- make a block with the source, then modify
    let run state m = pure $ runScoreIntegrate 1 (modify m state)
    let events cs =
            [(n, 1, Text.singleton c) | (n, c) <- zip (Lists.range_ 0 2) cs]
        extract = UiTest.extractTracks

    state <- run Ui.empty $ UiTest.mkblock
        [(">", events "ab"), ("c1", events "12")]
    equal (extract state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "ab"), ("c1", events "12")
        ]
    equal (UiTest.extractSkeleton state) [(1, 2), (3, 4)]

    equal (eIntegrateSkeleton state)
        [(Config.scoreIntegrateSkeleton, [(1, 3)])]

    -- Ensure a merge is happening.
    state <- run state $ UiTest.insertEvent 3 (4, 1, "z")
    equal (extract state)
        [ (">", events "ab"), ("c1", events "12")
        , (">", events "abz"), ("c1", events "12")
        ]
    state <- run state $ UiTest.insertEvent 1 (2, 1, "x")
    equal (extract state)
        [ (">", events "ax"), ("c1", events "12")
        , (">", events "axz"), ("c1", events "12")
        ]
    equal (UiTest.extractSkeleton state) [(1, 2), (3, 4)]

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

eIntegrateSkeleton :: Ui.State -> [(Color.Color, [(TrackNum, TrackNum)])]
eIntegrateSkeleton =
    concatMap Block.integrateSkeleton . Map.elems . Ui.stateBlocks

-- | Reproduce enough of the respond cycle for score integration to work.
-- Hard coded to integrate track 1.
runScoreIntegrate :: TrackNum -> Ui.State -> Ui.State
runScoreIntegrate tracknum state = UiTest.exec state $ do
    itracks <- Block.blockIntegratedTracks <$> Ui.getBlock blockId
    let scoreItracks = [dests | (_, Block.ScoreDestinations dests) <- itracks]
    dests <- Merge.scoreMergeTracks blockId (UiTest.mkTid tracknum)
        (fromMaybe [] $ Lists.head scoreItracks)
    Ui.modifyIntegratedTracks blockId $
        const [(UiTest.mkTid 1, Block.ScoreDestinations dests)]
    where blockId = UiTest.defaultBlockId

-- * util

modify :: Ui.StateId a -> Ui.State -> Ui.State
modify action state = UiTest.exec state action

mkindex :: [Event] -> Block.EventIndex
mkindex = Merge.makeIndex . map mkevent

type Event = (ScoreTime, ScoreTime, Text, Maybe ScoreTime)

mkevent :: Event -> Event.Event
mkevent (start, dur, text, mbStack) = addStack (Event.event start dur text)
    where
    addStack = case mbStack of
        Nothing -> id
        Just pos ->
            Event.stack_ #= Just (Event.Stack (Stack.call (showt pos)) pos)

extractEvent :: Event.Event -> Event
extractEvent event =
    (Event.start event, Event.duration event, Event.text event,
        Event.stackKey <$> Event.stack event)

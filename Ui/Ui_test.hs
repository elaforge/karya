-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Ui.Ui_test where
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Ranges as Ranges
import qualified Cmd.Create as Create
import qualified Cmd.Integrate as Integrate
import qualified Ui.Block as Block
import qualified Ui.Event as Event
import qualified Ui.Events as Events
import qualified Ui.Skeleton as Skeleton
import qualified Ui.Track as Track
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest
import qualified Ui.Update as Update

import           Global
import           Types
import           Util.Test


test_modify_integrated_tracks :: Test
test_modify_integrated_tracks = do
    let ([tid], state) = UiTest.runMkblock [(">", [(0, 1, "a")])]
    state <- pure $ scoreIntegrate tid $ expectRight $ Ui.exec state $
        addIntegratedTrack tid
    equal (extractTracks state)
        [ (UiTest.mkTid 1, [("a", False)])
        , (UiTest.mkTid 2, [("a", True)])
        ]
    rightEqual (extractTracks <$> Ui.exec state clearScoreTrack)
        [ (UiTest.mkTid 1, [("a", False)])
        , (UiTest.mkTid 2, [("a", False)])
        ]

-- This is all pretty complicated just for modify_integrated_tracks, maybe
-- I should use this technique in Integrate_test.
extractTracks :: Ui.State -> [(TrackId, [(Text, Bool)])]
extractTracks = map (second extract) . Map.toList . Ui.stateTracks
    where
    extract = map event . Events.ascending . Track.trackEvents
    event e = (Event.text e, Maybe.isJust $ Event.stack e)

scoreIntegrate :: TrackId -> Ui.State -> Ui.State
scoreIntegrate tid state = UiTest.traceLogs logs state2
    where
    (logs, state2, _damage) = expectRight $
        Integrate.scoreIntegrate [update] state
    update = Update.Track tid Update.TrackAllEvents

addIntegratedTrack :: Ui.M m => TrackId -> m ()
addIntegratedTrack tid =
    Ui.modifyIntegratedTracks UiTest.defaultBlockId $
        ((tid, Block.ScoreDestinations []) :)

clearScoreTrack :: Ui.M m => m ()
clearScoreTrack =
    Ui.modifyIntegratedTracks UiTest.defaultBlockId (const [])

test_implicit_skeleton :: Test
test_implicit_skeleton = do
    let run = runImplicit
    let create tracknum title =
            Create.track UiTest.defaultBlockId tracknum title mempty
    equal (run ["tempo"] (create 2 ">")) [(1, 2)]
    equal (run ["tempo", ">"] (create 3 "*")) [(1, 2), (2, 3)]
    equal (run ["tempo", ">"] (create 3 ">")) [(1, 2), (1, 3)]

    let remove = Ui.removeTrack UiTest.defaultBlockId
    equal (run ["tempo", ">"] (remove 2)) []
    equal (run ["tempo", ">", ">"] (remove 2)) [(1, 2)]

    let title = Ui.setTrackTitle . UiTest.mkTid
    equal (run [">", "*", "c"] (pure ())) [(1, 2), (2, 3)]
    equal (run [">", "*", "c"] (title 3 ">")) [(1, 2)]
    equal (run [">", "*", ">"] (title 3 "c")) [(1, 2), (2, 3)]

runImplicit :: [Text] -> Ui.StateId a -> [(TrackNum, TrackNum)]
runImplicit titles action = UiTest.extractSkeleton $
    UiTest.exec (snd $ UiTest.runMkblock (map (, []) titles)) $ do
        Ui.setSkeletonConfig UiTest.defaultBlockId Block.Implicit
        action

test_toggle_skeleton_edge :: Test
test_toggle_skeleton_edge = do
    let bid = UiTest.defaultBlockId
    let run allow edge = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock [("t" <> showt n, []) | n <- [1..3]]
            Ui.setSkeleton bid Skeleton.empty
            Ui.addEdges bid [(1, 2), (2, 3)]
            Ui.toggleSkeletonEdge allow bid edge
            Skeleton.flatten <$> Ui.getSkeleton bid
    equal (run True (1, 3)) (Right [(1, 2), (1, 3), (2, 3)])
    equal (run False (1, 3)) (Right [(1, 2), (1, 3)])
    equal (run True (2, 3)) (Right [(1, 2)])
    equal (run False (2, 3)) (Right [(1, 2)])

test_skeleton_cycles :: Test
test_skeleton_cycles = do
    let bid = UiTest.defaultBlockId
    let run ntracks m = first pretty $ Ui.eval Ui.empty $ do
            UiTest.mkblock [("t" <> showt n, []) | n <- [0..ntracks]]
            Ui.setSkeleton bid Skeleton.empty
            m
    equal (run 1 (Ui.toggleSkeletonEdge False bid (1, 1))) (Right False)
    leftLike (run 1 (Ui.toggleSkeletonEdge False bid (1, 10)))
        "toggle: tracknum out of range"
    leftLike (run 1 (Ui.toggleSkeletonEdge False bid (0, 1)))
        "toggle: edge points to non-event track"
    equal (run 1 (Ui.toggleSkeletonEdge False bid (1, 2))) (Right True)
    leftLike (run 1 (Ui.addEdges bid [(1, 1)]))
        "would have caused a cycle"
    leftLike (run 1 (Ui.addEdges bid [(1, 2), (2, 1)]))
        "would have caused a cycle"
    leftLike (run 1 (Ui.addEdges bid [(0, 1), (1, 2)]))
        "edge points to non-event track"
    equal (run 1 (Ui.addEdges bid [(1, 2)])) (Right ())

test_calculate_damage :: Test
test_calculate_damage = do
    let f xs ys = Ui.calculateDamage (mkevents xs) (mkevents ys)
    equal (f [(0, 1, "a")] [(0, 1, "a")]) Ranges.nothing
    equal (f [(0, 1, "a")] [(0, 1, "b")]) (Ranges.range 0 1)
    equal (f [(0, 1, "a")] [(2, 1, "a")]) (Ranges.ranges [(0, 1), (2, 3)])
    equal (f [(0, 1, "a"), (3, 1, "c")] [(2, 1, "b")])
        (Ranges.ranges [(0, 1), (2, 4)])
    equal (f [(0, 1, "a")] [(0, 2, "a")]) (Ranges.range 0 2)

mkevents :: [UiTest.EventSpec] -> Events.Events
mkevents = Events.fromList . map UiTest.makeEvent

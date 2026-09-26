-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.TimeStep_test where
import qualified Util.Lists as Lists
import qualified Cmd.TimeStep as TimeStep
import           Cmd.TimeStep (MarklistMatch(..), Step(..), Tracks(..))
import qualified Ui.Meter.Meter as Meter
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_show_parse_time_step :: Test
test_show_parse_time_step = do
    let f step = (trip step, Right (mk step))
        trip = TimeStep.parseTimeStep . TimeStep.showTimeStep . mk
        mk = TimeStep.fromList
    uncurry equal (f [Duration 2])
    uncurry equal (f [Duration 0.5])
    uncurry equal (f [RelativeMark (NamedMarklists ["hi", "there"]) Meter.E])
    uncurry equal (f [RelativeMark AllMarklists Meter.E])
    uncurry equal (f [BlockEdge])

    uncurry equal (f [EventStart CurrentTrack])
    uncurry equal (f [EventStart AllTracks])
    uncurry equal (f [EventStart (TrackNums [1, 2])])
    uncurry equal (f [EventEnd CurrentTrack])
    uncurry equal (f [EventEnd AllTracks])
    uncurry equal (f [EventEnd (TrackNums [1, 2])])

    -- Test AbsoluteMark especially well because it's at the end of the parse
    -- and has no special prefix.
    uncurry equal (f [AbsoluteMark (NamedMarklists ["hi"]) Meter.S])
    uncurry equal (f [AbsoluteMark AllMarklists Meter.T32])
    forM_ Meter.allRanks $ \rank ->
        uncurry equal (f [AbsoluteMark AllMarklists rank])
    forM_ Meter.allRanks $ \rank ->
        uncurry equal (f [AbsoluteMark AllMarklists rank])
    forM_ Meter.allRanks $ \rank ->
        uncurry equal (f [AbsoluteMark AllMarklists rank])
    uncurry equal
        (f [Duration 2, RelativeMark (NamedMarklists ["hi", "there"]) Meter.H])

test_parse_time_step :: Test
test_parse_time_step = do
    let f = TimeStep.parseTimeStep
    leftLike (f "sectionaoeu") "unexpected"
    rightEqual (f "section") $
        TimeStep.timeStep (AbsoluteMark AllMarklists Meter.Section)

test_snap :: Test
test_snap = do
    let f step prev ps = UiTest.eval defaultUiState $
            mapM (TimeStep.snap (TimeStep.timeStep step)
                UiTest.defaultBlockId 1 (Just prev)) ps
    equal (f (Duration 3) 3 (Lists.range 0 6 1)) [0, 0, 0, 3, 3, 3, 6]
    equal (f (AbsoluteMark AllMarklists Meter.W) 3 (Lists.range 0 6 1))
        [0, 0, 0, 0, 4, 4, 4]
    equal (f (RelativeMark AllMarklists Meter.W) 1 (Lists.range 0 6 1))
        [-3, 1, 1, 1, 1, 5, 5]
    equal (f (RelativeMark AllMarklists Meter.W) 5 (Lists.range 0 6 1))
        [-2, -2, 2, 2, 2, 5, 5]
        -- before Meter change: [0, 1, 1, 1, 1, 5, 5]
    equal (f (EventStart AllTracks) 3 (Lists.range 0 6 1))
        [0, 0, 2, 2, 2, 5, 5]

test_ascending_descending_points :: Test
test_ascending_descending_points = do
    let f = ascendDescend defaultUiState
    equal (f 3 (Duration 32)) ([3], [3])
    equal (f 0 (Duration 3)) ([0], [0, 3, 6])
    equal (f 3 (Duration 3)) ([3, 0], [3, 6])
    equal (f 1 (Duration 3)) ([1],  [1, 4, 7])

    equal (f 0 (AbsoluteMark AllMarklists Meter.W)) ([0], [0, 4, 7])
    equal (f 1 (AbsoluteMark AllMarklists Meter.W)) ([0], [4, 7])
    equal (f 0 (AbsoluteMark AllMarklists Meter.Q)) ([0], Lists.range 0 7 1)
    equal (f 1 (AbsoluteMark AllMarklists Meter.Q)) ([1, 0], Lists.range 1 7 1)

    equal (f 0 (RelativeMark AllMarklists Meter.W)) ([0], [0, 4, 7])
    equal (f 1 (RelativeMark AllMarklists Meter.W)) ([1, -3], [1, 5, 8])
    equal (f 4 (RelativeMark AllMarklists Meter.W)) ([4, 0], [4, 7])
    equal (f 4 (AbsoluteMark AllMarklists Meter.W)) ([4, 0], [4, 7])
    -- 5 gives 7, 4 0 -> 5, 2, -2
    equal (f 5 (RelativeMark AllMarklists Meter.W)) ([5, 2, -2], [5, 8])
    -- before Meter change: ([5, 1], [5])

    equal (f 1 BlockEdge) ([0], [7])
    equal (f 0 (EventStart CurrentTrack)) ([0], [0, 2])
    equal (f 1 (EventStart CurrentTrack)) ([0], [2])
    equal (f 0 (EventStart AllTracks)) ([0], [0, 2, 5])
    equal (f 1 (EventStart AllTracks)) ([0], [2, 5])
    equal (f 3 (EventStart AllTracks)) ([2, 0], [5])
    equal (f 1 (EventStart (TrackNums [1]))) ([0], [2])
    equal (f 1 (EventStart (TrackNums [1, 2]))) ([0], [2, 5])
    equal (f 0 (EventEnd CurrentTrack)) ([], [1, 3])
    equal (f 4 (EventEnd CurrentTrack)) ([3, 1], [])

test_get_points_from :: Test
test_get_points_from = do
    -- Ensure get_points_from is the same as merging the points myself.
    let state = defaultUiState
    let f start tstep = UiTest.eval state $ do
            desc <- TimeStep.getPointsFrom TimeStep.Rewind
                UiTest.defaultBlockId 1 start tstep
            asc <- TimeStep.getPointsFrom TimeStep.Advance
                UiTest.defaultBlockId 1 start tstep
            pure (desc, asc)
    let ascDesc start step1 step2 =
            (mergeDesc desc1 desc2, mergeAsc asc1 asc2)
            where
            (desc1, asc1) = ascendDescend state start step1
            (desc2, asc2) = ascendDescend state start step2
        mergeAsc xs ys = Lists.dropDups id $ Lists.merge xs ys
        mergeDesc xs ys =
            Lists.dropDups id $ Lists.mergeBy (flip compare) xs ys
    let merged start step1 step2 = (f start (TimeStep.fromList [step1, step2]),
            ascDesc start step1 step2)
    uncurry equal $ merged 0 (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged 2 (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged 0 (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $ merged 2 (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $
        merged 0 (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))
    uncurry equal $
        merged 2 (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))

ascendDescend :: Ui.State -> TrackTime -> Step -> ([TrackTime], [TrackTime])
ascendDescend state start step = UiTest.eval state $ do
    desc <- TimeStep.descendingPoints
        UiTest.defaultBlockId 1 start step
    asc <- TimeStep.ascendingPoints
        UiTest.defaultBlockId 1 start step
    pure (desc, asc)

defaultUiState :: Ui.State
defaultUiState = UiTest.exec Ui.empty $ do
    UiTest.mkblock
        [ (">", [(0, 1, "a"), (2, 1, "b")])
        , ("c", [(0, 0, "1"), (5, 0, "2")])
        ]
    Ui.modifyRuler UiTest.defaultRulerId $
        const $ Right (UiTest.mkruler44 7 1)

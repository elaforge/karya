-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.TimeStep_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.TimeStep as TimeStep
import Cmd.TimeStep (Step(..), MarklistMatch(..), Tracks(..))
import Types


test_show_parse_time_step = do
    let f step = (trip step, Right (mk step))
        trip = TimeStep.parse_time_step . TimeStep.show_time_step . mk
        mk = TimeStep.from_list
    uncurry equal (f [Duration 2])
    uncurry equal (f [Duration 0.5])
    uncurry equal (f [RelativeMark (NamedMarklists ["hi", "there"]) 5])
    uncurry equal (f [RelativeMark AllMarklists 5])
    uncurry equal (f [BlockEdge])

    uncurry equal (f [EventStart CurrentTrack])
    uncurry equal (f [EventStart AllTracks])
    uncurry equal (f [EventStart (TrackNums [1, 2])])
    uncurry equal (f [EventEnd CurrentTrack])
    uncurry equal (f [EventEnd AllTracks])
    uncurry equal (f [EventEnd (TrackNums [1, 2])])

    -- Test AbsoluteMark especially well because it's at the end of the parse
    -- and has no special prefix.
    uncurry equal (f [AbsoluteMark (NamedMarklists ["hi"]) 6])
    uncurry equal (f [AbsoluteMark AllMarklists 7])
    forM_ [0..7] $ \rank -> uncurry equal (f [AbsoluteMark AllMarklists rank])
    forM_ [0..7] $ \rank -> uncurry equal (f [AbsoluteMark AllMarklists rank])
    forM_ [0..7] $ \rank -> uncurry equal (f [AbsoluteMark AllMarklists rank])
    uncurry equal (f
        [Duration 2, RelativeMark (NamedMarklists ["hi", "there"]) 5])

test_snap = do
    let f step prev ps = UiTest.eval default_ui_state $
            mapM (TimeStep.snap (TimeStep.time_step step)
                UiTest.default_block_id 1 (Just prev)) ps
    equal (f (Duration 3) 3 (Seq.range 0 6 1)) [0, 0, 0, 3, 3, 3, 6]
    equal (f (AbsoluteMark AllMarklists UiTest.r_1) 3 (Seq.range 0 6 1))
        [0, 0, 0, 0, 4, 4, 4]
    equal (f (RelativeMark AllMarklists UiTest.r_1) 1 (Seq.range 0 6 1))
        [-3, 1, 1, 1, 1, 5, 5]
    equal (f (RelativeMark AllMarklists UiTest.r_1) 5 (Seq.range 0 6 1))
        [0, 1, 1, 1, 1, 5, 5]
    equal (f (EventStart AllTracks) 3 (Seq.range 0 6 1))
        [0, 0, 2, 2, 2, 5, 5]

test_ascending_descending_points = do
    let f = ascend_descend default_ui_state
    equal (f 3 (Duration 32)) ([3], [3])
    equal (f 0 (Duration 3)) ([0], [0, 3, 6])
    equal (f 3 (Duration 3)) ([3, 0], [3, 6])
    equal (f 1 (Duration 3)) ([1],  [1, 4, 7])

    equal (f 0 (AbsoluteMark AllMarklists UiTest.r_1)) ([0], [0, 4])
    equal (f 1 (AbsoluteMark AllMarklists UiTest.r_1)) ([0], [4])
    equal (f 0 (AbsoluteMark AllMarklists UiTest.r_4)) ([0], Seq.range 0 7 1)
    equal (f 1 (AbsoluteMark AllMarklists UiTest.r_4)) ([1, 0], Seq.range 1 7 1)

    equal (f 0 (RelativeMark AllMarklists UiTest.r_1)) ([0], [0, 4])
    equal (f 1 (RelativeMark AllMarklists UiTest.r_1)) ([1, -3], [1, 5])
    equal (f 4 (RelativeMark AllMarklists UiTest.r_1)) ([4, 0], [4])
    equal (f 5 (RelativeMark AllMarklists UiTest.r_1)) ([5, 1], [5])
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

test_get_points_from = do
    -- Ensure get_points_from is the same as merging the points myself.
    let state = default_ui_state
    let f start tstep = UiTest.eval state $ do
            desc <- TimeStep.get_points_from TimeStep.Rewind
                UiTest.default_block_id 1 start tstep
            asc <- TimeStep.get_points_from TimeStep.Advance
                UiTest.default_block_id 1 start tstep
            return (desc, asc)
    let asc_desc start step1 step2 =
            (merge_desc desc1 desc2, merge_asc asc1 asc2)
            where
            (desc1, asc1) = ascend_descend state start step1
            (desc2, asc2) = ascend_descend state start step2
        merge_asc xs ys = Seq.drop_dups id $ Seq.merge xs ys
        merge_desc xs ys = Seq.drop_dups id $ Seq.merge_by (flip compare) xs ys
    let merged start step1 step2 = (f start (TimeStep.from_list [step1, step2]),
            asc_desc start step1 step2)
    uncurry equal $ merged 0 (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged 2 (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged 0 (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $ merged 2 (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $
        merged 0 (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))
    uncurry equal $
        merged 2 (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))

ascend_descend :: State.State -> TrackTime -> Step -> ([TrackTime], [TrackTime])
ascend_descend state start step = UiTest.eval state $ do
    desc <- TimeStep.descending_points
        UiTest.default_block_id 1 start step
    asc <- TimeStep.ascending_points
        UiTest.default_block_id 1 start step
    return (desc, asc)

default_ui_state :: State.State
default_ui_state = UiTest.exec State.empty $ do
    UiTest.mkblock (UiTest.default_block_name,
        [ (">", [(0, 1, "a"), (2, 1, "b")])
        , ("c", [(0, 0, "1"), (5, 0, "2")])
        ])
    State.modify_ruler UiTest.default_ruler_id $
        const $ Right (UiTest.mkruler_44 7 1)

test_drop_before = do
    let f n = Seq.head $ TimeStep.drop_before n [1..4]
    equal (f 0) (Just 1)
    equal (f 1) (Just 1)
    equal (f 1.5) (Just 1)
    equal (f 2) (Just 2)

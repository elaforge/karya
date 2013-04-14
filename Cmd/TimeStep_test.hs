module Cmd.TimeStep_test where
import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Cmd.TimeStep as TimeStep
import Cmd.TimeStep (Step(..), MarklistMatch(..), Tracks(..))


test_show_parse_time_step = do
    let f step = (trip step, Right (mk step))
        trip = TimeStep.parse_time_step . txt . TimeStep.show_time_step . mk
        mk = TimeStep.TimeStep
    uncurry equal (f [(Duration 2, 0)])
    uncurry equal (f [(Duration 0.5, 1)])
    uncurry equal (f [(RelativeMark (NamedMarklists ["hi", "there"]) 5, 2)])
    uncurry equal (f [(RelativeMark AllMarklists 5, 2)])
    uncurry equal (f [(BlockEnd, 0)])

    uncurry equal (f [(EventStart CurrentTrack, 1)])
    uncurry equal (f [(EventStart AllTracks, 1)])
    uncurry equal (f [(EventStart (TrackNums [1, 2]), 1)])
    uncurry equal (f [(EventEnd CurrentTrack, 1)])
    uncurry equal (f [(EventEnd AllTracks, 1)])
    uncurry equal (f [(EventEnd (TrackNums [1, 2]), 1)])

    -- Test AbsoluteMark especially well because it's at the end of the parse
    -- and has no special prefix.
    uncurry equal (f [(AbsoluteMark (NamedMarklists ["hi"]) 6, 2)])
    uncurry equal (f [(AbsoluteMark AllMarklists 7, 0)])
    forM_ [0..7] $ \rank ->
        uncurry equal (f [(AbsoluteMark AllMarklists rank, 0)])
    forM_ [0..7] $ \rank ->
        uncurry equal (f [(AbsoluteMark AllMarklists rank, 1)])
    forM_ [0..7] $ \rank ->
        uncurry equal (f [(AbsoluteMark AllMarklists rank, -1)])

    uncurry equal (f
        [(Duration 2, 0), (RelativeMark (NamedMarklists ["hi", "there"]) 5, 2)])

test_get_points = do
    let ustate = UiTest.exec State.empty $ do
            UiTest.mkblock (UiTest.default_block_name,
                [ (">", [(0, 1, "a"), (2, 1, "b")])
                , ("c", [(0, 0, "1"), (5, 0, "2")])
                ])
            State.modify_ruler UiTest.default_ruler_id $
                const (UiTest.mkruler 7 1)
        mk steps ts = TimeStep.TimeStep [(ts, steps)]
    let f pos step = UiTest.eval ustate $
            TimeStep.get_points step UiTest.default_block_id 1 pos
    equal (f 0 (mk 0 (Duration 32))) (Just [0])
    equal (f 3 (mk 0 (Duration 32))) (Just [3])
    equal (f 0 (mk 0 (Duration 3))) (Just [0, 3, 6])
    equal (f 1 (mk 0 (Duration 3))) (Just [1, 4, 7])
    equal (f 0 (mk 1 (Duration 3))) (Just [0, 6])
    equal (f 0 (mk 0 (AbsoluteMark AllMarklists UiTest.r_1))) (Just [0, 4])
    equal (f 1 (mk 0 (AbsoluteMark AllMarklists UiTest.r_1))) (Just [0, 4])
    equal (f 0 (mk 0 (AbsoluteMark AllMarklists UiTest.r_4)))
        (Just (Seq.range 0 7 1))
    equal (f 0 (mk 1 (AbsoluteMark AllMarklists UiTest.r_4)))
        (Just [0, 2, 4, 6])
    equal (f 0 (mk 0 (RelativeMark AllMarklists UiTest.r_1))) (Just [0, 4])
    equal (f 1 (mk 0 (RelativeMark AllMarklists UiTest.r_1))) (Just [1, 5])
    equal (f 0 (mk 0 BlockEnd)) (Just [0, 7])
    equal (f 0 (mk 0 (EventStart CurrentTrack))) (Just [0, 2])
    equal (f 0 (mk 0 (EventStart AllTracks))) (Just [0, 2, 5])
    equal (f 0 (mk 0 (EventStart (TrackNums [1])))) (Just [0, 2])
    equal (f 0 (mk 0 (EventStart (TrackNums [1, 2])))) (Just [0, 2, 5])
    equal (f 0 (mk 0 (EventEnd CurrentTrack))) (Just [1, 3])
    -- merged
    let merged ts1 ts2 =
            (f 0 step, Seq.drop_dups id <$>
                (Seq.merge <$> f 0 (mk 0 ts1) <*> f 0 (mk 0 ts2)))
            where
            step = TimeStep.TimeStep [(ts1, 0), (ts2, 0)]
    uncurry equal $ merged (EventEnd AllTracks) (EventStart AllTracks)
    uncurry equal $ merged (EventEnd AllTracks) (EventStart (TrackNums [1]))
    uncurry equal $
        merged (EventStart (TrackNums [1])) (EventEnd (TrackNums [2]))

test_step_from_points = do
    let f n pos = TimeStep.step_from_points n pos (Seq.range 1 4 1)
    equal (f 1 0) (Just 1)
    equal (f 2 0) (Just 2)
    equal (f 1 1) (Just 2)
    equal (f 1 1.5) (Just 2)
    equal (f 2 1) (Just 3)
    equal (f 4 1) Nothing
    equal (f 1 4) Nothing

    equal (f 0 0) (Just 1)
    equal (f 0 1) (Just 1)
    equal (f 0 1.5) (Just 1)
    equal (f 0 5) (Just 4)

    equal (f (-1) 0) Nothing
    equal (f (-1) 1) Nothing
    equal (f (-1) 1.5) (Just 1)
    equal (f (-1) 2) (Just 1)
    equal (f (-2) 2) Nothing
    equal (f (-1) 5) (Just 4)

test_find_before_equal = do
    let f n = TimeStep.find_before_equal n [1..4]
    equal (f 0) Nothing
    equal (f 1) (Just 1)
    equal (f 1.5) (Just 1)
    equal (f 2) (Just 2)

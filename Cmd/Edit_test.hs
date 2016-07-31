-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Edit_test where
import qualified Util.Seq as Seq
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit

import Global
import Types


test_split_events = do
    let run events sel = e_start_dur $
            run_sel (start_dur_events events) Edit.cmd_split_events 1 sel sel
    equal (run [(0, 4)] 0) $ Right ([(0, 4)], [])
    equal (run [(0, 4)] 2) $ Right ([(0, 2), (2, 2)], [])
    equal (run [(2, 2)] 3) $ Right ([(2, 1), (3, 1)], [])
    equal (run [(0, 4)] 4) $ Right ([(0, 4)], [])

    equal (run [(4, -4)] 0) $ Right ([(4, -4)], [])
    equal (run [(4, -4)] 2) $ Right ([(2, -2), (4, -2)], [])
    equal (run [(4, -4)] 4) $ Right ([(4, -4)], [])

test_set_duration = do
    -- I don't know why this function is so hard to get right, but it is.
    let run events start end = e_start_dur $
            run_sel (start_dur_events events) Edit.cmd_set_duration 1 start end
    -- |--->   |---> => take pre
    let events = [(0, 2), (4, 2)]
    equal [run events p p | p <- Seq.range 0 6 1] $ map (Right . (,[]))
        [ [(0, 2), (4, 2)]
        , [(0, 1), (4, 2)]
        , [(0, 2), (4, 2)]
        , [(0, 3), (4, 2)]
        , [(0, 4), (4, 2)]
        , [(0, 2), (4, 1)]
        , [(0, 2), (4, 2)]
        ]
    -- No effect on zero-dur events.
    equal (run [(0, 0), (1, 0)] 0.5 3) $ Right ([(0, 0), (1, 0)], [])
    -- Non-point selection affects all selected events.
    equal (run [(0, 0.5), (1, 0.5)] 0 3) $ Right ([(0, 1), (1, 2)], [])

    -- <---|   <---| => take post
    let events = [(2, -2), (6, -2)]
    equal [run events p p | p <- Seq.range 0 6 1] $ map (Right . (,[]))
        [ [(2, -2), (6, -2)]
        , [(2, -1), (6, -2)]
        , [(2, -2), (6, -4)]
        , [(2, -2), (6, -3)]
        , [(2, -2), (6, -2)]
        , [(2, -2), (6, -1)]
        , [(2, -2), (6, -2)]
        ]
    -- No effect on zero-dur events.
    equal (run [(0, -0), (1, -0)] 0.5 3) $ Right ([(0, -0), (1, -0)], [])

    -- Non-point selection affects all selected events.
    equal (run [(1, -0.5), (2, -0.5)] 0 2) $ Right ([(1, -1), (2, -1)], [])

    -- 0 1 2 3 4 5 6
    -- |--->   <---| => take closer, favor pre
    let events = [(0, 2), (6, -2)]
    equal [run events p p | p <- Seq.range 0 6 1] $ map (Right . (,[]))
        [ [(0, 2), (6, -2)]
        , [(0, 1), (6, -2)]
        , [(0, 2), (6, -2)]
        , [(0, 3), (6, -2)]
        , [(0, 2), (6, -2)]
        , [(0, 2), (6, -1)]
        , [(0, 2), (6, -2)]
        ]

    -- 0 1 2 3 4 5 6
    -- <---|   |---> => in the middle, do nothing
    let events = [(2, -2), (4, 2)]
    equal [run events p p | p <- Seq.range 0 6 1] $ map (Right . (,[]))
        [ [(2, -2), (4, 2)]
        , [(2, -1), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 1)]
        , [(2, -2), (4, 2)]
        ]

test_move_events = do
    let run cmd events start end =
            e_start_dur_text $ run_events_sel cmd events start end
    let fwd = Edit.cmd_move_event_forward
        bwd = Edit.cmd_move_event_backward
    -- Clipped to the end of the ruler.
    equal (run fwd [(0, 2)] 1 1) $ Right ([(1, 1, "a")], [])
    equal (run bwd [(2, -1)] 0 0) $ Right ([(1, -1, "a")], [])

test_insert_time = do
    let run events start end = e_start_dur_text $
            run_events_sel Edit.cmd_insert_time events start end
    equal (run [(0, 2), (3, 0)] 0 0) $ Right ([(1, 2, "a")], [])
    -- (1, 2) is shortened.
    equal (run [(1, 2), (3, 0)] 0 0) $ Right ([(2, 1, "a")], [])
    equal (run [(0, 0), (3, 0)] 0 0) $ Right ([(1, 0, "a")], [])
    equal (run [(0, -0), (3, 0)] 0 0) $ Right ([(1, -0, "a")], [])
    equal (run [(2, -0), (3, 0)] 0 0) $ Right ([(3, 0, "a")], [])

    -- Extend the duration of an event from inside it.
    equal (run [(0, 2), (8, 0)] 1 1) $ Right ([(0, 3, "a")], [])
    equal (run [(2, -2), (8, 0)] 1 1) $ Right ([(3, -3, "a")], [])

    equal (run [(0, 2), (2, 2), (4, 2)] 3 3) $
        Right ([(0, 2, "a"), (2, 3, "b"), (5, 1, "c")], [])
    equal (run [(0, 2), (2, 2), (4, 2)] 3 5) $
        Right ([(0, 2, "a"), (2, 4, "b"), (6, 0, "c")], [])

test_delete_time = do
    let run events start end = e_start_dur $
            run_events_sel Edit.cmd_delete_time events start end
    -- positive
    equal (map (run [(2, 2)] 0) (Seq.range 0 3 1)) $ map (Right . (,[]))
        [ [(1, 2)]
        , [(1, 2)]
        , [(0, 2)]
        , []
        ]
    equal (map (run [(2, 2)] 3) [4, 5]) $ map (Right . (,[]))
        [ [(2, 1)]
        , [(2, 1)]
        ]
    equal (run [(2, 2)] 4 5) $ Right ([(2, 2)], [])

    -- negative
    equal (map (run [(4, -2)] 0) (Seq.range 0 4 1)) $ map (Right . (,[]))
        [ [(3, -2)]
        , [(3, -2)]
        , [(2, -2)]
        , [(1, -1)]
        , []
        ]
    -- zero dur doesn't get deleted when it touches the point.
    equal (map (run [(2, -0)] 0) (Seq.range 1 3 1)) $ map (Right . (,[]))
        [ [(1, -0)]
        , [(0, -0)]
        , []
        ]
    equal (run [(4, -2)] 4 6) $ Right ([(4, -2)], [])

test_toggle_zero_timestep = do
    let run tracks start end = e_start_dur $
            run_events_sel Edit.cmd_toggle_zero_timestep tracks start end
    equal (run [(0, 2)] 1 1) $ Right ([(0, 0)], [])
    equal (run [(0, 0), (2, 0), (4, 0)] 0 4) $
        Right ([(0, 2), (2, 2), (4, 0)], [])
    equal (run [(0, -0), (2, -0), (4, 0)] 0 4) $
        Right ([(2, -2), (4, -2), (4, 0)], [])
    equal (run [(2, -2), (4, -2)] 1 1) $ Right ([(0, -0), (4, -2)], [])
    -- Expand to a timestep.
    equal (run [(0, 0), (4, 0)] 1 1) $ Right ([(0, 1), (4, 0)], [])
    equal (run [(1, -0), (4, -0)] 1 1) $ Right ([(2, -1), (4, -0)], [])

test_join_events = do
    let run tracks start end = e_start_dur_text $
            run_events_sel Edit.cmd_join_events tracks start end
    equal (run [(0, 2), (2, 2)] 1 1) $ Right ([(0, 4, "a")], [])
    equal (run [(0, 2), (2, 2), (6, 2)] 2 8) $
        Right ([(0, 2, "a"), (2, 6, "b")], [])
    equal (run [(0, 0), (2, 0)] 1 1) $ Right ([(0, 0, "a")], [])
    equal (run [(0, -0), (2, -0)] 1 1) $ Right ([(2, -0, "b")], [])
    equal (run [(2, -2), (4, -2)] 1 1) $ Right ([(4, -4, "b")], [])
    -- Don't try for mixed polarities.
    equal (run [(0, 2), (4, -2)] 1 1) $ Right ([(0, 2, "a"), (4, -2, "b")], [])

-- * util

-- | Run with events and a selection.
run_events_sel :: Cmd.CmdId a -> [(TrackTime, TrackTime)]
    -> TrackTime -> TrackTime
    -> CmdTest.Result a
run_events_sel cmd events start end =
    CmdTest.run_tracks_ruler (start_dur_events events) $
    with_sel 1 start end cmd

run_sel :: [UiTest.TrackSpec] -> Cmd.CmdId a -> TrackNum -> ScoreTime
    -> ScoreTime -> CmdTest.Result a
run_sel tracks cmd tracknum start end = CmdTest.run_tracks tracks $
    with_sel tracknum start end cmd

with_sel :: Cmd.M m => TrackNum -> ScoreTime -> ScoreTime -> m a -> m a
with_sel tracknum start end = (CmdTest.set_sel tracknum start tracknum end >>)

start_dur_events :: [(TrackTime, TrackTime)] -> [UiTest.TrackSpec]
start_dur_events events =
    [(">", [(start, dur, c:"") | ((start, dur), c) <- zip events ['a'..'z']])]

e_start_dur :: CmdTest.Result a
    -> Either String ([(ScoreTime, ScoreTime)], [String])
e_start_dur = fmap (first (map (\(s, d, _) -> (s, d)))) . e_start_dur_text

e_start_dur_text :: CmdTest.Result a
    -> Either String ([(ScoreTime, ScoreTime, String)], [String])
e_start_dur_text = fmap (first (snd . head)) . CmdTest.e_tracks

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
    let run events sel = e_track $
            run_sel (null_events events) Edit.cmd_split_events 1 sel sel
    equal (run [(0, 4)] 0) $ Right ([(0, 4)], [])
    equal (run [(0, 4)] 2) $ Right ([(0, 2), (2, 2)], [])
    equal (run [(2, 2)] 3) $ Right ([(2, 1), (3, 1)], [])
    equal (run [(0, 4)] 4) $ Right ([(0, 4)], [])

    equal (run [(4, -4)] 0) $ Right ([(4, -4)], [])
    equal (run [(4, -4)] 2) $ Right ([(2, -2), (4, -2)], [])
    equal (run [(4, -4)] 4) $ Right ([(4, -4)], [])

null_events :: [(ScoreTime, ScoreTime)] -> [UiTest.TrackSpec]
null_events events = [(">", [(start, dur, "") | (start, dur) <- events])]

e_track :: CmdTest.Result a
    -> Either String ([(ScoreTime, ScoreTime)], [String])
e_track = fmap (first (map range . snd . head)) . CmdTest.e_tracks
    where range (start, dur, _) = (start, dur)

test_set_duration = do
    -- I don't know why this function is so hard to get right, but it is.
    let run events start end = e_track $
            run_sel (null_events events) Edit.cmd_set_duration 1 start end
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
    let run cmd events sel = e_track $
            CmdTest.run_tracks_ruler (null_events events) $
            with_sel 1 sel sel cmd
    let fwd = Edit.cmd_move_event_forward
    -- Clipped to the end of the ruler.
    equal (run fwd [(0, 2)] 1) $ Right ([(1, 1)], [])
    let bwd = Edit.cmd_move_event_backward
    equal (run bwd [(2, -1)] 0) $ Right ([(1, -1)], [])


run_sel :: [UiTest.TrackSpec] -> Cmd.CmdId a -> TrackNum -> ScoreTime
    -> ScoreTime -> CmdTest.Result a
run_sel tracks cmd tracknum start end = CmdTest.run_tracks tracks $
    with_sel tracknum start end cmd

with_sel :: Cmd.M m => TrackNum -> ScoreTime -> ScoreTime -> m a -> m a
with_sel tracknum start end = (CmdTest.set_sel tracknum start tracknum end >>)

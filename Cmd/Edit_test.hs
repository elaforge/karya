-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Edit_test where
import Util.Control
import Util.Test
import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit

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
    let run events start end = e_track $
            run_sel (null_events events) Edit.cmd_set_duration 1 start end
    equal (run [(0, 1)] 2 2) $ Right ([(0, 2)], [])
    -- Affects the previous event.
    equal (run [(0, 0.5), (1, 1)] 1 1) $ Right ([(0, 1), (1, 1)], [])
    -- No effect on zero-dur events.
    equal (run [(0, 0), (1, 0)] 0.5 3) $ Right ([(0, 0), (1, 0)], [])
    -- Non-point selection affects all selected events.
    equal (run [(0, 0.5), (1, 0.5)] 0 3) $ Right ([(0, 1), (1, 2)], [])

    -- Negative events, should be symmetrical with positive cases.
    equal (run [(2, -1)] 0 0) $ Right ([(2, -2)], [])
    -- Affects next event.
    equal (run [(1, -1), (2, -0.5)] 1 1) $ Right ([(1, -1), (2, -1)], [])
    -- No effect on zero-dur events.
    equal (run [(0, -0), (1, -0)] 0.5 3) $ Right ([(0, -0), (1, -0)], [])
    -- Non-point selection affects all selected events.
    equal (run [(1, -0.5), (2, -0.5)] 0 2) $ Right ([(1, -1), (2, -1)], [])

    -- Mixed negative and positive.
    -- When nothing overlaps, positive wins.
    equal (run [(0, 1), (3, -1)] 2 2) $ Right ([(0, 2), (3, -1)], [])
    -- Negative wins if it overlaps.
    equal (run [(0, 1), (3, -2)] 2 2) $ Right ([(0, 1), (3, -1)], [])
    -- Selection extends them all.
    equal (run [(2, -1), (3, 1)] 0 5) $ Right ([(2, -2), (3, 2)], [])
    -- The negative event loses and becomes -0, which is not ideal, but there's
    -- no good answer here.
    equal (run [(0, 1), (3, -1)] 0 3) $ Right ([(0, 3), (3, -0)], [])

test_move_events = do
    let run cmd events sel = e_track $
            CmdTest.run_tracks_ruler (null_events events) $
            with_sel 1 sel sel cmd
        fwd = Edit.cmd_move_event_forward
    -- Clipped to the end of the ruler.
    equal (run fwd [(0, 2)] 1) $ Right ([(1, 1)], [])
    let bwd = Edit.cmd_move_event_backward
    equal (run bwd [(2, -2)] 1) $ Right ([(1, -1)], [])


run_sel :: [UiTest.TrackSpec] -> Cmd.CmdId a -> TrackNum -> ScoreTime
    -> ScoreTime -> CmdTest.Result a
run_sel tracks cmd tracknum start end = CmdTest.run_tracks tracks $
    with_sel tracknum start end cmd

with_sel :: Cmd.M m => TrackNum -> ScoreTime -> ScoreTime -> m a -> m a
with_sel tracknum start end = (CmdTest.set_sel tracknum start tracknum end >>)

test_record_recent = do
    let f recent = Edit.record_recent recent
        n txt = Cmd.RecentGenerator txt False
        t txt = Cmd.RecentTransform txt False
    -- Generators get replaced, and are always in slot 1.
    equal (f (n "a") []) [(1, n "a")]
    equal (f (n "b") [(1, n "a"), (2, t "t")]) [(1, n "b"), (2, t "t")]

    -- Transformers go in 2..4 and cycle.
    equal (f (t "a") []) [(2, t "a")]
    equal (f (t "b") [(2, t "a")]) [(3, t "b"), (2, t "a")]

    -- The oldest one is bumped off, and its key reused.
    equal (f (t "a") [(4, t "x"), (3, t "y"), (2, t "z")])
        [(2, t "a"), (4, t "x"), (3, t "y")]
    -- Unless an existing one matches.
    equal (f (t "a 2") [(4, t "x"), (3, t "a 1"), (2, t "z")])
        [(3, t "a 2"), (4, t "x"), (2, t "z")]

test_record_recent_replace = do
    -- "similar" recent notes should replace existing ones
    let f = Edit.record_recent
    let note = Cmd.RecentGenerator
    equal (f (note "a" False) [(1, (note "a" True))])
        [(1, (note "a" False))]
    equal (f (note "a .2" True) [(1, (note "a .5" True))])
        [(1, (note "a .2" True))]

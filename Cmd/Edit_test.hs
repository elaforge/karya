-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Edit_test where
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Testing as Testing

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.Selection as Selection

import Global
import Types


test_cmd_clear_and_advance = do
    let run events start end = extract $ run_sel_events cmd events start end
        cmd = do
            Edit.cmd_clear_and_advance
            sel <- Selection.get
            return (Sel.start_pos sel, Sel.cur_pos sel)
        extract r = (e_start_dur_text r, expect_right $ CmdTest.result_val r)
    equal (run [(0, 1)] 0 0) (Right ([], []), Just (1, 1))
    equal (run [(0, 0), (1, 0)] 0 0) (Right ([(1, 0, "b")], []), Just (1, 1))
    equal (run [(0, 0), (1, 0)] 0 1) (Right ([(1, 0, "b")], []), Just (0, 1))

test_split_events = do
    let run events sel = e_start_dur $
            run_sel_events Edit.cmd_split_events events sel sel
    equal (run [(0, 4)] 0) $ Right ([(0, 4)], [])
    equal (run [(0, 4)] 2) $ Right ([(0, 2), (2, 2)], [])
    equal (run [(2, 2)] 3) $ Right ([(2, 1), (3, 1)], [])
    equal (run [(0, 4)] 4) $ Right ([(0, 4)], [])
    -- Negative.
    equal (run [(4, -4)] 0) $ Right ([(4, -4)], [])
    equal (run [(4, -4)] 2) $ Right ([(2, -2), (4, -2)], [])
    equal (run [(4, -4)] 4) $ Right ([(4, -4)], [])

test_set_duration = do
    -- I don't know why this function is so hard to get right, but it is.
    let run events start end = e_start_dur $
            run_sel_events Edit.cmd_set_duration events start end
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
    equal_sd (run [(0, 0), (1, 0)] 0.5 3) $ Right ([(0, 0), (1, 0)], [])
    -- Non-point selection affects all selected events.
    equal_sd (run [(0, 0.5), (1, 0.5)] 0 3) $ Right ([(0, 1), (1, 2)], [])

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
    equal_sd (run [(0, -0), (1, -0)] 0.5 3) $ Right ([(0, -0), (1, -0)], [])

    -- Non-point selection affects all selected events.
    equal_sd (run [(0, 1), (2, 1)] 0 4) $ Right ([(0, 2), (2, 2)], [])
    equal_sd (run [(2, -1), (4, -1)] 0 4) $ Right ([(2, -2), (4, -2)], [])

    -- 0 1 2 3 4 5 6
    -- |--->   <---| => take overlapping, or favor prev because selection is
    -- Positive
    let events = [(0, 2), (6, -2)]
    equal [run events p p | p <- Seq.range 0 6 1] $ map (Right . (,[]))
        [ [(0, 2), (6, -2)]
        , [(0, 1), (6, -2)]
        , [(0, 2), (6, -2)]
        , [(0, 3), (6, -2)]
        , [(0, 4), (6, -2)]
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
    let run cmd events start end = e_start_dur_text $
            run_sel_clip_ruler cmd (start_dur_events events) start end
    let fwd = Edit.cmd_move_event_forward
        bwd = Edit.cmd_move_event_backward
    -- Clipped to the end of the ruler.
    equal_e (run fwd [(0, 2)] 1 1) $ Right ([(1, 1, "a")], [])
    equal_e (run fwd [(0, 0), (2, 0)] 1 1) $
        Right ([(1, 0, "a"), (2, 0, "b")], [])

    equal_e (run bwd [(2, -1)] 1 1) $ Right ([(1, -1, "a")], [])
    equal_e (run bwd [(0, 0), (2, 0)] 1 1) $
        Right ([(0, 0, "a"), (1, 0, "b")], [])

test_set_start = do
    let run events sel = e_start_dur_text $
            run_sel_events Edit.cmd_set_start events sel sel
    equal_e (run [(0, 2), (2, 2)] 1) $ Right ([(0, 1, "a"), (1, 3, "b")], [])
    equal_e (run [(0, 0), (2, 0)] 1) $ Right ([(0, 0, "a"), (1, 0, "b")], [])
    equal_e (run [(2, -2), (4, -2)] 1) $
        Right ([(2, -2, "a"), (4, -2, "b")], [])
    equal_e (run [(2, -2), (4, -2)] 3) $
        Right ([(3, -3, "a"), (4, -1, "b")], [])

test_insert_time = do
    let run events start end = e_start_dur_text $
            run_sel_clip_ruler Edit.cmd_insert_time (start_dur_events events)
                start end
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
            run_sel_events Edit.cmd_delete_time events start end
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
        , [] -- deleted rather than reduced to 0
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
            run_sel_events Edit.cmd_toggle_zero_timestep tracks start end
    equal (run [(0, 2)] 1 1) $ Right ([(0, 0)], [])
    equal (run [(2, -2)] 1 1) $ Right ([(2, -0)], [])
    -- Make sure it's actually -0.
    equal (ScoreTime.is_negative $ snd $ head $ fst $ expect_right $
            run [(2, -2)] 1 1)
        True
    equal (run [(0, 0), (2, 0), (4, 0)] 0 4) $
        Right ([(0, 2), (2, 2), (4, 0)], [])
    equal (run [(0, -0), (2, -0), (4, -0)] 0 4) $
        Right ([(0, -0), (2, -2), (4, -2)], [])
    equal (run [(2, -2), (4, -2)] 1 1) $ Right ([(2, -0), (4, -2)], [])

test_join_events = do
    let run events start end = e_start_dur_text $
            run_sel_events Edit.cmd_join_events events start end
    -- positive
    equal_e (run [(0, 2), (2, 2)] 1 1) $ Right ([(0, 4, "a")], [])
    equal_e (run [(0, 2), (2, 2), (6, 2)] 2 8) $
        Right ([(0, 2, "a"), (2, 6, "b")], [])
    equal_e (run [(0, 0), (2, 0)] 1 1) $ Right ([(0, 0, "a")], [])

    -- negative
    equal_e (run [(0, -0), (2, -0)] 1 1) $ Right ([(2, -0, "b")], [])
    equal_e (run [(2, -2), (4, -2)] 1 1) $
        Right ([(2, -2, "a"), (4, -2, "b")], [])
    equal_e (run [(2, -2), (4, -2)] 3 3) $ Right ([(4, -4, "b")], [])
    -- Don't try for mixed polarities.
    equal_e (run [(0, 2), (4, -2)] 1 1) $
        Right ([(0, 2, "a"), (4, -2, "b")], [])

    -- A point selection on multiple tracks only deletes until the nearest.
    let run2 sel t1 t2 = CmdTest.e_tracks $
            CmdTest.run_tracks [(">", t1), ("*", t2)] $ do
                CmdTest.set_sel 1 sel 2 sel
                Edit.cmd_join_events
    -- Join both.
    equal (run2 0.5
            [(0, 1, "x"), (1, 1, "y"), (2, 1, "z")]
            [(0, 0, "c"), (1, 0, "d"), (2, 0, "e")])
        (Right (
            [ (">", [(0, 2, "x"), (2, 1, "z")])
            , ("*", [(0, 0, "c"), (2, 0, "e")])
            ], []))
    -- Join one.
    equal (run2 0.5
            [(0, 1, "x"), (1, 1, "y"), (2, 1, "z")]
            [(0, 0, "c"), (2, 0, "d")])
        (Right (
            [ (">", [(0, 2, "x"), (2, 1, "z")])
            , ("*", [(0, 0, "c"), (2, 0, "d")])
            ], []))
    -- Join both.
    equal (run2 2.5
            [(1, -1, "x"), (2, -1, "y"), (3, -1, "z")]
            [(1, -0, "c"), (2, -0, "d"), (3, -0, "e")])
        (Right (
            [ (">", [(1, -1, "x"), (3, -2, "z")])
            , ("*", [(1, -0, "c"), (3, 0, "e")])
            ], []))
    -- Join one.
    equal (run2 2.5
            [(1, -1, "x"), (2, -1, "y"), (3, -1, "z")]
            [(1, -0, "c"), (3, -0, "e")])
        (Right (
            [ (">", [(1, -1, "x"), (3, -2, "z")])
            , ("*", [(1, -0, "c"), (3, -0, "e")])
            ], []))

test_cmd_invert_orientation = do
    let run tracks = CmdTest.e_tracks $ CmdTest.run_tracks tracks $ do
            CmdTest.set_sel 1 1 2 1
            Edit.cmd_invert_orientation
    -- invert_notes
    equal (run [(">", [(0, 2, "1")])]) (Right ([(">", [(2, -2, "1")])], []))
    equal (run [(">", [(2, -2, "1")])]) (Right ([(">", [(0, 2, "1")])], []))

    -- Control gets flipped.
    equal (run [(">", [(0, 2, "1")]), ("*", [(0, 0, "4c"), (2, 0, "4d")])]) $
        Right ([(">", [(2, -2, "1")]), ("*", [(2, -0, "4c"), (2, 0, "4d")])],
            [])
        -- It's awkward that I can't actually test 0 vs -0, but presence of
        -- both controls is impicit proof.
    -- Flip back.
    equal (run [(">", [(2, -2, "1")]), ("*", [(2, -0, "4c"), (2, 0, "4d")])]) $
        Right ([(">", [(0, 2, "1")]), ("*", [(0, 0, "4c"), (2, 0, "4d")])], [])
    -- Don't move controls if there isn't exactly one control at start.
    equal (run [(">", [(0, 2, "1")]), ("*", [(1, 0, "4d")])])
        (Right ([(">", [(2, -2, "1")]), ("*", [(1, 0, "4d")])], []))

    -- invert_events
    equal (run [("c", [(1, 0, "1")])]) (Right ([("c", [(1, -0, "1")])], []))
    case run [("c", [(1, 0, "1")])] of
        Right ([("c", [(1, dur, "1")])], _) ->
            equal (ScoreTime.is_negative dur) True
        x -> failure $ "no match: " <> pretty x
    -- 0 dur events trade places.
    equal (run [("c", [(1, -0, "a"), (1, 0, "b")])]) $
        Right ([("c", [(1, -0, "b"), (1, 0, "a")])], [])


-- * util

equal_e :: CallStack.Stack =>
    Either Text ([UiTest.EventSpec], [Text])
    -> Either Text ([UiTest.EventSpec], [Text])
    -> IO Bool
equal_e = Testing.equal_fmt (UiTest.right_fst UiTest.fmt_events)

equal_sd :: CallStack.Stack =>
    Either Text ([(ScoreTime, ScoreTime)], [Text])
    -> Either Text ([(ScoreTime, ScoreTime)], [Text])
    -> IO Bool
equal_sd = Testing.equal_fmt (UiTest.right_fst UiTest.fmt_start_duration)

-- | Run with events and a selection.
run_sel_events :: Cmd.CmdId a -> [(TrackTime, TrackTime)]
    -> TrackTime -> TrackTime -> CmdTest.Result a
run_sel_events cmd events start end =
    run_sel cmd (start_dur_events events) start end

run_sel :: Cmd.CmdId a -> [UiTest.TrackSpec] -> ScoreTime
    -> ScoreTime -> CmdTest.Result a
run_sel cmd tracks start end = CmdTest.run_tracks tracks $ do
    CmdTest.set_sel 1 start 1 end
    cmd

run_sel_clip_ruler :: Cmd.CmdId a -> [UiTest.TrackSpec] -> ScoreTime
    -> ScoreTime -> CmdTest.Result a
run_sel_clip_ruler cmd tracks start end = CmdTest.run_tracks_ruler tracks $ do
    CmdTest.set_sel 1 start 1 end
    cmd

start_dur_events :: [(TrackTime, TrackTime)] -> [UiTest.TrackSpec]
start_dur_events events =
    [(">", [(start, dur, Text.singleton c) |
        ((start, dur), c) <- zip events ['a'..'z']])]

e_start_dur :: CmdTest.Result a
    -> Either Text ([(ScoreTime, ScoreTime)], [Text])
e_start_dur = fmap (first (map (\(s, d, _) -> (s, d)))) . e_start_dur_text

e_start_dur_text :: CmdTest.Result a -> Either Text ([UiTest.EventSpec], [Text])
e_start_dur_text = fmap (first (snd . head)) . CmdTest.e_tracks

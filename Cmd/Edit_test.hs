-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Edit_test where
import qualified Data.Text as Text
import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Test.Testing as Testing
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Edit as Edit
import qualified Cmd.Selection as Selection
import qualified Cmd.TimeStep as TimeStep

import qualified Ui.ScoreTime as ScoreTime
import qualified Ui.Sel as Sel
import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_cmd_clear_and_advance :: Test
test_cmd_clear_and_advance = do
    let run start end = extract . runSelEvents cmd start end
        cmd = do
            Edit.cmdClearAndAdvance
            sel <- Selection.get
            pure (Sel.startPos sel, Sel.curPos sel)
        extract r = (eStartDurText1 r, expectRight $ CmdTest.resultVal r)
    equal (run 0 0 [(0, 1)]) (Right ([], []), Just (1, 1))
    equal (run 0 0 [(0, 0), (1, 0)]) (Right ([(1, 0, "b")], []), Just (1, 1))
    equal (run 0 1 [(0, 0), (1, 0)]) (Right ([(1, 0, "b")], []), Just (0, 1))

test_extend_previous :: Test
test_extend_previous = do
    let run pos = extract . runSelEvents cmd pos pos
        cmd = do
            Cmd.modifyEditState $ \st ->
                st { Cmd.stateNoteDuration = TimeStep.eventEdge }
            Edit.cmdClearAndAdvance
        extract r = eStartDurText1 r
    rightEqual (run 0 [(0, 1), (1, 1)]) ([(1, 1, "b")], [])
    rightEqual (run 1 [(0, 1), (1, 1)]) ([(0, 32, "a")], [])
    -- Don't extend if there wasn't an event to delete!
    rightEqual (run 1 [(0, 1)]) ([(0, 1, "a")], [])
    -- Don't extend if it doesn't touch.
    rightEqual (run 1 [(0, 0), (1, 1)]) ([(0, 0, "a")], [])
    rightEqual (run 1 [(0, 0.5), (1, 1)]) ([(0, 0.5, "a")], [])
    -- Negative goes the other way.
    rightEqual (run (-2) [(1, -1), (2, -1)]) ([(1, -1, "a")], [])
    rightEqual (run (-1) [(1, -1), (2, -1)]) ([(2, -2, "b")], [])
    rightEqual (run (-1) [(1, -1), (2, -0.5)]) ([(2, -0.5, "b")], [])

test_split_events :: Test
test_split_events = do
    let run sel = eStartDur1
            . runSelEvents Edit.cmdSplitEvents sel sel
    equal (run 0 [(0, 4)]) $ Right ([(0, 4)], [])
    equal (run 2 [(0, 4)]) $ Right ([(0, 2), (2, 2)], [])
    equal (run 3 [(2, 2)]) $ Right ([(2, 1), (3, 1)], [])
    equal (run 4 [(0, 4)]) $ Right ([(0, 4)], [])
    -- Negative.
    equal (run 0 [(4, -4)]) $ Right ([(4, -4)], [])
    equal (run 2 [(4, -4)]) $ Right ([(2, -2), (4, -2)], [])
    equal (run 4 [(4, -4)]) $ Right ([(4, -4)], [])

test_set_duration :: Test
test_set_duration = do
    -- I don't know why this function is so hard to get right, but it is.
    let run start end = eStartDur1
            . runSelEvents Edit.cmdSetDuration start end
    -- |--->   |---> => take pre
    let events = [(0, 2), (4, 2)]
    equal [run p p events | p <- Lists.range 0 6 1] $ map (Right . (,[]))
        [ [(0, 2), (4, 2)]
        , [(0, 1), (4, 2)]
        , [(0, 2), (4, 2)]
        , [(0, 3), (4, 2)]
        , [(0, 4), (4, 2)]
        , [(0, 2), (4, 1)]
        , [(0, 2), (4, 2)]
        ]
    -- No effect on zero-dur events.
    equalSd (run 0.5 3 [(0, 0), (1, 0)]) $ Right ([(0, 0), (1, 0)], [])
    -- Non-point selection affects all selected events.
    equalSd (run 0 3 [(0, 0.5), (1, 0.5)]) $ Right ([(0, 1), (1, 2)], [])

    -- <---|   <---| => take post
    let events = [(2, -2), (6, -2)]
    equal [run p p events | p <- Lists.range 0 6 1] $ map (Right . (,[]))
        [ [(2, -2), (6, -2)]
        , [(2, -1), (6, -2)]
        , [(2, -2), (6, -4)]
        , [(2, -2), (6, -3)]
        , [(2, -2), (6, -2)]
        , [(2, -2), (6, -1)]
        , [(2, -2), (6, -2)]
        ]
    -- No effect on zero-dur events.
    equalSd (run 0.5 3 [(0, -0), (1, -0)]) $ Right ([(0, -0), (1, -0)], [])

    -- Non-point selection affects all selected events.
    equalSd (run 0 4 [(0, 1), (2, 1)]) $ Right ([(0, 2), (2, 2)], [])
    equalSd (run 0 4 [(2, -1), (4, -1)]) $ Right ([(2, -2), (4, -2)], [])

    -- 0 1 2 3 4 5 6
    -- |--->   <---| => take overlapping, or favor prev because selection is
    -- Positive
    let events = [(0, 2), (6, -2)]
    equal [run p p events | p <- Lists.range 0 6 1] $ map (Right . (,[]))
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
    equal [run p p events | p <- Lists.range 0 6 1] $ map (Right . (,[]))
        [ [(2, -2), (4, 2)]
        , [(2, -1), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 2)]
        , [(2, -2), (4, 1)]
        , [(2, -2), (4, 2)]
        ]

test_move_events :: Test
test_move_events = do
    let run cmd start end = eStartDurText1
            . runSelClipRuler cmd start end . startDurEvents
    let fwd = Edit.cmdMoveEventForward
        bwd = Edit.cmdMoveEventBackward
    -- Clipped to the end of the ruler.
    equalE (run fwd 1 1 [(0, 2)]) $ Right ([(1, 1, "a")], [])
    equalE (run fwd 1 1 [(0, 0), (2, 0)]) $
        Right ([(1, 0, "a"), (2, 0, "b")], [])
    equalE (run bwd 1 1 [(2, -1)]) $ Right ([(1, -1, "a")], [])
    equalE (run bwd 1 1 [(0, 0), (2, 0)]) $
        Right ([(0, 0, "a"), (1, 0, "b")], [])
    -- Move multiple.
    equalE (run fwd 0 2 [(0, 1), (1, 1), (4, 1)]) $
        Right ([(1, 1, "a"), (2, 1, "b"), (4, 1, "c")], [])
    equalE (run fwd 2 0 [(0, 1), (1, 1), (4, 1)]) $
        Right ([(1, 1, "a"), (2, 1, "b"), (4, 1, "c")], [])
    equalE (run bwd 0 3 [(1, 1), (2, 1), (4, 1)]) $
        Right ([(0, 1, "a"), (1, 1, "b"), (4, 1, "c")], [])
    equalE (run bwd 3 0 [(1, 1), (2, 1), (4, 1)]) $
        Right ([(0, 1, "a"), (1, 1, "b"), (4, 1, "c")], [])

test_move_events_multiple_tracks_back :: Test
test_move_events_multiple_tracks_back = do
    let run start end tracks = eStartDurText $
            CmdTest.runTracks (map (">",) tracks) $ do
                CmdTest.setSel 1 start 2 end
                Edit.cmdMoveEventBackward
    rightEqual (run 1 1
            [[(2, 0, "a"), (3, 0, "b")], [(3, 0, "x"), (4, 0, "y")]])
        ([[(1, 0, "a"), (3, 0, "b")], [(3, 0, "x"), (4, 0, "y")]], [])
    -- Covered "x" moves, but not "y".
    rightEqual (run 1 1
            [[(2, 0, "a"), (4, 0, "b")], [(2, 0, "x"), (4, 0, "y")]])
        ([[(1, 0, "a"), (4, 0, "b")], [(1, 0, "x"), (4, 0, "y")]], [])
    rightEqual (run 1 1
            [[(2, 2, "a"), (4, 0, "b")], [(3, 0, "x"), (4, 0, "y")]])
        ([[(1, 2, "a"), (4, 0, "b")], [(2, 0, "x"), (4, 0, "y")]], [])
    -- Events that would be overlapped are deleted.
    rightEqual (run 1 1 [[(2, 2, "a")], [(1, 0, "x"), (2, 0, "y")]])
        ([[(1, 2, "a")], [(1, 0, "y")]], [])

    -- Move multiple events if selected.
    rightEqual (run 1 3 [[(2, 0, "a"), (3, 0, "b"), (4, 0, "c")]])
        ([[(1, 0, "a"), (3, 0, "b"), (4, 0, "c")]], [])
    rightEqual (run 1 4 [[(2, 0, "a"), (3, 0, "b"), (4, 0, "c")]])
        ([[(1, 0, "a"), (2, 0, "b"), (4, 0, "c")]], [])
    rightEqual (run 1 4
            [ [(2, 0, "a"), (3, 0, "b"), (4, 0, "c")]
            , [(3, 0, "x"), (4, 0, "y")]
            ])
        ( [ [(1, 0, "a"), (2, 0, "b"), (4, 0, "c")]
          , [(2, 0, "x"), (4, 0, "y")]
          ]
        , []
        )

test_move_events_multiple_tracks_forward :: Test
test_move_events_multiple_tracks_forward = do
    let run start end tracks = eStartDurText $
            CmdTest.runTracks (map (">",) tracks) $ do
                CmdTest.setSel 1 start 2 end
                Edit.cmdMoveEventForward
    rightEqual (run 2 2
            [[(0, 0, "a"), (1, 0, "b")], [(0, 0, "x"), (1, 0, "y")]])
        ([[(0, 0, "a"), (2, 0, "b")], [(0, 0, "x"), (2, 0, "y")]], [])
    -- Covered "x" moves, but not "y".
    -- TODO this is weird because x jumps over y, should it delete it?
    rightEqual (run 2 2 [[(0, 2, "a")], [(1, 0, "x"), (2, 0, "y")]])
        ([[(2, 2, "a")], [(2, 0, "y"), (3, 0, "x")]], [])
    rightEqual (run 2 2 [[(0, 2, "a")], [(1, 0, "x"), (4, 0, "y")]])
        ([[(2, 2, "a")], [(3, 0, "x"), (4, 0, "y")]], [])

test_set_start :: Test
test_set_start = do
    let run sel = eStartDurText1 . runSelEvents Edit.cmdSetStart sel sel
    equalE (run 1 [(0, 2), (2, 2)]) $ Right ([(0, 1, "a"), (1, 3, "b")], [])
    equalE (run 1 [(0, 0), (2, 0)]) $ Right ([(0, 0, "a"), (1, 0, "b")], [])
    equalE (run 1 [(2, -2), (4, -2)]) $
        Right ([(2, -2, "a"), (4, -2, "b")], [])
    equalE (run 3 [(2, -2), (4, -2)]) $
        Right ([(3, -3, "a"), (4, -1, "b")], [])

test_insert_time :: Test
test_insert_time = do
    let run start end = eStartDurText1
            . runSelClipRuler Edit.cmdInsertTime start end
            . startDurEvents
    equal (run 0 0 [(0, 2), (3, 0)]) $ Right ([(1, 2, "a")], [])
    -- (1, 2) is shortened.
    equal (run 0 0 [(1, 2), (3, 0)]) $ Right ([(2, 1, "a")], [])
    equal (run 0 0 [(0, 0), (3, 0)]) $ Right ([(1, 0, "a")], [])
    equal (run 0 0 [(0, -0), (3, 0)]) $ Right ([(1, -0, "a")], [])
    equal (run 0 0 [(2, -0), (3, 0)]) $ Right ([(3, 0, "a")], [])

    -- Extend the duration of an event from inside it.
    equal (run 1 1 [(0, 2), (8, 0)]) $ Right ([(0, 3, "a")], [])
    equal (run 1 1 [(2, -2), (8, 0)]) $ Right ([(3, -3, "a")], [])

    equal (run 3 3 [(0, 2), (2, 2), (4, 2)]) $
        Right ([(0, 2, "a"), (2, 3, "b"), (5, 1, "c")], [])
    equal (run 3 5 [(0, 2), (2, 2), (4, 2)]) $
        Right ([(0, 2, "a"), (2, 4, "b"), (6, 0, "c")], [])

    equal (run 1.5 1.75 [(0, 1), (2.25, -1.25)]) $
        Right ([(0, 1, "a"), (2.5, -1.5, "b")], [])

test_delete_time :: Test
test_delete_time = do
    let run start end = eStartDur1
            . runSelEvents Edit.cmdDeleteTime start end
    -- positive
    equal (map (\e -> run 0 e [(2, 2)]) (Lists.range 0 3 1)) $
        map (Right . (,[]))
        [ [(1, 2)]
        , [(1, 2)]
        , [(0, 2)]
        , []
        ]
    equal (map (\e -> run 3 e [(2, 2)]) [4, 5]) $ map (Right . (,[]))
        [ [(2, 1)]
        , [(2, 1)]
        ]
    equal (run 4 5 [(2, 2)]) $ Right ([(2, 2)], [])

    -- negative
    equal (map (\e -> run 0 e [(4, -2)]) (Lists.range 0 4 1)) $
        map (Right . (,[]))
        [ [(3, -2)]
        , [(3, -2)]
        , [(2, -2)]
        , [(1, -1)]
        , [] -- deleted rather than reduced to 0
        ]
    -- zero dur doesn't get deleted when it touches the point.
    equal (map (\e -> run 0 e [(2, -0)]) (Lists.range 1 3 1)) $
        map (Right . (,[]))
        [ [(1, -0)]
        , [(0, -0)]
        , []
        ]
    equal (run 4 6 [(4, -2)]) $ Right ([(4, -2)], [])

    equal (run 1.5 1.75 [(0, 1), (2.25, -1.25)]) $
        Right ([(0, 1), (2, -1)], [])

test_toggle_zero_timestep :: Test
test_toggle_zero_timestep = do
    let run start end = eStartDur1
            . runSelEvents Edit.cmdToggleZeroTimestep start end
    equal (run 1 1 [(0, 2)]) $ Right ([(0, 0)], [])
    equal (run 1 1 [(2, -2)]) $ Right ([(2, -0)], [])
    -- Make sure it's actually -0.
    equal (ScoreTime.isNegative $ snd $ head $ fst $ expectRight $
            run 1 1 [(2, -2)])
        True
    equal (run 0 4 [(0, 0), (2, 0), (4, 0)]) $
        Right ([(0, 2), (2, 2), (4, 0)], [])
    equal (run 0 4 [(0, -0), (2, -0), (4, -0)]) $
        Right ([(0, -0), (2, -2), (4, -2)], [])
    equal (run 1 1 [(2, -2), (4, -2)]) $ Right ([(2, -0), (4, -2)], [])

test_join_events :: Test
test_join_events = do
    let run start end = eStartDurText1
            . runSelEvents Edit.cmdJoinEvents start end
    -- positive
    equalE (run 1 1 [(0, 2), (2, 2)]) $ Right ([(0, 4, "a")], [])
    equalE (run 2 8 [(0, 2), (2, 2), (6, 2)]) $
        Right ([(0, 2, "a"), (2, 6, "b")], [])
    equalE (run 1 1 [(0, 0), (2, 0)]) $ Right ([(0, 0, "a")], [])

    -- negative
    equalE (run 1 1 [(0, -0), (2, -0)]) $ Right ([(2, -0, "b")], [])
    equalE (run 1 1 [(2, -2), (4, -2)]) $
        Right ([(2, -2, "a"), (4, -2, "b")], [])
    equalE (run 3 3 [(2, -2), (4, -2)]) $ Right ([(4, -4, "b")], [])
    -- Don't try for mixed polarities.
    equalE (run 1 1 [(0, 2), (4, -2)]) $
        Right ([(0, 2, "a"), (4, -2, "b")], [])

    -- A point selection on multiple tracks only deletes until the nearest.
    let run2 sel t1 t2 = CmdTest.eTracks $
            CmdTest.runTracks [(">", t1), ("*", t2)] $ do
                CmdTest.setSel 1 sel 2 sel
                Edit.cmdJoinEvents
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

test_cmd_invert_orientation :: Test
test_cmd_invert_orientation = do
    let run tracks = CmdTest.eTracks $ CmdTest.runTracks tracks $ do
            CmdTest.setSel 1 1 2 1
            Edit.cmdInvertOrientation
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
            equal (ScoreTime.isNegative dur) True
        x -> failure $ "no match: " <> pretty x
    -- 0 dur events trade places.
    equal (run [("c", [(1, -0, "a"), (1, 0, "b")])]) $
        Right ([("c", [(1, -0, "b"), (1, 0, "a")])], [])


-- * util

equalE :: HasCallStack => Either Text ([UiTest.EventSpec], [Text])
    -> Either Text ([UiTest.EventSpec], [Text])
    -> Test
equalE = Testing.equalFmt (UiTest.rightFst UiTest.fmtEvents)

equalSd :: HasCallStack => Either Text ([(ScoreTime, ScoreTime)], [Text])
    -> Either Text ([(ScoreTime, ScoreTime)], [Text])
    -> Test
equalSd = Testing.equalFmt (UiTest.rightFst UiTest.fmtStartDuration)

-- | Run with events and a selection.
runSelEvents :: Cmd.CmdId a -> TrackTime -> TrackTime
    -> [(TrackTime, TrackTime)] -> CmdTest.Result a
runSelEvents cmd start end events =
    runSel cmd start end (startDurEvents events)

runSel :: Cmd.CmdId a -> ScoreTime -> ScoreTime
    -> [UiTest.TrackSpec] -> CmdTest.Result a
runSel cmd start end tracks = CmdTest.runTracks tracks $ do
    CmdTest.setSel 1 start 1 end
    cmd

runSelClipRuler :: Cmd.CmdId a -> ScoreTime -> ScoreTime
    -> [UiTest.TrackSpec] -> CmdTest.Result a
runSelClipRuler cmd start end tracks = CmdTest.runTracksRuler tracks $ do
    CmdTest.setSel 1 start 1 end
    cmd

startDurEvents :: [(TrackTime, TrackTime)] -> [UiTest.TrackSpec]
startDurEvents events =
    [ (">", [(start, dur, Text.singleton c)
    | ((start, dur), c) <- zip events ['a'..'z']])
    ]

eStartDur1 :: CmdTest.Result a
    -> Either Text ([(ScoreTime, ScoreTime)], [Text])
eStartDur1 = fmap (first (map (\(s, d, _) -> (s, d)))) . eStartDurText1

eStartDurText1 :: CmdTest.Result a
    -> Either Text ([UiTest.EventSpec], [Text])
eStartDurText1 = fmap (first head) . eStartDurText

eStartDurText :: CmdTest.Result a
    -> Either Text ([[UiTest.EventSpec]], [Text])
eStartDurText = fmap (first (map snd)) . CmdTest.eTracks

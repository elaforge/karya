module Cmd.StepPlay_test where
import qualified Data.List as List

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Midi as Midi
import Ui
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.StepPlay as StepPlay

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.TrackWarp as TrackWarp


test_zip_forward = do
    let f = StepPlay.zip_forward
    equal (f (>2) [-1, -2] [])
        ([], [-1, -2], [])
    equal (f (>2) [-1, -2] [0..4])
        ([0, 1, 2], [2, 1, 0, -1, -2], [3, 4])
    equal (f (>99) [-1, -2] [0..4])
        ([0..4], [4, 3 .. -2], [])

test_zipper = do
    equal (thread_zipper [(>3), (<=1), const False] [0..6])
        [[0, 1, 2, 3], [3, 2], [2, 3, 4, 5, 6]]

thread_zipper :: [a -> Bool] -> [a] -> [[a]]
thread_zipper fs xs = snd $ List.mapAccumL go (True, [], xs) fs
    where
    go (forward, pre1, post1) f = ((not forward, pre2, post2), vals)
        where
        (vals, pre2, post2) = zipper f pre1 post1
        zipper = if forward then StepPlay.zip_forward
            else StepPlay.zip_backward

-- testt0 = do
--     let simple_block =
--             [ (">s/1", [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")])
--             , ("*twelve", [(p, 0, n) | (p, n) <- zip (Seq.range 0 3 1)
--                 ["4c", "4d", "4e", "4f"]])
--             ]
--         ustate = DeriveTest.with_instrument $ snd $
--             UiTest.run_mkview simple_block
--         cstate = CmdTest.default_cmd_state
--     let e_midi = map snd . CmdTest.result_midi
--     res <- CmdTest.update_perf ustate $ CmdTest.run ustate cstate (return ())
--     res <- CmdTest.run_again res $ do
--         perf <- Perf.get_root
--         return (Cmd.perf_events perf)
--     pprint (CmdTest.extract id res)

test_move = do
    res <- prepare_blocks UiTest.default_block_name simple_block

    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 1 3
            StepPlay.cmd_set
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    -- TODO missing pitchbend because it's too early
    -- but I think I need midi state to do this properly
    equal (e_midi res)
        [ Midi.ChannelMessage 0 (Midi.NoteOn 64 100)
        ]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res)
        [ Midi.ChannelMessage 0 (Midi.NoteOff 64 100)
        , Midi.ChannelMessage 0 (Midi.NoteOn 65 100)
        ]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

    -- Ran out of notes.
    failed <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi failed) []
    left_like (CmdTest.extract_ui get_sel failed) "can't advance for step play"

    -- Rewind then forward.
    res <- return $ CmdTest.run_again res StepPlay.cmd_rewind
    equal (map snd (CmdTest.result_midi res)) []
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res)
        [ Midi.ChannelMessage 0 (Midi.NoteOff 64 100)
        , Midi.ChannelMessage 0 (Midi.NoteOn 65 100)
        ]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

test_reset = do
    res <- prepare_blocks UiTest.default_block_name simple_block

    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 1 3
            StepPlay.cmd_set
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_rewind
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 1, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])
    equal (e_midi res)
        [ Midi.ChannelMessage 0 (Midi.NoteOn 62 100)
        ]

test_nested = do
    res <- prepare_blocks "sub"
        [ ("b", [(">", [(0, 8, "sub"), (8, 8, "sub")])])
        , ("sub", simple_tracks)
        ]
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel_block "sub" 1 3
            StepPlay.cmd_set
    pprint (CmdTest.extract_ui (get_block_sel "sub") res)
    pprint (CmdTest.extract_ui (get_block_sel "b") res)
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    pprint (CmdTest.extract_ui (get_block_sel "sub") res)
    pprint (CmdTest.extract_ui (get_block_sel "b") res)
    pprint (e_midi res)

    -- The problem is that realtime of 8 gets both the end of the block
    -- and the beginning of the block.
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    pprint (CmdTest.extract_ui (get_block_sel "sub") res)
    pprint (CmdTest.extract_ui (get_block_sel "b") res)
    pprint (e_midi res)

test_tempo = do
    res <- prepare_blocks UiTest.default_block_name
        [(UiTest.default_block_name,
            ("tempo", [(0, 0, ".987")]) : simple_tracks)]
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 2 2
            StepPlay.cmd_set
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 1, [])
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance

    let get_perf = fmap (Cmd.perf_warps . Cmd.step_performance)
            . Cmd.state_step . Cmd.state_play
    -- pprint (CmdTest.extract_state (const get_perf) res)
    let Right (Just tws, _) = CmdTest.extract_state (const get_perf) res
    let inv = TrackWarp.inverse_tempo_func tws
        tempo = TrackWarp.tempo_func tws
    pprint (map (tempo UiTest.default_block_id (UiTest.tid "b1.t0")) [1, 2])
    let rtimes = concatMap (tempo UiTest.default_block_id (UiTest.tid "b1.t0"))
            [1, 2]
    pprint tws
    pprint rtimes
    pprint (map inv rtimes)
    -- let stimes = concatMap (inv . RealTime.seconds) [1, 2]
    -- pprint stimes

    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])
    -- res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    -- equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])


e_midi = map snd . CmdTest.result_midi

simple_block :: [UiTest.BlockSpec]
simple_block = [(UiTest.default_block_name, simple_tracks)]

simple_tracks :: [UiTest.TrackSpec]
simple_tracks =
    [ (">s/1", [(0, 1, ""), (1, 1, ""), (2, 1, ""), (3, 1, "")])
    , ("*twelve", [(p, 0, n) | (p, n) <- zip (Seq.range 0 3 1)
        ["4c", "4d", "4e", "4f"]])
        -- 0   1   2   3
        -- 60, 62, 64, 65
    ]

prepare_blocks :: String -> [UiTest.BlockSpec] -> IO (CmdTest.Result ())
prepare_blocks focus blocks = CmdTest.update_perf ustate $
        CmdTest.run ustate cstate (return ())
    where
    ustate = DeriveTest.with_instrument $
        UiTest.exec State.empty (UiTest.mkviews blocks)
    cstate = CmdTest.default_cmd_state
        { Cmd.state_focused_view = Just (UiTest.mk_vid_name focus) }

step_state = CmdTest.extract_state $
    \_ st -> Cmd.state_step (Cmd.state_play st)

get_sel :: (State.M m) => m (Maybe ScoreTime)
get_sel = fmap Types.sel_cur_pos <$>
    State.get_selection UiTest.default_view_id StepPlay.selnum

get_block_sel :: (State.M m) => String -> m (Maybe ScoreTime)
get_block_sel name = fmap Types.sel_cur_pos <$>
    State.get_selection (UiTest.mk_vid_name name) StepPlay.selnum

module Cmd.StepPlay_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Midi as Midi
import qualified Midi.State
import qualified Ui.State as State
import qualified Ui.Types as Types
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Derive.DeriveTest as DeriveTest
import qualified Perform.RealTime as RealTime
import Types


test_make_states = do
    let chan_msgs msgs = [Midi.WriteMessage (Midi.write_device "dev") t
            (Midi.ChannelMessage 0 m) | (t, m) <- msgs]
    let f ts msgs = map extract $
            StepPlay.make_states (map RealTime.seconds ts) (chan_msgs msgs)
        extract (Midi.State.State chans) = Map.elems $
            Map.map (Map.toList . Midi.State.chan_notes)  chans
    let msgs =
            [ (0, Midi.NoteOn 60 100)
            , (1, Midi.NoteOff 60 0)
            , (1, Midi.NoteOn 61 100)
            , (2, Midi.NoteOff 61 0)
            ]
    equal (f [0, 1, 2] msgs)
        [ [[(60, 100)]]
        , [[(61, 100)]]
        , [[]]
        ]

test_move_to = do
    res <- prepare_blocks UiTest.default_block_name simple_block
    let sel_from p = do
        res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 1 p
            Cmd.modify_play_state $ \st -> st { Cmd.state_play_step =
                TimeStep.step (TimeStep.Absolute 1) }
            StepPlay.cmd_set False
        return (e_midi res, CmdTest.extract_ui get_sel res)
    -- Ensure that cmd_set picks the previous step pos, rounding downward
    -- if the match isn't exact.
    io_equal (sel_from 2) ([Midi.NoteOn 62 100], Right ((Just 1), []))
    io_equal (sel_from 1) ([Midi.NoteOn 60 100], Right ((Just 0), []))
    io_equal (sel_from 0) ([Midi.NoteOn 60 100], Right ((Just 0), []))
    io_equal (sel_from 1.5) ([Midi.NoteOn 60 100], Right ((Just 0), []))

test_move = do
    res <- prepare_blocks UiTest.default_block_name simple_block
    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
        CmdTest.set_point_sel 1 3
        StepPlay.cmd_set False
    equal (e_midi res) [Midi.NoteOn 64 100]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff 64 0, Midi.NoteOn 65 100]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff 65 0]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

    -- Ran out of notes.
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) []
    left_like (CmdTest.extract_ui get_sel res) "can't advance for step play"

    -- Rewind
    res <- return $ CmdTest.run_again res StepPlay.cmd_rewind
    equal (e_midi res) [Midi.NoteOn 65 100]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])

    -- Go back forward to make sure zipping is zipping properly.
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff 65 0]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

test_move_tracks = do
    res <- prepare_blocks UiTest.default_block_name
        [(UiTest.default_block_name,
            note_track ["4c", "4d", "4e"] ++ note_track ["5c", "5d", "5e"])]
    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
        CmdTest.set_point_sel 2 3
        StepPlay.cmd_set True
    equal (e_midi res) [Midi.NoteOn 64 100]
    -- Ensure the selection is only on one track.
    equal (CmdTest.extract_ui get_sel_tracks res) $ Right (Just (2, [2]), [])

{-
These test the more complicated version that puts the selections in the right
spots for all blocks.  Disabled since that won't work anyway until I can do
discontiguous selections.

-- test_nested = do
    res <- prepare_blocks "sub"
        [ ("b", [(">", [(0, 8, "sub"), (8, 8, "sub")])])
        , ("sub", simple_tracks)
        ]
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel_block "sub" 1 3
            StepPlay.cmd_set False
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

-- test_tempo = do
    res <- prepare_blocks UiTest.default_block_name
        [(UiTest.default_block_name,
            ("tempo", [(0, 0, ".987")]) : simple_tracks)]
    res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 2 2
            StepPlay.cmd_set False
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
-}


e_midi :: CmdTest.Result val -> [Midi.ChannelMessage]
e_midi = mapMaybe Midi.channel_message . map snd . CmdTest.result_midi

simple_block :: [UiTest.BlockSpec]
simple_block = [(UiTest.default_block_name, simple_tracks)]

simple_tracks :: [UiTest.TrackSpec]
simple_tracks = note_track
    ["4c", "4d", "4e", "4f"]
    -- 0   1   2   3
    -- 60  62  64  65

note_track :: [String] -> [UiTest.TrackSpec]
note_track notes = UiTest.note_spec
    ("s/1", [(t, 1, pitch) | (t, pitch) <- zip (Seq.range_ 0 1) notes], [])

prepare_blocks :: String -> [UiTest.BlockSpec] -> IO (CmdTest.Result ())
prepare_blocks focus blocks = CmdTest.update_perf ustate $
        CmdTest.run ustate cstate (return ())
    where
    ustate = DeriveTest.with_instrument $
        UiTest.exec State.empty (UiTest.mkviews blocks)
    cstate = CmdTest.default_cmd_state
        { Cmd.state_focused_view = Just (UiTest.mk_vid_name focus) }

step_state = Cmd.state_step . Cmd.state_play . CmdTest.result_cmd_state

get_sel :: (State.M m) => m (Maybe ScoreTime)
get_sel = fmap Types.sel_cur_pos <$>
    State.get_selection UiTest.default_view_id StepPlay.selnum

get_sel_tracks :: (State.M m) => m (Maybe (ScoreTime, [TrackNum]))
get_sel_tracks = fmap (\s -> (Types.sel_cur_pos s, Types.sel_tracknums s)) <$>
    State.get_selection UiTest.default_view_id StepPlay.selnum

get_block_sel :: (State.M m) => String -> m (Maybe ScoreTime)
get_block_sel name = fmap Types.sel_cur_pos <$>
    State.get_selection (UiTest.mk_vid_name name) StepPlay.selnum

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.StepPlay_test where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Midi.State

import qualified Ui.Sel as Sel
import qualified Ui.Ui as Ui
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.StepPlay as StepPlay
import qualified Cmd.TimeStep as TimeStep

import qualified Perform.RealTime as RealTime
import Global
import Types


test_make_states :: Test
test_make_states = do
    let chan_msgs msgs = [Midi.WriteMessage (Midi.write_device "dev") t
            (Midi.ChannelMessage 0 m) | (t, m) <- msgs]
    let f ts msgs = map extract $
            StepPlay.make_states (map RealTime.seconds ts) (chan_msgs msgs)
        extract (Midi.State.State chans) = Map.elems $
            Map.map (Map.toList . Midi.State.chan_notes) chans
    let msgs =
            [ (0, Midi.NoteOn 60 127)
            , (1, Midi.NoteOff 60 0)
            , (1, Midi.NoteOn 61 127)
            , (2, Midi.NoteOff 61 0)
            ]
    equal (f [0, 1, 2] msgs)
        [ [[(60, 127)]]
        , [[(61, 127)]]
        , [[]]
        ]

test_move_to :: Test
test_move_to = do
    res <- prepare_blocks UiTest.default_block_name simple_block
    let sel_from p = do
        res <- return $ CmdTest.run_again res $ do
            CmdTest.set_point_sel 1 p
            Cmd.modify_play_state $ \st -> st { Cmd.state_play_step =
                TimeStep.time_step (TimeStep.Duration 1) }
            StepPlay.cmd_set False
        return (e_midi res, CmdTest.extract_ui get_sel res)
    -- Ensure that cmd_set picks the previous step pos, rounding downward
    -- if the match isn't exact.
    io_equal (sel_from 2) ([Midi.NoteOn Key.d4 127], Right ((Just 1), []))
    io_equal (sel_from 1) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))
    io_equal (sel_from 0) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))
    io_equal (sel_from 1.5) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))

test_move :: Test
test_move = do
    res <- prepare_blocks UiTest.default_block_name simple_block
    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
        CmdTest.set_point_sel 1 3
        StepPlay.cmd_set False
    equal (e_midi res) [Midi.NoteOn Key.e4 127]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 2, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff Key.e4 0, Midi.NoteOn Key.f4 127]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])

    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff Key.f4 0]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

    -- Ran out of notes.
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) []
    left_like (CmdTest.extract_ui get_sel res) "can't advance for step play"

    -- Rewind
    res <- return $ CmdTest.run_again res StepPlay.cmd_rewind
    equal (e_midi res) [Midi.NoteOn Key.f4 127]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 3, [])

    -- Go back forward to make sure zipping is zipping properly.
    res <- return $ CmdTest.run_again res StepPlay.cmd_advance
    equal (e_midi res) [Midi.NoteOff Key.f4 0]
    equal (CmdTest.extract_ui get_sel res) $ Right (Just 4, [])

test_move_tracks :: Test
test_move_tracks = do
    res <- prepare_blocks UiTest.default_block_name
        [(UiTest.default_block_name,
            note_track ["4c", "4d", "4e"] ++ note_track ["5c", "5d", "5e"])]
    -- run cmd_set, verify selection is there
    res <- return $ CmdTest.run_again res $ do
        CmdTest.set_point_sel 2 3
        StepPlay.cmd_set True
    equal (e_midi res) [Midi.NoteOn 64 127]
    -- Ensure the selection is only on one track.
    equal (CmdTest.extract_ui get_sel_tracks res) $ Right (Just (2, [2]), [])

e_midi :: CmdTest.Result a -> [Midi.ChannelMessage]
e_midi = mapMaybe Midi.channel_message . CmdTest.e_midi

simple_block :: [UiTest.BlockSpec]
simple_block = [(UiTest.default_block_name, simple_tracks)]

simple_tracks :: [UiTest.TrackSpec]
simple_tracks = note_track
    ["4c", "4d", "4e", "4f"]
    -- 0   1   2   3
    -- 60  62  64  65

note_track :: [Text] -> [UiTest.TrackSpec]
note_track notes = UiTest.note_spec
    ("i1", [(t, 1, pitch) | (t, pitch) <- zip (Lists.range_ 0 1) notes], [])

prepare_blocks :: Text -> [UiTest.BlockSpec] -> IO (CmdTest.Result ())
prepare_blocks focus blocks =
    CmdTest.update_perf ustate $ CmdTest.run ustate cstate (return ())
    where
    ustate = UiTest.exec Ui.empty (UiTest.mkviews blocks)
    cstate = CmdTest.default_cmd_state
        { Cmd.state_focused_view = Just (UiTest.mk_vid_name focus) }

get_sel :: Ui.M m => m (Maybe ScoreTime)
get_sel = fmap Sel.cur_pos <$>
    Ui.get_selection UiTest.default_view_id StepPlay.selnum

get_sel_tracks :: Ui.M m => m (Maybe (ScoreTime, [TrackNum]))
get_sel_tracks =
    fmap (\s -> (Sel.cur_pos s, Sel.tracknums 99 s)) <$>
        Ui.get_selection UiTest.default_view_id StepPlay.selnum

get_block_sel :: Ui.M m => Text -> m (Maybe ScoreTime)
get_block_sel name = fmap Sel.cur_pos <$>
    Ui.get_selection (UiTest.mk_vid_name name) StepPlay.selnum

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
    let chanMsgs msgs = [Midi.WriteMessage (Midi.writeDevice "dev") t
            (Midi.ChannelMessage 0 m) | (t, m) <- msgs]
    let f ts msgs = map extract $
            StepPlay.makeStates (map RealTime.seconds ts) (chanMsgs msgs)
        extract (Midi.State.State chans) = Map.elems $
            Map.map (Map.toList . Midi.State.chanNotes) chans
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
    res <- prepareBlocks UiTest.defaultBlockName simpleBlock
    let selFrom p = do
        res <- pure $ CmdTest.runAgain res $ do
            CmdTest.setPointSel 1 p
            Cmd.modifyPlayState $ \st -> st { Cmd.statePlayStep =
                TimeStep.timeStep (TimeStep.Duration 1) }
            StepPlay.cmdSet False
        pure (eMidi res, CmdTest.extractUi getSel res)
    -- Ensure that cmd_set picks the previous step pos, rounding downward
    -- if the match isn't exact.
    ioEqual (selFrom 2) ([Midi.NoteOn Key.d4 127], Right ((Just 1), []))
    ioEqual (selFrom 1) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))
    ioEqual (selFrom 0) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))
    ioEqual (selFrom 1.5) ([Midi.NoteOn Key.c4 127], Right ((Just 0), []))

test_move :: Test
test_move = do
    res <- prepareBlocks UiTest.defaultBlockName simpleBlock
    -- run cmd_set, verify selection is there
    res <- pure $ CmdTest.runAgain res $ do
        CmdTest.setPointSel 1 3
        StepPlay.cmdSet False
    equal (eMidi res) [Midi.NoteOn Key.e4 127]
    equal (CmdTest.extractUi getSel res) $ Right (Just 2, [])

    res <- pure $ CmdTest.runAgain res StepPlay.cmdAdvance
    equal (eMidi res) [Midi.NoteOff Key.e4 0, Midi.NoteOn Key.f4 127]
    equal (CmdTest.extractUi getSel res) $ Right (Just 3, [])

    res <- pure $ CmdTest.runAgain res StepPlay.cmdAdvance
    equal (eMidi res) [Midi.NoteOff Key.f4 0]
    equal (CmdTest.extractUi getSel res) $ Right (Just 4, [])

    -- Ran out of notes.
    res <- pure $ CmdTest.runAgain res StepPlay.cmdAdvance
    equal (eMidi res) []
    leftLike (CmdTest.extractUi getSel res) "can't advance for step play"

    -- Rewind
    res <- pure $ CmdTest.runAgain res StepPlay.cmdRewind
    equal (eMidi res) [Midi.NoteOn Key.f4 127]
    equal (CmdTest.extractUi getSel res) $ Right (Just 3, [])

    -- Go back forward to make sure zipping is zipping properly.
    res <- pure $ CmdTest.runAgain res StepPlay.cmdAdvance
    equal (eMidi res) [Midi.NoteOff Key.f4 0]
    equal (CmdTest.extractUi getSel res) $ Right (Just 4, [])

test_move_tracks :: Test
test_move_tracks = do
    res <- prepareBlocks UiTest.defaultBlockName
        [(UiTest.defaultBlockName,
            noteTrack ["4c", "4d", "4e"] ++ noteTrack ["5c", "5d", "5e"])]
    -- run cmd_set, verify selection is there
    res <- pure $ CmdTest.runAgain res $ do
        CmdTest.setPointSel 2 3
        StepPlay.cmdSet True
    equal (eMidi res) [Midi.NoteOn 64 127]
    -- Ensure the selection is only on one track.
    equal (CmdTest.extractUi getSelTracks res) $ Right (Just (2, [2]), [])

eMidi :: CmdTest.Result a -> [Midi.ChannelMessage]
eMidi = mapMaybe Midi.channelMessage . CmdTest.eMidi

simpleBlock :: [UiTest.BlockSpec]
simpleBlock = [(UiTest.defaultBlockName, simpleTracks)]

simpleTracks :: [UiTest.TrackSpec]
simpleTracks = noteTrack
    ["4c", "4d", "4e", "4f"]
    -- 0   1   2   3
    -- 60  62  64  65

noteTrack :: [Text] -> [UiTest.TrackSpec]
noteTrack notes = UiTest.noteSpec
    ("i1", [(t, 1, pitch) | (t, pitch) <- zip (Lists.range_ 0 1) notes], [])

prepareBlocks :: Text -> [UiTest.BlockSpec] -> IO (CmdTest.Result ())
prepareBlocks focus blocks =
    CmdTest.updatePerf ustate $ CmdTest.run ustate cstate (pure ())
    where
    ustate = UiTest.exec Ui.empty (UiTest.mkviews blocks)
    cstate = CmdTest.defaultCmdState
        { Cmd.stateFocusedView = Just (UiTest.mkVidName focus) }

getSel :: Ui.M m => m (Maybe ScoreTime)
getSel = fmap Sel.curPos <$>
    Ui.getSelection UiTest.defaultViewId StepPlay.selnum

getSelTracks :: Ui.M m => m (Maybe (ScoreTime, [TrackNum]))
getSelTracks =
    fmap (\s -> (Sel.curPos s, Sel.tracknums 99 s)) <$>
        Ui.getSelection UiTest.defaultViewId StepPlay.selnum

getBlockSel :: Ui.M m => Text -> m (Maybe ScoreTime)
getBlockSel name = fmap Sel.curPos <$>
    Ui.getSelection (UiTest.mkVidName name) StepPlay.selnum

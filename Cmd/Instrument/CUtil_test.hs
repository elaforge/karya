-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.Instrument.CUtil_test where
import qualified Data.Map as Map

import qualified Util.Lists as Lists
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))

import qualified Ui.UiTest as UiTest
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Cmd.Instrument.MidiInst as MidiInst
import qualified Cmd.Msg as Msg

import qualified Derive.Attrs as Attrs
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import Global
import Types


test_insert_call :: Test
test_insert_call = do
    let noteOn key = Midi.ChannelMessage 0 (Midi.NoteOn key 127)
        noteOff key = Midi.ChannelMessage 0 (Midi.NoteOff key 127)
        emptyTracks = [(">i1", [])]
    -- NoEdit means midi but no note.
    ioEqual (insertCall emptyTracks 1 False (CmdTest.keyDown 'a'))
        (Right emptyTracks, [noteOn Key.c2])
    ioEqual (insertCall emptyTracks 1 True (CmdTest.keyDown 'a'))
        (Right [(">i1", [(0, 0, "anote")])], [noteOn Key.c2])
    ioEqual (insertCall emptyTracks 1 True (CmdTest.keyUp 'a'))
        (Right [(">i1", [])], [noteOff Key.c2])

insertCall :: [UiTest.TrackSpec] -> TrackNum -> Bool -> Msg.Msg
    -> IO (Either Text [UiTest.TrackSpec], [Midi.Message])
insertCall tracks tracknum valEdit msg =
    fmap extract $ CmdTest.runWithPerformance ustate cstate $ do
        Cmd.modifyEditState (setEditMode valEdit)
        CmdTest.setSel tracknum 0 tracknum 0
        Cmd.call (CUtil.insertCall CUtil.MidiThru charToCall) msg
    where
    (ustate, cstate) = CmdTest.setSynthsSimple [makeSynth strokeKeys]
        [("i1", "synth/1")] (CmdTest.makeTracks tracks)
        CmdTest.defaultCmdState
    charToCall = CUtil.strokesToCalls (map fst strokeKeys)
    strokeKeys =
        [ (Drums.stroke 'a' "anote" (Attrs.attr "a"), Key.c2)
        , (Drums.stroke 'b' "bnote" (Attrs.attr "b"), Key.d2)
        ]
    extract r = (CmdTest.traceLogs (CmdTest.eTracks r), CmdTest.eMidi r)

setEditMode :: Bool -> Cmd.EditState -> Cmd.EditState
setEditMode valEdit state = state
    { Cmd.stateEditMode = if valEdit then Cmd.ValEdit else Cmd.NoEdit
    , Cmd.stateKbdEntry = True
    }

test_drum_instrument :: Test
test_drum_instrument = do
    let run = DeriveTest.deriveTracksSetup
            (DeriveTest.withSynthsSimple allocs [drumSynth]) ""
        allocs = [("x", "synth/1")]
        extract = DeriveTest.extract DeriveTest.eAttributes
    let perform = DeriveTest.performSynthsSimple allocs [drumSynth]
            . Derive.rEvents
    let result = run [(">x", [(0, 0, "bd"), (1, 0, "sn")])]
    equal (extract result) (["+bd", "+snare"], [])

    let ((_, midi), logs) = perform result
    equal logs []
    let eMidi = Lists.mapMaybeSnd Midi.channelMessage
            . filter (Midi.isNote . snd) . DeriveTest.extractMidi
    equal (eMidi midi)
        [ (0, NoteOn Key.c2 127), (20, NoteOff Key.c2 127)
        , (1000, NoteOn Key.d2 127), (1020, NoteOff Key.d2 127)
        ]

    -- Ensure the generated calls invert.
    equal (DeriveTest.extract Score.initialDynamic $
            run [(">x", [(0, 0, "bd")]), ("dyn", [(0, 0, ".5")])])
        ([0.5], [])

test_make_cc_keymap :: Test
test_make_cc_keymap = do
    let f = CUtil.makeCcKeymap
        cw = Patch.ControlSwitch
    equal (f 0 12 Key.c4
        [[Attrs.left, Attrs.right], [Attrs.open], [Attrs.low, Attrs.high]]) $
        Map.fromList
            [ (Attrs.left, ([cw 102 0], Key.c_1, Key.b_1, Key.c4))
            , (Attrs.right, ([cw 102 1], Key.c_1, Key.b_1, Key.c4))
            , (Attrs.open, ([], Key.c0, Key.b0, Key.c4))
            , (Attrs.low, ([cw 103 0], Key.c1, Key.b1, Key.c4))
            , (Attrs.high, ([cw 103 1], Key.c1, Key.b1, Key.c4))
            ]

drumSynth :: MidiInst.Synth
drumSynth = makeSynth [(Drums.cBd, Key.c2), (Drums.cSn, Key.d2)]

makeSynth :: [(Drums.Stroke, Midi.Key)] -> MidiInst.Synth
makeSynth strokeKeys = UiTest.makeSynth "synth" [patch]
    where
    patch = MidiInst.code #= code $
        CUtil.drumPatch strokeKeys $ MidiInst.namedPatch (-24, 24) "1" []
    code =
        MidiInst.noteGenerators
            (CUtil.drumCalls (map ((,CUtil.callConfig) . fst) strokeKeys))
        <> MidiInst.cmd (CUtil.drumCmd [] CUtil.MidiThru (map fst strokeKeys))

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.MidiThru_test where
import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import           Cmd.CmdTest (control, noteOff)
import qualified Cmd.InputNote as InputNote
import qualified Cmd.MidiThru as MidiThru
import qualified Cmd.Perf as Perf
import qualified Cmd.Selection as Selection

import qualified Derive.Attrs as Attrs
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Pitch as Pitch
import qualified Ui.Ui as Ui
import qualified Ui.UiConfig as UiConfig
import qualified Ui.UiTest as UiTest

import           Global
import           Util.Test


test_midi_thru_instrument :: Test
test_midi_thru_instrument = do
    -- Use *just instead of *twelve, because *twelve has a special thru that
    -- just passes the NN directly.
    let run cmdState title =
            runThru cmdState (" | scale=just" <> title) (pure ())
            . (mempty,)
        eNote =
            first (mapMaybe Midi.channelMessage . filter Midi.isNote)
            . eMidi
        middleC = CmdTest.noteOn 1 Pitch.middleC
        cstate = CmdTest.defaultCmdState

    -- Input out of range is not an error.
    ioEqual (eNote <$> run cstate "" (CmdTest.noteOn 1 (Pitch.pitch 99 0)))
        ([], [])
    ioEqual (eNote <$> run cstate " | t-oct=1" middleC)
        ([Midi.NoteOn Key.c5 127], [])
    -- Only octave transposition is applied.
    ioEqual (eNote <$> run cstate " | t-dia=1" middleC)
        ([Midi.NoteOn Key.c4 127], [])

    -- io_equal (run e_note "" middle_c) ([Midi.NoteOn Key.c4 127], [])
    result <- run cstate "" middleC
    equal (eNote result) ([Midi.NoteOn Key.c4 127], [])
    result <- run (CmdTest.resultCmdState result) "" (CmdTest.noteOff 1)
    equal (eNote result) ([Midi.NoteOff Key.c4 127], [])

    -- With *wayang, I should get different pitches based on tuning.
    umbang <- eMidi <$> run cstate " | scale=wayang | tuning=umbang"
        (CmdTest.noteOn 1 Pitch.middleC)
    isep <- eMidi <$> run cstate " | scale=wayang | tuning=isep"
        (CmdTest.noteOn 1 Pitch.middleC)
    notEqual umbang isep

test_patch_scale :: Test
test_patch_scale = do
    let run cmdState instDb scale =
            runThru cmdState " | scale=legong" (setConfig instDb scale)
        ePitches =
            first (mapMaybe Midi.channelMessage . filter Midi.isPitched)
            . eMidi
        setConfig instDb scale =
            setMidiConfig instDb (Patch.settings#Patch.scale #= scale)
    let legong = Legong.completeInstrumentScale
            Legong.larasRambat BaliScales.Umbang
        c4 = CmdTest.noteOn 1 (Pitch.pitch 4 0)
        cstate = CmdTest.defaultCmdState
    -- No Patch.Scale means it assumes the patch is in 12TET and needs a tweak.
    let db = UiTest.defaultDb
    ioEqual (ePitches <$> run cstate db Nothing (mempty, c4))
        ([Midi.PitchBend 0.455, Midi.NoteOn Key.b3 127], [])

    ioEqual (ePitches <$> run cstate db (Just legong) (mempty, c4))
        ([Midi.PitchBend 0, Midi.NoteOn Key.c4 127], [])

    -- PitchKeymap also goes through the Patch.Scale.
    let instDb = UiTest.makeDb [("s", [pitchedKeymapPatch legong])]
    ioEqual (ePitches <$> run cstate instDb (Just legong) (mempty, c4))
        ([Midi.NoteOn 1 64, Midi.PitchBend 0, Midi.NoteOn Key.c3 127], [])
    ioEqual (ePitches <$> run cstate instDb (Just legong) (Attrs.mute, c4))
        ([Midi.NoteOn 0 64, Midi.PitchBend 0, Midi.NoteOn Key.c1 127], [])

    result <- run cstate instDb (Just legong) (mempty, c4)
    equal (ePitches result)
        ([Midi.NoteOn 1 64, Midi.PitchBend 0, Midi.NoteOn Key.c3 127], [])
    result <- run (CmdTest.resultCmdState result) instDb (Just legong)
        (mempty, CmdTest.noteOff 1)
    equal (ePitches result)
        ([Midi.NoteOff 1 64, Midi.NoteOff Key.c3 127], [])

pitchedKeymapPatch :: Patch.Scale -> Patch.Patch
pitchedKeymapPatch scale =
    Patch.attributeMap #= attrMap $ makePatch (Just scale)
    where
    makePatch scale = Patch.defaults#Patch.scale #= scale $
        Patch.patch (-1, 1) "1"
    attrMap = Common.attributeMap
        [ (Attrs.mute, ([Patch.Keyswitch 0],
            Just (Patch.PitchedKeymap Key.c1 Key.c2 Key.c4)))
        , (mempty, ([Patch.Keyswitch 1],
            Just (Patch.PitchedKeymap Key.c3 Key.c4 Key.c4)))
        ]

-- | There is also CmdTest.set_synths, but it expects to work with raw states
-- instead of Cmd.M.
setMidiConfig :: Cmd.M m => Cmd.InstrumentDb -> (Patch.Config -> Patch.Config)
    -> m ()
setMidiConfig instDb modifyConfig = do
    Ui.modifyConfig $ UiConfig.allocations
        %= UiTest.modifyMidiConfig "i1" modifyConfig
    Cmd.modify $ \st -> st
        { Cmd.stateConfig = (Cmd.stateConfig st)
            { Cmd.configInstrumentDb = instDb }
        }

runThru :: Cmd.State -> Text -> Cmd.CmdT IO ()
    -> (Attrs.Attributes, InputNote.Input) -> IO (CmdTest.Result ())
runThru cmdState title setup (attrs, input) =
    CmdTest.runWithPerformance (CmdTest.makeTracks tracks) cmdState $ do
        CmdTest.setPointSel 1 0
        setup
        Cmd.liftId $ do
            scale <- Perf.getScale =<< Selection.track
            mapM_ Cmd.writeThru
                =<< MidiThru.forInstrument inst scale attrs input
    where
    tracks = [(">i1" <> title, [])]
    inst = "i1"

eMidi :: CmdTest.Result a -> ([Midi.Message], [Text])
eMidi result = (CmdTest.eMidi result, CmdTest.eLogs result)

test_input_to_midi :: Test
test_input_to_midi = do
    let wdev = UiTest.wdev
        addrs = [(wdev, 0), (wdev, 1), (wdev, 2)]
    let f = extract . thread . map (addrs,)
        extract = map (extractMsg . snd) . fst
        thread = threadInputToMidi Cmd.emptyWdevState
    let pitch = CmdTest.pitchChangeNn
        noteOn = CmdTest.noteOnNn
    let on key = Midi.NoteOn key 127
        off key = Midi.NoteOff key 127
        pb = Midi.PitchBend

    -- orphan controls are ignored
    equal (f [control 1 "cc1" 127, pitch 1 64]) []

    -- unrelated pitch_changes don't show up
    equal (f [noteOn 64, pitch 1 65, pitch 64 63, pitch 64 1])
        [ (0, pb 0)
        , (0, on 64)
        , (0, pb (-0.5))
        , (0, pb (-1))
        ]

    -- null addrs discards msgs
    equal (thread [([], noteOn 1)])
        ([], Cmd.emptyWdevState)

    -- Too many notes get addrs in round-robin.
    equal (f (map noteOn (Lists.range 1 6 1))) $ concat
        [[(chan, pb 0), (chan, on n)] | (chan, n) <- zip (cycle [0..2]) [1..6]]

    -- It's round-robin even after a note-off.
    equal (f [noteOn 1, noteOff 1, noteOn 2])
        [(0, pb 0), (0, on 1), (0, off 1), (1, pb 0), (1, on 2)]

    -- once assigned a note_id, controls get mapped to that channel
    equal (f [noteOn 64, noteOn 66, control 64 "mod" 1,
            control 66 "breath" 0.5])
        [ (0, pb 0), (0, on 64)
        , (1, pb 0), (1, on 66)
        , (0, Midi.ControlChange 1 127), (1, Midi.ControlChange 2 63)
        ]

    -- Not misled by the same note on a different addr.
    equal (extract $ thread
            [ ([(wdev, 2)], noteOn 64)
            , ([(wdev, 3)], noteOn 64)
            ])
        [ (2, pb 0), (2, on 64)
        , (3, pb 0), (3, on 64)
        ]

extractMsg :: HasCallStack => Midi.Message
    -> (Midi.Channel, Midi.ChannelMessage)
extractMsg (Midi.ChannelMessage chan msg) = (chan, msg)
extractMsg msg = error $ "bad msg: " <> show msg

-- | Thread the given values through 'MidiThru.input_to_midi'.
threadInputToMidi :: Cmd.WriteDeviceState
    -> [([Patch.Addr], InputNote.Input)]
    -> ([(Midi.WriteDevice, Midi.Message)], Cmd.WriteDeviceState)
threadInputToMidi initialState = foldl go ([], initialState)
    where
    go (prevMsgs, state) (addrs, input) = (prevMsgs ++ msgs, nextState)
        where
        (msgs, nextState) = fromMaybe ([], state) $
            MidiThru.inputToMidi (-2, 2) state addrs (convertInput input)

convertInput :: InputNote.Input -> InputNote.InputNn
convertInput input = case input of
    InputNote.NoteOn noteId input vel ->
        InputNote.NoteOn noteId (InputNote.inputToNn input) vel
    InputNote.PitchChange noteId input ->
        InputNote.PitchChange noteId (InputNote.inputToNn input)
    InputNote.NoteOff noteId vel -> InputNote.NoteOff noteId vel
    InputNote.Control noteId control val ->
        InputNote.Control noteId control val

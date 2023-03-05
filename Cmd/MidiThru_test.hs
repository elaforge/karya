-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.MidiThru_test where
import qualified Util.CallStack as CallStack
import qualified Util.Seq as Seq
import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import Cmd.CmdTest (note_off, control)
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

import Global
import Util.Test


test_midi_thru_instrument :: Test
test_midi_thru_instrument = do
    -- Use *just instead of *twelve, because *twelve has a special thru that
    -- just passes the NN directly.
    let run cmd_state title =
            run_thru cmd_state (" | scale=just" <> title) (return ())
            . (mempty,)
        e_note =
            first (mapMaybe Midi.channel_message . filter Midi.is_note)
            . e_midi
        middle_c = CmdTest.note_on 1 Pitch.middle_c
        cstate = CmdTest.default_cmd_state

    -- Input out of range is not an error.
    io_equal (e_note <$> run cstate "" (CmdTest.note_on 1 (Pitch.pitch 99 0)))
        ([], [])
    io_equal (e_note <$> run cstate " | t-oct=1" middle_c)
        ([Midi.NoteOn Key.c5 127], [])
    -- Only octave transposition is applied.
    io_equal (e_note <$> run cstate " | t-dia=1" middle_c)
        ([Midi.NoteOn Key.c4 127], [])

    -- io_equal (run e_note "" middle_c) ([Midi.NoteOn Key.c4 127], [])
    result <- run cstate "" middle_c
    equal (e_note result) ([Midi.NoteOn Key.c4 127], [])
    result <- run (CmdTest.result_cmd_state result) "" (CmdTest.note_off 1)
    equal (e_note result) ([Midi.NoteOff Key.c4 127], [])

    -- With *wayang, I should get different pitches based on tuning.
    umbang <- e_midi <$> run cstate " | scale=wayang | tuning=umbang"
        (CmdTest.note_on 1 Pitch.middle_c)
    isep <- e_midi <$> run cstate " | scale=wayang | tuning=isep"
        (CmdTest.note_on 1 Pitch.middle_c)
    not_equal umbang isep

test_patch_scale :: Test
test_patch_scale = do
    let run cmd_state inst_db scale =
            run_thru cmd_state " | scale=legong" (set_config inst_db scale)
        e_pitches =
            first (mapMaybe Midi.channel_message . filter Midi.is_pitched)
            . e_midi
        set_config inst_db scale =
            set_midi_config inst_db (Patch.settings#Patch.scale #= scale)
    let legong = Legong.complete_instrument_scale
            Legong.laras_rambat BaliScales.Umbang
        c4 = CmdTest.note_on 1 (Pitch.pitch 4 0)
        cstate = CmdTest.default_cmd_state
    -- No Patch.Scale means it assumes the patch is in 12TET and needs a tweak.
    let db = UiTest.default_db
    io_equal (e_pitches <$> run cstate db Nothing (mempty, c4))
        ([Midi.PitchBend 0.455, Midi.NoteOn Key.b3 127], [])

    io_equal (e_pitches <$> run cstate db (Just legong) (mempty, c4))
        ([Midi.PitchBend 0, Midi.NoteOn Key.c4 127], [])

    -- PitchKeymap also goes through the Patch.Scale.
    let inst_db = UiTest.make_db [("s", [pitched_keymap_patch legong])]
    io_equal (e_pitches <$> run cstate inst_db (Just legong) (mempty, c4))
        ([Midi.NoteOn 1 64, Midi.PitchBend 0, Midi.NoteOn Key.c3 127], [])
    io_equal (e_pitches <$> run cstate inst_db (Just legong) (Attrs.mute, c4))
        ([Midi.NoteOn 0 64, Midi.PitchBend 0, Midi.NoteOn Key.c1 127], [])

    result <- run cstate inst_db (Just legong) (mempty, c4)
    equal (e_pitches result)
        ([Midi.NoteOn 1 64, Midi.PitchBend 0, Midi.NoteOn Key.c3 127], [])
    result <- run (CmdTest.result_cmd_state result) inst_db (Just legong)
        (mempty, CmdTest.note_off 1)
    equal (e_pitches result)
        ([Midi.NoteOff 1 64, Midi.NoteOff Key.c3 127], [])

pitched_keymap_patch :: Patch.Scale -> Patch.Patch
pitched_keymap_patch scale =
    Patch.attribute_map #= attr_map $ make_patch (Just scale)
    where
    make_patch scale = Patch.defaults#Patch.scale #= scale $
        Patch.patch (-1, 1) "1"
    attr_map = Common.attribute_map
        [ (Attrs.mute, ([Patch.Keyswitch 0],
            Just (Patch.PitchedKeymap Key.c1 Key.c2 Key.c4)))
        , (mempty, ([Patch.Keyswitch 1],
            Just (Patch.PitchedKeymap Key.c3 Key.c4 Key.c4)))
        ]

-- | There is also CmdTest.set_synths, but it expects to work with raw states
-- instead of Cmd.M.
set_midi_config :: Cmd.M m => Cmd.InstrumentDb -> (Patch.Config -> Patch.Config)
    -> m ()
set_midi_config inst_db modify_config = do
    Ui.modify_config $ UiConfig.allocations
        %= UiTest.modify_midi_config "i1" modify_config
    Cmd.modify $ \st -> st
        { Cmd.state_config = (Cmd.state_config st)
            { Cmd.config_instrument_db = inst_db }
        }

run_thru :: Cmd.State -> Text -> Cmd.CmdT IO ()
    -> (Attrs.Attributes, InputNote.Input) -> IO (CmdTest.Result ())
run_thru cmd_state title setup (attrs, input) =
    CmdTest.run_with_performance (CmdTest.make_tracks tracks) cmd_state $ do
        CmdTest.set_point_sel 1 0
        setup
        Cmd.lift_id $ do
            scale <- Perf.get_scale =<< Selection.track
            mapM_ Cmd.write_thru
                =<< MidiThru.for_instrument inst scale attrs input
    where
    tracks = [(">i1" <> title, [])]
    inst = "i1"

e_midi :: CmdTest.Result a -> ([Midi.Message], [Text])
e_midi result = (CmdTest.e_midi result, CmdTest.e_logs result)

test_input_to_midi :: Test
test_input_to_midi = do
    let wdev = UiTest.wdev
        addrs = [(wdev, 0), (wdev, 1), (wdev, 2)]
    let f = extract . thread . map (addrs,)
        extract = map (extract_msg . snd) . fst
        thread = thread_input_to_midi Cmd.empty_wdev_state
    let pitch = CmdTest.pitch_change_nn
        note_on = CmdTest.note_on_nn
    let on key = Midi.NoteOn key 127
        off key = Midi.NoteOff key 127
        pb = Midi.PitchBend

    -- orphan controls are ignored
    equal (f [control 1 "cc1" 127, pitch 1 64]) []

    -- unrelated pitch_changes don't show up
    equal (f [note_on 64, pitch 1 65, pitch 64 63, pitch 64 1])
        [ (0, pb 0)
        , (0, on 64)
        , (0, pb (-0.5))
        , (0, pb (-1))
        ]

    -- null addrs discards msgs
    equal (thread [([], note_on 1)])
        ([], Cmd.empty_wdev_state)

    -- Too many notes get addrs in round-robin.
    equal (f (map note_on (Seq.range 1 6 1))) $ concat
        [[(chan, pb 0), (chan, on n)] | (chan, n) <- zip (cycle [0..2]) [1..6]]

    -- It's round-robin even after a note-off.
    equal (f [note_on 1, note_off 1, note_on 2])
        [(0, pb 0), (0, on 1), (0, off 1), (1, pb 0), (1, on 2)]

    -- once assigned a note_id, controls get mapped to that channel
    equal (f [note_on 64, note_on 66, control 64 "mod" 1,
            control 66 "breath" 0.5])
        [ (0, pb 0), (0, on 64)
        , (1, pb 0), (1, on 66)
        , (0, Midi.ControlChange 1 127), (1, Midi.ControlChange 2 63)
        ]

    -- Not misled by the same note on a different addr.
    equal (extract $ thread
            [ ([(wdev, 2)], note_on 64)
            , ([(wdev, 3)], note_on 64)
            ])
        [ (2, pb 0), (2, on 64)
        , (3, pb 0), (3, on 64)
        ]

extract_msg :: CallStack.Stack => Midi.Message
    -> (Midi.Channel, Midi.ChannelMessage)
extract_msg (Midi.ChannelMessage chan msg) = (chan, msg)
extract_msg msg = error $ "bad msg: " <> show msg

-- | Thread the given values through 'MidiThru.input_to_midi'.
thread_input_to_midi :: Cmd.WriteDeviceState
    -> [([Patch.Addr], InputNote.Input)]
    -> ([(Midi.WriteDevice, Midi.Message)], Cmd.WriteDeviceState)
thread_input_to_midi initial_state = foldl go ([], initial_state)
    where
    go (prev_msgs, state) (addrs, input) = (prev_msgs ++ msgs, next_state)
        where
        (msgs, next_state) = fromMaybe ([], state) $
            MidiThru.input_to_midi (-2, 2) state addrs (convert_input input)

convert_input :: InputNote.Input -> InputNote.InputNn
convert_input input = case input of
    InputNote.NoteOn note_id input vel ->
        InputNote.NoteOn note_id (InputNote.input_to_nn input) vel
    InputNote.PitchChange note_id input ->
        InputNote.PitchChange note_id (InputNote.input_to_nn input)
    InputNote.NoteOff note_id vel -> InputNote.NoteOff note_id vel
    InputNote.Control note_id control val ->
        InputNote.Control note_id control val

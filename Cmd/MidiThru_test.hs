-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Cmd.MidiThru_test where
import qualified Data.Map as Map

import qualified Util.Seq as Seq
import Util.Test
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.State as State
import qualified Ui.StateConfig as StateConfig
import qualified Ui.UiTest as UiTest

import qualified Cmd.Cmd as Cmd
import qualified Cmd.CmdTest as CmdTest
import Cmd.CmdTest (note_off, control)
import qualified Cmd.InputNote as InputNote
import qualified Cmd.MidiThru as MidiThru

import qualified Derive.Attrs as Attrs
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Scale.Legong as Legong
import qualified Derive.Score as Score

import qualified Perform.Midi.Patch as Patch
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch

import qualified Instrument.Common as Common
import Global


test_midi_thru_instrument = do
    -- Use *just instead of *twelve, because *twelve has a special thru that
    let run extract title = fmap (first extract)
            . run_thru (" | scale=just" <> title) (return ())
        e_note_on = mapMaybe Midi.channel_message . filter Midi.is_note_on
        middle_c = CmdTest.note_on 1 Pitch.middle_c

    -- Input out of range is not an error.
    io_equal (run e_note_on "" (CmdTest.note_on 1 (Pitch.pitch 99 0))) ([], [])
    -- ignores non-octave transposition.
    io_equal (run e_note_on "" middle_c) ([Midi.NoteOn Key.c4 127], [])
    io_equal (run e_note_on " | %t-oct=1" middle_c)
        ([Midi.NoteOn Key.c5 127], [])
    -- Only octave transposition is applied.
    io_equal (run e_note_on " | %t-dia=1" middle_c)
        ([Midi.NoteOn Key.c4 127], [])

    -- With *wayang, I should get different pitches based on tuning.
    umbang <- run id " | scale=wayang | tuning=umbang"
        (CmdTest.note_on 1 Pitch.middle_c)
    isep <- run id " | scale=wayang | tuning=isep"
        (CmdTest.note_on 1 Pitch.middle_c)
    not_equal umbang isep

test_patch_scale = do
    let run inst_db scale = fmap (first e_pitches)
            . run_thru " | scale=legong" (set_config inst_db scale)
        e_pitches = mapMaybe Midi.channel_message . filter Midi.is_pitched
        make_config scale = Patch.cscale #= scale $ Patch.config1 UiTest.wdev 0
        set_config inst_db scale = do
            set_midi_config inst_db (make_config scale)
    let legong = Legong.complete_instrument_scale BaliScales.Umbang
        c4 = CmdTest.note_on 1 (Pitch.pitch 4 0)
    io_equal (run DeriveTest.default_db Nothing c4)
        ([Midi.PitchBend 0.365, Midi.NoteOn Key.c4 127], [])
    io_equal (run DeriveTest.default_db (Just legong) c4)
        ([Midi.NoteOn Key.c4 127], [])
    -- PitchKeymap also goes through the Patch.Scale.
    let inst_db = DeriveTest.make_db [("s", [pitched_keymap_patch legong])]
    io_equal (run inst_db (Just legong) c4)
        ([Midi.NoteOn Key.c3 127], [])

pitched_keymap_patch :: Patch.Scale -> Patch.Patch
pitched_keymap_patch scale =
    Patch.attribute_map #= attr_map $ make_patch (Just scale)
    where
    make_patch scale = Patch.scale #= scale $ Patch.patch (-1, 1) "1"
    attr_map = Common.attribute_map
        [ (Attrs.mute, ([Patch.Keyswitch 0],
            Just (Patch.PitchedKeymap Key.c1 Key.c2 Key.c4)))
        , (mempty, ([Patch.Keyswitch 1],
            Just (Patch.PitchedKeymap Key.c3 Key.c4 Key.c4)))
        ]

-- | There is also CmdTest.set_synths, but it expects to work with raw states
-- instead of Cmd.M.
set_midi_config :: Cmd.M m => Cmd.InstrumentDb -> Patch.Config -> m ()
set_midi_config inst_db config = do
    State.modify_config $ StateConfig.allocations_map
        %= Map.insert UiTest.i1 alloc
    Cmd.modify $ \st -> st
        { Cmd.state_config = (Cmd.state_config st)
            { Cmd.config_instrument_db = inst_db }
        }
    where
    alloc = StateConfig.Allocation UiTest.i1_qualified Common.empty_config
        (StateConfig.Midi config)

run_thru :: String -> Cmd.CmdT IO () -> InputNote.Input
    -> IO ([Midi.Message], [String])
run_thru title setup input =
    fmap extract $ CmdTest.run_perf_tracks [(">i1" <> title, [])] $ do
        CmdTest.set_point_sel 1 0
        setup
        MidiThru.midi_thru_instrument (Score.Instrument "i1") input
    where extract result = (CmdTest.e_midi result, CmdTest.e_logs result)

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

    -- orphan controls are ignored
    equal (f [control 1 "cc1" 127, pitch 1 64]) []

    -- redundant and unrelated pitch_changes filtered
    equal (f [note_on 64, pitch 1 65, pitch 64 63, pitch 64 1, pitch 64 1])
        [ (0, on 64)
        , (0, Midi.PitchBend (-0.5))
        , (0, Midi.PitchBend (-1))
        ]

    -- null addrs discards msgs
    equal (thread [([], note_on 1)])
        ([], Cmd.empty_wdev_state)

    -- Too many notes get addrs in round-robin.
    equal (f (map note_on (Seq.range 1 6 1)))
        [(chan, on n) | (chan, n) <- zip (cycle [0..2]) [1..6]]

    -- It's round-robin even after a note-off.
    equal (f [note_on 1, note_off 1, note_on 2])
        [(0, on 1), (0, off 1), (1, on 2)]

    -- once assigned a note_id, controls get mapped to that channel
    equal (f [note_on 64, note_on 66, control 64 "mod" 1,
            control 66 "breath" 0.5])
        [ (0, Midi.NoteOn 64 127), (1, Midi.NoteOn 66 127)
        , (0, Midi.ControlChange 1 127), (1, Midi.ControlChange 2 63)
        ]

    -- Not misled by the same note on a different addr.
    equal (extract $ thread
            [ ([(wdev, 2)], note_on 64)
            , ([(wdev, 3)], note_on 64)
            ])
        [(2, Midi.NoteOn 64 127), (3, Midi.NoteOn 64 127)]


extract_msg :: Midi.Message -> (Midi.Channel, Midi.ChannelMessage)
extract_msg (Midi.ChannelMessage chan msg) = (chan, msg)
extract_msg msg = error $ "bad msg: " ++ show msg

-- | Thread the given values through 'MidiThru.input_to_midi'.
thread_input_to_midi :: Cmd.WriteDeviceState
    -> [([Patch.Addr], InputNote.Input)]
    -> ([(Midi.WriteDevice, Midi.Message)], Cmd.WriteDeviceState)
thread_input_to_midi initial_state = foldl go ([], initial_state)
    where
    go (prev_msgs, state) (addrs, input) = case next_state of
            Nothing -> (next_msgs, state)
            Just next_state -> (next_msgs, next_state)
        where
        (msgs, next_state) = MidiThru.input_to_midi (-2, 2) state addrs
            (convert_input input)
        next_msgs = prev_msgs ++ msgs

convert_input :: InputNote.Input -> InputNote.InputNn
convert_input input = case input of
    InputNote.NoteOn note_id input vel ->
        InputNote.NoteOn note_id (InputNote.input_to_nn input) vel
    InputNote.PitchChange note_id input ->
        InputNote.PitchChange note_id (InputNote.input_to_nn input)
    InputNote.NoteOff note_id vel -> InputNote.NoteOff note_id vel
    InputNote.Control note_id control val ->
        InputNote.Control note_id control val

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Convert_test where
import qualified Data.Map as Map
import qualified Data.Set as Set

import Util.Control
import qualified Util.Log as Log
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Cmd.Instrument.CUtil as CUtil
import qualified Cmd.Instrument.Drums as Drums
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified App.MidiInst as MidiInst
import Types


test_lazy = do
    let events = zipWith ($) (cycle [noinst, nopitch, good])
            (map RealTime.seconds [0..])
    equal (length (take 3 (convert events))) 3

test_convert = do
    equal (convert [noinst 0, nopitch 1, good 2])
        [ Right $ "event requires patch in instrument db: "
            ++ ">noinst (further warnings suppressed)"
        -- emits an event anyway so the previous pitch doesn't continue
        , Left (1, [])
        , Left (2, [(2, 60)])
        ]
    equal (convert [good 2, good 0, good 1])
        [ Left (2, [(2, 60)])
        , Left (0, [(0, 60)])
        , Right "start time 0s less than previous of 2s"
        , Left (1, [(1, 60)])
        ]

test_convert_controls = do
    let f pressure = first Map.toList
            . Convert.convert_controls pressure Map.empty
            . DeriveTest.mkcontrols
    equal (f False [("dyn", [(0, 0.5)])])
        ([(Controls.velocity, Signal.constant 0.5)], Nothing)
    equal (f True [("dyn", [(0, 0.5)])])
        ([(Controls.breath, Signal.constant 0.5)], Nothing)
    -- If both vel and dyn are present, dyn wins.  This is because calls
    -- should tend to use dyn, since its more generic.  But if the track is
    -- using vel directly, the the dyn information will be shadowed.
    equal (f False [("vel", [(0, 1)]), ("dyn", [(0, 0.5)])])
        ([(Controls.velocity, Signal.constant 0.5)],
            Just ("vel", Score.untyped (Signal.signal [(0, 1)])))
    -- No warning if it was null.
    equal (f False [("vel", []), ("dyn", [(0, 0.5)])])
        ([(Controls.velocity, Signal.constant 0.5)], Nothing)

noinst n = mkevent n "4c" "noinst"
nopitch n = (mkevent n "4c" "s/1") { Score.event_pitch = mempty }
good n = mkevent n "4c" "s/1"

mkevent :: RealTime -> String -> Text -> Score.Event
mkevent start pitch inst =
    DeriveTest.mkevent (start, 1, pitch, [], Score.Instrument inst)

convert :: [Score.Event] -> [Either (Double, [(Signal.X, Signal.Y)]) String]
convert events =
    show_logs extract_event $
        Convert.convert DeriveTest.default_convert_lookup events

extract_event e = (RealTime.to_seconds (Perform.event_start e),
    Signal.unsignal (Perform.event_pitch e))

show_logs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.show_log)

-- * patch scale

test_patch_scale = do
    let res = DeriveTest.derive_tracks
            [ (">s/inst", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4c#"), (2, 0, "4d")])
            ]
    let (evts, _midi, _logs) =
            DeriveTest.perform (DeriveTest.make_convert_lookup db) config
                (Derive.r_events res)
    equal (map (Signal.unsignal . Perform.event_pitch) evts)
        [[(0, 1)], [(1, 1.5)], [(2, 2)]]
    where
    db = DeriveTest.make_db [("s", [patch])]
    pscale = Instrument.make_patch_scale [(1, 60), (2, 62), (3, 63)]
    patch = Instrument.set_scale pscale $
        Instrument.patch $ Instrument.instrument "inst" [] (-12, 12)
    config = UiTest.midi_config [("s/inst", [0])]

-- * composite instrument

test_composite_instrument = do
    let patch =
            (set_composite Nothing [] . CUtil.drum_instrument notes,
                mempty)
        notes = [(Drums.c_bd, Key.c4), (Drums.c_sn, Key.d4)]
    let (events, _, logs) = perform patch [("s/i", [0]), ("s/x", [1])]
            [ (">s/i", [(0, 1, "+bd"), (1, 1, "+snare")])
            , ("*", [(0, 0, "3c"), (0.5, 0, "3d")])
            ]
        extract e =
            ( Instrument.inst_name $ Perform.event_instrument e
            , Perform.event_start e
            , Signal.unsignal $ Perform.event_pitch e
            )
    equal logs []
    equal (map extract events)
        [ ("i", 0, [(0, 60)]), ("x", 0, [(0, 48), (0.5, 50)])
        , ("i", 1, [(0, 62)]), ("x", 1, [(0.5, 50)])
        ]

set_composite :: Maybe Score.Control -> [Score.Control] -> Instrument.Patch
    -> Instrument.Patch
set_composite pitch controls = Instrument.composite
    #= [(Score.Instrument "s/x", pitch, Set.fromList controls)]

-- * keymap

test_keymap = do
    let patch = (set_keymap [("bd", (Key.c2, Key.c3, Just NN.c4))], mempty)
        set_keymap kmap = Instrument.keymap
            #= Map.fromList (map (first Score.attr) kmap)
        mktracks ps =
            [ (">s/i", [(n, 1, "+bd") | (n, _) <- vals])
            , ("*", [(n, 0, p) | (n, p) <- vals])
            ]
            where vals = zip (Seq.range_ 0 1) ps
        unsig = map (second Pitch.nn) . Signal.unsignal
    let (events, _, logs) = perform patch [("s/i", [0])]
            (mktracks ["3c", "4c", "5c", "6c"])
    equal logs []
    equal (map (unsig . Perform.event_pitch) events)
        [ [(0, NN.c2)]
        , [(1, NN.c2)]
        , [(2, NN.c3)]
        , [(3, NN.c3)]
        ]

-- * implementation

perform :: (Instrument.Patch -> Instrument.Patch, MidiInst.Code)
    -> [(Text, [Midi.Channel])] -> [UiTest.TrackSpec]
    -> ([Perform.Event], [Midi.WriteMessage], [Log.Msg])
perform (set_patch, code) alloc tracks =
    DeriveTest.perform_inst synth alloc (Derive.r_events result)
    where
    synth = mksynth code set_patch
    result = DeriveTest.derive_tracks_with
        (DeriveTest.with_inst_db synth) tracks

mksynth :: MidiInst.Code -> (Instrument.Patch -> Instrument.Patch)
    -> [MidiInst.SynthDesc]
mksynth code set_patch = MidiInst.make $
    (MidiInst.softsynth "s" "Synth" (-2, 2) [])
    { MidiInst.extra_patches = [(set_patch (DeriveTest.make_patch "i"), code)]
    }

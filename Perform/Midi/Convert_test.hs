-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Convert_test where
import qualified Data.Map as Map

import qualified Util.Log as Log
import qualified Util.Num as Num
import qualified Util.Seq as Seq
import Util.Test

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Ui.UiTest as UiTest
import qualified Cmd.Simple as Simple
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Environ as Environ
import qualified Derive.LEvent as LEvent
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Global
import Types


test_convert = do
    let noinst n = mkevent n "4c" "noinst"
        nopitch n = Score.set_pitch mempty $ mkevent n "4c" "i1"
        good n = mkevent n "4c" "i1"

    equal (convert [noinst 0, nopitch 1, good 2])
        [ Right $ "event requires patch in instrument db: "
            ++ ">noinst (further warnings suppressed)"
        -- emits an event anyway so the previous pitch doesn't continue
        , Left (1, [])
        , Left (2, [(2, 60)])
        ]
    -- Make sure it's lazy.
    let events = zipWith ($) (cycle [noinst, nopitch, good])
            (map RealTime.seconds [0..])
    equal (length (take 3 (convert events))) 3

test_convert_dynamic = do
    let f pressure controls dyn_function = first e_controls $
            Convert.convert_dynamic pressure
                (DeriveTest.mkcontrols_const controls)
                dyn_function
        e_controls = map (second (Signal.unsignal . Score.typed_val))
            . filter ((`elem` [Controls.velocity, Controls.breath]) . fst)
            . Map.toList
    equal (f False [(Controls.dynamic, 1)] (Just 0.5))
        ([(Controls.velocity, [(0, 0.5)])], Nothing)
    equal (f True [(Controls.dynamic, 1)] (Just 0.5))
        ([(Controls.breath, [(0, 1)])], Nothing)

    -- If both vel and dyn are present, dyn shadows vel, but warn about vel.
    equal (f False [(Controls.velocity, 1)] (Just 0.5))
        ([(Controls.velocity, [(0, 0.5)])], Just Controls.velocity)

test_rnd_vel = do
    let run dyn notes = first extract $ DeriveTest.perform_block
            [ (">i1 | %dyn = " <> dyn,
                [(n, 1, "") | n <- Seq.range' 0 notes 1])
            , ("*", [(0, 0, "4c")])
            ]
        extract midi = [vel | (_, _, vel) <- DeriveTest.note_on_vel midi]

    equal (run ".5" 1) ([64], [])
    let (vels, logs) = run "(cf-rnd 0 1)" 10
    equal logs []
    check $ not (all (== head vels) vels)
    check $ all (Num.in_range 40 90) vels

test_convert_pitch = do
    let event tsig = DeriveTest.mkevent
            (0, 1, "4c", [(Controls.diatonic, tsig)], DeriveTest.i1)
    equal (convert [event []]) [Left (0, [(0, 60)])]
    equal (convert [event [(0, 1)]]) [Left (0, [(0, 62)])]
    equal (convert [event [(0, 100)]])
        [ Left (0, [])
        , Right "convert pitch: note can't be transposed: 100d"
        ]
    -- An out of range transposition shouldn't cause a warning.
    equal (convert [event [(0, 0), (100, 100)]]) [Left (0, [(0, 60)])]

    -- Convert applies the environ to pitches.
    let event = (DeriveTest.mkevent (0, 1, "4i", [], DeriveTest.i1))
            { Score.event_untransformed_pitch =
                PitchSignal.signal [(0, DeriveTest.mkpitch legong "4i")]
            }
        Just (Scale.Simple legong) = Map.lookup "legong" Scale.All.scales
    equal (convert [event]) [Left (0, [(0, 72.46)])]
    let insert = Score.modify_environ $
            TrackLang.insert_val Environ.tuning Environ.isep
    equal (convert [insert event]) [Left (0, [(0, 72.588)])]

mkevent :: RealTime -> String -> Text -> Score.Event
mkevent start pitch inst =
    DeriveTest.mkevent (start, 1, pitch, [], Score.Instrument inst)

convert :: [Score.Event] -> [Either (Double, [(Signal.X, Signal.Y)]) String]
convert events =
    show_logs extract_event $
        Convert.convert DeriveTest.default_convert_lookup events

extract_event :: Perform.Event -> (Double, [(Signal.X, Signal.Y)])
extract_event e = (RealTime.to_seconds (Perform.event_start e),
    Signal.unsignal (Perform.event_pitch e))

show_logs :: (a -> b) -> [LEvent.LEvent a] -> [Either b String]
show_logs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.show_log)

-- * patch scale

test_patch_scale = do
    let (_, (evts, _midi, logs)) = perform patch [("i1", "s/1")]
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4c#"), (2, 0, "4d")])
            ]
    equal logs []
    equal (map (Signal.unsignal . Perform.event_pitch) evts)
        [[(0, 1)], [(1, 1.5)], [(2, 2)]]
    where
    patch = Instrument.scale #= Just pscale $ DeriveTest.make_patch "1"
    pscale = Instrument.make_patch_scale "test" [(1, 60), (2, 62), (3, 63)]

-- * keymap

test_pitched_keymap = do
    let patch = set_keymap [bd] $ DeriveTest.make_patch "1"
        bd = ("bd", Instrument.PitchedKeymap Key.c2 Key.c3 NN.c4)
        set_keymap kmap = Instrument.attribute_map
            #= Instrument.keymap (map (first Score.attr) kmap)
        mktracks ps =
            [ (">i1", [(n, 1, "+bd") | (n, _) <- vals])
            , ("*", [(n, 0, p) | (n, p) <- vals])
            ]
            where vals = zip (Seq.range_ 0 1) ps
    let (res, (events, _, logs)) = perform patch [("i1", "s/1")]
            (mktracks ["3c", "4c", "5c", "6c"])
    equal (DeriveTest.r_logs res) []
    equal logs []
    equal (map (nn_signal . Perform.event_pitch) events)
        [ [(0, NN.c2)]
        , [(1, NN.c2)]
        , [(2, NN.c3)]
        , [(3, NN.c3)]
        ]

-- * implementation

nn_signal :: Signal.NoteNumber -> [(Signal.X, Pitch.NoteNumber)]
nn_signal = map (second Pitch.nn) . Signal.unsignal

perform :: Instrument.Patch -> Simple.Aliases -> [UiTest.TrackSpec]
    -> (Derive.Result, ([Perform.Event], [Midi.WriteMessage], [Log.Msg]))
perform patch aliases tracks = (result, performance)
    where
    performance = DeriveTest.perform
        (DeriveTest.make_convert_lookup aliases db) config
        (Derive.r_events result)
    config = UiTest.midi_config
        [(inst, [n]) | (n, inst) <- zip [0..] (map fst aliases)]
    db = DeriveTest.make_db [("s", [patch])]
    result = DeriveTest.derive_tracks_setup
        (DeriveTest.with_inst_db aliases db) "" tracks

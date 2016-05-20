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
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Types
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Instrument.Common as Common
import Global
import Types


test_convert = do
    let run = convert DeriveTest.default_convert_lookup e_pitch
    let noinst n = mkevent n "4c" "noinst"
        nopitch n = Score.set_pitch mempty $ mkevent n "4c" "i1"
        good n = mkevent n "4c" "i1"

    equal (run [noinst 0, nopitch 1, good 2])
        [ Right $ "instrument not found: >noinst"
        -- emits an event anyway so the previous pitch doesn't continue
        , Left (1, [])
        , Left (2, [(2, NN.c4)])
        ]
    -- Make sure it's lazy.
    let events = zipWith ($) (cycle [noinst, nopitch, good])
            (map RealTime.seconds [0..])
    equal (length (take 3 (run events))) 3

test_rnd_vel = do
    let run dyn notes = first extract $ DeriveTest.perform_block
            [ (">i1 | %dyn = " <> dyn, [(n, 1, "") | n <- Seq.range' 0 notes 1])
            , ("*", [(0, 0, "4c")])
            ]
        extract midi = [vel | (_, _, vel) <- DeriveTest.note_on_vel midi]

    equal (run ".5" 1) ([64], [])
    let (vels, logs) = run "(cf-rnd 0 1)" 10
    equal logs []
    check $ not (all (== head vels) vels)
    check $ all (Num.inRange 40 90) vels

test_convert_pitch = do
    let run = convert DeriveTest.default_convert_lookup e_pitch
    let event tsig = DeriveTest.mkevent
            (0, 1, "4c", [(Controls.diatonic, tsig)], UiTest.i1)
    equal (run [event []]) [Left (0, [(0, NN.c4)])]
    equal (run [event [(0, 1)]]) [Left (0, [(0, NN.d4)])]
    equal (run [event [(0, 100)]])
        [ Left (0, [])
        , Right "convert pitch: 232nn is out of range: {%t-dia: 100}"
        ]
    -- An out of range transposition shouldn't cause a warning.
    equal (run [event [(0, 0), (100, 100)]]) [Left (0, [(0, NN.c4)])]

    -- Convert applies the environ to pitches.
    let event = (DeriveTest.mkevent (0, 1, "4i", [], UiTest.i1))
            { Score.event_untransformed_pitch =
                PSignal.signal [(0, DeriveTest.mkpitch legong "4i")]
            }
        Just (Scale.Simple legong) = Map.lookup "legong" Scale.All.scales
    equal (run [event]) [Left (0, [(0, 60.73)])]
    let insert = Score.modify_environ $
            Env.insert_val EnvKey.tuning (ShowVal.show_val BaliScales.Isep)
    equal (run [insert event]) [Left (0, [(0, 60.982)])]

test_patch_scale = do
    let run config patch pitch =
            first (convert (make_lookup config patch) e_pitch) $
            DeriveTest.extract id $ DeriveTest.derive_tracks "" $
                UiTest.note_spec ("i1", [(0, 1, pitch)], [])
        make_lookup config patch = DeriveTest.make_convert_lookup_for UiTest.i1
            (make_config config) patch
        make_config scale = Patch.cscale #= scale $ Patch.config1 UiTest.wdev 0
        make_patch scale = Patch.scale #= scale $ Patch.patch (-1, 1) "1"

    equal (run Nothing (make_patch Nothing) "4c") ([Left (0, [(0, NN.c4)])], [])
    let scale_g = Patch.make_scale "scale" [(Key.c4, NN.g4)]
        scale_e = Patch.make_scale "scale" [(Key.c4, NN.e4)]
    equal (run (Just scale_g) (make_patch Nothing) "4g")
        ([Left (0, [(0, NN.c4)])], [])
    equal (run Nothing (make_patch (Just scale_e)) "4e")
        ([Left (0, [(0, NN.c4)])], [])
    -- Patch config wins.
    equal (run (Just scale_e) (make_patch (Just scale_g)) "4e")
        ([Left (0, [(0, NN.c4)])], [])

    let scale1 = Patch.make_scale "scale"
            [(k, Midi.from_key k + 1) | k <- [0..127]]
    equal (run Nothing (make_patch (Just scale1)) "4c")
        ([Left (0, [(0, NN.b3)])], [])
    -- Keyswitches go through the Patch.Scale.
    let attr_patch attr_map =
            Patch.attribute_map #= attr_map $ make_patch (Just scale1)
    let ks_map = Common.attribute_map
            [ (Attrs.mute, ([Patch.Keyswitch 0], Nothing))
            , (mempty, ([Patch.Keyswitch 1], Nothing))
            ]
    equal (run Nothing (attr_patch ks_map) "4c") ([Left (0, [(0, NN.b3)])], [])
    -- PitchKeymap also goes through the Patch.Scale.
    let pitched_map = Common.attribute_map
            [ (Attrs.mute,
                ([], Just (Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)))
            , (mempty, ([], Just (Patch.PitchedKeymap Key.c4 Key.c5 Key.c4)))
            ]
        pitched_patch =
            Patch.attribute_map #= pitched_map $ make_patch (Just scale1)
    equal (run Nothing pitched_patch "5c")
        ([Left (0, [(0, NN.b4)])], [])
    equal (run Nothing pitched_patch "+mute -- 5c")
        ([Left (0, [(0, NN.b2)])], [])

test_convert_dynamic = do
    let run inst = first (convert clookup extract)
            . DeriveTest.extract id . DeriveTest.derive_tracks ""
            . (++ [(inst, [(1, 4, "")])])
        extract e =
            ( Types.event_start_velocity e
            , maybe [] Signal.unsignal $ Map.lookup Controls.breath $
                Types.event_controls e
            )
        clookup = DeriveTest.make_convert_lookup UiTest.default_allocations $
            DeriveTest.make_db [("s", [p "1", Patch.pressure $ p "2"])]
        p = DeriveTest.make_patch
    -- >i1 is non-Pressure, >i2 is Pressure.
    equal (run ">i1" [("dyn", [(0, 0, "1")])])
        ([Left (1, [])], [])
    equal (run ">i1" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.5, [])], [])
    -- For pressure, dyn goes to breath.
    equal (run ">i2" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.5, [(1, 0.5), (2, 1)])], [])
    -- %attack-vel overrides the velocity.
    equal (run ">i2 | %attack-vel=.75" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.75, [(1, 0.5), (2, 1)])], [])

test_release_velocity = do
    let run = first (convert DeriveTest.default_convert_lookup extract)
            . DeriveTest.extract id . DeriveTest.derive_tracks "inst = >i1"
            . UiTest.note_track
        extract e =
            (Types.event_start_velocity e, Types.event_end_velocity e)
    equal (run [(0, 1, "%dyn=.5 | -- 4c")]) ([Left (0.5, 0.5)], [])
    equal (run [(0, 1, "%dyn=.5 | %release-vel=.25 | -- 4c")])
        ([Left (0.5, 0.25)], [])

mkevent :: RealTime -> String -> Text -> Score.Event
mkevent start pitch inst =
    DeriveTest.mkevent (start, 1, pitch, [], Score.Instrument inst)

convert :: DeriveTest.Lookup -> (Types.Event -> a) -> [Score.Event]
    -> [Either a String]
convert lookup extract =
    show_logs extract . Convert.convert (snd lookup) (fst lookup)

e_pitch :: Types.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
e_pitch e =
    ( Types.event_start e
    , map (fmap Pitch.nn) $ Signal.unsignal (Types.event_pitch e)
    )

show_logs :: (a -> b) -> [LEvent.LEvent a] -> [Either b String]
show_logs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.show_log)

-- * instrument scale

test_instrument_scale = do
    let (_, (evts, _midi, logs)) = perform patch [("i1", "s/1")]
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4c#"), (2, 0, "4d")])
            ]
    equal logs []
    equal (map (Signal.unsignal . Types.event_pitch) evts)
        [[(0, 1)], [(1, 1.5)], [(2, 2)]]
    where
    patch = Patch.scale #= Just scale $ DeriveTest.make_patch "1"
    scale = Patch.make_scale "test" [(1, 60), (2, 62), (3, 63)]

-- * keymap

test_pitched_keymap = do
    let patch = set_keymap [bd] $ DeriveTest.make_patch "1"
        bd = ("bd", Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)
        set_keymap kmap = Patch.attribute_map
            #= Patch.keymap (map (first Attrs.attr) kmap)
        mktracks ps =
            [ (">i1", [(n, 1, "+bd") | (n, _) <- vals])
            , ("*", [(n, 0, p) | (n, p) <- vals])
            ]
            where vals = zip (Seq.range_ 0 1) ps
    let (res, (events, _, logs)) = perform patch [("i1", "s/1")]
            (mktracks ["3c", "4c", "5c", "6c"])
    equal (DeriveTest.r_logs res) []
    equal logs []
    equal (map (nn_signal . Types.event_pitch) events)
        [ [(0, NN.c2)]
        , [(1, NN.c2)]
        , [(2, NN.c3)]
        , [(3, NN.c3)]
        ]

-- * implementation

nn_signal :: Signal.NoteNumber -> [(Signal.X, Pitch.NoteNumber)]
nn_signal = map (second Pitch.nn) . Signal.unsignal

perform :: Patch.Patch -> [(Text, Text)] -> [UiTest.TrackSpec]
    -> (Derive.Result, ([Types.Event], [Midi.WriteMessage], [Log.Msg]))
perform patch allocs tracks = (result, performance)
    where
    allocations = UiTest.allocations allocs
    performance = DeriveTest.perform
        (DeriveTest.make_convert_lookup allocations db)
        allocations (Derive.r_events result)
    db = DeriveTest.make_db [("s", [patch])]
    result = DeriveTest.derive_tracks_setup
        (DeriveTest.with_instrument_db allocations db) "" tracks

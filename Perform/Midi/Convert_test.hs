-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Convert_test where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.Attrs as Attrs
import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Env as Env
import qualified Derive.EnvKey as EnvKey
import qualified Derive.LEvent as LEvent
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.All as Scale.All
import qualified Derive.Scale.BaliScales as BaliScales
import qualified Derive.Score as Score
import qualified Derive.ScoreT as ScoreT
import qualified Derive.ShowVal as ShowVal

import qualified Instrument.Common as Common
import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as Types
import qualified Perform.NN as NN
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import qualified Ui.UiTest as UiTest

import           Global
import           Types
import           Util.Test


test_convert :: Test
test_convert = do
    let run = convert DeriveTest.default_convert_lookup e_pitch
    let noinst n = mkevent n "4c" (ScoreT.Instrument "noinst")
        nopitch n = Score.set_pitch mempty $ mkevent n "4c" UiTest.i1
        good n = mkevent n "4c" UiTest.i1

    equal (run [noinst 0, nopitch 1, good 2])
        [ Right $ "instrument not found: noinst"
        -- emits an event anyway so the previous pitch doesn't continue
        , Left (1, [])
        , Left (2, [(2, NN.c4)])
        ]
    -- Make sure it's lazy.
    let events = zipWith ($) (cycle [noinst, nopitch, good])
            (map RealTime.seconds [0..])
    equal (length (take 3 (run events))) 3

test_rnd_vel :: Test
test_rnd_vel = do
    let run dyn notes = first extract $ DeriveTest.perform_block
            [ (">i1 | dyn = " <> dyn, [(n, 1, "") | n <- Seq.range' 0 notes 1])
            , ("*", [(0, 0, "4c")])
            ]
        extract midi = [vel | (_, _, vel) <- DeriveTest.note_on_vel midi]

    equal (run ".5" 1) ([64], [])
    let (vels, logs) = run "(cf-rnd 0 1)" 10
    equal logs []
    check ("not all equal: " <> pretty vels) $ not (all (== head vels) vels)
    check ("in range 40--90: " <> pretty vels) $ all (Num.inRange 40 90) vels

test_convert_pitch :: Test
test_convert_pitch = do
    let run = convert DeriveTest.default_convert_lookup e_pitch
    let event tsig = DeriveTest.mkevent
            (0, 1, "4c", [(Controls.diatonic, tsig)], UiTest.i1)
    equal (run [event []]) [Left (0, [(0, NN.c4)])]
    equal (run [event [(0, 1)]]) [Left (0, [(0, NN.d4)])]
    equal (run [event [(0, 100)]])
        [ Left (0, [])
        , Right "convert pitch: 0s: twelve:4c with transposition:\
            \ {t-dia: 100}: pitch 232nn out of range {t-dia: 100}"
        ]
    equal (run [event [(0, 0), (2, 2)]])
        [Left (0, [(0, NN.c4), (1, NN.d4), (2, NN.e4)])]
    -- An out of range transposition shouldn't cause a warning.
    equal (run [event [(0, 0), (100, 0), (100, 100)]]) [Left (0, [(0, NN.c4)])]

    -- Convert applies the environ to pitches.
    let event = (DeriveTest.mkevent (0, 1, "4c", [], UiTest.i1))
            { Score.event_pitch =
                PSignal.from_sample 0 (DeriveTest.mkpitch legong "4i")
            }
        Just (Scale.Simple legong) = Map.lookup "legong" Scale.All.scales
    equal (run [event]) [Left (0, [(0, 59.91)])]
    let insert = Score.modify_environ $
            Env.insert_val EnvKey.tuning (ShowVal.show_val BaliScales.Isep)
    equal (run [insert event]) [Left (0, [(0, 60.4)])]

test_patch_scale :: Test
test_patch_scale = do
    let run config patch pitch =
            first (convert (make_lookup config patch) e_pitch) $
            DeriveTest.extract id $ DeriveTest.derive_tracks "" $
                UiTest.note_spec ("i1", [(0, 1, pitch)], [])
        make_lookup config patch = DeriveTest.make_convert_lookup_for UiTest.i1
            (make_config config) patch
        make_config scale = Patch.settings#Patch.scale #= scale $
            UiTest.midi_config [0]
        patch = Patch.patch (-1, 1) "1"

    equal (run Nothing patch "4c") ([Left (0, [(0, NN.c4)])], [])
    let scale_g = Patch.make_scale "scale" [(Key.c4, NN.g4)]
        scale_e = Patch.make_scale "scale" [(Key.c4, NN.e4)]
    equal (run (Just scale_g) patch "4g") ([Left (0, [(0, NN.c4)])], [])
    equal (run (Just scale_e) patch "4e") ([Left (0, [(0, NN.c4)])], [])

    let scale1 = Patch.make_scale "scale"
            [(k, Midi.from_key k + 1) | k <- [0..127]]
    equal (run (Just scale1) patch "4c") ([Left (0, [(0, NN.b3)])], [])
    -- Keyswitches go through the Patch.Scale.
    let attr_patch attr_map = Patch.attribute_map #= attr_map $ patch
    let ks_map = Common.attribute_map
            [ (Attrs.mute, ([Patch.Keyswitch 0], Nothing))
            , (mempty, ([Patch.Keyswitch 1], Nothing))
            ]
    equal (run (Just scale1) (attr_patch ks_map) "4c")
        ([Left (0, [(0, NN.b3)])], [])

    -- PitchKeymap also goes through the Patch.Scale.
    let pitched_map = Common.attribute_map
            [ (Attrs.mute,
                ([], Just (Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)))
            , (mempty, ([], Just (Patch.PitchedKeymap Key.c4 Key.c5 Key.c4)))
            ]
        pitched_patch = Patch.attribute_map #= pitched_map $ patch
    equal (run (Just scale1) pitched_patch "5c")
        ([Left (0, [(0, NN.b4)])], [])
    equal (run (Just scale1) pitched_patch "+mute -- 5c")
        ([Left (0, [(0, NN.b2)])], [])

test_convert_dynamic :: Test
test_convert_dynamic = do
    let run inst = first (convert clookup extract)
            . DeriveTest.extract id . DeriveTest.derive_tracks ""
            . (++ [(inst, [(1, 4, "")])])
        extract e =
            ( Types.event_start_velocity e
            , maybe [] MSignal.to_pairs $ Map.lookup Controls.breath $
                Types.event_controls e
            )
        clookup = DeriveTest.make_convert_lookup allocs UiTest.default_db
        allocs = UiTest.modify_midi_config "i2"
            (Patch.settings#Patch.flags
                %= Just . Patch.add_flag Patch.Pressure . fromMaybe mempty)
            UiTest.default_allocations
    -- >i1 is non-Pressure, >i2 is Pressure.
    equal (run ">i1" [("dyn", [(0, 0, "1")])])
        ([Left (1, [])], [])
    equal (run ">i1" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.5, [])], [])
    -- For pressure, dyn goes to breath.
    equal (run ">i2" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.5, [(1, 0.5), (2, 1)])], [])
    -- attack-vel overrides the velocity.
    equal (run ">i2 | attack-vel=.75" [("dyn", [(1, 0, ".5"), (2, 0, "1")])])
        ([Left (0.75, [(1, 0.5), (2, 1)])], [])

test_release_velocity :: Test
test_release_velocity = do
    let run = first (convert DeriveTest.default_convert_lookup extract)
            . DeriveTest.extract id . DeriveTest.derive_tracks "inst=i1"
            . UiTest.note_track
        extract e =
            (Types.event_start_velocity e, Types.event_end_velocity e)
    equal (run [(0, 1, "dyn=.5 | -- 4c")]) ([Left (0.5, 0.5)], [])
    equal (run [(0, 1, "dyn=.5 | release-vel=.25 | -- 4c")])
        ([Left (0.5, 0.25)], [])

mkevent :: RealTime -> Text -> ScoreT.Instrument -> Score.Event
mkevent start pitch inst = DeriveTest.mkevent (start, 1, pitch, [], inst)

convert :: DeriveTest.Lookup -> (Types.Event -> a) -> [Score.Event]
    -> [Either a Text]
convert lookup extract =
    show_logs extract . Convert.convert 1 (snd lookup) (fst lookup)

e_pitch :: Types.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
e_pitch e =
    ( Types.event_start e
    , map (fmap Pitch.nn) $ MSignal.to_pairs (Types.event_pitch e)
    )

show_logs :: (a -> b) -> [LEvent.LEvent a] -> [Either b Text]
show_logs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.show_log)

-- * instrument scale

test_instrument_scale :: Test
test_instrument_scale = do
    let ((events, _), logs) = perform patch [("i1", "s/1")]
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4c#"), (2, 0, "4d")])
            ]
    equal logs []
    equal (map (MSignal.to_pairs . Types.event_pitch) events)
        [[(0, 1)], [(1, 1.5)], [(2, 2)]]
    where
    patch = Patch.defaults#Patch.scale #= Just scale $ UiTest.make_patch "1"
    scale = Patch.make_scale "test" [(1, 60), (2, 62), (3, 63)]

-- * keymap

test_pitched_keymap :: Test
test_pitched_keymap = do
    let patch = set_keymap [bd] $ UiTest.make_patch "1"
        bd = ("bd", Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)
        set_keymap kmap = Patch.attribute_map
            #= Patch.keymap (map (first Attrs.attr) kmap)
        mktracks ps =
            [ (">i1", [(n, 1, "+bd") | (n, _) <- vals])
            , ("*", [(n, 0, p) | (n, p) <- vals])
            ]
            where vals = zip (Seq.range_ 0 1) ps
    let ((events, _), logs) = perform patch [("i1", "s/1")]
            (mktracks ["3c", "4c", "5c", "6c"])
    equal logs []
    equal (map (nn_signal . Types.event_pitch) events)
        [ [(0, NN.c2)]
        , [(1, NN.c2)]
        , [(2, NN.c3)]
        , [(3, NN.c3)]
        ]

test_keyswitches :: Test
test_keyswitches = do
    let run event = extract $
            perform patch [("i1", "s/")] [(">i1 | #=(4c)", [(0, 1, event)])]
        patch =
            Patch.attribute_map #= Patch.single_keyswitches [(Attrs.pizz, 42)] $
            Patch.mode_map #= Patch.make_mode_map
                [("mode", [ ("x", ("cc1", 2)), ("y", ("cc1", 3))])] $
            UiTest.make_patch ""
        extract ((_, midi), logs) =
            ( filter wanted $ mapMaybe (Midi.channel_message . Midi.wmsg_msg)
                midi
            , logs
            )
        wanted (Midi.NoteOn {}) = True
        wanted (Midi.ControlChange {}) = True
        wanted _ = False
    let note = Midi.NoteOn 60 127
    equal (run "") ([Midi.ControlChange 1 2, note], [])
    equal (run "+pizz") ([Midi.NoteOn 42 64, Midi.ControlChange 1 2, note], [])
    equal (run "mode=x | +pizz")
        ([Midi.NoteOn 42 64, Midi.ControlChange 1 2, note], [])
    equal (run "mode=y |") ([Midi.ControlChange 1 3, note], [])
    -- No warning, but it's consistent with no warning for unrecognized attrs.
    equal (run "mode=z |") ([Midi.ControlChange 1 2, note], [])

-- * implementation

nn_signal :: MSignal.Signal -> [(Signal.X, Pitch.NoteNumber)]
nn_signal = map (second Pitch.nn) . MSignal.to_pairs

perform :: Patch.Patch -> DeriveTest.SimpleAllocations -> [UiTest.TrackSpec]
    -> (([Types.Event], [Midi.WriteMessage]), [Text])
perform patch allocs_ tracks =
    DeriveTest.perform_result perform $
        DeriveTest.derive_tracks_setup
            (DeriveTest.with_instrument_db allocs db) "" tracks
    where
    perform = DeriveTest.perform
        (DeriveTest.make_convert_lookup allocs db) allocs
    allocs = DeriveTest.simple_allocs allocs_
    db = UiTest.make_db [("s", [patch])]

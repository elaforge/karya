-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Convert_test where
import qualified Data.Map as Map

import qualified Util.Num as Num
import qualified Util.Lists as Lists
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
    let run = convert DeriveTest.defaultConvertLookup ePitch
    let noinst n = mkevent n "4c" (ScoreT.Instrument "noinst")
        nopitch n = Score.setPitch mempty $ mkevent n "4c" UiTest.i1
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
    let run dyn notes = first extract $ DeriveTest.performBlock
            [ (">i1 | dyn = " <> dyn
              , [(n, 1, "") | n <- Lists.range' 0 notes 1]
              )
            , ("*", [(0, 0, "4c")])
            ]
        extract midi = [vel | (_, _, vel) <- DeriveTest.noteOnVel midi]

    equal (run ".5" 1) ([64], [])
    let (vels, logs) = run "(cf-rnd 0 1)" 10
    equal logs []
    check ("not all equal: " <> pretty vels) $ not (all (== head vels) vels)
    check ("in range 40--90: " <> pretty vels) $ all (Num.inRange 40 90) vels

test_convert_pitch :: Test
test_convert_pitch = do
    let run = convert DeriveTest.defaultConvertLookup ePitch
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
            { Score.eventPitch =
                PSignal.fromSample 0 (DeriveTest.mkpitch legong "4i")
            }
        Just (Scale.Simple legong) = Map.lookup "legong" Scale.All.scales
    equal (run [event]) [Left (0, [(0, 59.91)])]
    let insert = Score.modifyEnviron $
            Env.insertVal EnvKey.tuning (ShowVal.showVal BaliScales.Isep)
    equal (run [insert event]) [Left (0, [(0, 60.4)])]

test_patch_scale :: Test
test_patch_scale = do
    let run config patch pitch =
            first (convert (makeLookup config patch) ePitch) $
            DeriveTest.extract id $ DeriveTest.deriveTracks "" $
                UiTest.noteSpec ("i1", [(0, 1, pitch)], [])
        makeLookup config patch = DeriveTest.makeConvertLookupFor UiTest.i1
            (makeConfig config) patch
        makeConfig scale = Patch.settings#Patch.scale #= scale $
            UiTest.midiConfig [0]
        patch = Patch.patch (-1, 1) "1"

    equal (run Nothing patch "4c") ([Left (0, [(0, NN.c4)])], [])
    let scaleG = Patch.makeScale "scale" [(Key.c4, NN.g4)]
        scaleE = Patch.makeScale "scale" [(Key.c4, NN.e4)]
    equal (run (Just scaleG) patch "4g") ([Left (0, [(0, NN.c4)])], [])
    equal (run (Just scaleE) patch "4e") ([Left (0, [(0, NN.c4)])], [])

    let scale1 = Patch.makeScale "scale"
            [(k, Midi.fromKey k + 1) | k <- [0..127]]
    equal (run (Just scale1) patch "4c") ([Left (0, [(0, NN.b3)])], [])
    -- Keyswitches go through the Patch.Scale.
    let attrPatch attrMap = Patch.attributeMap #= attrMap $ patch
    let ksMap = Common.attributeMap
            [ (Attrs.mute, ([Patch.Keyswitch 0], Nothing))
            , (mempty, ([Patch.Keyswitch 1], Nothing))
            ]
    equal (run (Just scale1) (attrPatch ksMap) "4c")
        ([Left (0, [(0, NN.b3)])], [])

    -- PitchKeymap also goes through the Patch.Scale.
    let pitchedMap = Common.attributeMap
            [ (Attrs.mute,
                ([], Just (Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)))
            , (mempty, ([], Just (Patch.PitchedKeymap Key.c4 Key.c5 Key.c4)))
            ]
        pitchedPatch = Patch.attributeMap #= pitchedMap $ patch
    equal (run (Just scale1) pitchedPatch "5c")
        ([Left (0, [(0, NN.b4)])], [])
    equal (run (Just scale1) pitchedPatch "+mute -- 5c")
        ([Left (0, [(0, NN.b2)])], [])

test_convert_dynamic :: Test
test_convert_dynamic = do
    let run inst = first (convert clookup extract)
            . DeriveTest.extract id . DeriveTest.deriveTracks ""
            . (++ [(inst, [(1, 4, "")])])
        extract e =
            ( Types.eventStartVelocity e
            , maybe [] MSignal.toPairs $ Map.lookup Controls.breath $
                Types.eventControls e
            )
        clookup = DeriveTest.makeConvertLookup allocs UiTest.defaultDb
        allocs = UiTest.modifyMidiConfig "i2"
            (Patch.settings#Patch.flags
                %= Just . Patch.addFlag Patch.Pressure . fromMaybe mempty)
            UiTest.defaultAllocations
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
    let run = first (convert DeriveTest.defaultConvertLookup extract)
            . DeriveTest.extract id . DeriveTest.deriveTracks "inst=i1"
            . UiTest.noteTrack
        extract e =
            (Types.eventStartVelocity e, Types.eventEndVelocity e)
    equal (run [(0, 1, "dyn=.5 | -- 4c")]) ([Left (0.5, 0.5)], [])
    equal (run [(0, 1, "dyn=.5 | release-vel=.25 | -- 4c")])
        ([Left (0.5, 0.25)], [])

mkevent :: RealTime -> Text -> ScoreT.Instrument -> Score.Event
mkevent start pitch inst = DeriveTest.mkevent (start, 1, pitch, [], inst)

convert :: DeriveTest.Lookup -> (Types.Event -> a) -> [Score.Event]
    -> [Either a Text]
convert lookup extract =
    showLogs extract . Convert.convert 1 (snd lookup) (fst lookup)

ePitch :: Types.Event -> (RealTime, [(RealTime, Pitch.NoteNumber)])
ePitch e =
    ( Types.eventStart e
    , map (fmap Pitch.nn) $ MSignal.toPairs (Types.eventPitch e)
    )

showLogs :: (a -> b) -> [LEvent.LEvent a] -> [Either b Text]
showLogs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.showLog)

-- * instrument scale

test_instrument_scale :: Test
test_instrument_scale = do
    let ((events, _), logs) = perform patch [("i1", "s/1")]
            [ (">i1", [(0, 1, ""), (1, 1, ""), (2, 1, "")])
            , ("*", [(0, 0, "4c"), (1, 0, "4c#"), (2, 0, "4d")])
            ]
    equal logs []
    equal (map (MSignal.toPairs . Types.eventPitch) events)
        [[(0, 1)], [(1, 1.5)], [(2, 2)]]
    where
    patch = Patch.defaults#Patch.scale #= Just scale $ UiTest.makePatch "1"
    scale = Patch.makeScale "test" [(1, 60), (2, 62), (3, 63)]

-- * keymap

test_pitched_keymap :: Test
test_pitched_keymap = do
    let patch = setKeymap [bd] $ UiTest.makePatch "1"
        bd = ("bd", Patch.PitchedKeymap Key.c2 Key.c3 Key.c4)
        setKeymap kmap = Patch.attributeMap
            #= Patch.keymap (map (first Attrs.attr) kmap)
        mktracks ps =
            [ (">i1", [(n, 1, "+bd") | (n, _) <- vals])
            , ("*", [(n, 0, p) | (n, p) <- vals])
            ]
            where vals = zip (Lists.range_ 0 1) ps
    let ((events, _), logs) = perform patch [("i1", "s/1")]
            (mktracks ["3c", "4c", "5c", "6c"])
    equal logs []
    equal (map (nnSignal . Types.eventPitch) events)
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
            Patch.attributeMap #= Patch.singleKeyswitches [(Attrs.pizz, 42)] $
            Patch.modeMap #= Patch.makeModeMap
                [("mode", [ ("x", ("cc1", 2)), ("y", ("cc1", 3))])] $
            UiTest.makePatch ""
        extract ((_, midi), logs) =
            ( filter wanted $ mapMaybe (Midi.channelMessage . Midi.wmsgMsg)
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

nnSignal :: MSignal.Signal -> [(Signal.X, Pitch.NoteNumber)]
nnSignal = map (second Pitch.nn) . MSignal.toPairs

perform :: Patch.Patch -> DeriveTest.SimpleAllocations -> [UiTest.TrackSpec]
    -> (([Types.Event], [Midi.WriteMessage]), [Text])
perform patch allocs_ tracks =
    DeriveTest.performResult perform $
        DeriveTest.deriveTracksSetup
            (DeriveTest.withInstrumentDb allocs db) "" tracks
    where
    perform = DeriveTest.perform
        (DeriveTest.makeConvertLookup allocs db) allocs
    allocs = DeriveTest.simpleAllocs allocs_
    db = UiTest.makeDb [("s", [patch])]

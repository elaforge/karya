module Derive.Control_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest
import qualified Ui.Track as Track

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Twelve as Twelve
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


test_control_track = do
    let derive = do_derive (DeriveTest.e_control "cont")
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    let (val, logs) = derive ("*bork *bork *bork", events)
    equal val []
    strings_like logs ["track title: control track must be one of"]

    let (val, logs) = derive ("cont", [(0, 0, "abc"), (1, 0, "def")])
    equal val [Just []]
    strings_like logs ["call not found: abc", "call not found: def"]
    equal (derive ("cont", events)) ([Just [(0, 1), (1, 2)]], [])

test_track_expression = do
    let derive = do_derive (DeriveTest.e_control "cont")
    equal (derive ("cont", [(0, 0, "0"), (4, 0, "i 1")]))
        ([Just [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]], [])
    equal (derive ("cont | srate = 2", [(0, 0, "0"), (4, 0, "i 1")]))
        ([Just [(0, 0), (2, 0.5), (4, 1)]], [])

    let derive_pitch = do_derive (PitchSignal.unsignal . Score.event_pitch)
    equal
        (derive_pitch ("*twelve | srate = 2", [(0, 0, "4c"), (4, 0, "i (4d)")]))
        ([[(0, (60, 60, 0)), (2, (60, 62, 0.5)), (4, (60, 62, 1))]], [])

test_derive_control = do
    let ex (sig, logs) = (Signal.unsignal sig, map DeriveTest.show_log logs)
    let derive events = DeriveTest.extract_run ex $ DeriveTest.run State.empty
            (Control.derive_control 10 [] (map UiTest.make_event events))
    equal (derive [(0, 0, "1"), (1, 0, "2")])
        (Right ([(0, 1), (1, 2)], []))
    equal (derive [(0, 0, "1"), (2, 0, "i 2")])
        (Right ([(0, 1), (1, 1.5), (2, 2)], []))
    equal (derive [(0, 0, "1"), (2, 0, "i 2"), (4, 0, "i 1")])
        (Right ([(0, 1), (1, 1.5), (2, 2), (3, 1.5), (4, 1)], []))

    -- evaluation continues after an error
    equal (derive [(0, 0, "1"), (1, 0, "def")])
        (Right ([(0, 1)], ["Error: control call not found: def"]))
    equal (derive [(0, 0, "1"), (1, 0, "def"), (2, 0, "i 2")])
        (Right ([(0, 1), (1, 1.5), (2, 2)],
            ["Error: control call not found: def"]))

test_pitch_track = do
    let derive = do_derive (PitchSignal.unsignal . Score.event_pitch)

    let (val, logs) = derive ("*no-scale", [(0, 0, "1"), (1, 0, "2")])
    equal val []
    strings_like logs ["unknown ScaleId \"no-scale\""]

    let (val, logs) = derive ("*twelve", [(0, 0, "1"), (1, 0, "2")])
    equal val [[]]
    strings_like logs ["call not found: 1", "call not found: 2"]
    let (val, logs) = derive
            ("*twelve", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4hc")])
    equal val [[(0, (60, 60, 0)), (1, (62, 62, 0))]]
    strings_like logs ["call not found: 4hc"]
    equal (derive ("*twelve", [(0, 0, "4c"), (1, 0, "4d")]))
        ([[(0, (60, 60, 0)), (1, (62, 62, 0))]], [])

    equal (derive ("*twelve", [(0, 0, "4c"), (2, 0, "i (4d)")]))
        ([[(0, (60, 60, 0)), (1, (60, 62, 0.5)), (2, (60, 62, 1))]], [])

do_derive :: (Score.Event -> a) -> UiTest.TrackSpec -> ([a], [String])
do_derive extract track = DeriveTest.extract extract $
        DeriveTest.derive_tracks [(">", [(0, 8, "")]), track]

test_relative_control = do
    let (events, logs) = DeriveTest.extract extract $ DeriveTest.derive_tracks
            [ (">", [(0, 5, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("cont", [(0, 0, "0"), (2, 0, "i 2"), (4, 0, "i 0")])
            , ("add cont", [(0, 0, "1")])
            ]
        extract = (\sig -> map (at sig) [0..5])
            . (Map.! Score.Control "cont") . Score.event_controls
        at sig t = Signal.at (RealTime.seconds t) sig
    equal logs []
    equal events [[1, 2, 3, 2, 1, 1]]

    -- putting relative and absolute in the wrong order causes a warning
    let (events, logs) = DeriveTest.extract Score.event_controls $
            DeriveTest.derive_tracks
                [ (">", [(0, 10, "")])
                , ("add cont", [(0, 0, "1")])
                , ("cont", [(0, 0, "1")])
                ]
    let controls = Map.union Derive.initial_controls $
            Map.fromList [(Score.Control "cont", Signal.signal [(0, 1)])]
    equal events [controls]
    strings_like logs ["no absolute control is in scope"]

test_relative_pitch = do
    let f track = DeriveTest.extract Score.event_pitch $
            DeriveTest.derive_tracks
                [ (">", [(0, 10, "")])
                , ("*twelve", [(0, 0, "4c")])
                , ("add #", track)
                ]
        base = 60
    let mksig = PitchSignal.signal (Pitch.ScaleId "twelve")
    equal (f []) ([mksig [(0, (base, base, 0))]], [])

    equal (f [(0, 0, "0"), (4, 0, "i (2)")])
        ([mksig ((0, (60, 60, 0))
            : DeriveTest.pitch_interpolate 0 base 4 (base+2))],
        [])

    -- putting relative and absolute in the wrong order overrides the relative
    let (pitches, logs) = DeriveTest.extract Score.event_pitch $
            DeriveTest.derive_tracks
                [ (">", [(0, 10, "")])
                , ("add #", [(0, 0, "1")])
                , ("*twelve", [(0, 0, "4c")])
                ]
    equal pitches [mksig [(0, (base, base, 0))]]
    -- no warning because of default pitch
    strings_like logs []

test_stash_signal = do
    -- make sure that TrackSignals are recorded when control tracks are derived
    let itrack = (">i", [])
        ctrack = ("cont", [(0, 0, "1"), (1, 0, "0")])
        csig = Track.Control $ Signal.signal [(0, 1), (1, 0)]
    let run = extract . DeriveTest.derive_tracks
        extract r = Log.trace_logs (snd $ DeriveTest.r_split r)
            (map e_tsig (Map.elems (Derive.r_track_signals r)))
        e_tsig result = case result of
            Left logs -> Left $ map DeriveTest.show_log logs
            Right (Track.TrackSignal sig shift stretch) ->
                Right (sig, shift, stretch)
    let tsig samples p x = Right (Track.Control (Signal.signal samples), p, x)

    equal (run [ctrack, itrack]) [Right (csig, 0, 1)]
    -- constant tempo stretches track sig
    -- tempo track itself is unstretched
    equal (run [("tempo", [(0, 0, "2")]), ctrack, itrack]) $
        [ tsig [(0, 2), (1, 2)] 0 1
        , tsig [(0, 1), (0.5, 0)] 0 0.5
        ]

    -- but a complicated tempo forces a rederive so output is still in
    -- RealTime
    equal (run [("tempo", [(0, 0, "2"), (4, 0, "i 1")]), ctrack, itrack])
        [ tsig [(0, 2), (1, 1.75), (2, 1.5), (3, 1.25), (4, 1)] 0 1
        , tsig [(0, 1), (1, 0)] 0 1
        ]

    -- pitch tracks work too
    let ptrack = ("*twelve", [(0, 0, "4c"), (1, 0, "4d")])
        psig = Track.Pitch (PitchSignal.signal Twelve.scale_id
                [(0, (60, 60, 0)), (1, (62, 62, 0))])
            (Scale.scale_map Twelve.scale)
    equal (run [ptrack, itrack]) [Right (psig, 0, 1)]


    -- Subtracks should be rendered, even though they're never evaluated as
    -- a whole.
    equal (run [itrack, ctrack]) [Right (csig, 0, 1)]
    equal (run [itrack, ("$ broken", [(0, 0, "0")])]) []
    equal (run [itrack, itrack]) []
    equal (run [itrack, ptrack]) [Right (psig, 0, 1)]

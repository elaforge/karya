{-# LANGUAGE ParallelListComp #-}
module Derive.Control_test where
import qualified Data.Map as Map
import Control.Monad

import Util.Test
import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Control as Control
import qualified Derive.Derive as Derive
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


test_control_track = do
    let derive = do_derive (fmap Signal.unsignal
            . Map.lookup (Score.Control "cont") . Score.event_controls)
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    left_like (fst (derive ("", events))) "failed to parse"

    let (val, logs) = derive ("cont", [(0, 0, "abc"), (1, 0, "def")])
    equal val (Right [Just []])
    strings_like logs ["unknown Symbol \"abc\"", "unknown Symbol \"def\""]
    equal (derive ("cont", events)) (Right [Just [(0, 1), (1, 2)]], [])

test_track_expression = do
    let derive = do_derive (fmap Signal.unsignal
            . Map.lookup (Score.Control "cont") . Score.event_controls)
    equal (derive ("cont", [(0, 0, "0"), (4, 0, "i 1")]))
        (Right [Just [(0, 0), (1, 0.25), (2, 0.5), (3, 0.75), (4, 1)]], [])
    equal (derive ("srate = 2 | cont", [(0, 0, "0"), (4, 0, "i 1")]))
        (Right [Just [(0, 0), (2, 0.5), (4, 1)]], [])

    let derive_pitch = do_derive (PitchSignal.unsignal . Score.event_pitch)
    equal
        (derive_pitch ("srate = 2 | *twelve", [(0, 0, "4c"), (4, 0, "i *4d")]))
        (Right [[(0, (60, 60, 0)), (2, (60, 62, 0.5)), (4, (60, 62, 1))]], [])


test_derive_control = do
    let extract (Left err) = Left err
        extract (Right (val, _, logs)) =
            Right (Signal.unsignal val, map Log.msg_string logs)
    let derive events = extract $ DeriveTest.run State.empty
            (join $ Control.derive_control [] (map UiTest.mkevent events))
    equal (derive [(0, 0, "1"), (1, 0, "2")])
        (Right ([(0, 1), (1, 2)], []))
    equal (derive [(0, 0, "1"), (0.1, 0, "i 2")])
        (Right ([(0, 1), (0.05, 1.5), (0.1, 2)], []))
    equal (derive [(0, 0, "1"), (0.1, 0, "i 2"), (0.2, 0, "i 1")])
        (Right ([(0, 1), (0.05, 1.5), (0.1, 2),
            (0.15000000000000002, 1.5), (0.2, 1)], []))

    -- evaluation continues after an error
    equal (derive [(0, 0, "1"), (1, 0, "def")])
        (Right ([(0, 1)], ["lookup_control_call: unknown Symbol \"def\""]))
    equal (derive [(0, 0, "1"), (0.05, 0, "def"), (0.1, 0, "i 2")])
        (Right ([(0, 1), (0.05, 1.5), (0.1, 2)],
            ["lookup_control_call: unknown Symbol \"def\""]))

test_pitch_track = do
    let derive = do_derive (PitchSignal.unsignal . Score.event_pitch)

    let (val, logs) = derive ("*no_scale", [(0, 0, "1"), (1, 0, "2")])
    left_like val "unknown ScaleId \"no_scale\""
    equal logs []

    let (val, logs) = derive ("*twelve", [(0, 0, "1"), (1, 0, "2")])
    equal val (Right [[]])
    strings_like logs
        [ "generator <null>: Note \"1\" not in ScaleId"
        , "generator <null>: Note \"2\" not in ScaleId"
        ]
    let (val, logs) = derive
            ("*twelve", [(0, 0, "4c"), (1, 0, "4d"), (2, 0, "4hc")])
    equal val (Right [[(0, (60, 60, 0)), (1, (62, 62, 0))]])
    strings_like logs ["Note \"4hc\" not in ScaleId"]
    equal (derive ("*twelve", [(0, 0, "4c"), (1, 0, "4d")]))
        (Right [[(0, (60, 60, 0)), (1, (62, 62, 0))]], [])

    equal (derive ("*twelve", [(0, 0, "4c"), (2, 0, "i *4d")]))
        (Right [[(0, (60, 60, 0)), (1, (60, 62, 0.5)), (2, (60, 62, 1))]], [])

do_derive :: (Score.Event -> a) -> UiTest.TrackSpec
    -> (Either String [a], [String])
do_derive extract track = DeriveTest.extract extract Log.msg_string $
        DeriveTest.derive_tracks [(">", [(0, 2, "")]), track]

test_relative_control = do
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (">", [(0, 1, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("add cont", [(0, 0, "1")])
            , ("cont", [(0, 0, "0"), (2, 0, "i 2"), (4, 0, "i 0")])
            ]
    let extract = (\sig -> map (flip Signal.at sig) [0..5])
            . (Map.! Score.Control "cont") . Score.event_controls
    equal logs []
    equal (fmap (map extract) events) $ Right [[1, 2, 3, 2, 1, 1]]

    -- putting relative and absolute in the wrong order causes a warning
    let (events, logs) = DeriveTest.e_logs $ DeriveTest.derive_tracks
            [ (">", [(0, 10, "")])
            , ("cont", [(0, 0, "1")])
            , ("add cont", [(0, 0, "1")])
            ]
    let controls = Map.union (Score.unwarp_controls Derive.initial_controls) $
            Map.fromList [(Score.Control "cont", Signal.signal [(0, 1)])]
    equal (fmap (map Score.event_controls) events) $ Right [controls]
    strings_like logs ["no absolute control is in scope"]

test_relative_pitch = do
    let extract result = (fmap (map Score.event_pitch) events, logs)
            where (events, logs) = DeriveTest.e_logs result
    let f track = extract $ DeriveTest.derive_tracks
                [ (">", [(0, 10, "")])
                , ("add *", track)
                , ("*twelve", [(0, 0, "4c")])
                ]
        base = 60
    let mksig = PitchSignal.signal (Pitch.ScaleId "twelve")
    equal (f []) (Right [mksig [(0, (base, base, 0))]], [])
    equal (f [(0, 0, "0"), (4, 0, "i *2")])
        (Right
            [mksig ((0, (60, 60, 0))
                : DeriveTest.pitch_interpolate 0 base 4 (base+2))],
            [])

    -- putting relative and absolute in the wrong order overrides the relative
    let (pitches, logs) = extract $ DeriveTest.derive_tracks
            [ (">", [(0, 10, "")])
            , ("*twelve", [(0, 0, "4c")])
            , ("add *", [(0, 0, "1")])
            ]
    equal pitches $ Right [mksig [(0, (base, base, 0))]]
    -- no warning because of default pitch
    strings_like logs []

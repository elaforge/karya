{-# LANGUAGE ParallelListComp #-}
module Derive.Control_test where
import qualified Data.Map as Map

import Util.Test
import qualified Util.Log as Log

import qualified Ui.State as State
import qualified Ui.UiTest as UiTest

import qualified Derive.Control as Control
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.Score as Score

import qualified Perform.PitchSignal as PitchSignal
import qualified Perform.Signal as Signal


test_control_track = do
    let derive = do_derive (fmap Signal.unsignal
            . Map.lookup (Score.Control "cont") . Score.event_controls)
    let events = [(0, 0, "1"), (1, 0, "2")]

    -- various failures
    left_like (fst (derive ("", events))) "failed to parse"
    left_like (fst (derive ("cont | cont", events))) "composition not supported"

    let (val, logs) = derive ("cont", [(0, 0, "abc"), (1, 0, "def")])
    equal val (Right [Just []])
    strings_like logs ["unknown Symbol \"abc\"", "unknown Symbol \"def\""]
    equal (derive ("cont", events)) (Right [Just [(0, 1), (1, 2)]], [])

test_derive_signal = do
    let extract (Left err) = Left err
        extract (Right (val, _, logs)) =
            Right (Signal.unsignal val, map Log.msg_string logs)
    let derive events = extract $ DeriveTest.run State.empty
            (Control.derive_signal (map UiTest.mkevent events))
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

    equal (derive ("*twelve", [(0, 0, "4c"), (0.1, 0, "i *4d")]))
        (Right [[(0, (60, 60, 0)), (0.05, (60, 62, 0.5)), (0.1, (60, 62, 1))]],
            [])

do_derive :: (Score.Event -> a) -> UiTest.TrackSpec
    -> (Either String [a], [String])
do_derive extract track = DeriveTest.extract extract Log.msg_string $
        DeriveTest.derive_tracks [(">", [(0, 2, "")]), track]

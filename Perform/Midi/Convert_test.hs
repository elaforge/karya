module Perform.Midi.Convert_test where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Log as Log
import Util.Test

import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Convert as Convert
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal


test_lazy = do
    equal (take 1 (convert (repeat (mklog "hi")))) [Right "hi"]
    let events = zipWith ($) (cycle [noinst, nopitch, good])
            (map RealTime.seconds [0..])
    equal (length (take 3 (convert events))) 3

test_convert = do
    equal (convert [mklog "hi"]) [Right "hi"]
    equal (convert [noinst 0, nopitch 1, good 2])
        [ Right $ "Convert: event requires midi instrument in instrument db: "
            ++ "Instrument \"noinst\" (further warnings suppressed)"
        -- emits an event anyway so the previous pitch doesn't continue
        , Left (1, [])
        , Left (2, [(2, 62)])
        ]
    equal (convert [good 2, good 0, good 1])
        [ Left (2, [(2, 62)])
        , Left (0, [(0, 62)])
        , Right "Convert: start time less than previous of 2s"
        , Left (1, [(1, 62)])
        ]

test_convert_controls = do
    let f pressure = first Map.toList
            . Convert.convert_controls pressure Map.empty
            . DeriveTest.mkcontrols
    equal (f False [("dyn", [(0, 0.5)])])
        ([(Control.c_velocity, Signal.constant 0.5)], Nothing)
    equal (f True [("dyn", [(0, 0.5)])])
        ([(Control.c_breath, Signal.constant 0.5)], Nothing)
    -- If both vel and dyn are present, dyn wins.  This is because calls
    -- should tend to use dyn, since its more generic.  But if the track is
    -- using vel directly, the the dyn information will be shadowed.
    equal (f False [("vel", [(0, 1)]), ("dyn", [(0, 0.5)])])
        ([(Control.c_velocity, Signal.constant 0.5)],
            Just (Score.Control "vel", Score.untyped (Signal.signal [(0, 1)])))
    -- No warning if it was null.
    equal (f False [("vel", []), ("dyn", [(0, 0.5)])])
        ([(Control.c_velocity, Signal.constant 0.5)], Nothing)

noinst n = LEvent.Event $ mkevent n "c" "noinst"
nopitch n = LEvent.Event $ (mkevent n "c" "s/1") { Score.event_pitch = mempty }
good n = LEvent.Event $ mkevent n "c" "s/1"

mklog = LEvent.Log  . Log.msg Log.Warn Nothing
mkevent start text inst =
    DeriveTest.mkevent (start, 1, text, [], Score.Instrument inst)
convert = show_logs extract_event
    . Convert.convert DeriveTest.default_convert_lookup

extract_event e = (RealTime.to_seconds (Perform.event_start e),
    Signal.unsignal (Perform.event_pitch e))

show_logs extract =
    map $ LEvent.either (Left . extract) (Right . DeriveTest.show_log)

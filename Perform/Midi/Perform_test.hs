module Perform.Midi.Perform_test where
import qualified Data.Map as Map
import Text.Printf

import Util.Pretty

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


-- TODO this mostly just prints results out for eyeball inspection.  I need to
-- come up with properties and assert them

-- * perform

-- TODO test laziness by giving it an infinite Event stream

test_perform_notes = do
    let msgs = test_one_chan
            [ ("a", 0, 4, [])
            , ("b", 4, 4, [])
            ]
    -- print_ts_msgs msgs

    -- print_ts_msgs $ test_one_chan
    --     [ ("a", 0, 4, [c_vol])
    --     , ("b", 4, 4, [c_vol])
    --     ]

    print_ts_msgs $ test_one_chan
        [ ("a", 0, 4, [c_vol2])
        , ("b", 2, 4, [c_vol2])
        ]
    -- TODO check that msgs are ordered

pb_range = (-12, 12)

test_one_chan events =
    Perform.perform_notes pb_range (with_chan 0 (map mkevent events))
    where with_chan chan = map (\evt -> (evt, chan))

print_ts_msgs = mapM_ (putStrLn . show_ts_msg)
show_ts_msg (ts, msg) = pretty ts ++ ": " ++ show msg

-- * controller

secs = Timestamp.seconds

test_perform_controller = do
    let msgs = Perform.perform_controller (secs 0) (secs 10) 0 c_vol
    -- controls are not emitted after they reach steady values
    print_ts_msgs msgs


-- * channelize

test_channelize = do
    -- Events with the same pitch get different channels.

    -- All under volume, but "pb" also has a pitchbend, so it gets its own
    -- track.
    print $ Perform.channelize $ map mkevent
            [ ("a", 0, 4, [c_vol])
            , ("pb", 2, 4, [c_vol, c_pitch])
            , ("c", 4, 4, [c_vol, c_pitch])
            ]

-- * allot

-- allot_chan1 = Perform.allot_channels [0, 1] (zip events2 [0..])

-- * setup

mkevent (pitch, start, dur, controls) =
    Perform.Event (ts start) (ts dur) (mkpitch pitch) (Map.fromList controls)
    where ts = Timestamp.seconds
mkevents = map mkevent

events1 = map mkevent
    [ ("a", 0, 8, [])
    , ("b", 2, 8, [])
    ]


mksignal ts_vals = Signal.signal
    [(secs sec, Signal.Linear, val) | (sec, val) <- ts_vals]

c_vol = (Controller.c_volume, mksignal [(0, 1), (4, 0)])
c_vol2 = (Controller.c_volume, mksignal [(0, 1), (2, 0), (4, 1)])
c_pitch = (Controller.c_pitch, mksignal [(0, 0), (8, 1)])

mkpitch s = Pitch.Pitch s (Pitch.NoteNumber p)
    where
    p = maybe (error ("no pitch " ++ show s)) id (lookup (head s) to_pitch)
to_pitch = zip ['a'..'z'] [60..]
inst name = Instrument.Instrument name "z1" (Instrument.InitializeMidi [])
    (-12, 12) Nothing

inst1 = inst "inst1"
inst_config = Instrument.Config
    (Map.fromList [(inst1, Midi.WriteDevice "inst1 dev")])
    (Map.fromList [(0, inst1), (1, inst1)])


instance Show Perform.Event where
    show e = printf "<%s--%s: %s>"
        (pretty (Perform.event_start e)) (pretty (Perform.event_end e))
        (pretty_pitch (Perform.event_pitch e))
pretty_pitch (Pitch.Pitch s _) = show s


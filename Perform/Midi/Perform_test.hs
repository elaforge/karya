module Perform.Midi.Perform_test where
import qualified Data.Map as Map
import Text.Printf

import Util.Pretty
import Util.Test

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

test_perform_instruments = do
    let perform events = do
        let msgs = Perform.perform inst_config2 (map mkevent events)
        print_msgs msgs
        putStrLn ""
        return msgs

    -- channel 0 reused for inst1, inst2 gets its own channel
    perform
        [ (inst1, "a", 0, 1, [])
        , (inst1, "b", 0, 1, [])
        , (inst2, "c", 0, 1, [])
        ]

    -- identical notes get split across channels 0 and 1
    perform
        [ (inst1, "a", 0, 2, [])
        , (inst1, "a", 1, 2, [])
        , (inst1, "b", 1, 2, [])
        , (inst1, "b", 1.5, 2, [])
        ]

    return ()

    -- TODO check that msgs are ordered

test_perform_lazy = do
    let endless = map (\(n, ts) -> (n:"", ts, 4, [c_vol]))
            (zip (cycle ['a'..'g']) [0,4..])
    let msgs = test_one_chan endless
    -- TODO check that this doesn't hang
    print_msgs (take 60 msgs)

    -- TODO check for dragging by going over a huge list and watching memory

pb_range = (-12, 12)

test_one_chan events =
    Perform.perform_notes (with_chan 0 (mkevents events))
    where with_chan chan = map (\evt -> (evt, (dev1, chan)))

print_msgs = mapM_ (putStrLn . show_msg)
show_msg (Midi.WriteMessage dev ts msg) =
    show dev ++ ": " ++ pretty ts ++ ": " ++ show msg

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
    let channelize = map snd . Perform.channelize . mkevents
    -- Re-use channels when the pitch is different, but don't when it's the
    -- same.
    equal [0, 1, 0, 1, 2] $ channelize
            [ ("a", 0, 2, [])
            , ("a", 1, 2, [])
            , ("b", 1, 2, [])
            , ("b", 1.5, 2, [])
            , ("a", 1.75, 2, [])
            ]

    -- TODO test cents and controls differences

    -- All under volume, but "pb" also has a pitchbend, so it gets its own
    -- track.
    -- print $ Perform.channelize $ mkevents
    --         [ ("a", 0, 4, [c_vol])
    --         , ("pb", 2, 4, [c_vol, c_pitch])
    --         , ("c", 4, 4, [c_vol, c_pitch])
    --         ]

test_overlap_map = do
    let f overlapping event = (event, map fst overlapping)
    let events = mkevents
            [ ("a", 0, 2, [])
            , ("b", 1, 2, [])
            , ("c", 1.5, 2, [])
            , ("d", 3, 2, [])
            ]
    plist $ Perform.overlap_map f events

-- * allot

test_allot = do
    let mk inst chan start = (mkevent (inst, "a", start, 1, []), chan)
    let mk1 = mk inst1
    let in_time mks = zipWith ($) mks [0..]
    let allot_chans events = map snd $
            map snd (Perform.allot inst_config events)

    -- They should alternate channels, according to LRU.
    equal (allot_chans (in_time [mk1 0, mk1 1, mk1 2, mk1 3]))
        [0, 1, 0, 1]

    -- Repeated chans get reused.
    equal (allot_chans (in_time [mk1 3, mk1 2, mk1 2, mk1 3]))
        [0, 1, 1, 0]

    -- Instruments with no allocation get filtered out.
    equal (allot_chans (in_time [mk1 1, mk inst2 1, mk1 2]))
        [0, 1]

-- * setup

mkevent (inst, pitch, start, dur, controls) =
    Perform.Event inst (ts start) (ts dur) (mkpitch pitch)
        (Map.fromList controls)
    where ts = Timestamp.seconds
mkevents = map (\ (p, s, d, c) -> mkevent (inst1, p, s, d, c))

events1 = mkevents
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
inst2 = inst "inst2"
dev1 = Midi.WriteDevice "inst1 dev"
inst_config = Instrument.Config
    (Map.fromList [((dev1, 0), inst1), ((dev1, 1), inst1)])

-- Also includes inst2.
inst_config2 = Instrument.Config
    (Map.fromList [((dev1, 0), inst1), ((dev1, 1), inst1),
        ((dev1, 2), inst2)])

instance Show Perform.Event where
    show e = printf "<%s--%s: %s>"
        (pretty (Perform.event_start e)) (pretty (Perform.event_end e))
        (pretty_pitch (Perform.event_pitch e))
pretty_pitch (Pitch.Pitch s _) = show s

instance Pretty Instrument.Instrument where
    pretty inst = "<inst: " ++ Instrument.inst_name inst ++ ">"

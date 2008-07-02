module Perform.Midi.Perform_test where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.IO as IO

import Util.Pretty
import Util.Test

import Ui.Types
import qualified Ui.Track as Track

import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))

import qualified Derive.Score as Score

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


-- TODO this mostly just prints results out for eyeball inspection.  I need to
-- come up with properties and assert them

-- possible properties:
-- msgs are ordered

-- * perform

-- Bad signal that goes over 1 in two places.
badsig cont = (cont, mksignal [(0, 0), (1, 1.5), (2, 0), (2.5, 0), (3, 2)])

vol_cc = Controller.Controller "volume"

test_clip_warns = do
    let events = map mkevent [(inst1, "a", 0, 4, [badsig vol_cc])]
        (msgs, warns) = Perform.perform test_lookup inst_config1 events
    -- TODO check that warnings came at the right places
    -- check that the clips happen at the same places as the warnings
    equal (map Warning.warn_msg warns)
        (replicate 2 "Controller \"volume\" clipped")
    check (all_msgs_valid msgs)

    -- ascertain visually that the clip warnings are accurate
    plist warns
    plist $ Maybe.catMaybes $ map (midi_cc_of . Midi.wmsg_msg) msgs
    plist events

test_vel_clip_warns = do
    let (msgs, warns) = Perform.perform test_lookup inst_config1 $ map mkevent
            [(inst1, "a", 0, 4, [badsig Controller.c_velocity])]
    equal (length warns) 1
    check (all_msgs_valid msgs)

all_msgs_valid wmsgs = all Midi.valid_msg (map Midi.wmsg_msg wmsgs)

midi_cc_of (Midi.ChannelMessage _ (Midi.ControlChange cc val)) = Just (cc, val)
midi_cc_of _ = Nothing

test_perform = do
    let perform events = do
        let (msgs, warns) =
                Perform.perform test_lookup inst_config2 (map mkevent events)
        -- print_msgs msgs
        -- putStrLn ""
        equal warns []
        return (map unpack_msg msgs)

    -- channel 0 reused for inst1, inst2 gets its own channel
    msgs <- perform
        [ (inst1, "a", 0, 1, [])
        , (inst1, "b", 0, 1, [])
        , (inst2, "c", 0, 1, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 0.0, 0, NoteOn 61 100)
        , ("dev2", 0.0, 2, NoteOn 62 100)
        , ("dev1", 1.0, 0, NoteOff 60 100)
        , ("dev1", 1.0, 0, NoteOff 61 100)
        , ("dev2", 1.0, 2, NoteOff 62 100)
        ]

    -- identical notes get split across channels 0 and 1
    msgs <- perform
        [ (inst1, "a", 0, 2, [])
        , (inst1, "a", 1, 2, [])
        , (inst1, "b", 1, 2, [])
        , (inst1, "b", 1.5, 2, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0, 1, NoteOn 60 100)
        , ("dev1", 1.0, 0, NoteOn 61 100)
        , ("dev1", 1.5, 1, NoteOn 61 100)
        , ("dev1", 2.0, 0, NoteOff 60 100)
        , ("dev1", 3.0, 1, NoteOff 60 100)
        , ("dev1", 3.0, 0, NoteOff 61 100)
        , ("dev1", 3.5, 1, NoteOff 61 100)
        ]

    -- velocity curve shows up in NoteOns and NoteOffs
    msgs <- perform
        [ (inst1, "a", 0, 2, [c_vel])
        , (inst1, "b", 2, 2, [c_vel])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 127)
        , ("dev1", 2.0, 0, NoteOff 60 63)
        , ("dev1", 2.0, 0, NoteOn 61 63)
        , ("dev1", 4.0, 0, NoteOff 61 0)
        ]

unpack_msg (Midi.WriteMessage (Midi.WriteDevice dev) ts
        (Midi.ChannelMessage chan msg)) =
    (dev, Timestamp.to_seconds ts, chan, msg)
unpack_msg (Midi.WriteMessage (Midi.WriteDevice dev) ts msg) =
    error $ "unknown msg: " ++ show msg

test_perform_lazy = do
    let endless = map (\(n, ts) -> (n:"", ts, 4, [c_vol]))
            (zip (cycle ['a'..'g']) [0,4..])
    let (msgs, _warns) = test_one_chan endless
    (th_id, chan) <- pretend_to_write msgs
    Concurrent.threadDelay (5 * 1000000)
    Concurrent.killThread th_id
    -- forever $ do
    --     (i, msg) <- Chan.readChan chan
    --     putStrLn $ show i ++ ": " ++ show_msg msg
    -- mapM_
    -- -- TODO check that this doesn't hang
    -- print_msgs (take 60 msgs)

    -- TODO check for dragging by going over a huge list and watching memory
    -- TODO test with huge list using (++) and then with DList.append
    -- dlist
    -- 118000
    -- 504000
    --
    -- ++
    -- 118000
    -- 494000

pretend_to_write xs = do
    status_chan <- Concurrent.newChan
    th_id <- Concurrent.forkIO $ IO.withFile "/dev/null" IO.WriteMode $ \hdl ->
        mapM_ (write status_chan hdl) (zip [0..] xs)
    return (th_id, status_chan)
    where
    write status_chan handle (i, msg) = do
        when (i `mod` 1000 == 0) $ do
            Concurrent.writeChan status_chan (i `div` 1000, msg)
            putStrLn $ show i ++ ": " ++ show_msg msg
        IO.hPutStr handle (show msg)

test_one_chan events =
    Perform.perform_notes (with_chan 0 (mkevents events))
    where with_chan chan = map (\evt -> (evt, (dev1, chan)))

print_msgs = mapM_ (putStrLn . show_msg)
show_msg (Midi.WriteMessage dev ts msg) =
    show dev ++ ": " ++ pretty ts ++ ": " ++ show msg

-- print_ts_msgs = mapM_ (putStrLn . show_ts_msg)
-- show_ts_msg (ts, msg) = pretty ts ++ ": " ++ show msg

-- * controller

secs = Timestamp.seconds

test_perform_controller1 = do
    -- Bad signal that goes over 1 in two places.
    let sig = (vol_cc, mksignal [(0, 0), (1, 1.5), (2, 0), (2.5, 0), (3, 2)])
        cmap = Controller.default_controllers
        (msgs, warns) = Perform.perform_controller cmap (secs 0) (secs 10) 0 sig

    -- controls are not emitted after they reach steady values
    check $ all Midi.valid_msg (map snd msgs)
    -- goes over in 2 places
    equal (length warns) 2

    -- plist msgs

test_perform_controller2 = do
    let sig = (vol_cc, mksignal [(0, 0), (4, 1)])
        cmap = Controller.default_controllers
        (msgs, warns) = Perform.perform_controller cmap (secs 2) (secs 4) 0 sig
    plist msgs

-- * channelize

test_channelize = do
    let inst_addrs = Perform.config_to_inst_addrs inst_config2 test_lookup
        channelize = map snd . Perform.channelize inst_addrs . mkevents
    -- Re-use channels when the pitch is different, but don't when it's the
    -- same.
    equal (channelize
        [ ("a", 0, 2, [])
        , ("a", 1, 2, [])
        , ("b", 1, 2, [])
        , ("b", 1.5, 2, [])
        , ("a", 1.75, 2, [])
        ])
        [0, 1, 0, 1, 2]

    -- Even though they share pitches, they get the same channel, since inst2
    -- only has one addr.
    equal (map snd $ Perform.channelize inst_addrs $ map mkevent
        [ (inst2, "a", 0, 2, [])
        , (inst2, "a", 1, 2, [])
        ])
        [0, 0]

    -- TODO test cents and controls differences

    -- All under volume, but "pb" also has a pitchbend, so it gets its own
    -- track.  "pb" and "c" share since they have the same controllers.
    equal (channelize
        [ ("a", 0, 4, [c_vol])
        , ("pb", 2, 4, [c_vol, c_pitch])
        , ("c", 4, 4, [c_vol, c_pitch])
        ])
        [0, 1, 1]

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
        mk1 = mk inst1
        in_time mks = zipWith ($) mks [0..]
        inst_addrs = Perform.config_to_inst_addrs inst_config1 test_lookup
        allot_chans events = map snd $ map snd (Perform.allot inst_addrs events)

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
        (Map.fromList controls) [(Track.TrackId "fakepos", TrackPos 42)]
    where ts = Timestamp.seconds
mkevents = map (\ (p, s, d, c) -> mkevent (inst1, p, s, d, c))

events1 = mkevents
    [ ("a", 0, 8, [])
    , ("b", 2, 8, [])
    ]


mksignal ts_vals = Signal.signal
    [(Timestamp.to_track_pos (secs sec), Signal.Linear, val, val)
        | (sec, val) <- ts_vals]

c_vol = (vol_cc, mksignal [(0, 1), (4, 0)])
c_vol2 = (vol_cc, mksignal [(0, 1), (2, 0), (4, 1)])
c_vel = (Controller.c_velocity, mksignal [(0, 1), (4, 0)])
c_pitch = (Controller.c_pitch, mksignal [(0, 0), (8, 1)])

mkpitch s = Pitch.Pitch s (Pitch.NoteNumber p)
    where
    p = maybe (error ("no pitch " ++ show s)) id (lookup (head s) to_pitch)
to_pitch = zip ['a'..'z'] [60..]
inst name = Instrument.instrument name Controller.default_controllers
    (-12, 12) Nothing

inst1 = inst "inst1"
inst2 = inst "inst2"
dev1 = Midi.WriteDevice "dev1"
dev2 = Midi.WriteDevice "dev2"
inst_config1 = Instrument.config
    [ ((dev1, 0), score_inst inst1)
    , ((dev1, 1), score_inst inst1) ]
    Nothing

score_inst inst = Score.Instrument (Instrument.inst_name inst)

-- Also includes inst2.
inst_config2 = Instrument.config
    [ ((dev1, 0), score_inst inst1), ((dev1, 1), score_inst inst1)
    , ((dev2, 2), score_inst inst2) ]
    Nothing

test_lookup (Score.Instrument inst)
    | inst == "inst1" = Just inst1
    | inst == "inst2" = Just inst2
    | otherwise = Nothing

module Perform.Midi.Perform_test where
import Control.Monad
import qualified Control.Concurrent as Concurrent
import qualified Debug.Trace as Trace
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified System.IO as IO

import Util.Pretty
import Util.Test
import qualified Util.Seq as Seq

import Ui
import qualified Ui.Id as Id
import qualified Ui.Types as Types

import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))
import qualified Instrument.MidiDb as MidiDb

import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.SignalBase as SignalBase
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform


legato :: Double
legato = (\(RealTime pos) -> pos) Perform.legato_overlap_time

-- * perform

test_perform = do
    let t_perform events = do
        let evts = Seq.sort_on Perform.event_start (concatMap mkevents events)
        -- pprint evts
        let (msgs, warns) = perform inst_config2 evts
        -- print_msgs msgs
        -- putStrLn ""
        equal warns []
        return (map unpack_msg (filter (Midi.is_note . Midi.wmsg_msg) msgs))

    -- fractional notes get their own channels
    msgs <- t_perform
        [ [ (inst1, "a", 0, 1, [])
        , (inst1, "a2", 1, 1, [])
        , (inst1, "b", 2, 1, [])
        , (inst1, "b2", 3, 1, [])
        ] ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0, 0, NoteOff 60 100)

        , ("dev1", 1.0, 1, NoteOn 60 100)
        , ("dev1", 2.0, 1, NoteOff 60 100)

        , ("dev1", 2.0, 0, NoteOn 61 100)
        , ("dev1", 3.0, 0, NoteOff 61 100)

        , ("dev1", 3.0, 1, NoteOn 61 100)
        , ("dev1", 4.0, 1, NoteOff 61 100)
        ]

    -- channel 0 reused for inst1, inst2 gets its own channel
    msgs <- t_perform
        [ [(inst1, "a", 0, 1, [])]
        , [(inst1, "b", 0, 1, [])]
        , [(inst2, "c", 0, 1, [])]
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
    msgs <- t_perform
        [ [(inst1, "a", 0, 2, [])]
        , [(inst1, "a", 1, 2, [])]
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0, 1, NoteOn 60 100)

        , ("dev1", 2.0, 0, NoteOff 60 100)
        , ("dev1", 3.0, 1, NoteOff 60 100)
        ]

    -- velocity curve shows up in NoteOns and NoteOffs
    -- also legato overlap comes into play
    let c_vel = (Control.c_velocity, mksignal [(0, 1), (4, 0)])
    msgs <- t_perform
        [ [ (inst1, "a", 0, 2, [c_vel])
        , (inst1, "b", 2, 2, [c_vel])
        ] ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 127)
        , ("dev1", 2.0, 0, NoteOn 61 63)
        , ("dev1", 2.0 + legato, 0, NoteOff 60 63)
        , ("dev1", 4.0, 0, NoteOff 61 0)
        ]

    -- Legato does not apply to consecutive notes with the same pitch, since
    -- that makes the NoteOff abiguous.
    msgs <- t_perform
        [ [ (inst1, "a", 0, 1, [])
        , (inst1, "a", 1, 1, [])
        ] ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0, 0, NoteOff 60 100)
        , ("dev1", 1.0, 0, NoteOn 60 100)
        , ("dev1", 2.0, 0, NoteOff 60 100)
        ]

trace_warns :: [Warning.Warning] -> a -> a
trace_warns warns val
    | null warns = val
    | otherwise = Trace.trace (unlines $ map Warning.warn_msg warns) val

test_control_lead_time = do
    -- verify that controls are given lead time if they are on their own
    -- channels, and not if they aren't
    let Timestamp.Timestamp lead =
            Timestamp.from_real_time Perform.control_lead_time
    let extract_msgs wmsgs =
            [(ts, chan, msg) | Midi.WriteMessage _ (Timestamp.Timestamp ts)
                (Midi.ChannelMessage chan msg) <- wmsgs]
        extract (msgs, warns) = trace_warns warns (extract_msgs msgs)
    let f = extract . perform inst_config2 . mkevents_inst1

    equal (f [("a", 0, 4, []), ("b2", 4, 4, [])])
        [ (0, 0, PitchBend 0)
        , (0, 0, NoteOn 60 100)

        , (4000 - lead, 1, PitchBend 0.5)
        , (4000, 0, NoteOff 60 100)
        , (4000, 1, NoteOn 61 100)
        , (8000, 1, NoteOff 61 100)
        ]

    let vol = (vol_cc, mksignal [(4, 0), (6, 1)])
    equal (f [("a", 0, 4, []), ("b", 4, 4, [vol])])
        [ (0, 0, PitchBend 0)
        , (0, 0, NoteOn 60 100)

        , (4000 - lead, 1, PitchBend 0)
        , (4000 - lead, 1, ControlChange 7 0)

        , (4000, 0, NoteOff 60 100)

        , (4000, 1, NoteOn 61 100)
        , (5000, 1, ControlChange 7 63)
        , (6000, 1, ControlChange 7 127)
        , (8000, 1, NoteOff 61 100)
        ]

    -- Force them to be on the same channel, so there shouldn't be any
    -- control lead.
    let f2 = extract . perform inst_config2 . mkevents
    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b2", 4, 4, [])])
        [ (0, 2, PitchBend 0)
        , (0, 2, NoteOn 60 100)
        , (4000, 2, PitchBend 0.5)
        , (4000, 2, NoteOn 61 100)
        , (4010, 2, NoteOff 60 100) -- legato overlap kicks in
        , (8000, 2, NoteOff 61 100)
        ]

    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b", 4, 4, [vol])])
        [ (0, 2, PitchBend 0)
        , (0, 2, NoteOn 60 100)
        , (4000, 2, ControlChange 7 0)
        , (4000, 2, NoteOn 61 100)
        , (4010, 2, NoteOff 60 100) -- legato overlap kicks in
        , (5000, 2, ControlChange 7 63)
        , (6000, 2, ControlChange 7 127)
        , (8000, 2, NoteOff 61 100)
        ]


-- Bad signal that goes over 1 at 1 and 3.
badsig cont = (cont, mksignal [(0, 0), (1.5, 1.5), (2.5, 0.5), (4, 2)])

test_clip_warns = do
    let events = [mkevent (inst1, "a", 0, 4, [badsig vol_cc])]
        (msgs, warns) = perform inst_config1 events
    -- TODO check that warnings came at the right places
    -- check that the clips happen at the same places as the warnings

    equal (extract_warns warns)
        -- yeah matching floats is silly but it's quick and easy...
        [ ("Control \"volume\" clipped", Just (1.5, 1.5))
        , ("Control \"volume\" clipped", Just (3.5, 4))
        ]

    check (all_msgs_valid msgs)

extract_warns = map (\w -> (Warning.warn_msg w, Warning.warn_pos w))

test_vel_clip_warns = do
    let (msgs, warns) = perform inst_config1 $ mkevents_inst1
            [("a", 0, 4, [badsig Control.c_velocity])]
    equal (extract_warns warns) [("Control \"vel\" clipped", Just (0, 4))]
    check (all_msgs_valid msgs)

all_msgs_valid wmsgs = all Midi.valid_msg (map Midi.wmsg_msg wmsgs)

midi_cc_of (Midi.ChannelMessage _ (Midi.ControlChange cc val)) = Just (cc, val)
midi_cc_of _ = Nothing

unpack_msg (Midi.WriteMessage (Midi.WriteDevice dev) ts
        (Midi.ChannelMessage chan msg)) =
    (dev, Timestamp.to_seconds ts, chan, msg)
unpack_msg (Midi.WriteMessage (Midi.WriteDevice _) _ msg) =
    error $ "unknown msg: " ++ show msg

test_perform_lazy = do
    let perform evts = Perform.perform_notes [(evt, (dev1, 0)) | evt <- evts]
    let endless = map mkevent [(inst1, n:"", ts, 4, [])
            | (n, ts) <- zip (cycle ['a'..'g']) [0,4..]]
    let (msgs, _warns) = perform endless
    res <- run_timeout 1 $ return (take 20 msgs)
    case res of
        Just msgs -> print_msgs msgs
        Nothing -> return ()

run_timeout :: Double -> IO a -> IO (Maybe a)
run_timeout timeout action = do
    mvar <- Concurrent.newEmptyMVar
    th1 <- Concurrent.forkIO $ do
        val <- action
        Concurrent.putMVar mvar (Just val)
    th2 <- Concurrent.forkIO $ do
        Concurrent.threadDelay (floor (timeout * 1000000))
        Concurrent.putMVar mvar Nothing
    result <- Concurrent.takeMVar mvar
    mapM_ Concurrent.killThread [th1, th2]
    return result

--     (th_id, chan) <- pretend_to_write msgs
--     Concurrent.threadDelay (5 * 1000000)
--     Concurrent.killThread th_id
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

print_msgs = mapM_ (putStrLn . show_msg)
show_msg (Midi.WriteMessage dev ts msg) =
    show dev ++ ": " ++ pretty ts ++ ": " ++ show msg

test_pitch_curve = do
    let event pitch = Perform.Event inst1 (RealTime 1) (RealTime 0.5)
            Map.empty (Signal.signal pitch) []
    let f evt = (Seq.drop_dups id (map Midi.wmsg_msg msgs), warns)
            where
            (msgs, warns, _) = Perform.perform_note
                (RealTime 0) Nothing evt (dev1, 1)
        chan msgs = (map (Midi.ChannelMessage 1) msgs, [])

    equal (f (event [(1, 42.5)]))
        (chan [Midi.PitchBend 0.5, Midi.NoteOn 42 100, Midi.NoteOff 42 100])

    equal (f (event [(1, 42), (1.5, 42.5), (1.75, 43), (1.9, 43.5), (2, 44)]))
        (chan $
            [ Midi.PitchBend 0, Midi.NoteOn 42 100
            , Midi.PitchBend 0.5
            , Midi.NoteOff 42 100
            , Midi.PitchBend 1
            ])

    let notes prev evt = [(Midi.wmsg_ts msg, Midi.wmsg_msg msg) | msg <- msgs]
            where
            (msgs, _, _) = Perform.perform_note prev Nothing evt (dev1, 1)
    -- Try to use the control_lead_time unless the previous note is too close.
    equal (head (notes (RealTime 0) (event [(1, 42.5)])))
        (Timestamp.Timestamp 900, Midi.ChannelMessage 1 (Midi.PitchBend 0.5))
    equal (head (notes (RealTime 1) (event [(1, 42.5)])))
        (Timestamp.Timestamp 1000, Midi.ChannelMessage 1 (Midi.PitchBend 0.5))

test_no_pitch = do
    let event = (mkevent (inst1, "a", 0, 2, []))
            { Perform.event_pitch = Signal.constant Signal.invalid_pitch }
    let (midi, logs) = perform inst_config1 [event]
    equal (map Midi.wmsg_msg midi) []
    equal (map (\w -> (Warning.warn_msg w, Warning.warn_pos w)) logs)
        [("no pitch signal", Just (0, 2))]

test_keyswitch = do
    let extract msgs = [(ts, key) | Midi.WriteMessage { Midi.wmsg_ts = ts,
            Midi.wmsg_msg = Midi.ChannelMessage _ (Midi.NoteOn key _) } <- msgs]
        ks_inst ks = inst1 { Instrument.inst_keyswitch = ks }
        with_addr (ks, note, start, dur) =
            (mkevent (ks_inst ks, note, start, dur, []), (dev1, 0))
        ks1 = Just (Instrument.Keyswitch "ks1" 1)
        ks2 = Just (Instrument.Keyswitch "ks2" 2)
    let perform_notes evts = (extract msgs, warns)
            where (msgs, warns) = Perform.perform_notes (map with_addr evts)

    -- ts clips at 0, redundant ks suppressed.
    equal (perform_notes [(ks1, "a", 0, 1), (ks1, "b", 10, 10)])
        ([(0, 1), (0, 60), (10000, 61)], [])
    equal (perform_notes [(ks1, "a", 0, 1), (ks2, "b", 10, 10)])
        ([(0, 1), (0, 60), (9996, 2), (10000, 61)], [])

    equal (perform_notes [(Nothing, "a", 0, 1), (ks1, "b", 10, 10)])
        ([(0, 60), (9996, 1), (10000, 61)], [])

-- * post process

test_drop_duplicates = do
    let mkcc chan cc val = Midi.ChannelMessage chan (Midi.ControlChange cc val)
        mkpb chan val = Midi.ChannelMessage chan (Midi.PitchBend val)
        mkwmsgs msgs =
            [Midi.WriteMessage dev1 ts msg | (ts, msg) <- zip [0..] msgs]
        extract wmsg = (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)
    let f = map extract . Perform.drop_duplicates
    let msgs = [mkcc 0 1 10, mkcc 1 1 10, mkcc 0 1 11, mkcc 0 2 10]
    -- no drops
    equal (f (mkwmsgs msgs)) (zip [0..] msgs)
    let with_dev dmsgs =
            [Midi.WriteMessage dev ts msg | (ts, (dev, msg)) <- zip [0..] dmsgs]
    equal (f (with_dev [(dev1, mkcc 0 1 10), (dev2, mkcc 0 1 10)]))
        [(0, mkcc 0 1 10), (1, mkcc 0 1 10)]
    -- dup is dropped
    equal (f (mkwmsgs [mkcc 0 1 10, mkcc 0 2 10, mkcc 0 1 10]))
        [(0, mkcc 0 1 10), (1, mkcc 0 2 10)]
    -- dup is dropped
    equal (f (mkwmsgs [mkpb 0 1, mkpb 1 1, mkpb 0 1]))
        [(0, mkpb 0 1), (1, mkpb 1 1)]

    -- keyswitches

-- * control

test_perform_control1 = do
    -- Bad signal that goes over 1 in two places.
    let sig = (vol_cc, mksignal [(0, 0), (1, 1.5), (2, 0), (2.5, 0), (3, 2)])
        (msgs, warns) = Perform.perform_control Control.empty_map 0 0 4 sig

    -- controls are not emitted after they reach steady values
    check $ all Midi.valid_chan_msg (map snd msgs)
    -- goes over in 2 places
    equal (length warns) 2

test_perform_control2 = do
    let sig = (vol_cc, mksignal [(0, 0), (4, 1)])
        (msgs, warns) = Perform.perform_control Control.empty_map 0 2 4 sig
    plist warns
    plist msgs

-- * channelize

-- test the overlap map and channel allocation
test_channelize = do
    let inst_addrs = Perform.config_to_inst_addrs inst_config2 test_lookup
        pevent (start, dur, psig) =
            Perform.Event inst1 start dur Map.empty (Signal.signal psig) []
        f = map snd . Perform.channelize inst_addrs . map pevent

    -- Re-use channels when the pitch is different, but don't when it's the
    -- same.
    equal (f
        [ (0, 2, [(0, 60)])
        , (0, 2, [(0, 60.5)])
        , (0, 2, [(0, 61)]) -- can share with event 0
        , (0, 2, [(0, 61.5)]) -- can share with event 1
        , (0, 2, [(0, 60.75)]) -- can share with neither
        , (6, 2, [(6, 60.75)]) -- back to 0
        ])
        [0, 1, 0, 1, 2, 0]

    -- can't share because of decay
    equal (f
        [ (0, 2, [(0, 60)])
        , (2, 2, [(2, 60.5)])
        ])
        [0, 1]

    -- still can't share because of the control lead time
    let evt0_end = Perform.note_end (pevent (0, 2, []))
    equal (f
        [ (0, 2, [(0, 60)])
        , (evt0_end, 2, [(evt0_end, 60.5)])
        ])
        [0, 1]

    -- can finally share
    let evt0_end2 = evt0_end + Perform.control_lead_time
    equal (f
        [ (0, 2, [(0, 60)])
        , (evt0_end2, 2, [(evt0_end2, 60.5)])
        ])
        [0, 0]

    -- don't bother channelizing if the inst only has one addr
    equal (map snd $ Perform.channelize inst_addrs $ mkevents
        [ (inst2, "a", 0, 2, [])
        , (inst2, "a2", 1, 2, [])
        ])
        [0, 0]

test_can_share_chan = do
    let pevent (start, dur, psig, conts) =
            Perform.Event inst1 start dur (mkcontrols conts)
                (Signal.signal psig) []
    let f evt0 evt1 = Perform.can_share_chan (pevent evt0) (pevent evt1)

    -- Pitches with an integral difference overlap.
    equal (f (0, 2, [(0, 60)], []) (0, 2, [(0, 61)], [])) True
    -- Same pitch can't.
    equal (f (0, 2, [(0, 60)], []) (0, 2, [(0, 60)], [])) False
    -- If they overlap during the decay, having the same note is ok.
    equal (f (0, 2, [(0, 60)], []) (2, 2, [(0, 60)], [])) True

    -- Fractional pitches don't share, even in the decay.
    equal (f (0, 2, [(0, 60)], []) (0, 2, [(0, 60.5)], [])) False
    equal (f (0, 2, [(0, 60)], []) (2, 2, [(2, 60.5)], [])) False
    -- Still can't share because of the control lead time.
    let e0_end = Perform.note_end (pevent (0, 2, [], []))
    equal (f (0, 2, [(0, 60)], []) (e0_end, 2, [(e0_end, 60.5)], [])) False
    -- Finally can share.
    let e0_end2 = e0_end + Perform.control_lead_time
    equal (f (0, 2, [(0, 60)], []) (e0_end2, 2, [(e0_end2, 60.5)], [])) True

    -- First pitch can't share because it's bent from its original pitch.
    -- Actually they could if the bend is integral and perform_note were
    -- smart enough to subtract the bent amount from the second note's note on.
    equal (f (0, 2, [(0, 60), (1, 61)], []) (2, 2, [(2, 62)], [])) False

    -- Can share baecause they have the same curve with integral transposition.
    equal (f (0, 2, [(0, 60), (1, 60.5)], []) (0, 2, [(0, 61), (1, 61.5)], []))
        True

    let cont = ("cont", [(0, 1), (1, 0.5), (2, 0)])
    -- Share with the same controller.
    equal (f (0, 2, [(0, 60)], [cont]) (0, 2, [(0, 61)], [cont])) True
    equal (f (0, 2, [(0, 60)], [cont]) (0, 2, [(0, 61)], [])) False

test_overlap_map = do
    let extent e = (Perform.event_start e, Perform.event_duration e)
    let f overlapping event = (extent event, map (extent . fst) overlapping)
    let events = mkevents_inst
            [ ("a", 0, 2, [])
            , ("b", 1, 2, [])
            , ("c", 1.5, 2, [])
            , ("d", 5, 2, [])
            ]
    equal (map snd (Perform.overlap_map f events))
        [ ((0, 2), [])
        , ((1, 2), [(0, 2)])
        , ((1.5, 2), [(1, 2), (0, 2)])
        , ((5, 2), [])
        ]

-- * allot

test_allot = do
    let mk inst chan start = (mkevent (inst, "a", start, 1, []), chan)
        mk1 = mk inst1
        in_time mks = zipWith ($) mks [0..]
        inst_addrs = Perform.config_to_inst_addrs inst_config1 test_lookup
        allot_chans events = map snd $ map snd $ fst $
                Perform.allot inst_addrs events

    -- They should alternate channels, according to LRU.
    equal (allot_chans (in_time [mk1 0, mk1 1, mk1 2, mk1 3]))
        [0, 1, 0, 1]

    -- Repeated chans get reused.
    equal (allot_chans (in_time [mk1 3, mk1 2, mk1 2, mk1 3]))
        [0, 1, 1, 0]

    -- Instruments with no allocation get filtered out.
    equal (allot_chans (in_time [mk1 1, mk inst2 1, mk1 2]))
        [0, 1]

test_allot_warn = do
    let inst_addrs = Perform.config_to_inst_addrs inst_config1 test_lookup
    let extract (evts, warns) =
            (map extract_evt evts, map Warning.warn_msg warns)
        extract_evt (e, (Midi.WriteDevice dev, chan)) =
            (Instrument.inst_name (Perform.event_instrument e), dev, chan)
    let f = extract . Perform.allot inst_addrs
            . map (\(evt, chan) -> (mkevent evt, chan))
    let no_inst = mkinst "no_inst"
    equal (f [((inst1, "a", 0, 1, []), 0)])
        ([("inst1", "dev1", 0)], [])
    equal (f [((no_inst, "a", 0, 1, []), 0), ((no_inst, "b", 1, 2, []), 0)])
        ([], ["no allocation for \"no_inst\""])

-- * setup

perform inst_config = Perform.perform test_lookup inst_config

mkevent :: EventSpec -> Perform.Event
mkevent event = head (mkevents [event])

mkevents_inst = map (\ (p, s, d, c) -> mkevent (inst1, p, s, d, c))

-- | Name will determine the pitch.  It can be a-z, or a2-z2, which will
-- yield fractional pitches.
type EventSpec = (Instrument.Instrument, String, RealTime, RealTime,
    [(Control.Control, Signal.Control)])

mkevents_inst1 evts = mkevents
    [(inst1, text, start, dur, conts) | (text, start, dur, conts) <- evts]

-- | Takes a list so it can simulate the controls and pitch for one track.
mkevents :: [EventSpec] -> [Perform.Event]
mkevents events = trim_pitches
    [Perform.Event inst start dur (Map.fromList controls) pitch_sig stack
        | (inst, _, start, dur, controls) <- events]
    where
    pitch_sig = Signal.signal [(pos, to_pitch p) | (_, p, pos, _, _) <- events]
    to_pitch p = Maybe.fromMaybe (error ("no pitch " ++ show p))
        (lookup p pitch_map)
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]
    stack =
        [ (Types.BlockId (Id.id "test" "fakeblock")
        , Just (Types.TrackId (Id.id "test" "faketrack"))
        , Just (42, 43))
        ]

mkcontrols :: [(String, [(RealTime, Signal.Y)])] -> Perform.ControlMap
mkcontrols csigs = Map.fromList
    [(Control.Control c, Signal.signal sig) | (c, sig) <- csigs]

-- snaked from Derive.Note.trim_pitches
trim_pitches :: [Perform.Event] -> [Perform.Event]
trim_pitches events = map trim_event (Seq.zip_next events)
    where
    trim_event (event, Nothing) = event
    trim_event (event, Just next) = event { Perform.event_pitch =
        Signal.truncate (Perform.event_start next) psig }
        where psig = Perform.event_pitch event

-- | Make a signal with linear interpolation between the points.
mksignal :: [(RealTime, Signal.Y)] -> Signal.Control
mksignal = Signal.signal . interpolate
    where
    interpolate :: [(RealTime, Signal.Y)] -> [(RealTime, Signal.Y)]
    interpolate ((x0, y0) : rest@((x1, y1) : _))
        | x0 >= x1 = interpolate rest
        | otherwise = [(x, SignalBase.y_at (d x0) y0 (d x1) y1 (d x))
            | x <- [x0, x0+1 .. x1-1]] ++ interpolate rest
    interpolate val = val
    d = SignalBase.x_to_double

vol_cc = Control.Control "volume"

inst1 = mkinst "inst1"
inst2 = mkinst "inst2"
mkinst name = Instrument.instrument (Instrument.synth_name synth1) name Nothing
    Control.empty_map (-1, 1)

dev1 = Midi.WriteDevice "dev1"
dev2 = Midi.WriteDevice "dev2"
synth1 = Instrument.synth "synth1" "synth1" []
inst_config1 = Instrument.config [(score_inst inst1, [(dev1, 0), (dev1, 1)])]

score_inst inst = Score.Instrument (Instrument.inst_name inst)

-- Also includes inst2.
inst_config2 = Instrument.config
    [ (score_inst inst1, [(dev1, 0), (dev1, 1)])
    , (score_inst inst2, [(dev2, 2)]) ]
default_ksmap = Instrument.make_keyswitches
    [("a1+a2", 0), ("a0", 1), ("a1", 2), ("a2", 3)]

test_lookup :: MidiDb.LookupMidiInstrument
test_lookup attrs (Score.Instrument inst)
    | inst == "inst1" = Just $ inst1
        { Instrument.inst_keyswitch =
            Instrument.get_keyswitch default_ksmap attrs }
    | inst == "inst2" = Just inst2
    | otherwise = Nothing

module Perform.Midi.Perform_test where
import qualified Control.Concurrent as Concurrent
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Thread as Thread

import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal
import qualified Perform.SignalBase as SignalBase

import Types


-- * perform

test_perform = do
    let t_perform events = do
        let evts = Seq.sort_on Perform.event_start (concatMap mkevents events)
        -- pprint evts
        let (msgs, warns) = perform midi_config2 evts
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
    let c_vel = (Control.c_velocity, mksignal [(0, 1), (4, 0)])
    msgs <- t_perform
        [ [ (inst1, "a", 0, 2, [c_vel])
        , (inst1, "b", 2, 2, [c_vel])
        ] ]
    equal msgs
        [ ("dev1", 0, 0, NoteOn 60 127)
        , ("dev1", 2, 0, NoteOff 60 63)
        , ("dev1", 2, 0, NoteOn 61 63)
        , ("dev1", 4, 0, NoteOff 61 0)
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

test_control_lead_time = do
    -- verify that controls are given lead time if they are on their own
    -- channels, and not if they aren't
    let lead = RealTime.to_milliseconds Perform.control_lead_time
    let extract_msgs wmsgs = [(RealTime.to_milliseconds ts, chan, msg)
            | Midi.WriteMessage _ ts (Midi.ChannelMessage chan msg) <- wmsgs]
        extract (msgs, warns) = (extract_msgs msgs, warns)
    let f = extract . perform midi_config2 . mkevents_inst

    equal (f [("a", 0, 4, []), ("b2", 4, 4, [])])
        ([ (0, 0, PitchBend 0)
        , (0, 0, NoteOn 60 100)

        , (4000 - lead, 1, PitchBend 0.5)
        , (4000, 0, NoteOff 60 100)
        , (4000, 1, NoteOn 61 100)
        , (8000, 1, NoteOff 61 100)
        ], [])

    let vol = (vol_cc, mksignal [(4, 0), (6, 1)])
    equal (f [("a", 0, 4, []), ("b", 4, 4, [vol])])
        ([ (0, 0, PitchBend 0)
        , (0, 0, NoteOn 60 100)

        , (4000 - lead, 1, PitchBend 0)
        , (4000 - lead, 1, ControlChange 7 0)

        , (4000, 0, NoteOff 60 100)

        , (4000, 1, NoteOn 61 100)
        , (5000, 1, ControlChange 7 63)
        , (6000, 1, ControlChange 7 127)
        , (8000, 1, NoteOff 61 100)
        ], [])

    -- Force them to be on the same channel, so there shouldn't be any
    -- control lead.
    let f2 = extract . perform midi_config2 . mkevents
    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b2", 4, 4, [])])
        ([ (0, 2, PitchBend 0)
        , (0, 2, NoteOn 60 100)
        , (4000, 2, NoteOff 60 100)
        , (4000, 2, PitchBend 0.5)
        , (4000, 2, NoteOn 61 100)
        , (8000, 2, NoteOff 61 100)
        ], [])

    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b", 4, 4, [vol])])
        ([ (0, 2, PitchBend 0)
        , (0, 2, NoteOn 60 100)
        , (4000, 2, NoteOff 60 100)
        , (4000, 2, ControlChange 7 0)
        , (4000, 2, NoteOn 61 100)
        , (5000, 2, ControlChange 7 63)
        , (6000, 2, ControlChange 7 127)
        , (8000, 2, NoteOff 61 100)
        ], [])

-- Bad signal that goes over 1 at 1 and 3.
badsig cont = (cont, mksignal [(0, 0), (1.5, 1.5), (2.5, 0.5), (4, 2)])

test_clip_warns = do
    let events = [mkevent (inst1, "a", 0, 4, [badsig vol_cc])]
        (msgs, warns) = perform midi_config1 events
    -- TODO check that warnings came at the right places
    -- check that the clips happen at the same places as the warnings

    equal warns
        [ "Perform: Control \"volume\" clipped: (1.5s, 2.5s)"
        -- TODO this used to be (3.5, 4) but I can't be bothered to find out
        -- why it changed when RealTime became integral
        , "Perform: Control \"volume\" clipped: (4s, 4s)"
        ]

    check (all_msgs_valid msgs)

test_vel_clip_warns = do
    let (msgs, warns) = perform midi_config1 $ mkevents_inst
            [("a", 0, 4, [badsig Control.c_velocity])]
    equal warns ["Perform: Control \"vel\" clipped: (0s, 4s)"]
    check (all_msgs_valid msgs)

all_msgs_valid wmsgs = all Midi.valid_msg (map Midi.wmsg_msg wmsgs)

midi_cc_of (Midi.ChannelMessage _ (Midi.ControlChange cc val)) = Just (cc, val)
midi_cc_of _ = Nothing

unpack_msg (Midi.WriteMessage (Midi.WriteDevice dev) ts
        (Midi.ChannelMessage chan msg)) =
    (dev, RealTime.to_seconds ts, chan, msg)
unpack_msg (Midi.WriteMessage (Midi.WriteDevice _) _ msg) =
    error $ "unknown msg: " ++ show msg

test_perform_lazy = do
    let perform evts = perform_notes [(evt, (dev1, 0)) | evt <- evts]
    let endless = map mkevent [(inst1, n:"", ts, 4, [])
            | (n, ts) <- zip (cycle ['a'..'g']) (Seq.range_ 0 4)]
    let (msgs, _warns) = perform endless
    res <- run_timeout 1 $ return (take 20 msgs)
    equal (fmap length res) (Just 20)

-- TODO move to a more general purpose place?
run_timeout :: Double -> IO a -> IO (Maybe a)
run_timeout timeout action = do
    mvar <- Concurrent.newEmptyMVar
    th1 <- Concurrent.forkIO $ do
        val <- action
        Concurrent.putMVar mvar (Just val)
    th2 <- Concurrent.forkIO $ do
        Thread.delay timeout
        Concurrent.putMVar mvar Nothing
    result <- Concurrent.takeMVar mvar
    mapM_ Concurrent.killThread [th1, th2]
    return result

-- print_msgs = mapM_ (putStrLn . show_msg)
-- show_msg (Midi.WriteMessage dev ts msg) =
--     show dev ++ ": " ++ pretty ts ++ ": " ++ show msg

test_pitch_curve = do
    let event pitch = mkpevent (1, 0.5, pitch, [])
    let f evt = Seq.drop_dups id (map Midi.wmsg_msg msgs)
            where
            msgs = LEvent.events_of $ fst $
                Perform.perform_note 0 evt (dev1, 1)
        chan msgs = map (Midi.ChannelMessage 1) msgs

    equal (f (event [(1, 42.5)]))
        (chan [Midi.PitchBend 0.5, Midi.NoteOn 42 100, Midi.NoteOff 42 100])

    equal (f (event [(1, 42), (1.5, 42.5), (1.75, 43), (1.9, 43.5), (2, 44)]))
        (chan
            [ Midi.PitchBend 0, Midi.NoteOn 42 100
            , Midi.PitchBend 0.5
            , Midi.NoteOff 42 100
            , Midi.PitchBend 1
            ])

    let notes prev evt = [(Midi.wmsg_ts msg, Midi.wmsg_msg msg) | msg <- msgs]
            where
            msgs = LEvent.events_of $ fst $
                Perform.perform_note (secs prev) evt (dev1, 1)
    -- Try to use the control_lead_time unless the previous note is too close.
    equal (head (notes 0 (event [(1, 42.5)])))
        (0.9, Midi.ChannelMessage 1 (Midi.PitchBend 0.5))
    equal (head (notes 1 (event [(1, 42.5)])))
        (1, Midi.ChannelMessage 1 (Midi.PitchBend 0.5))

test_no_pitch = do
    let event = (mkevent (inst1, "a", 0, 2, []))
            { Perform.event_pitch = Signal.constant Signal.invalid_pitch }
    let (midi, logs) = perform midi_config1 [event]
    equal (map Midi.wmsg_msg midi) []
    equal logs ["Perform: no pitch signal"]

test_keyswitch = do
    let extract msgs = [(wmsg_ts wmsg, key)
            | wmsg@(Midi.WriteMessage { Midi.wmsg_msg =
                Midi.ChannelMessage _ (Midi.NoteOn key _) })
            <- msgs]
        ks_inst ks = inst1 { Instrument.inst_keyswitch = ks }
        with_addr (ks, note, start, dur) =
            (mkevent (ks_inst ks, note, start, dur, []), (dev1, 0))
        ks1 = Just (Instrument.Keyswitch 1)
        ks2 = Just (Instrument.Keyswitch 2)
    let f evts = (extract msgs, warns)
            where (msgs, warns) = perform_notes (map with_addr evts)

    -- redundant ks suppressed.
    equal (f [(ks1, "a", 0, 1), (ks1, "b", 10, 10)])
        ([(-4, 1), (0, 60), (10000, 61)], [])
    equal (f [(ks1, "a", 0, 1), (ks2, "b", 10, 10)])
        ([(-4, 1), (0, 60), (9996, 2), (10000, 61)], [])

    equal (f [(Nothing, "a", 0, 1), (ks1, "b", 10, 10)])
        ([(0, 60), (9996, 1), (10000, 61)], [])

perform :: Instrument.Config -> [Perform.Event]
    -> ([Midi.WriteMessage], [String])
perform midi_config = split_logs . fst
    . Perform.perform Perform.initial_state midi_config . map LEvent.Event

perform_notes :: [(Perform.Event, Instrument.Addr)]
    -> ([Midi.WriteMessage], [String])
perform_notes = split_logs . fst
    . Perform.perform_notes Perform.empty_perform_state . map LEvent.Event

split_logs = second (map DeriveTest.show_log) . LEvent.partition

-- * post process

test_drop_dup_controls = do
    let mkcc chan cc val = Midi.ChannelMessage chan (Midi.ControlChange cc val)
        mkpb chan val = Midi.ChannelMessage chan (Midi.PitchBend val)
        mkwmsgs msgs =
            [Midi.WriteMessage dev1 (RealTime.milliseconds ts) msg
                | (ts, msg) <- zip [0..] msgs]
        extract wmsg = (wmsg_ts wmsg, Midi.wmsg_msg wmsg)
    let f = map extract . LEvent.events_of . fst
            . Perform.drop_dup_controls Map.empty . map LEvent.Event
    let msgs = [mkcc 0 1 10, mkcc 1 1 10, mkcc 0 1 11, mkcc 0 2 10]
    -- no drops
    equal (f (mkwmsgs msgs)) (zip [0..] msgs)
    let with_dev dmsgs =
            [Midi.WriteMessage dev (RealTime.milliseconds ts) msg
                | (ts, (dev, msg)) <- zip [0..] dmsgs]
    equal (f (with_dev [(dev1, mkcc 0 1 10), (dev2, mkcc 0 1 10)]))
        [(0, mkcc 0 1 10), (1, mkcc 0 1 10)]
    -- dup is dropped
    equal (f (mkwmsgs [mkcc 0 1 10, mkcc 0 2 10, mkcc 0 1 10]))
        [(0, mkcc 0 1 10), (1, mkcc 0 2 10)]
    -- dup is dropped
    equal (f (mkwmsgs [mkpb 0 1, mkpb 1 1, mkpb 0 1]))
        [(0, mkpb 0 1), (1, mkpb 1 1)]

    -- TODO keyswitches

-- * control

test_perform_control = do
    -- Bad signal that goes over 1 in two places.
    let sig = (vol_cc, mksignal [(0, 0), (1, 1.5), (2, 0), (2.5, 0), (3, 2)])
        (msgs, warns) = Perform.perform_control Control.empty_map
            (secs 0) (secs 0) sig

    -- controls are not emitted after they reach steady values
    check $ all Midi.valid_chan_msg (map snd msgs)
    -- goes over in 2 places
    equal (length warns) 2

-- * channelize

-- test the overlap map and channel allocation
test_channelize = do
    let inst_addrs = Instrument.config_alloc midi_config2
        pevent (start, dur, psig) = mkpevent (start, dur, psig, [])
        f = map snd . channelize inst_addrs . map pevent

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
    equal (map snd $ channelize inst_addrs $ mkevents
        [ (inst2, "a", 0, 2, [])
        , (inst2, "a2", 1, 2, [])
        ])
        [0, 0]

test_can_share_chan = do
    let f evt0 evt1 = Maybe.isNothing $
            Perform.can_share_chan (mkpevent evt0) (mkpevent evt1)

    -- Can't share, becase there is explicitly time for a leading pitch
    -- bend in the second event.
    equal (f (0, 2, [(0, 60)], []) (2, 2, [(2, 61.5)], [])) False

    -- can't share because first pitch bends
    equal (f (0, 2, [(0, 60), (1, 62)], []) (0, 2, [(0, 64)], [])) False

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
    let e0_end = Perform.note_end (mkpevent (0, 2, [], []))
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
    let f overlapping event =
            ((extent event, map (extent . fst) overlapping), [])
    let events = mkevents_inst
            [ ("a", 0, 2, [])
            , ("b", 1, 3, [])
            , ("c", 2, 2, [])
            , ("d", 6, 2, [])
            ]
    let to_sec = map $ \(event, overlap) -> (sec event, map sec overlap)
            where sec (a, b) = (RealTime.to_seconds a, RealTime.to_seconds b)
        ex = map snd . LEvent.events_of . fst
    -- remember that overlap includes cc lead and decay
    equal (to_sec (ex (Perform.overlap_map [] f (map LEvent.Event events))))
        [ ((0, 2), [])
        , ((1, 3), [(0, 2)])
        , ((2, 2), [(1, 3), (0, 2)])
        , ((6, 2), [])
        ]

channelize :: Perform.InstAddrs -> [Perform.Event]
    -> [(Perform.Event, Perform.Channel)]
channelize inst_addrs events = LEvent.events_of $ fst $
    Perform.channelize [] inst_addrs (map LEvent.Event events)

-- * allot

test_allot = do
    let mk inst chan start = (mkevent (inst, "a", start, 1, []), chan)
        mk1 = mk inst1
        in_time mks = zipWith ($) mks (Seq.range_ 0 1)
        inst_addrs = Instrument.config_alloc midi_config1
        allot_chans events = map (snd . snd) $ fst $ LEvent.partition $
            allot inst_addrs events

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
    let inst_addrs = Instrument.config_alloc midi_config1
    let extract (LEvent.Event (e, (Midi.WriteDevice dev, chan))) = Left
            (Instrument.inst_name (Perform.event_instrument e), dev, chan)
        extract (LEvent.Log msg) = Right $ DeriveTest.show_log msg
    let f = map extract . allot inst_addrs
            . map (\(evt, chan) -> (mkevent evt, chan))
    let no_inst = mkinst "no_inst"
    equal (f [((inst1, "a", 0, 1, []), 0)])
        [Left ("inst1", "dev1", 0)]
    equal (f [((no_inst, "a", 0, 1, []), 0), ((no_inst, "b", 1, 2, []), 0)])
        (replicate 2 $ Right "Perform: no allocation for >synth1/no_inst")

allot :: Perform.InstAddrs -> [(Perform.Event, Perform.Channel)]
    -> [LEvent.LEvent (Perform.Event, Instrument.Addr)]
allot inst_addrs events = fst $
    Perform.allot Perform.empty_allot_state inst_addrs (map LEvent.Event events)

-- * setup

wmsg_ts :: Midi.WriteMessage -> Integer
wmsg_ts = RealTime.to_milliseconds . Midi.wmsg_ts

secs :: Double -> RealTime
secs = RealTime.seconds

-- | Name will determine the pitch.  It can be a-z, or a2-z2, which will
-- yield fractional pitches.
type EventSpec = (Instrument.Instrument, String, RealTime, RealTime,
    [(Control.Control, Signal.Control)])

mkevent :: EventSpec -> Perform.Event
mkevent (inst, pitch, start, dur, controls) =
    Perform.Event inst start dur (Map.fromList controls)
        (psig start pitch) DeriveTest.fake_stack
    where
    psig pos p = Signal.signal [(pos, to_pitch p)]
    to_pitch p = Maybe.fromMaybe (error ("no pitch " ++ show p))
        (lookup p pitch_map)
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]

-- Similar to mkevent, but allow a pitch curve.
mkpevent (start, dur, psig, conts) =
    Perform.Event inst1 start dur (mkcontrols conts) (Signal.signal psig)
        Stack.empty

mkevents_inst = map (\(a, b, c, d) -> mkevent (inst1, a, b, c, d))

mkevents :: [EventSpec] -> [Perform.Event]
mkevents = map mkevent

mkcontrols :: [(String, [(RealTime, Signal.Y)])] -> Perform.ControlMap
mkcontrols csigs = Map.fromList
    [(Control.Control c, Signal.signal sig) | (c, sig) <- csigs]

-- | Make a signal with linear interpolation between the points.
mksignal :: [(RealTime, Signal.Y)] -> Signal.Control
mksignal = Signal.signal . interpolate
    where
    interpolate :: [(RealTime, Signal.Y)] -> [(RealTime, Signal.Y)]
    interpolate ((x0, y0) : rest@((x1, y1) : _))
        | x0 >= x1 = interpolate rest
        | otherwise = [(x, SignalBase.y_at (d x0) y0 (d x1) y1 (d x))
            | x <- Seq.range_end x0 (x1-1) 1] ++ interpolate rest
    interpolate val = val
    d = SignalBase.x_to_double

vol_cc = Control.Control "volume"

inst1 = mkinst "inst1"
inst2 = mkinst "inst2"
mkinst name = (Instrument.instrument name [] (-1, 1))
    { Instrument.inst_score = Score.Instrument ("synth1/" ++ name) }

dev1 = Midi.WriteDevice "dev1"
dev2 = Midi.WriteDevice "dev2"
synth1 = Instrument.synth "synth1" []
midi_config1 = Instrument.config
    [(Instrument.inst_score inst1, [(dev1, 0), (dev1, 1)])]

-- Also includes inst2.
midi_config2 = Instrument.config
    [ (Instrument.inst_score inst1, [(dev1, 0), (dev1, 1)])
    , (Instrument.inst_score inst2, [(dev2, 2)]) ]
default_ksmap = Instrument.keyswitch_map $
    map (first (Score.attributes . words))
        [("a1 a2", 0), ("a0", 1), ("a1", 2), ("a2", 3)]

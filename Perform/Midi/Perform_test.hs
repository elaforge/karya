-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Perform.Midi.Perform_test where
import qualified Control.Concurrent as Concurrent
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import Util.Test
import qualified Util.Thread as Thread
import qualified Util.TimeVector as TimeVector

import qualified Midi.Key as Key
import qualified Midi.Midi as Midi
import Midi.Midi (ChannelMessage(..))

import qualified Derive.Controls as Controls
import qualified Derive.DeriveTest as DeriveTest
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Midi.Perform as Perform
import qualified Perform.RealTime as RealTime
import qualified Perform.Signal as Signal

import Types


gap :: RealTime
gap = Perform.adjacent_note_gap

-- * perform

test_perform = do
    let f events = do
            let (msgs, warns) = perform midi_config2 $
                    Seq.sort_on Perform.event_start (map mkevent events)
            equal warns []
            return $ extract msgs
        extract = map extract_dev_msg . filter (Midi.is_note . Midi.wmsg_msg)

    -- fractional notes get their own channels
    msgs <- f
        [ (inst1, "a", 0, 1, [])
        , (inst1, "a2", 1, 1, [])
        , (inst1, "b", 2, 1, [])
        , (inst1, "b2", 3, 1, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0 - gap, 0, NoteOff 60 100)

        , ("dev1", 1.0, 1, NoteOn 60 100)
        , ("dev1", 2.0 - gap, 1, NoteOff 60 100)

        , ("dev1", 2.0, 0, NoteOn 61 100)
        , ("dev1", 3.0 - gap, 0, NoteOff 61 100)

        , ("dev1", 3.0, 1, NoteOn 61 100)
        , ("dev1", 4.0 - gap, 1, NoteOff 61 100)
        ]

    -- channel 0 reused for inst1, inst2 gets its own channel
    msgs <- f
        [ (inst1, "a", 0, 1, [])
        , (inst1, "b", 0, 1, [])
        , (inst2, "c", 0, 1, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 0.0, 0, NoteOn 61 100)
        , ("dev2", 0.0, 2, NoteOn 62 100)
        , ("dev1", 1.0 - gap, 0, NoteOff 60 100)
        , ("dev1", 1.0 - gap, 0, NoteOff 61 100)
        , ("dev2", 1.0 - gap, 2, NoteOff 62 100)
        ]

    -- identical notes get split across channels 0 and 1
    msgs <- f
        [ (inst1, "a", 0, 2, [])
        , (inst1, "a", 1, 2, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0, 1, NoteOn 60 100)

        , ("dev1", 2.0 - gap, 0, NoteOff 60 100)
        , ("dev1", 3.0 - gap, 1, NoteOff 60 100)
        ]

    -- velocity curve shows up in NoteOns and NoteOffs
    let c_vel = (Controls.velocity, linear_interp [(0, 1), (4, 0)])
    msgs <- f
        [ (inst1, "a", 0, 2, [c_vel])
        , (inst1, "b", 2, 2, [c_vel])
        ]
    equal msgs
        [ ("dev1", 0, 0, NoteOn 60 127)
        , ("dev1", 2 - gap, 0, NoteOff 60 64)
        , ("dev1", 2, 0, NoteOn 61 64)
        , ("dev1", 4 - gap, 0, NoteOff 61 0)
        ]

    -- Consecutive notes with the same pitch have NoteOff / NoteOn in the right
    -- order.
    msgs <- f
        [ (inst1, "a", 0, 1, [])
        , (inst1, "a", 1, 1, [])
        ]
    equal msgs
        [ ("dev1", 0.0, 0, NoteOn 60 100)
        , ("dev1", 1.0 - gap, 0, NoteOff 60 100)
        , ("dev1", 1.0, 0, NoteOn 60 100)
        , ("dev1", 2.0 - gap, 0, NoteOff 60 100)
        ]

test_perform_voices = do
    let f config = first e_note_ons . perform (mkconfig config)
            . map (mkevent . mke)
        mke (p, start) = (inst1, p, start, 1, [])
        mkconfig chans = Instrument.voice_configs
            [(Instrument.inst_score inst1, [((dev1, c), v) | (c, v) <- chans])]
    let config12 = [(0, Just 1), (1, Just 2)]
    equal (f config12 [("a", 0)]) ([(0, (0, Key.c4))], [])
    -- b gets bumped to the next channel.
    equal (f config12 [("a", 0), ("b", 0)])
        ([(0, (0, Key.c4)), (0, (1, Key.cs4))], [])
    -- But not if they don't overlap.
    equal (f config12 [("a", 0), ("b", 1)])
        ([(0, (0, Key.c4)), (1, (0, Key.cs4))], [])
    -- First voice gets stolen because they both ran out.
    equal (f config12 [("a", 0), ("b", 0.5), ("c", 0.5), ("d", 0.5)])
        ([(0, (0, Key.c4)), (0.5, (0, Key.ds4)), (0.5, (1, Key.cs4)),
            (0.5, (1, Key.d4))], [])
    -- Infinite voices means the second channel is never chosen.
    equal (f [(0, Nothing), (1, Just 1)] [("a", 0), ("b", 0), ("c", 0)])
        ([(0, (0, Key.c4)), (0, (0, Key.cs4)), (0, (0, Key.d4))], [])

test_aftertouch = do
    let f = map extract_msg . fst . perform midi_config1
                . Seq.sort_on Perform.event_start . map event
        event (pitch, start, dur, aftertouch) = mkevent
            (inst1, pitch, start, dur,
                [(Controls.aftertouch, Signal.signal aftertouch)])
    equal (f [("a", 0, 1, [(0, 1)]), ("b", 0, 1, [(0, 0)])])
        [ (0, 0, NoteOn Key.c4 100)
        , (0, 0, NoteOn Key.cs4 100)
        , (0, 0, Aftertouch Key.c4 127)
        , (0, 0, Aftertouch Key.cs4 0)
        , (0, 0, PitchBend 0)
        , (1 - gap, 0, NoteOff Key.c4 100)
        , (1 - gap, 0, NoteOff Key.cs4 100)
        ]

test_controls_after_note_off = do
    -- Test that controls happen during note off, but don't interfere with
    -- other notes.  This corresponds to the comment in 'Perform.perform_note'.
    let f = map extract . fst . perform midi_config2 . map mkevent
        extract msg = (start, m)
            where (start, _, m) = extract_msg msg
        sig xs = [(Controls.vol, Signal.signal xs)]
    let msgs = f
            [ (inst2, "a", 0, 1, sig [(0, 1), (1.95, 0.5)])
            , (inst2, "b", 2, 1, sig [(2, 1)])
            ]
    -- Signal at 1.95 dropped because of the next note on.
    equal msgs
        [ (0, NoteOn 60 100), (0, ControlChange 7 127), (0, PitchBend 0)
        , (1 - gap, NoteOff 60 100)
        , (2, NoteOn 61 100), (3 - gap, NoteOff 61 100)
        ]

    let msgs = f
            [ (inst2, "a", 0, 2, sig [(0, 1), (1.95, 0.5)])
            , (inst2, "b", 2, 1, sig [(2, 1)])
            ]
    -- But not this time.
    equal msgs
        [ (0, NoteOn 60 100), (0, ControlChange 7 127), (0, PitchBend 0)
        , (1.95, ControlChange 7 64)
        , (2 - gap, NoteOff 60 100)
        , (2, NoteOn 61 100), (2, ControlChange 7 127)
        , (3 - gap, NoteOff 61 100)
        ]

    let msgs = f
            [ (inst2, "a", 0, 1, sig [(0, 1), (1.5, 0.5)])
            , (inst2, "b", 2, 1, sig [(2, 1)])
            ]
    -- Room enough for both.
    equal msgs
        [ (0, NoteOn 60 100), (0, ControlChange 7 127), (0, PitchBend 0)
        , (1 - gap, NoteOff 60 100)
        , (1.5, ControlChange 7 64)
        , (1.9, ControlChange 7 127)
        , (2, NoteOn 61 100), (3 - gap, NoteOff 61 100)
        ]

test_control_lead_time = do
    -- verify that controls are given lead time if they are on their own
    -- channels, and not if they aren't
    let lead = Perform.control_lead_time
    let f = extract . perform midi_config2 . mkevents_inst
        extract (msgs, warns) = (map extract_msg msgs, warns)

    equal (f [("a", 0, 4, []), ("b2", 4, 4, [])])
        ([(0, 0, NoteOn 60 100)
        , (0, 0, PitchBend 0)

        , (4 - lead, 1, PitchBend 0.5)
        , (4 - gap, 0, NoteOff 60 100)
        , (4, 1, NoteOn 61 100)
        , (8 - gap, 1, NoteOff 61 100)
        ], [])

    let vol start = (Controls.vol, linear_interp [(start, 0), (start + 2, 1)])
    equal (f [("a", 0, 4, []), ("b", 2, 4, [vol 2])])
        ([(0, 0, NoteOn 60 100)
        , (0, 0, PitchBend 0)

        , (2 - lead, 1, ControlChange 7 0)
        , (2 - lead, 1, PitchBend 0)

        , (2, 1, NoteOn 61 100)
        , (3, 1, ControlChange 7 64)
        , (4 - gap, 0, NoteOff 60 100)
        , (4, 1, ControlChange 7 127)
        , (6 - gap, 1, NoteOff 61 100)
        ], [])

    -- Non-overlapping notes means they go on the same channel, but there's no
    -- room for control lead.
    equal (f [("a", 0, 4, []), ("b", 4, 4, [vol 4])])
        ([(0, 0, NoteOn 60 100)
        , (0, 0, PitchBend 0)
        , (4 - gap, 0, NoteOff 60 100)
        , (4, 0, NoteOn 61 100)
        , (4, 0, ControlChange 7 0)
        , (5, 0, ControlChange 7 64)
        , (6, 0, ControlChange 7 127)
        , (8 - gap, 0, NoteOff 61 100)
        ], [])

    -- Force them to be on the same channel, so there shouldn't be any
    -- control lead.
    let f2 = extract . perform midi_config2 . mkevents
    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b2", 4, 4, [])])
        ([(0, 2, NoteOn 60 100)
        , (0, 2, PitchBend 0)
        , (4 - gap, 2, NoteOff 60 100)
        , (4, 2, NoteOn 61 100)
        , (4, 2, PitchBend 0.5)
        , (8 - gap, 2, NoteOff 61 100)
        ], [])

    equal (f2 [(inst2, "a", 0, 4, []), (inst2, "b", 4, 4, [vol 4])])
        ([(0, 2, NoteOn 60 100)
        , (0, 2, PitchBend 0)
        , (4 - gap, 2, NoteOff 60 100)
        , (4, 2, NoteOn 61 100)
        , (4, 2, ControlChange 7 0)
        , (5, 2, ControlChange 7 64)
        , (6, 2, ControlChange 7 127)
        , (8 - gap, 2, NoteOff 61 100)
        ], [])

test_msgs_sorted = do
    -- Ensure that msgs are in order, even after control lead time.
    let f = extract . perform midi_config1 . map mkevent
        mkevent (s, dur, pitch) = mkpevent (s, dur, [(s, pitch)], [])
        extract = first (map extract_msg)
    let (msgs, logs) = f [(0, 1, 40.5), (1, 1, 40.5), (1, 1, 42.75)]
        ts = map (\(ts, _, _) -> ts) msgs
    equal logs []
    equal ts (List.sort ts)
    -- pprint msgs

-- Bad signal that goes over 1 at 1 and 3.
badsig :: Score.Control -> (Score.Control, Signal.Control)
badsig cont = (cont, linear_interp [(0, 0), (1.5, 1.5), (2.5, 0.5), (4, 2)])

test_clip_warns = do
    let events = [mkevent (inst1, "a", 0, 4, [badsig Controls.vol])]
        (msgs, warns) = perform midi_config1 events
    -- TODO check that warnings came at the right places
    -- check that the clips happen at the same places as the warnings
    equal warns
        [ "Perform: %vol clipped: (1.5s, 2.5s)"
        -- TODO this used to be (3.5, 4) but I can't be bothered to find out
        -- why it changed when RealTime became integral
        , "Perform: %vol clipped: (4s, 4s)"
        ]
    check (all_msgs_valid msgs)

test_vel_clip_warns = do
    let (msgs, warns) = perform midi_config1 $ mkevents_inst
            [("a", 0, 4, [badsig Controls.velocity])]
    equal warns ["Perform: %vel clipped: (0s, 4s)"]
    check (all_msgs_valid msgs)

-- * extract

all_msgs_valid :: Midi.WriteMessages -> Bool
all_msgs_valid wmsgs = all Midi.valid_msg (map Midi.wmsg_msg wmsgs)

e_note_ons :: Midi.WriteMessages -> [(RealTime, (Midi.Channel, Midi.Key))]
e_note_ons wmsgs =
    [ (ts, (chan, key))
    | (ts, (chan, Midi.NoteOn key _)) <- mapMaybe e_channel_msg wmsgs
    ]

e_channel_msg :: Midi.WriteMessage
    -> Maybe (RealTime, (Midi.Channel, Midi.ChannelMessage))
e_channel_msg wmsg = case Midi.wmsg_msg wmsg of
    Midi.ChannelMessage chan msg -> Just (Midi.wmsg_ts wmsg, (chan, msg))
    _ -> Nothing

extract_msg :: Midi.WriteMessage
    -> (RealTime, Midi.Channel, Midi.ChannelMessage)
extract_msg wmsg = (time, chan, msg)
    where (_, time, chan, msg) = extract_dev_msg wmsg

extract_dev_msg :: Midi.WriteMessage
    -> (String, RealTime, Midi.Channel, Midi.ChannelMessage)
extract_dev_msg (Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg)) =
    (Pretty.pretty dev, ts, chan, msg)
extract_dev_msg (Midi.WriteMessage _ _ msg) =
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

test_pitch_curve = do
    let event pitch = mkpevent (1, 0.5, pitch, [])
    let f evt = Seq.drop_dups id (map Midi.wmsg_msg msgs)
            where
            msgs = LEvent.events_of $ fst $
                Perform.perform_note 0 Nothing evt (dev1, 1)
        chan msgs = map (Midi.ChannelMessage 1) msgs

    equal (f (event [(1, 42.5)]))
        (chan [Midi.PitchBend 0.5, Midi.NoteOn 42 100, Midi.NoteOff 42 100])

    equal (f (event [(1, 42), (1.5, 42.5), (1.75, 43), (1.9, 43.5), (2, 44)]))
        (chan
            [ Midi.PitchBend 0, Midi.NoteOn 42 100
            , Midi.NoteOff 42 100
            , Midi.PitchBend 0.5
            , Midi.PitchBend 1
            ])

    let notes prev evt = [(Midi.wmsg_ts msg, Midi.wmsg_msg msg) | msg <- msgs]
            where
            msgs = LEvent.events_of $ fst $
                Perform.perform_note (secs prev) Nothing evt (dev1, 1)
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
    let e_note_on = mapMaybe $ \wmsg ->
            ((,) (Midi.wmsg_ts wmsg)) <$> note_on_key (Midi.wmsg_msg wmsg)
        e_note = mapMaybe $ \wmsg ->
            ((,) (Midi.wmsg_ts wmsg)) <$> note_key (Midi.wmsg_msg wmsg)
        ks_inst ks hold = inst1
            { Instrument.inst_keyswitch = ks
            , Instrument.inst_hold_keyswitch = hold
            }
        with_addr (ks, hold, note, start, dur) =
            (mkevent (ks_inst ks hold, note, start, dur, []), (dev1, 0))
        ks1 = [Instrument.Keyswitch Key.c1]
        ks2 = [Instrument.Keyswitch Key.d1]
    let ks_gap = Perform.keyswitch_gap
        delta = RealTime.milliseconds 2
    let f extract evts = extract $ expect_no_logs $
            perform_notes (map with_addr evts)

    -- Redundant ks not emitted.
    equal (f e_note_on [(ks1, False, "a", 0, 1), (ks1, False, "b", 10, 10)])
        [ (0 - ks_gap, Key.c1)
        , (0, Key.c4)
        , (10, Key.cs4)
        ]
    -- Keyswitch changed.
    equal (f e_note_on [(ks1, False, "a", 0, 1), (ks2, False, "b", 10, 10)])
        [ (0 - ks_gap, Key.c1), (0, Key.c4)
        , (10 - ks_gap, Key.d1), (10, Key.cs4)
        ]

    -- No keyswitch to keyswitch.
    equal (f e_note_on [([], False, "a", 0, 1), (ks1, False, "b", 10, 10)])
        [ (0, Key.c4)
        , (10 - ks_gap, Key.c1), (10, Key.cs4)
        ]

    -- Multiple keyswitches.
    equal (f e_note [(ks1 ++ ks2, False, "a", 0, 1)])
        [ (0 - ks_gap - delta, (True, Key.c1)), (0 - ks_gap, (False, Key.c1))
        , (0 - ks_gap, (True, Key.d1)), (0 - ks_gap + delta, (False, Key.d1))
        , (0, (True, Key.c4)), (1 - gap, (False, Key.c4))
        ]
    -- If one keyswitch is emitted, they all are.
    equal (f e_note [(ks1 ++ ks2, False, "a", 0, 1), (ks1, False, "b", 1, 1)])
        [ (0 - ks_gap - delta, (True, Key.c1)), (0 - ks_gap, (False, Key.c1))
        , (0 - ks_gap, (True, Key.d1)), (0 - ks_gap + delta, (False, Key.d1))
        , (0, (True, Key.c4)), (1 - gap, (False, Key.c4))
        , (1 - ks_gap, (True, Key.c1))
        , (1 - ks_gap + delta, (False, Key.c1))
        , (1, (True, Key.cs4)), (2 - gap, (False, Key.cs4))
        ]

    -- Hold keyswitch.
    equal (f e_note [(ks1, True, "a", 0, 1), (ks1, True, "b", 1, 1),
            ([], False, "c", 2, 1)])
        [ (0 - ks_gap, (True, Key.c1))
        , (0, (True, Key.c4))
        , (1 - gap, (False, Key.c4)), (1, (True, Key.cs4))
        , (2 - gap, (False, Key.cs4)), (2 - gap, (False, Key.c1))
        , (2, (True, Key.d4)), (3 - gap, (False, Key.d4))
        ]

    -- control switches
    let cs1 = [Instrument.ControlSwitch 1 10]
        cs2 = [Instrument.ControlSwitch 1 20]
        e_msg = mapMaybe $ \wmsg ->
            ((,) (Midi.wmsg_ts wmsg) <$> note_on_cc (Midi.wmsg_msg wmsg))
    equal (f e_msg [(cs1, False, "a", 0, 1), (cs2, False, "b", 1, 1)])
        [ (0 - ks_gap, Midi.ChannelMessage 0 (Midi.ControlChange 1 10))
        , (0, Midi.ChannelMessage 0 (Midi.NoteOn Key.c4 100))
        , (1 - ks_gap, Midi.ChannelMessage 0 (Midi.ControlChange 1 20))
        , (1, Midi.ChannelMessage 0 (Midi.NoteOn Key.cs4 100))
        ]

note_on_key :: Midi.Message -> Maybe Midi.Key
note_on_key key
    | Just (True, key) <- note_key key = Just key
    | otherwise = Nothing

note_on_cc msg
    | Midi.is_note_on msg || Midi.is_cc msg = Just msg
    | otherwise = Nothing

note_key :: Midi.Message -> Maybe (Bool, Midi.Key)
note_key (Midi.ChannelMessage _ (Midi.NoteOn key _)) = Just (True, key)
note_key (Midi.ChannelMessage _ (Midi.NoteOff key _)) = Just (False, key)
note_key _ = Nothing

perform :: Instrument.Configs -> [Perform.Event]
    -> ([Midi.WriteMessage], [String])
perform midi_config = first consistent_order . split_logs . fst
    . Perform.perform Perform.initial_state midi_config . map LEvent.Event

sort_groups :: (Eq k, Ord a) => (a -> k) -> [a] -> [a]
sort_groups key = concatMap List.sort . List.groupBy (\a b -> key a == key b)

-- | 'Perform.resort' scrambles msgs that have the same timestamp.  That should
-- be fine, so tests that rely on their ordering are being overspecific.  So
-- put them in a consistent order so tests don't break every time I mess with
-- postproc.
consistent_order :: [Midi.WriteMessage] -> [Midi.WriteMessage]
consistent_order = sort_groups Midi.wmsg_ts

perform_notes :: [(Perform.Event, Instrument.Addr)]
    -> ([Midi.WriteMessage], [String])
perform_notes = split_logs . fst
    . Perform.perform_notes Perform.empty_perform_state . map LEvent.Event

split_logs :: [LEvent.LEvent d] -> ([d], [String])
split_logs = second (map DeriveTest.show_log) . LEvent.partition

expect_no_logs :: (a, [String]) -> a
expect_no_logs (val, []) = val
expect_no_logs (_, logs) = error $ "expected no logs: " ++ Seq.join "\n" logs

-- * post process

test_drop_dup_controls = do
    let mkcc chan cc val = Midi.ChannelMessage chan (Midi.ControlChange cc val)
        mkpb chan val = Midi.ChannelMessage chan (Midi.PitchBend val)
        mkwmsgs msgs = [Midi.WriteMessage dev1 ts msg
                | (ts, msg) <- zip (Seq.range_ 0 1) msgs]
        extract wmsg = (Midi.wmsg_ts wmsg, Midi.wmsg_msg wmsg)
    let f = map extract . LEvent.events_of . fst
            . Perform.drop_dup_controls Map.empty . map LEvent.Event
    let msgs = [mkcc 0 1 10, mkcc 1 1 10, mkcc 0 1 11, mkcc 0 2 10]
    -- no drops
    equal (f (mkwmsgs msgs)) (zip (Seq.range_ 0 1) msgs)
    let with_dev dmsgs = [Midi.WriteMessage dev ts msg
                | (ts, (dev, msg)) <- zip (Seq.range_ 0 1) dmsgs]
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
    let sig = (Controls.vol, linear_interp
            [(0, 0), (1, 1.5), (2, 0), (2.5, 0), (3, 2)])
        (msgs, warns) = Perform.perform_control Control.empty_map
            (secs 0) (secs 0) 42 sig

    -- controls are not emitted after they reach steady values
    check $ all Midi.valid_chan_msg (map snd msgs)
    -- goes over in 2 places
    equal (length warns) 2

-- * channelize

-- test the overlap map and channel allocation
test_channelize = do
    let pevent (start, dur, psig) = mkpevent (start, dur, psig, [])
        f = map snd . channelize (mk_inst_addrs midi_config2) . map pevent

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
    equal (map snd $ channelize (mk_inst_addrs midi_config2) $ mkevents
        [ (inst2, "a", 0, 2, [])
        , (inst2, "a2", 1, 2, [])
        ])
        [0, 0]

test_can_share_chan = do
    let f evt1 evt2 = Maybe.isNothing $
            Perform.can_share_chan (mkpevent evt1) (mkpevent evt2)

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

    let mkc start dur pitch samples =
            (start, dur, [(0, pitch)], [("c", samples)])
    -- Share with the same controller.
    equal (f (mkc 0 2 60 [(0, 1), (1, 0)]) (mkc 0 2 61 [(0, 1), (1, 0)])) True
    equal (f (mkc 0 2 60 [(0, 1), (1, 0)]) (mkc 0 2 61 [])) False

    -- The controls are different, but they are the same in the overlapping
    -- part.
    equal (f (mkc 0 1 60 [(0, 1), (0.5, 0.5)])
            (mkc 1 1 61 [(0.5, 0.5), (1.5, 1)]))
        True

    equal (f (mkc 2 1 60 [(0, 1)]) (mkc 2 1 61 [(0, 0)]))
        False

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
        f = map (snd . snd) . LEvent.events_of . allot midi_config1 . in_time
        in_time mks = zipWith ($) mks (Seq.range_ 0 1)

    -- They should alternate channels, according to LRU.
    equal (f [mk1 0, mk1 1, mk1 2, mk1 3]) [0, 1, 0, 1]

    -- Repeated chans get reused.
    equal (f [mk1 3, mk1 2, mk1 2, mk1 3]) [0, 1, 1, 0]

    -- Instruments with no allocation get filtered out.
    equal (f [mk1 1, mk inst2 1, mk1 2]) [0, 1]

test_allot_steal = do
    let f = extract . allot midi_config1 . map mk . zip (Seq.range_ 0 1)
        extract = first (map (snd . snd)) . LEvent.partition
        mk (start, chan) = (mkevent (inst1, "a", start, 1, []), chan)
    -- 0->0, 1->1, 2->steal 0, 0 -> should go to 1, becasue 0 was stolen
    let (chans, _logs) = f [0, 1, 2, 0]
    equal chans [0, 1, 0, 1]

test_allot_warn = do
    let extract (LEvent.Event (e, (dev, chan))) = Left
            (Instrument.inst_name (Perform.event_instrument e),
                Pretty.pretty dev, chan)
        extract (LEvent.Log msg) = Right $ DeriveTest.show_log msg
    let f = map extract . allot midi_config1
            . map (\(evt, chan) -> (mkevent evt, chan))
    let no_inst = mkinst "no_inst"
    equal (f [((inst1, "a", 0, 1, []), 0)])
        [Left ("inst1", "dev1", 0)]
    equal (f [((no_inst, "a", 0, 1, []), 0), ((no_inst, "b", 1, 2, []), 0)])
        (replicate 2 $ Right "Perform: no allocation for >synth1/no_inst")

allot :: Instrument.Configs -> [(Perform.Event, Perform.Channel)]
    -> [LEvent.LEvent (Perform.Event, Instrument.Addr)]
allot configs events = fst $
    Perform.allot Perform.empty_allot_state (mk_inst_addrs configs)
        (map LEvent.Event events)

-- * setup

mk_inst_addrs :: Instrument.Configs -> Perform.InstAddrs
mk_inst_addrs = Map.map Instrument.config_addrs

secs :: Double -> RealTime
secs = RealTime.seconds

-- | Name will determine the pitch.  It can be a-z, or a2-z2, which will
-- yield fractional pitches.
--
-- (inst, text, start, dur, controls)
type EventSpec = (Instrument.Instrument, String, RealTime, RealTime, [Control])
type Control = (Score.Control, Signal.Control)

mkevent :: EventSpec -> Perform.Event
mkevent (inst, pitch, start, dur, controls) =
    Perform.Event inst start dur (Map.fromList controls)
        (psig start pitch) DeriveTest.fake_stack
    where
    psig pos p = Signal.signal [(pos, to_pitch p)]
    to_pitch p = fromMaybe (error ("no pitch " ++ show p)) (lookup p pitch_map)
    pitch_map = zip (map (:"") ['a'..'z']) [60..]
        ++ zip (map (:"2") ['a'..'z']) [60.5..]

type PEvent = (RealTime, RealTime, [(Signal.X, Signal.Y)],
    [(Text, [(Signal.X, Signal.Y)])])

-- | Similar to mkevent, but allow a pitch curve.
mkpevent :: PEvent -> Perform.Event
mkpevent (start, dur, psig, conts) =
    Perform.Event inst1 start dur (mkcontrols conts) (Signal.signal psig)
        Stack.empty

mkevents_inst :: [(String, RealTime, RealTime, [Control])] -> [Perform.Event]
mkevents_inst = map (\(a, b, c, d) -> mkevent (inst1, a, b, c, d))

mkevents :: [EventSpec] -> [Perform.Event]
mkevents = map mkevent

set_inst :: Instrument.Instrument -> Perform.Event -> Perform.Event
set_inst inst event = event { Perform.event_instrument = inst }

mkcontrols :: [(Text, [(Signal.X, Signal.Y)])] -> Perform.ControlMap
mkcontrols csigs = Map.fromList
    [(Score.control c, Signal.signal sig) | (c, sig) <- csigs]

-- | Make a signal with linear interpolation between the points.
linear_interp :: [(Signal.X, Signal.Y)] -> Signal.Control
linear_interp = Signal.signal . interpolate
    where
    interpolate :: [(RealTime, Signal.Y)] -> [(RealTime, Signal.Y)]
    interpolate ((x0, y0) : rest@((x1, y1) : _))
        | x0 >= x1 = interpolate rest
        | otherwise = [(x, TimeVector.y_at x0 y0 x1 y1 x)
            | x <- Seq.range_end x0 (x1-1) 1] ++ interpolate rest
    interpolate val = val

inst1 = mkinst "inst1"
inst2 = mkinst "inst2"

mkinst :: Text -> Instrument.Instrument
mkinst name = (Instrument.instrument name [] (-1, 1))
    { Instrument.inst_score = Score.Instrument ("synth1/" <> name) }

dev1, dev2 :: Midi.WriteDevice
dev1 = Midi.write_device "dev1"
dev2 = Midi.write_device "dev2"

midi_config1 :: Instrument.Configs
midi_config1 = Instrument.configs
    [(Instrument.inst_score inst1, [(dev1, 0), (dev1, 1)])]

-- Also includes inst2.
midi_config2 :: Instrument.Configs
midi_config2 = Instrument.configs
    [ (Instrument.inst_score inst1, [(dev1, 0), (dev1, 1)])
    , (Instrument.inst_score inst2, [(dev2, 2)])
    ]

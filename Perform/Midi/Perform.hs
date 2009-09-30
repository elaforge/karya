{-# LANGUAGE PatternGuards #-}
{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
    midi events.

    Keyswitch implementation:

    Keyswitches are implemented as separate instruments that are allocated
    the same channels.  That way the normal channel sharing stuff in
    'can_share_chan' will try to give each instrument on its own channel to
    minimize keyswitches.  Every note with a keyswitch will emit the keyswitch
    slightly before the note, relying on postprocessing to strip out the
    redundant ones.

    Score.Event level Attributes are mapped to keyswitches.  This happens at
    Convert time so that the Perform.Events can get their slightly different
    Instruments.  It's up to the conversion code to convert an arbitrary set
    of attributes into a keyswitch.
-}
module Perform.Midi.Perform where
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


-- TODO: use default_decay, and use decay for overlaps and max lookahead

-- * constants

-- | This winds up being 100, which is loud but not too loud and
-- distinctive-looking.
default_velocity :: Signal.Val
default_velocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.
keyswitch_interval :: Timestamp.Timestamp
keyswitch_interval = 4

-- | Most synths don't respond to pitch bend instantly, but smooth it out, so
-- if you set pitch bend immediately before playing the note you will get
-- a little sproing.  Try to put pitch bends before their notes by this amount,
-- unless there's another note there.
pitch_bend_lead_time :: TrackPos
pitch_bend_lead_time = Timestamp.to_track_pos (Timestamp.seconds 0.01)

-- * perform

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: MidiDb.LookupMidiInstrument
    -> Instrument.Config -> [Event] -> ([Midi.WriteMessage], [Warning.Warning])
perform lookup_inst config events = (post_process msgs, warns)
    where
    inst_addrs = config_to_inst_addrs config lookup_inst
    (msgs, warns) = (perform_notes . allot inst_addrs
        . channelize inst_addrs) events

config_to_inst_addrs :: Instrument.Config -> MidiDb.LookupMidiInstrument
    -> InstAddrs
config_to_inst_addrs config lookup_inst = Map.fromList
    [(Instrument.inst_name inst, addrs) | (Just inst, addrs) <- inst_addrs]
    where
    inst_addrs = [(lookup_inst Score.no_attrs inst, addrs)
        | (inst, addrs) <- Map.assocs (Instrument.config_alloc config)]

-- | Map each instrument to its allocated Addrs.
type InstAddrs = Map.Map Instrument.InstrumentName [Instrument.Addr]

-- | As in 'Cmd.Cmd.WriteDeviceState', map an Addr to the Instrument active
-- at that address.
type AddrInst = Map.Map Instrument.Addr Instrument.Instrument


-- * perform notes

-- | Given an ordered list of note events, produce the apprapriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: [(Event, Instrument.Addr)]
    -> ([Midi.WriteMessage], [Warning.Warning])
    -- Pass an empty AddrInst because I can't make any assumptions about the
    -- state of the synthesizer.  The one from the wdev state might be out of
    -- date by the time this performance is played.
perform_notes events = _perform_notes Map.empty [] (TrackPos 0) events

_perform_notes :: AddrInst -> [Midi.WriteMessage]
    -> TrackPos -> [(Event, Instrument.Addr)]
    -> ([Midi.WriteMessage], [Warning.Warning])
_perform_notes _ overlapping _ [] = (overlapping, [])
_perform_notes addr_inst overlapping prev_note_off ((event, addr):events) =
    (play ++ rest_msgs, chan_state_warns ++ warns ++ rest_warns)
    where
    (rest_msgs, rest_warns) = _perform_notes addr_inst2 not_yet note_off events
    -- This find could demand lots or all of events if the
    -- (instrument, chan) doesn't play for a long time, or ever.  It only
    -- happens once at gaps though, but if it's a problem I can abort after
    -- n seconds, which would possibly save generating controllers there, at
    -- the cost of assuming decays are < n sec.
    next_note_on = case List.find ((==addr) . snd) events of
        Nothing -> event_end event -- TODO plus decay_time?
        Just (evt, _chan) -> event_start evt
    (msgs, warns, note_off) = perform_note prev_note_off next_note_on event addr
    first_ts = case msgs of
        [] -> Timestamp.Timestamp 0 -- perform_note decided to play nothing?
        (msg:_) -> Midi.wmsg_ts msg
    -- These will be in the \"past\", so messages may get slightly out of order
    -- if they are before 'overlapping', which means that a pchange with a long
    -- lead time may lose its lead time.  TODO fix this if it becomes a problem
    (chan_state_msgs, chan_state_warns, addr_inst2) =
        adjust_chan_state addr_inst addr event
    (play, not_yet) = List.partition ((<= first_ts) . Midi.wmsg_ts)
        (merge_messages [overlapping, chan_state_msgs ++ msgs])

-- | Figure out of any msgs need to be emitted to convert the channel state to
-- the given event on the given addr.
--
-- If there's no chan state always emit msgs, since in general there's no way
-- to know what state the synth is in.  If I do know (e.g. playback will
-- pass the current addr_inst) I can filter out expensive messages like
-- program change.
-- TODO implement playback with addr_inst when I implement pchange
--
-- Another strategy would be to always emit msgs and rely on playback filter,
-- but that would triple the number of msgs, which seems excessive.
adjust_chan_state :: AddrInst -> Instrument.Addr -> Event
    -> ([Midi.WriteMessage], [Warning.Warning], AddrInst)
adjust_chan_state addr_inst addr event =
    case chan_state_msgs addr ts old_inst inst of
        Left err -> ([], [event_warning event err], new_addr_inst)
        Right msgs -> (msgs, [], new_addr_inst)
    where
    new_addr_inst = Map.insert addr inst addr_inst
    inst = event_instrument event
    old_inst = Map.lookup addr addr_inst
    ts = Timestamp.from_track_pos (event_start event)

-- | TODO support program change, I'll have to get ahold of patch_initialize.
chan_state_msgs :: Instrument.Addr -> Timestamp.Timestamp
    -> Maybe Instrument.Instrument -> Instrument.Instrument
    -> Either String [Midi.WriteMessage]
chan_state_msgs addr@(wdev, chan) ts maybe_old_inst new_inst
    | not same_synth = Left $ "two synths on " ++ show addr ++ ": " ++ inst_desc
    | not same_inst = Left $ "program change not supported yet on "
        ++ show addr ++ ": " ++ inst_desc
    | not same_ks = Right ks_msgs
    | otherwise = Right []
    where
    inst_desc = show (fmap extract maybe_old_inst, extract new_inst)
    extract inst = (Instrument.inst_synth inst, Instrument.inst_name inst)

    same_synth = case maybe_old_inst of
        Nothing -> True
        Just o -> Instrument.inst_synth o == Instrument.inst_synth new_inst
    same_inst = same_synth && case maybe_old_inst of
        Nothing -> True -- when pchange is supported I can assume false
        Just o -> Instrument.inst_name o == Instrument.inst_name new_inst
    same_ks = same_inst && case maybe_old_inst of
        Nothing -> False
        Just o -> Maybe.isNothing (Instrument.inst_keyswitch new_inst)
            || Instrument.inst_keyswitch o == Instrument.inst_keyswitch new_inst

    ks_msgs = maybe [] (mk_ks . Instrument.ks_key)
        (Instrument.inst_keyswitch new_inst)
    -- The velocity is arbitrary, but this is loud enough to hear if you got
    -- the keyswitches wrong.
    mk_ks nn =
        [mkmsg start (Midi.NoteOn nn 64), mkmsg (start+2) (Midi.NoteOff nn 64)]
    mkmsg ts msg = Midi.WriteMessage wdev ts (Midi.ChannelMessage chan msg)
    start = max 0 (ts - keyswitch_interval)

-- * post process

-- | Some context free post-processing on the midi stream.
post_process :: [Midi.WriteMessage] -> [Midi.WriteMessage]
post_process = reorder_control_messages . drop_duplicates

drop_duplicates :: [Midi.WriteMessage] -> [Midi.WriteMessage]
drop_duplicates = drop_dup_controllers Map.empty

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState =
    (Maybe Midi.PitchBendValue, Map.Map Midi.Controller Midi.ControlValue)
type RunningState = Map.Map Instrument.Addr AddrState

drop_dup_controllers :: RunningState -> [Midi.WriteMessage]
    -> [Midi.WriteMessage]
drop_dup_controllers _ [] = []
drop_dup_controllers running (wmsg:wmsgs) = case wmsg of
    Midi.WriteMessage dev _ (Midi.ChannelMessage chan cmsg) ->
        let addr = (dev, chan)
            state = Map.lookup addr running
            (keep, state2) = analyze_msg state cmsg
            running2 = maybe running (\s -> Map.insert addr s running) state2
            rest = drop_dup_controllers running2 wmsgs
        in if keep then wmsg : rest else rest
    _ -> drop_dup_controllers running wmsgs

analyze_msg :: Maybe AddrState -> Midi.ChannelMessage -> (Bool, Maybe AddrState)
analyze_msg Nothing msg = case msg of
    Midi.PitchBend v -> (True, Just (Just v, Map.empty))
    Midi.ControlChange c v -> (True, Just (Nothing, Map.singleton c v))
    _ -> (True, Nothing)
analyze_msg (Just (pb_val, cmap)) msg = case msg of
    Midi.PitchBend v
        | Just v == pb_val -> (False, Nothing)
        | otherwise -> (True, Just (Just v, cmap))
    Midi.ControlChange c v
        | Just v == Map.lookup c cmap -> (False, Nothing)
        | otherwise -> (True, Just (pb_val, Map.insert c v cmap))
    _ -> (True, Nothing)

-- | If a control message and a note message happen at the same time, the
-- control should go first, just to make sure the synth doesn't make a popping
-- noise.
reorder_control_messages :: [Midi.WriteMessage] -> [Midi.WriteMessage]
reorder_control_messages = sort2 (compare `on` msg_key)
    where
    msg_key msg = (Midi.wmsg_ts msg, key msg)
    key msg = case Midi.wmsg_msg msg of
        Midi.ChannelMessage _ (Midi.ControlChange _ _) -> 0
        _ -> 1

-- | A single bubblesort pass.  It's lazy and sorts almost-sorted stuff well.
sort2 cmp (a:b:zs) = case cmp a b of
    GT -> b : sort2 cmp (a:zs)
    _ -> a : sort2 cmp (b:zs)
sort2 cmp xs = List.sortBy cmp xs


-- * perform note

perform_note :: TrackPos -> TrackPos -> Event -> Instrument.Addr
    -> ([Midi.WriteMessage], [Warning.Warning], TrackPos)
perform_note prev_note_off next_note_on event (dev, chan)
    | midi_nn == 0 =
        ([], [event_warning event "no pitch signal"], prev_note_off)
    | otherwise = (merge_messages [controller_msgs, note_msgs], warns, note_off)
    where
    note_msgs =
        [ chan_msg pb_time (Midi.PitchBend pb)
        , chan_msg note_on (Midi.NoteOn midi_nn on_vel)
        , chan_msg note_off (Midi.NoteOff midi_nn off_vel)
        ]
    chan_msg pos msg = Midi.WriteMessage dev (Timestamp.from_track_pos pos)
        (Midi.ChannelMessage chan msg)
    pb_time = min note_on $ max prev_note_off (note_on - pitch_bend_lead_time)
    note_on = event_start event
    note_off = event_end event
    (on_vel, off_vel, vel_clip_warns) = note_velocity event note_on note_off

    pb_range = Instrument.inst_pitch_bend_range (event_instrument event)
    (midi_nn, pb) = Maybe.fromMaybe (0, 0)
        (event_pitch_at pb_range event note_on)

    control_sigs = Map.assocs (event_controls event)
    cmap = Instrument.inst_controller_map (event_instrument event)
    (controller_pos_msgs, clip_warns) = unzip $
        map (perform_controller cmap note_on next_note_on) control_sigs
    pitch_pos_msgs = maybe []
        (perform_pitch pb_range midi_nn note_on next_note_on)
        (Map.lookup Controller.c_pitch (event_controls event))
    controller_msgs = merge_messages $
        map (map mkmsg) (pitch_pos_msgs : controller_pos_msgs)
    mkmsg (pos, msg) = Midi.WriteMessage dev (Timestamp.from_track_pos pos)
        (Midi.ChannelMessage chan msg)

    warns = concatMap (make_clip_warnings event)
        (zip (map fst control_sigs) clip_warns ++ vel_clip_warns)

-- | Get pitch at the given point of the signal.
--
-- The pitch bend always tunes upwards from the tempered note.  It would be
-- slicker to use a negative offset if the note is eventually going above
-- unity, but that's too much work.
event_pitch_at :: Controller.PbRange -> Event -> TrackPos
    -> Maybe (Midi.Key, Midi.PitchBendValue)
event_pitch_at pb_range event pos = fmap (Controller.pitch_to_midi pb_range)
    (control_at event Controller.c_pitch pos)

note_velocity :: Event -> TrackPos -> TrackPos
    -> (Midi.Velocity, Midi.Velocity, [(Controller.Controller, [ClipWarning])])
note_velocity event note_on note_off =
    (clipped_vel on_sig, clipped_vel off_sig, clip_warns)
    where
    on_sig = Maybe.fromMaybe default_velocity $
        control_at event Controller.c_velocity note_on
    off_sig = Maybe.fromMaybe default_velocity $
        control_at event Controller.c_velocity note_off
    clipped_vel val = Controller.val_to_cc (fst (clip_val 0 1 val))
    clip_warns =
        if snd (clip_val 0 1 on_sig) || snd (clip_val 0 1 off_sig)
        then [(Controller.c_velocity, [(note_on, note_off)])]
        else []

make_clip_warnings :: Event -> (Controller.Controller, [(TrackPos, TrackPos)])
    -> [Warning.Warning]
make_clip_warnings event (controller, clip_warns) =
    [ Warning.warning (show controller ++ " clipped")
        (event_stack event) (Just (start, end))
    | (start, end) <- clip_warns ]

control_at :: Event -> Controller.Controller -> TrackPos
    -> Maybe Signal.Val
control_at event controller pos = do
    sig <- Map.lookup controller (event_controls event)
    return (Signal.at pos sig)

perform_pitch :: Controller.PbRange -> Midi.Key -> TrackPos -> TrackPos
    -> Signal.Signal -> [(TrackPos, Midi.ChannelMessage)]
perform_pitch pb_range nn start end sig =
    [ (pos, Midi.PitchBend (Controller.pb_from_nn pb_range nn val))
    | (pos, val) <- pos_vals ]
    where
    pos_vals = takeWhile ((<end) . fst) $
        Signal.sample Signal.default_srate start sig

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed controller range, 0--1.
perform_controller :: Controller.ControllerMap -> TrackPos -> TrackPos
    -> (Controller.Controller, Signal.Signal)
    -> ([(TrackPos, Midi.ChannelMessage)], [ClipWarning])
perform_controller cmap start end (controller, sig) =
    case Controller.controller_constructor cmap controller of
        Nothing -> ([], []) -- TODO warn about a controller not in the cmap
        Just cons -> ([(pos, cons val) | (pos, val) <- pos_cvals], clip_warns)
    where
        -- TODO get srate from a controller
    pos_vals = takeWhile ((<end) . fst) $
        Signal.sample Signal.default_srate start sig
    (low, high) = Controller.controller_range
    -- arrows?
    (cvals, clips) = unzip (map (clip_val low high) (map snd pos_vals))
    clip_warns = extract_clip_warns (zip pos_vals clips)
    pos_cvals = zip (map fst pos_vals) cvals

type ClipWarning = (TrackPos, TrackPos)

extract_clip_warns :: [((TrackPos, Signal.Val), Bool)] -> [ClipWarning]
extract_clip_warns pos_val_clips = [(head pos, last pos) | pos <- clip_pos]
    where
    groups = List.groupBy ((==) `on` snd) pos_val_clips
    clip_pos = [[pos | ((pos, _val), _clipped) <- g ]
        | g <- groups, snd (head g)]

clip_val low high val
    | val < low = (low, True)
    | val > high = (high, True)
    | otherwise = (val, False)

-- | Merge the sorted midi messages into a single sorted list.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = foldr (Seq.merge_by (compare `on` Midi.wmsg_ts)) []

-- * channelize

-- | Assign channels.  Events will be merged into the same channel where they
-- can be.
channelize :: InstAddrs -> [Event] -> [(Event, Channel)]
channelize inst_addrs events = overlap_map (channelize_event inst_addrs) events

channelize_event :: InstAddrs -> [(Event, Channel)] -> Event -> Channel
channelize_event i_addrs overlapping event =
    case Map.lookup (Instrument.inst_name (event_instrument event)) i_addrs of
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        Just (_:_:_) -> chan
        _ -> 0
    where
    -- Pick the first overlapping channel or make up a channel one higher than
    -- the maximum channel in use.
    chan = maybe (maximum (-1 : map snd overlapping) + 1) id
        (shareable_chan overlapping event)

-- | Find a channel from the list of overlapping (Event, Channel) all of whose
-- events can share with the given event.
shareable_chan :: [(Event, Channel)] -> Event -> Maybe Channel
shareable_chan overlapping event = fmap fst (List.find all_share by_chan)
    where
    by_chan = Seq.keyed_group_with snd overlapping
    all_share (_chan, evt_chans) =
        all (can_share_chan event) (map fst evt_chans)

-- | Can the two events coexist in the same channel without interfering?
can_share_chan :: Event -> Event -> Bool
can_share_chan event1 event2 =
    event_instrument event1 == event_instrument event2
    && pitches_share start end (pitch_control event1) (pitch_control event2)
    && controls_equal start end (relevant event1) (relevant event2)
    where
    start = event_start event1
    end = event_end event2
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controllers require their own channel.
    relevant event = filter (Controller.is_channel_controller . fst)
        (Map.assocs (event_controls event))
    pitch_control event = Map.lookup Controller.c_pitch (event_controls event)

-- | Are the controllers equal in the given range?
controls_equal :: TrackPos -> TrackPos
    -> [(Controller.Controller, Signal.Signal)]
    -> [(Controller.Controller, Signal.Signal)] -> Bool
controls_equal start end c0 c1 = all (uncurry eq) (zip c0 c1)
    where
    -- Since the controllers are compared in sorted order, if the events don't
    -- have the same controllers, they won't be equal.
    eq (c0, sig0) (c1, sig1) = c0 == c1
        && Signal.equal start end sig0 sig1

pitches_share start end (Just sig0) (Just sig1) =
    Signal.pitches_share start end sig0 sig1
-- 0 pitch events should get filtered, but in case they aren't, they can go
-- with anyone.
pitches_share _ _ _ _ = True

-- * allot channels

-- | 'channelize' will assign channels based on whether the notes can coexist
-- without interfering with each other.  'allot' reduces those channels down
-- to the real midi channels assigned to the instrument, stealing if necessary.
--
-- Events with instruments that have no address allocation in the config
-- will be dropped.
allot :: InstAddrs -> [(Event, Channel)] -> [(Event, Instrument.Addr)]
allot inst_addrs events = Maybe.catMaybes $
    snd $ List.mapAccumL allot_event (initial_allot_state inst_addrs) events

data AllotState = AllotState {
    -- | Allocated addresses, and when they were last used.
    ast_available :: Map.Map Instrument.Addr TrackPos
    -- | Map arbitrary input channels to an instrument address in the allocated
    -- range.
    , ast_map :: Map.Map (Instrument.Instrument, Channel) Instrument.Addr
    -- | Addresses allocated to each instrument.
    , ast_inst_addrs :: InstAddrs
    } deriving (Show)
initial_allot_state inst_addrs = AllotState Map.empty Map.empty inst_addrs

allot_event :: AllotState -> (Event, Channel)
    -> (AllotState, Maybe (Event, Instrument.Addr))
allot_event state (event, ichan) =
    case Map.lookup (inst, ichan) (ast_map state) of
        Just addr -> (update_avail addr state, Just (event, addr))
        Nothing -> case steal_addr inst state of
            -- nothing allocated to this instrument
            -- TODO if I'm going to log about insts without allocation this
            -- is the spot
            Nothing -> (state, Nothing)
            Just addr ->
                (update_avail addr (update_map addr state), Just (event, addr))
    where
    inst = event_instrument event
    update_avail addr state = state { ast_available =
        Map.insert addr (event_end event) (ast_available state) }
    update_map addr state =
        state { ast_map = Map.insert (inst, ichan) addr (ast_map state) }

-- | Steal the least recently used address for the given instrument.
steal_addr :: Instrument.Instrument -> AllotState -> Maybe Instrument.Addr
steal_addr inst state =
    case Map.lookup (Instrument.inst_name inst) (ast_inst_addrs state) of
        Just addrs -> let avail = zip addrs (map mlookup addrs)
            in if null avail then Nothing -- no addrs assigned
                else let (addr, _) = List.minimumBy (compare `on` snd) avail
                in Just addr
        _ -> Nothing
    where
    mlookup addr = Map.findWithDefault (TrackPos 0) addr (ast_available state)

-- * data

data Event = Event {
    event_instrument :: Instrument.Instrument
    , event_start :: TrackPos
    , event_duration :: TrackPos
    , event_controls :: ControllerMap
    -- original (TrackId, TrackPos) for errors
    , event_stack :: Warning.Stack
    } deriving (Show)

event_end event = event_start event + event_duration event

-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer
type ControllerMap = Map.Map Controller.Controller Signal.Signal


-- * util

-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.
overlap_map :: ([(Event, a)] -> Event -> a) -> [Event] -> [(Event, a)]
overlap_map = go []
    where
    go _ _ [] = []
    go prev f (e:events) = (e, val) : go ((e, val) : overlapping) f events
        where
        start = event_start e
        -- TODO add decay
        overlapping = takeWhile ((> start) . event_end . fst) prev
        val = f overlapping e

event_warning :: Event -> String -> Warning.Warning
event_warning event msg = Warning.warning msg (event_stack event)
    (Just (event_start event, event_end event))

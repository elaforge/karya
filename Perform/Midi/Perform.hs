{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
    midi events.

    TODO Implement key switches.  How about an integral signal, and then
    a per-instrument map from signal to keyswitch keys.  The signal is rendered
    as key switch keys when it changes, and the signal makes the channel
    unshareable.
-}
module Perform.Midi.Perform where
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Util.Data

import Ui.Types

import qualified Midi.Midi as Midi

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.Db


-- TODO: use default_decay, and use decay for overlaps and max lookahead


-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: Instrument.Db.LookupMidiInstrument -> Instrument.Config -> [Event]
    -> ([Midi.WriteMessage], [Warning.Warning])
perform lookup_inst config =
    perform_notes . allot inst_addrs . channelize inst_addrs
    where inst_addrs = config_to_inst_addrs config lookup_inst

config_to_inst_addrs :: Instrument.Config -> Instrument.Db.LookupMidiInstrument
    -> InstAddrs
config_to_inst_addrs config lookup_inst =
    Util.Data.multimap [(inst, addr) | (addr, Just inst)
        <- Map.assocs (Map.map lookup_inst (Instrument.config_alloc config))]

-- | Map each instrument to its allocated Addrs.
-- TODO It would be slightly more efficient to just use the instrument name.
type InstAddrs = Map.Map Instrument.Instrument [Instrument.Addr]


-- * perform notes

-- | Given an ordered list of note events, produce the apprapriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: [(Event, Instrument.Addr)]
    -> ([Midi.WriteMessage], [Warning.Warning])
perform_notes events = (post_process msgs, warns)
    where (msgs, warns) = _perform_notes [] events

_perform_notes overlapping [] = (overlapping, [])
_perform_notes overlapping ((event, addr):events) =
    (play ++ rest_msgs, warns ++ rest_warns)
    where
    (rest_msgs, rest_warns) = _perform_notes not_yet events
    -- This find could demand lots or all of events if the
    -- (instrument, chan) doesn't play for a long time, or ever.  It only
    -- happens once at gaps though, but if it's a problem I can abort after
    -- n seconds, which would possibly save generating controllers there, at
    -- the cost of assuming decays are < n sec.
    next_note_on = case List.find ((==addr) . snd) events of
        Nothing -> event_end event -- TODO plus decay_time?
        Just (evt, _chan) -> event_start evt
    (msgs, warns) = perform_note next_note_on event addr
    first_ts = case msgs of
        [] -> Timestamp.Timestamp 0 -- perform_note decided to play nothing?
        (msg:_) -> Midi.wmsg_ts msg
    (play, not_yet) = List.partition ((<= first_ts) . Midi.wmsg_ts)
        (merge_messages [overlapping, msgs])

-- | Some context free post-processing on the midi stream.
post_process :: [Midi.WriteMessage] -> [Midi.WriteMessage]
post_process = reorder_control_messages . drop_duplicates

drop_duplicates = Seq.drop_dups (==)

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


perform_note :: TrackPos -> Event -> Instrument.Addr
    -> ([Midi.WriteMessage], [Warning.Warning])
perform_note next_note_on event (dev, chan)
    | midi_nn == 0 = ([], [Warning.warning "no pitch signal"
        (event_stack event) (Just (note_on, note_off))])
    | otherwise = (merge_messages [controller_msgs, note_msgs], warns)
    where
    note_msgs = [chan_msg note_on (Midi.NoteOn midi_nn on_vel),
        chan_msg note_off (Midi.NoteOff midi_nn off_vel)]
    chan_msg pos msg = Midi.WriteMessage dev (Timestamp.from_track_pos pos)
        (Midi.ChannelMessage chan msg)
    note_on = event_start event
    note_off = event_end event
    (on_vel, off_vel, vel_clip_warns) = note_velocity event note_on note_off

    -- pb_range = Instrument.inst_pitch_bend_range (event_instrument event)
    midi_nn = Maybe.fromMaybe 0 (event_pitch_at event note_on)

    control_sigs = Map.assocs (event_controls event)
    cmap = Instrument.inst_controller_map (event_instrument event)
    (controller_pos_msgs, clip_warns) = unzip $
        map (perform_controller cmap note_on next_note_on chan) control_sigs
    controller_msgs = merge_messages (map (map mkmsg) controller_pos_msgs)
    mkmsg (pos, msg) = Midi.WriteMessage dev (Timestamp.from_track_pos pos) msg

    warns = concatMap (make_clip_warnings event)
        (zip (map fst control_sigs) clip_warns ++ vel_clip_warns)

-- | Get pitch at the given point of the signal.
--
-- The pitch bend always tunes upwards from the tempered note.  It would be
-- slicker to use a negative offset if the note is eventually going above
-- unity, but that's too much work.
event_pitch_at :: Event -> TrackPos -> Maybe Midi.Key
event_pitch_at event pos =
    fmap Controller.pitch_to_midi_key (control_at event Controller.c_pitch pos)

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

-- | This winds up being 100, which is a good default and distinctive-looking.
default_velocity :: Signal.Val
default_velocity = 0.79

control_at :: Event -> Controller.Controller -> TrackPos
    -> Maybe Signal.Val
control_at event controller pos = do
    sig <- Map.lookup controller (event_controls event)
    return (Signal.at pos sig)

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed controller range, 0--1.
perform_controller :: Controller.ControllerMap
    -> TrackPos -> TrackPos -> Midi.Channel
    -> (Controller.Controller, Signal.Signal)
    -> ([(TrackPos, Midi.Message)], [ClipWarning])
perform_controller cmap start end chan (controller, sig) =
    case Controller.controller_constructor cmap controller of
        Nothing -> ([], []) -- TODO warn about a controller not in the cmap
        Just ctor ->
            let msgs = [(pos, Midi.ChannelMessage chan (ctor val))
                    | (pos, val) <- pos_cvals]
            in (msgs, clip_warns)
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
channelize_event inst_addrs overlapping event =
    case Map.lookup (event_instrument event) inst_addrs of
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or filter the event if there
        -- are none.
        Just (_:_:_) -> chan
        _ -> 0
    where
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
-- will be silently dropped.  A higher level should have warned about those.
-- This is because deallocating its Addrs is an easy way to mute an instrument,
-- so it's not necessarily an error to have no allocation.
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
steal_addr inst state = case Map.lookup inst (ast_inst_addrs state) of
    Just addrs -> let avail = zip addrs (map mlookup addrs)
        in if null avail then Nothing -- no addrs assigned to this instrument
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
    , event_controls :: Map.Map Controller.Controller Signal.Signal
    -- original (TrackId, TrackPos) for errors
    , event_stack :: [Warning.StackPos]
    } deriving (Show)

event_end event = event_start event + event_duration event

-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer


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

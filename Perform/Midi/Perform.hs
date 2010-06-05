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

import Util.Control
import qualified Util.Map as Map
import qualified Util.Seq as Seq

import Ui

import qualified Midi.Midi as Midi

import qualified Derive.Score as Score

import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp
import qualified Perform.Warning as Warning

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Instrument.MidiDb as MidiDb


-- * constants

-- | This winds up being 100, which is loud but not too loud and
-- distinctive-looking.
default_velocity :: Signal.Y
default_velocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.
keyswitch_interval :: Timestamp.Timestamp
keyswitch_interval = 4

-- | Most synths don't respond to pitch bend instantly, but smooth it out, so
-- if you set pitch bend immediately before playing the note you will get
-- a little sproing.  Put pitch bends before their notes by this amount.
control_lead_time :: RealTime
control_lead_time = Timestamp.to_real_time (Timestamp.seconds 0.1)

-- | When a track's NoteOff lines up with the next NoneOn, make them overlap by
-- this amount.  Normally this won't be audible, but if the instrument is set
-- for fingered portamento then this should trigger it.
legato_overlap_time :: RealTime
legato_overlap_time = Timestamp.to_real_time (Timestamp.seconds 0.01)

-- Neither of those are exactly representable, but they get rounded to
-- milliseconds anyway.

-- * perform

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: MidiDb.LookupMidiInstrument
    -> Instrument.Config -> [Event] -> ([Midi.WriteMessage], [Warning.Warning])
perform lookup_inst config events = (post_process msgs, warns)
    where
    inst_addrs = config_to_inst_addrs config lookup_inst
    (event_channels, allot_warns) = allot inst_addrs $
        channelize inst_addrs events
    (msgs, perform_warns) = perform_notes event_channels
    warns = allot_warns ++ perform_warns

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
perform_notes events = (merge_sorted_messages msgs, concat warns)
    where
    (msgs, warns) = unzip $
        map_notes _perform_note (Map.empty, Map.empty) events
    -- This is like mapAccumL, but looks for the next event.
    map_notes _ _ [] = []
    map_notes f state (x@(_, addr):xs) = y : map_notes f state2 xs
        where
        next = fst <$> List.find ((==addr) . snd) xs
        (state2, y) = f state next x

-- | Map from an address to the last time a note was playing on that address.
-- This includes the last note's decay time, so the channel should be reusable
-- after this time.
type NoteOffMap = Map.Map Instrument.Addr RealTime

_perform_note :: (AddrInst, NoteOffMap) -> Maybe Event
    -> (Event, Instrument.Addr)
    -> ((AddrInst, NoteOffMap), ([Midi.WriteMessage], [Warning.Warning]))
_perform_note (addr_inst, note_off_map) next_event (event, addr) =
    ((addr_inst2, Map.insert addr note_off note_off_map), (msgs, warns))
    where
    (note_msgs, note_warns, note_off) = perform_note
        (Map.findWithDefault 0 addr note_off_map) next_event event addr
    (chan_state_msgs, chan_state_warns, addr_inst2) =
        adjust_chan_state addr_inst addr event
    msgs = merge_messages [chan_state_msgs, note_msgs]
    warns = note_warns ++ chan_state_warns


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
    ts = Timestamp.from_real_time (event_start event)

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
post_process = drop_duplicates

drop_duplicates :: [Midi.WriteMessage] -> [Midi.WriteMessage]
drop_duplicates = drop_dup_controls Map.empty

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState =
    (Maybe Midi.PitchBendValue, Map.Map Midi.Control Midi.ControlValue)
type RunningState = Map.Map Instrument.Addr AddrState

drop_dup_controls :: RunningState -> [Midi.WriteMessage]
    -> [Midi.WriteMessage]
drop_dup_controls _ [] = []
drop_dup_controls running (wmsg:wmsgs) = case wmsg of
    Midi.WriteMessage dev _ (Midi.ChannelMessage chan cmsg) ->
        let addr = (dev, chan)
            state = Map.lookup addr running
            (keep, state2) = analyze_msg state cmsg
            running2 = maybe running (\s -> Map.insert addr s running) state2
            rest = drop_dup_controls running2 wmsgs
        in if keep then wmsg : rest else rest
    _ -> drop_dup_controls running wmsgs

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


-- * perform note

-- | Emit MIDI for a single event.
--
-- @next_event@ is the next event with the same Addr which is used for the
-- legato tweak and to see how far to render controls.
perform_note :: RealTime -> Maybe Event -> Event -> Instrument.Addr
    -> ([Midi.WriteMessage], [Warning.Warning], RealTime)
    -- ^ (msgs, warns, note_off)
perform_note prev_note_off next_event event addr =
    case event_pitch_at (event_pb_range event) event (event_start event) of
        Nothing -> ([], [event_warning event "no pitch signal"], prev_note_off)
        Just (midi_nn, _) ->
            let (note_msgs, note_warns, note_off) = _note_msgs midi_nn
                (control_msgs, control_warns) = _control_msgs midi_nn
                warns = note_warns ++ control_warns
            in (merge_messages [control_msgs, note_msgs], warns, note_off)
    where
    -- 'perform_note_msgs' and 'perform_control_msgs' are really part of one
    -- big function.  Splitting it apart led to a bit of duplicated work but
    -- hopefully it's easier to understand this way.
    _note_msgs = perform_note_msgs next_event event addr
    _control_msgs = perform_control_msgs prev_note_off next_event event addr

-- | Perform the note on and note off.
perform_note_msgs :: Maybe Event -> Event -> Instrument.Addr
    -> Midi.Key -> ([Midi.WriteMessage], [Warning.Warning], RealTime)
perform_note_msgs next_event event (dev, chan) midi_nn =
    ([ chan_msg note_on (Midi.NoteOn midi_nn on_vel)
    , chan_msg note_off (Midi.NoteOff midi_nn off_vel)
    ], warns, note_off)
    where
    note_on = event_start event
    -- Don't legato between repeated notes.
    should_legato = event_end event == next_note_on
        && Just midi_nn /= next_midi_nn
    note_off = event_end event
        + if should_legato then legato_overlap_time else 0
    (on_vel, off_vel, vel_clip_warns) = note_velocity event note_on note_off
    next_midi_nn = do
        next <- next_event
        fmap fst $ event_pitch_at (event_pb_range event) next next_note_on
    next_note_on = maybe (note_end event) event_start next_event
    warns = make_clip_warnings event (Control.c_velocity, vel_clip_warns)
    chan_msg pos msg = Midi.WriteMessage dev (Timestamp.from_real_time pos)
        (Midi.ChannelMessage chan msg)

-- | Perform control change messages.
perform_control_msgs :: RealTime -> Maybe Event -> Event -> Instrument.Addr
    -> Midi.Key -> ([Midi.WriteMessage], [Warning.Warning])
perform_control_msgs prev_note_off next_event event (dev, chan) midi_nn =
    (control_msgs, warns)
    where
    control_msgs = merge_messages $
        map (map chan_msg) (pitch_pos_msgs : control_pos_msgs)
    control_sigs = Map.assocs (event_controls event)
    cmap = Instrument.inst_control_map (event_instrument event)
    (control_pos_msgs, clip_warns) = unzip $
        map (perform_control cmap prev_note_off note_on next_note_on)
            control_sigs
    pitch_pos_msgs = perform_pitch (event_pb_range event)
        midi_nn prev_note_off note_on next_note_on (event_pitch event)
    note_on = event_start event

    next_note_on = maybe (note_end event) event_start next_event
    warns = concatMap (make_clip_warnings event)
        (zip (map fst control_sigs) clip_warns)
    chan_msg (pos, msg) = Midi.WriteMessage dev (Timestamp.from_real_time pos)
        (Midi.ChannelMessage chan msg)

event_pb_range :: Event -> Control.PbRange
event_pb_range = Instrument.inst_pitch_bend_range . event_instrument

-- | Get pitch at the given point of the signal.
--
-- The pitch bend always tunes upwards from the tempered note.  It would be
-- slicker to use a negative offset if the note is eventually going above
-- unity, but that's too much work.
event_pitch_at :: Control.PbRange -> Event -> RealTime
    -> Maybe (Midi.Key, Midi.PitchBendValue)
event_pitch_at pb_range event pos =
    Control.pitch_to_midi pb_range (Signal.at pos (event_pitch event))

note_velocity :: Event -> RealTime -> RealTime
    -> (Midi.Velocity, Midi.Velocity, [ClipWarning])
note_velocity event note_on note_off =
    (clipped_vel on_sig, clipped_vel off_sig, clip_warns)
    where
    on_sig = Maybe.fromMaybe default_velocity $
        control_at event Control.c_velocity note_on
    off_sig = Maybe.fromMaybe default_velocity $
        control_at event Control.c_velocity note_off
    clipped_vel val = Control.val_to_cc (fst (clip_val 0 1 val))
    clip_warns =
        if snd (clip_val 0 1 on_sig) || snd (clip_val 0 1 off_sig)
        then [(note_on, note_off)] else []

type ClipWarning = (RealTime, RealTime)
make_clip_warnings :: Event -> (Control.Control, [ClipWarning])
    -> [Warning.Warning]
make_clip_warnings event (control, clip_warns) =
    [ Warning.warning (show control ++ " clipped")
        (event_stack event) (Just (start, end))
    | (start, end) <- clip_warns ]

control_at :: Event -> Control.Control -> RealTime -> Maybe Signal.Y
control_at event control pos = do
    sig <- Map.lookup control (event_controls event)
    return (Signal.at pos sig)

perform_pitch :: Control.PbRange -> Midi.Key -> RealTime -> RealTime
    -> RealTime -> Signal.NoteNumber -> [(RealTime, Midi.ChannelMessage)]
perform_pitch pb_range nn prev_note_off start end sig =
    [(pos, Midi.PitchBend (Control.pb_from_nn pb_range nn val)) |
        (pos, val) <- pos_vals2]
    where
    pos_vals = takeWhile ((<end) . fst) $ Signal.sample start sig
    pos_vals2 = create_leading_cc prev_note_off start sig pos_vals

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed control range, 0--1.
perform_control :: Control.ControlMap -> RealTime -> RealTime -> RealTime
    -> (Control.Control, Signal.Control)
    -> ([(RealTime, Midi.ChannelMessage)], [ClipWarning])
perform_control cmap prev_note_off start end (control, sig) =
    case Control.control_constructor cmap control of
        Nothing -> ([], []) -- TODO warn about a control not in the cmap
        Just ctor -> ([(pos, ctor val) | (pos, val) <- pos_vals3], clip_warns)
    where
    pos_vals3 = create_leading_cc prev_note_off start sig pos_vals2
    pos_vals = takeWhile ((<end) . fst) $ Signal.sample start sig
    (low, high) = Control.control_range
    -- Ack.  Extract the vals, clip them, zip clipped vals back in.
    (clipped_vals, clips) = unzip (map (clip_val low high) (map snd pos_vals))
    clip_warns = extract_clip_warns (zip pos_vals clips)
    pos_vals2 = zip (map fst pos_vals) clipped_vals

-- | I rely on postprocessing to eliminate the redundant msgs.
-- Since 'channelize' respects the 'control_lead_time', I expect msgs to be
-- scheduled on their own channels if possible.
create_leading_cc :: RealTime -> RealTime -> Signal.Signal y
    -> [(RealTime, Signal.Y)] -> [(RealTime, Signal.Y)]
create_leading_cc prev_note_off start sig pos_vals =
    initial : dropWhile ((<=start) . fst) pos_vals
    where
    -- Don't go before 0.  Don't go before the previous note, but don't go
    -- after the start of this note, in case the previous note ends after this
    -- one begins.
    tweak = max 0 . max (min prev_note_off start)
    initial = (tweak (start - control_lead_time), Signal.at start sig)

extract_clip_warns :: [((RealTime, Signal.Y), Bool)] -> [ClipWarning]
extract_clip_warns pos_val_clips = [(head pos, last pos) | pos <- clip_pos]
    where
    groups = List.groupBy ((==) `on` snd) pos_val_clips
    clip_pos = [[pos | ((pos, _val), _clipped) <- g ]
        | g <- groups, snd (head g)]

clip_val :: Signal.Y -> Signal.Y -> Signal.Y -> (Signal.Y, Bool)
clip_val low high val
    | val < low = (low, True)
    | val > high = (high, True)
    | otherwise = (val, False)

-- | Merge the sorted midi messages into a single sorted list.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = Seq.merge_lists Midi.wmsg_ts

merge_sorted_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_sorted_messages = Seq.merge_asc_lists Midi.wmsg_ts

-- * channelize

-- | Assign channels.  Events will be merged into the same channel where they
-- can be.
--
-- A less aggressive policy would be to distribute the instrument among all of
-- its addrs and only share when out of channels, but it seems like this would
-- quickly eat up all the channels, forcing a new note that can't share to snag
-- a used one.
channelize :: InstAddrs -> [Event] -> [(Event, Channel)]
channelize inst_addrs events = overlap_map (channelize_event inst_addrs) events

channelize_event :: InstAddrs -> [(Event, Channel)] -> Event -> Channel
channelize_event inst_addrs overlapping event =
    case Map.lookup inst_name inst_addrs of
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        Just (_:_:_) -> chan
        _ -> 0
    where
    inst_name = Instrument.inst_name (event_instrument event)
    -- If there's no shareable channel, make up a channel one higher than the
    -- maximum channel in use.
    chan = maybe (maximum (-1 : map snd overlapping) + 1) id
        (shareable_chan overlapping event)

-- | Find a channel from the list of overlapping (Event, Channel) all of whose
-- events can share with the given event.
shareable_chan :: [(Event, Channel)] -> Event -> Maybe Channel
shareable_chan overlapping event = fmap fst (List.find all_share by_chan)
    where
    by_chan = Seq.keyed_group_on snd overlapping
    all_share (_chan, evt_chans) =
        all (flip can_share_chan event) (map fst evt_chans)

-- | Can the two events coexist in the same channel without interfering?
-- The reason this is not commutative is so I can assume the start of @old@
-- precedes the start of @new@ and save a little computation.
can_share_chan :: Event -> Event -> Bool
can_share_chan old new
    | start < end = event_instrument new == event_instrument old
        && pitches_share
        && controls_equal start end (event_controls new) (event_controls old)
    | otherwise = True
    where
    start = note_begin new
    end = min (note_end new) (note_end old)

    pitches_share = initial_pitch == current_pitch
        && Signal.pitches_share in_decay start end
            (event_pitch old) (event_pitch new)
        where
        -- If the overlap is in the decay of one or both notes, the rules are
        -- slightly different.
        in_decay = event_end new <= event_start old
            || event_end old <= event_start new
        -- If the old event is not at its initial pitch, it will have pitch
        -- bend applied and therefore can't share with this one.  Actually, it
        -- could possibly share if 'new' started on a pitch other than its
        -- initial pitch and relied on the existing pitch bend to put it in
        -- place, but I think this is hard with the current architecture.
        -- TODO this will improperly split parallel pitch slides and whole
        -- degree slides so it's not ideal.  I need to think more about how to
        -- efficiently determine sharing.  Perhaps if can_share returned
        -- a number if the signals are parallel with an integral offset, then
        -- the caller can subtract that offset from the pitch.
        initial_pitch = Signal.at (event_start old) (event_pitch old)
        -- Note the current pitch is 'event_start' and *not* start, which
        -- will be slightly before the current start, due to control lead.
        current_pitch = Signal.at (event_start new) (event_pitch old)

-- | Are the controls equal in the given range?
controls_equal :: RealTime -> RealTime
    -> ControlMap -> ControlMap -> Bool
controls_equal start end cs0 cs1 = all eq pairs
    where
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controls require their own channel.
    relevant = Map.filterWithKey (\k _ -> Control.is_channel_control k)
    pairs = Map.pairs (relevant cs0) (relevant cs1)
    eq (_, Just sig0, Just sig1) = Signal.equal start end sig0 sig1
    eq _ = False

-- * allot channels

-- | 'channelize' will assign channels based on whether the notes can coexist
-- without interfering with each other.  'allot' reduces those channels down
-- to the real midi channels assigned to the instrument, stealing if necessary.
--
-- Events with instruments that have no address allocation in the config
-- will be dropped.
allot :: InstAddrs -> [(Event, Channel)]
    -> ([(Event, Instrument.Addr)], [Warning.Warning])
allot inst_addrs events = (Maybe.catMaybes event_addrs, warnings)
    where
    (state, event_addrs) = List.mapAccumL allot_event
        (initial_allot_state inst_addrs) events
    warnings = [Warning.warning ("no allocation for " ++ show inst) stack
        Nothing | (inst, stack) <- Map.assocs (ast_no_alloc state)]

data AllotState = AllotState {
    -- | Allocated addresses, and when they were last used.
    ast_available :: Map.Map Instrument.Addr RealTime
    -- | Map arbitrary input channels to an instrument address in the allocated
    -- range.
    , ast_map :: Map.Map (Instrument.Instrument, Channel) Instrument.Addr
    -- | Addresses allocated to each instrument.
    , ast_inst_addrs :: InstAddrs
    , ast_no_alloc :: Map.Map Instrument.InstrumentName Warning.Stack
    } deriving (Show)
initial_allot_state inst_addrs = AllotState Map.empty Map.empty inst_addrs
    Map.empty

allot_event :: AllotState -> (Event, Channel)
    -> (AllotState, Maybe (Event, Instrument.Addr))
allot_event state (event, ichan) =
    case Map.lookup (inst, ichan) (ast_map state) of
        Just addr -> (update_avail addr state, Just (event, addr))
        Nothing -> case steal_addr inst state of
            -- nothing allocated to this instrument
            -- TODO if I'm going to log about insts without allocation this
            -- is the spot
            Nothing -> (insert_warning, Nothing)
            Just addr ->
                (update_avail addr (update_map addr state), Just (event, addr))
    where
    inst = event_instrument event
    update_avail addr state = state { ast_available =
        Map.insert addr (event_end event) (ast_available state) }
    update_map addr state =
        state { ast_map = Map.insert (inst, ichan) addr (ast_map state) }
    insert_warning = state { ast_no_alloc =
        Map.insertWith' const (Instrument.inst_score_name inst)
            (event_stack event) (ast_no_alloc state) }


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
    mlookup addr = Map.findWithDefault (RealTime 0) addr (ast_available state)

-- * data

data Event = Event {
    event_instrument :: Instrument.Instrument
    , event_start :: RealTime
    , event_duration :: RealTime
    , event_controls :: ControlMap
    , event_pitch :: Signal.NoteNumber
    -- original (TrackId, ScoreTime) for errors
    , event_stack :: Warning.Stack
    } deriving (Show)

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event


note_begin :: Event -> RealTime
note_begin event = event_start event - control_lead_time

-- | The end of an event after taking decay into account.  The note shouldn't
-- be sounding past this time.
note_end :: Event -> RealTime
note_end event =
    event_end event + RealTime (Instrument.inst_decay (event_instrument event))


-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer
type ControlMap = Map.Map Control.Control Signal.Control


-- * util

-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.  The overlapping events are
-- passed in reverse order, so the most recently overlapping is first.
overlap_map :: ([(Event, a)] -> Event -> a) -> [Event] -> [(Event, a)]
overlap_map = go []
    where
    go _ _ [] = []
    go prev f (e:events) = (e, val) : go ((e, val) : overlapping) f events
        where
        start = note_begin e
        overlapping = takeWhile ((> start) . note_end . fst) prev
        val = f overlapping e

event_warning :: Event -> String -> Warning.Warning
event_warning event msg = Warning.warning msg (event_stack event)
    (Just (event_start event, event_end event))

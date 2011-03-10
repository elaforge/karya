{-# LANGUAGE PatternGuards #-}
{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
    midi events.

    Keyswitch implementation:

    Keyswitches are implemented as separate instruments that are allocated
    the same channels.  That way the normal channel sharing stuff in
    'can_share_chan' will try to give each instrument on its own channel to
    minimize keyswitches.  Every note with a keyswitch will emit the keyswitch
    slightly before the note if necessary.

    Score.Event level Attributes are mapped to keyswitches.  This happens at
    Convert by 'MidiDb.LookupMidiInstrument' so that the Perform.Events can get
    their slightly different Instruments.  It's up to the conversion code to
    convert an arbitrary set of attributes into a keyswitch.

    Misc notes:

    Not knowing the next event remains a problem.  If I clip the signal to the
    end of the block I may miss control changes during the decay.  While
    unfortunate, this isn't fatal, so oh well.

    I can't know the next score event because it might be a call.  I would have
    to evaluate the next event to get the current event, and that leads to
    evaluating from back to front... and then I have the same problem with the
    previous event.

    The performer can't know the next event because it might be in the next
    chunk.  That means evaluating the chunks back to front, with the same
    problem.
-}
module Perform.Midi.Perform where
import qualified Control.DeepSeq as DeepSeq
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi

import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Signal as Signal
import qualified Perform.RealTime as RealTime
import Perform.RealTime (RealTime)

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument


-- * constants

-- | This winds up being 100, which is loud but not too loud and
-- distinctive-looking.
default_velocity :: Signal.Y
default_velocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.
keyswitch_interval :: RealTime
keyswitch_interval = RealTime.milliseconds 4

-- | Most synths don't respond to pitch bend instantly, but smooth it out, so
-- if you set pitch bend immediately before playing the note you will get
-- a little sproing.  Put pitch bends before their notes by this amount.
control_lead_time :: RealTime
control_lead_time = RealTime.milliseconds 100


-- * perform

-- | These may later become a more efficient type.
type Events = [LEvent.LEvent Event]
type MidiEvents = [LEvent.LEvent Midi.WriteMessage]

-- | Performance state.  This is a snapshot of the state of the various
-- functions in the performance pipeline.  You should be able to resume
-- performance at any point given a RealTime and a State.
data State = State {
    state_channelize :: !ChannelizeState
    , state_allot :: !AllotState
    , state_perform :: !PerformState
    } deriving (Eq, Show)

initial_state :: State
initial_state = State [] empty_allot_state empty_perform_state

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: State -> Instrument.Config -> Events -> (MidiEvents, State)
perform state _ [] = ([], state)
perform state config events = (final_msgs, final_state)
    where
    final_state = State channelize_state allot_state perform_state
    inst_addrs = Instrument.config_alloc config
    (event_channels, channelize_state) =
        channelize (state_channelize state) inst_addrs events
    (event_allotted, allot_state) =
        allot (state_allot state) inst_addrs event_channels
    (msgs, perform_state) = perform_notes (state_perform state) event_allotted
    -- PostprocState is used to drop redundant msgs.  But I can't do that
    -- across chunk boundaries since earlier chunks may not have been played.
    -- Emitting a few redundant control messages is simpler than trying to
    -- reset state based on the previous chunk.
    (final_msgs, _) = post_process mempty msgs

-- | Map each instrument to its allocated Addrs.
type InstAddrs = Map.Map Score.Instrument [Instrument.Addr]


-- * channelize

-- | Overlapping events and the channels they were given.
type ChannelizeState = [(Event, Channel)]

-- | Assign channels.  Events will be merged into the the lowest channel they
-- can coexist with.
--
-- A less aggressive policy would be to distribute the instrument among all of
-- its addrs and only share when out of channels, but it seems like this would
-- quickly eat up all the channels, forcing a new note that can't share to snag
-- a used one.
channelize :: ChannelizeState -> InstAddrs -> Events
    -> ([LEvent.LEvent (Event, Channel)], ChannelizeState)
channelize overlapping inst_addrs events =
    overlap_map overlapping (channelize_event inst_addrs) events

channelize_event :: InstAddrs -> [(Event, Channel)] -> Event -> Channel
channelize_event inst_addrs overlapping event =
    case Map.lookup inst_name inst_addrs of
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        Just (_:_:_) -> chan
        _ -> 0
    where
    inst_name = Instrument.inst_score (event_instrument event)
    -- If there's no shareable channel, make up a channel one higher than the
    -- maximum channel in use.
    chan = maybe (maximum (-1 : map snd overlapping) + 1) id
        (shareable_chan overlapping event)

-- | Find a channel from the list of overlapping (Event, Channel) all of whose
-- events can share with the given event.
shareable_chan :: [(Event, Channel)] -> Event -> Maybe Channel
shareable_chan overlapping event = fst <$> List.find (all_share . snd) by_chan
    where
    by_chan = Seq.keyed_group_on snd overlapping
    all_share evt_chans = all (flip can_share_chan event) (map fst evt_chans)

-- | Can the two events coexist in the same channel without interfering?
-- The reason this is not commutative is so I can assume the start of @old@
-- precedes the start of @new@ and save a little computation.
can_share_chan :: Event -> Event -> Bool
can_share_chan old new = case (initial_pitch old, initial_pitch new) of
        _ | start >= end -> True
        _ | event_instrument old /= event_instrument new -> False
        (Just (initial_old, _), Just (initial_new, _)) ->
            Signal.pitches_share in_decay start end
                initial_old (event_pitch old) initial_new (event_pitch new)
            && controls_equal (event_controls new) (event_controls old)
        _ -> True
    where
    start = note_begin new
    end = min (note_end new) (note_end old)
    initial_pitch event = event_pitch_at (event_pb_range event)
        event (event_start event)
    -- If the overlap is in the decay of one or both notes, the rules are
    -- slightly different.
    in_decay = event_end new <= event_start old
        || event_end old <= event_start new

-- | Are the controls equal in the given range?
controls_equal :: ControlMap -> ControlMap -> Bool
controls_equal cs0 cs1 = all eq pairs
    where
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controls require their own channel.
    relevant = Map.filterWithKey (\k _ -> Control.is_channel_control k)
    pairs = Map.pairs (relevant cs0) (relevant cs1)
    -- Previously I would compare only the overlapping range.  But controls
    -- for multiplexed instruments are trimmed to the event start and end
    -- already.  Comparing the entire signal will fail to merge events that
    -- have different signals after the decay is over, but what are you
    -- doing creating those anyway?
    eq (_, Just sig0, Just sig1) = sig0 == sig1
    eq _ = False


-- * allot channels

-- | 'channelize' will assign channels based on whether the notes can coexist
-- without interfering with each other.  'allot' reduces those channels down
-- to the real midi channels assigned to the instrument, stealing if necessary.
--
-- Events with instruments that have no address allocation in the config
-- will be dropped.
allot :: AllotState -> InstAddrs -> [LEvent.LEvent (Event, Channel)]
    -> ([LEvent.LEvent (Event, Instrument.Addr)], AllotState)
allot state inst_addrs events = (event_addrs, final_state)
    where
    (final_state, event_addrs) = List.mapAccumL allot1 state events
    allot1 state (LEvent.Event e) = allot_event inst_addrs state e
    allot1 state (LEvent.Log log) = (state, LEvent.Log log)

data AllotState = AllotState {
    -- | Allocated addresses, and when they were last used.
    -- This is used by the voice stealer to figure out which voice is ripest
    -- for plunder.
    ast_available :: !(Map.Map Instrument.Addr RealTime)
    -- | Map arbitrary input channels to an instrument address in the allocated
    -- range.
    , ast_allotted ::
        !(Map.Map (Instrument.Instrument, Channel) Instrument.Addr)
    } deriving (Eq, Show)
empty_allot_state = AllotState Map.empty Map.empty

-- | Try to find an Addr for the given Event.  If that's impossible, return
-- a log msg.
allot_event :: InstAddrs -> AllotState -> (Event, Channel)
    -> (AllotState, LEvent.LEvent (Event, Instrument.Addr))
allot_event inst_addrs state (event, ichan) =
    case Map.lookup (inst, ichan) (ast_allotted state) of
        Just addr -> (update addr state, LEvent.Event (event, addr))
        Nothing -> case steal_addr inst_addrs inst state of
            Just addr -> (update addr (update_map addr state),
                LEvent.Event (event, addr))
            -- This will return lots of msgs if an inst has no allocation.
            -- A higher level should filter out the duplicates.
            Nothing -> (state, LEvent.Log no_alloc)
    where
    inst = event_instrument event
    update addr state = update_avail addr state
    update_avail addr state = state { ast_available =
            Map.insert addr (event_end event) (ast_available state) }
    update_map addr state = state { ast_allotted =
        Map.insert (inst, ichan) addr (ast_allotted state) }
    no_alloc = event_warning event
        ("no allocation for " ++ Pretty.pretty (Instrument.inst_score inst))

-- | Steal the least recently used address for the given instrument.
steal_addr :: InstAddrs -> Instrument.Instrument -> AllotState
    -> Maybe Instrument.Addr
steal_addr inst_addrs inst state =
    case Map.lookup (Instrument.inst_score inst) inst_addrs of
        Just addrs -> let avail = zip addrs (map mlookup addrs)
            in if null avail then Nothing -- no addrs assigned
                else let (addr, _) = List.minimumBy (compare `on` snd) avail
                in Just addr
        _ -> Nothing
    where
    mlookup addr = Map.findWithDefault 0 addr (ast_available state)


-- * perform notes

type PerformState = (AddrInst, NoteOffMap)

-- | As in 'Cmd.Cmd.WriteDeviceState', map an Addr to the Instrument active
-- at that address.
--
-- Used to emit keyswitches or program changes.
type AddrInst = Map.Map Instrument.Addr Instrument.Instrument

-- | Map from an address to the last time a note was playing on that address.
-- This includes the last note's decay time, so the channel should be reusable
-- after this time.
--
-- Used to give leading cc times a little breathing room.
--
-- It only needs to be 'min cc_lead (now - note_off)'
type NoteOffMap = Map.Map Instrument.Addr RealTime

-- | Pass an empty AddrInst because I can't make any assumptions about the
-- state of the synthesizer.  The one from the wdev state might be out of
-- date by the time this performance is played.
empty_perform_state :: PerformState
empty_perform_state = (Map.empty, Map.empty)

-- | Given an ordered list of note events, produce the apprapriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: PerformState -> [LEvent.LEvent (Event, Instrument.Addr)]
    -> (MidiEvents, PerformState)
perform_notes state events =
    (Seq.merge_asc_lists levent_start midi_msgs, final_state)
    where
    (final_state, midi_msgs) = List.mapAccumL go state events
    go state (LEvent.Log log) = (state, [LEvent.Log log])
    go state (LEvent.Event event) = _perform_note state event

_perform_note :: PerformState -> (Event, Instrument.Addr)
    -> ((AddrInst, NoteOffMap), MidiEvents)
_perform_note (addr_inst, note_off_map) (event, addr) =
    ((addr_inst2, Map.insert addr note_off note_off_map), msgs)
    where
    (note_msgs, note_off) = perform_note
        (Map.findWithDefault 0 addr note_off_map) event addr
    (chan_state_msgs, addr_inst2) = adjust_chan_state addr_inst addr event
    msgs = Seq.merge_on levent_start chan_state_msgs note_msgs


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
    -> (MidiEvents, AddrInst)
adjust_chan_state addr_inst addr event =
    case chan_state_msgs addr (event_start event) old_inst inst of
        Left err -> ([LEvent.Log $ event_warning event err], new_addr_inst)
        Right msgs -> (map LEvent.Event msgs, new_addr_inst)
    where
    new_addr_inst = Map.insert addr inst addr_inst
    inst = event_instrument event
    old_inst = Map.lookup addr addr_inst

-- | TODO support program change, I'll have to get ahold of patch_initialize.
chan_state_msgs :: Instrument.Addr -> RealTime
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
    extract inst = (Instrument.inst_synth inst, Instrument.inst_score inst)

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
    mk_ks nn = [mkmsg start (Midi.NoteOn nn 64),
        mkmsg (nudge start) (Midi.NoteOff nn 64)]
    nudge = (+ RealTime.milliseconds 2)
    mkmsg ts msg = Midi.WriteMessage wdev ts (Midi.ChannelMessage chan msg)
    start = ts - keyswitch_interval

-- ** perform note

-- | Emit MIDI for a single event.
perform_note :: RealTime -> Event -> Instrument.Addr
    -> (MidiEvents, RealTime)
    -- ^ (msgs, warns, note_off)
perform_note prev_note_off event addr =
    case event_pitch_at (event_pb_range event) event (event_start event) of
        Nothing -> ([LEvent.Log $ event_warning event "no pitch signal"],
            prev_note_off)
        Just (midi_nn, _) ->
            let (note_msgs, note_off) = _note_msgs midi_nn
                control_msgs = _control_msgs midi_nn
            in (merge_events control_msgs note_msgs, note_off)
    where
    -- 'perform_note_msgs' and 'perform_control_msgs' are really part of one
    -- big function.  Splitting it apart led to a bit of duplicated work but
    -- hopefully it's easier to understand this way.
    _note_msgs = perform_note_msgs event addr
    _control_msgs = perform_control_msgs prev_note_off event addr

-- | Perform the note on and note off.
perform_note_msgs :: Event -> Instrument.Addr -> Midi.Key
    -> (MidiEvents, RealTime)
perform_note_msgs event (dev, chan) midi_nn = (events, note_off)
    where
    events = map LEvent.Event
        [ chan_msg note_on (Midi.NoteOn midi_nn on_vel)
        , chan_msg note_off (Midi.NoteOff midi_nn off_vel)
        ]
        ++ map LEvent.Log warns
    note_on = event_start event
    note_off = event_end event
    (on_vel, off_vel, vel_clip_warns) = note_velocity event note_on note_off
    warns = make_clip_warnings event (Control.c_velocity, vel_clip_warns)
    chan_msg pos msg = Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

-- | Perform control change messages.
perform_control_msgs :: RealTime -> Event -> Instrument.Addr -> Midi.Key
    -> MidiEvents
perform_control_msgs prev_note_off event (dev, chan) midi_nn =
    map LEvent.Event control_msgs ++ map LEvent.Log warns
    where
    control_msgs = merge_messages $
        map (map chan_msg) (pitch_pos_msgs : control_pos_msgs)
    control_sigs = Map.assocs (event_controls event)
    cmap = Instrument.inst_control_map (event_instrument event)
    (control_pos_msgs, clip_warns) = unzip $
        map (perform_control cmap prev_note_off note_on) control_sigs
    pitch_pos_msgs = perform_pitch (event_pb_range event)
        midi_nn prev_note_off note_on (event_pitch event)
    note_on = event_start event

    warns = concatMap (make_clip_warnings event)
        (zip (map fst control_sigs) clip_warns)
    chan_msg (pos, msg) =
        Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

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
    -> (Midi.Velocity, Midi.Velocity, [ClipRange])
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

clip_val :: Signal.Y -> Signal.Y -> Signal.Y -> (Signal.Y, Bool)
clip_val low high val
    | val < low = (low, True)
    | val > high = (high, True)
    | otherwise = (val, False)

type ClipRange = (RealTime, RealTime)
make_clip_warnings :: Event -> (Control.Control, [ClipRange]) -> [Log.Msg]
make_clip_warnings event (control, clip_warns) =
    [event_warning event (show control ++ " clipped: " ++ Pretty.pretty clip)
        | clip <- clip_warns]

control_at :: Event -> Control.Control -> RealTime -> Maybe Signal.Y
control_at event control pos = do
    sig <- Map.lookup control (event_controls event)
    return (Signal.at pos sig)

perform_pitch :: Control.PbRange -> Midi.Key -> RealTime
    -> RealTime -> Signal.NoteNumber -> [(RealTime, Midi.ChannelMessage)]
perform_pitch pb_range nn prev_note_off start sig =
    [(x, Midi.PitchBend (Control.pb_from_nn pb_range nn y))
        | (x, y) <- pos_vals]
    where
    -- As per 'perform_control', there shouldn't be much to drop here.
    trim = dropWhile ((< start) . fst)
    pos_vals = create_leading_cc prev_note_off start sig $
        trim (Signal.unsignal sig)

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed control range, 0--1.
perform_control :: Control.ControlMap -> RealTime -> RealTime
    -> (Control.Control, Signal.Control)
    -> ([(RealTime, Midi.ChannelMessage)], [ClipRange])
perform_control cmap prev_note_off start (control, sig) =
    case Control.control_constructor cmap control of
        Nothing -> ([], []) -- TODO warn about a control not in the cmap
        Just ctor -> ([(x, ctor y) | (x, y) <- pos_vals], clip_warns)
    where
    -- The signal should already be trimmed to the event range, except that,
    -- as per the behaviour of Signal.drop_before, it may have a leading
    -- sample.  I can drop that since it's handled specially be
    -- 'create_leading_cc'.
    pos_vals = create_leading_cc prev_note_off start sig $
        trim (Signal.unsignal clipped)
    trim = dropWhile ((< start) . fst)
    (clipped, out_of_bounds) = Signal.clip_bounds sig
        -- (tracef (\s -> (start, Signal.first s, Signal.last s)) sig)
    clip_warns = [(s, e) | (s, e) <- out_of_bounds]

-- | I rely on postprocessing to eliminate the redundant msgs.
-- Since 'channelize' respects the 'control_lead_time', I expect msgs to be
-- scheduled on their own channels if possible.
create_leading_cc :: RealTime -> RealTime -> Signal.Signal y
    -> [(Signal.X, Signal.Y)] -> [(Signal.X, Signal.Y)]
create_leading_cc prev_note_off start sig pos_vals =
    initial : dropWhile ((<= start) . fst) pos_vals
    where
    -- Don't go before the previous note, but don't go after the start of this
    -- note, in case the previous note ends after this one begins.
    tweak t = max (min prev_note_off start) (t - control_lead_time)
    initial = ((tweak start), Signal.at start sig)

-- * post process

type PostprocState = Map.Map Instrument.Addr AddrState

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState =
    (Maybe Midi.PitchBendValue, Map.Map Midi.Control Midi.ControlValue)

-- | Some context free post-processing on the midi stream.
post_process :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
post_process = drop_dup_controls

-- | Having to deal with Log is ugly... can't I get that out with fmap?
drop_dup_controls :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
drop_dup_controls state [] = ([], state)
drop_dup_controls state (log@(LEvent.Log _) : events) =
    let (rest, final_state) = drop_dup_controls state events
    in (log : rest, final_state)
drop_dup_controls state (event@(LEvent.Event wmsg) : wmsgs) = case wmsg of
    Midi.WriteMessage dev _ (Midi.ChannelMessage chan cmsg) ->
        let addr = (dev, chan)
            addr_state = Map.lookup addr state
            (keep, addr_state2) = analyze_msg addr_state cmsg
            state2 = maybe state (\s -> Map.insert addr s state) addr_state2
            (rest, final_state) = drop_dup_controls state2 wmsgs
        in (if keep then event : rest else rest, final_state)
    _ -> drop_dup_controls state wmsgs

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


-- * data

data Event = Event {
    event_instrument :: !Instrument.Instrument
    , event_start :: !RealTime
    , event_duration :: !RealTime
    , event_controls :: !ControlMap
    , event_pitch :: !Signal.NoteNumber
    , event_stack :: !Stack.Stack
    } deriving (Eq, Show)

instance DeepSeq.NFData Event where
    rnf (Event inst start dur controls pitch stack) =
        rnf inst `seq` rnf start `seq` rnf dur `seq` rnf controls
        `seq` rnf pitch `seq` rnf stack
        where
        rnf :: DeepSeq.NFData a => a -> ()
        rnf = DeepSeq.rnf

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

note_begin :: Event -> RealTime
note_begin event = event_start event - control_lead_time

-- | The end of an event after taking decay into account.  The note shouldn't
-- be sounding past this time.
note_end :: Event -> RealTime
note_end event = event_end event
    + RealTime.seconds (Instrument.inst_decay (event_instrument event))


-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer
type ControlMap = Map.Map Control.Control Signal.Control


-- * util

-- | Merge an unsorted list of sorted lists of midi messages.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = Seq.merge_lists Midi.wmsg_ts

merge_events :: MidiEvents -> MidiEvents -> MidiEvents
merge_events = Seq.merge_on levent_start

merge_sorted_events :: [MidiEvents] -> MidiEvents
merge_sorted_events = Seq.merge_asc_lists levent_start

levent_start :: LEvent.LEvent Midi.WriteMessage -> RealTime
levent_start (LEvent.Log _) = 0
levent_start (LEvent.Event msg) = Midi.wmsg_ts msg

-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.  The overlapping events are
-- passed in reverse order, so the most recently overlapping is first.
overlap_map :: [(Event, a)] -> ([(Event, a)] -> Event -> a) -> Events
    -> ([LEvent.LEvent (Event, a)], [(Event, a)])
    -- ^ (output for each event, final overlapping state)
overlap_map initial = go initial
    where
    go prev _ [] = ([], prev)
    go prev f (LEvent.Log log : events) = (LEvent.Log log : rest, final_state)
        where (rest, final_state) = go prev f events
    go prev f (LEvent.Event e : events) =
        (LEvent.Event (e, val) : vals, final_state)
        where
        start = note_begin e
        overlapping = takeWhile ((> start) . note_end . fst) prev
        val = f overlapping e
        (vals, final_state) = go ((e, val) : overlapping) f events

event_warning :: Event -> String -> Log.Msg
event_warning event msg =
    Log.msg Log.Warn (Just (event_stack event)) ("Perform: " ++ msg)

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

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
    their slightly different Instruments.  It's up to "Perform.Midi.Convert" to
    convert an arbitrary set of attributes into a keyswitch.
-}
module Perform.Midi.Perform where
import qualified Control.DeepSeq as DeepSeq
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import Util.Control
import qualified Util.Log as Log
import qualified Util.Map as Map
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.LEvent as LEvent
import qualified Derive.Score as Score
import qualified Derive.Stack as Stack

import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.Instrument as Instrument
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import Perform.RealTime (RealTime)
import qualified Perform.Signal as Signal


-- * constants

-- | Turn on debug logging.  This is hardcoded because debugging can generate
-- lots of logs and performance has to be efficient.
logging :: Bool
logging = False

-- | This winds up being 100, which is loud but not too loud and
-- distinctive-looking.
default_velocity :: Signal.Y
default_velocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.
keyswitch_gap :: RealTime
keyswitch_gap = RealTime.milliseconds 4

-- | Most synths don't respond to pitch bend instantly, but smooth it out, so
-- if you set pitch bend immediately before playing the note you will get
-- a little sproing.  Put pitch bends before their notes by this amount.
control_lead_time :: RealTime
control_lead_time = RealTime.milliseconds 100

-- | Subtract this much from every NoteOff.  Some synthesizers don't handle
-- simultaneous note on and note off of the same pitch well.  I actually only
-- need the gap for a NoteOff followed by NoteOn of the same pitch, but it's
-- simpler to just subtract it from all notes.
adjacent_note_gap :: RealTime
adjacent_note_gap = RealTime.milliseconds 10


-- * perform

type Events = [LEvent.LEvent Event]
type MidiEvents = [LEvent.LEvent Midi.WriteMessage]

-- | Performance state.  This is a snapshot of the state of the various
-- functions in the performance pipeline.  You should be able to resume
-- performance at any point given a RealTime and a State.
--
-- I don't do that anymore, and this is left over from when I cached the
-- performance.  I removed the cache but left the state visible.
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
perform :: State -> Instrument.Configs -> Events -> (MidiEvents, State)
perform state _ [] = ([], state)
perform state configs events = (final_msgs, final_state)
    where
    final_state = State channelize_state allot_state perform_state
    inst_addrs = Map.map Instrument.config_addrs configs
    (event_channels, channelize_state) =
        channelize (state_channelize state) inst_addrs events
    (event_allotted, allot_state) =
        allot (state_allot state) inst_addrs event_channels
    (msgs, perform_state) = perform_notes (state_perform state) event_allotted
    (final_msgs, _) = post_process mempty msgs

-- | Map each instrument to its allocated Addrs.
type InstAddrs =
    Map.Map Score.Instrument [(Instrument.Addr, Maybe Instrument.Voices)]

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

-- | This doesn't pay any mind to instrument channel assignments, except as an
-- optimization for instruments with only a single channel.  Channels are
-- actually assigned later by 'allot'.
channelize_event :: InstAddrs -> [(Event, Channel)] -> Event
    -> (Channel, [Log.Msg])
channelize_event inst_addrs overlapping event =
    case Map.lookup inst_name inst_addrs of
        Just (_:_:_) -> (chan, [log])
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        _ -> (0, [])
    where
    inst_name = Instrument.inst_score (event_instrument event)
    -- If there's no shareable channel, make up a channel one higher than the
    -- maximum channel in use.
    chan = fromMaybe (maximum (-1 : map snd overlapping) + 1) maybe_chan
    (maybe_chan, reasons) = shareable_chan overlapping event
    log = Log.msg Log.Warn (Just stack) $ Text.unlines $
        (log_prefix event <> ": found chan " <> showt maybe_chan <> ", picked "
            <> showt chan)
        : map mkmsg reasons
    stack = Stack.to_strings (event_stack event)
    mkmsg (chan, reason) = "can't share with " <> showt chan <> ": " <> reason

-- | This is redundant since log msgs have a stack, but it's convenient for
-- filtering.
log_prefix :: Event -> Text
log_prefix event =
    prettyt inst <> " at " <> prettyt (event_start event) <> ": "
    where inst = Instrument.inst_score (event_instrument event)

-- | Find a channel from the list of overlapping (Event, Channel) all of whose
-- events can share with the given event.  Return the rest of the channels and
-- the reason why they can't be used.
shareable_chan :: [(Event, Channel)] -> Event
    -> (Maybe Channel, [(Channel, Text)])
shareable_chan overlapping event =
    (fst <$> List.find (null . snd) unshareable_reasons,
        map (second (Text.intercalate "; ")) $
            filter (not . null . snd) unshareable_reasons)
    where
    unshareable_reasons = [(chan, reasons evts) | (chan, evts) <- by_chan]
    by_chan = Seq.keyed_group_on snd overlapping
    reasons = mapMaybe (flip can_share_chan event . fst)

-- | Can the two events coexist in the same channel without interfering?
-- The reason this is not commutative is so I can assume the start of @old@
-- is equal to or precedes the start of @new@ and save a little computation.
--
-- This is by far the most finicky function in the whole module, because
-- this is the core decision when multiplexing channels.
can_share_chan :: Event -> Event -> Maybe Text
can_share_chan old new = case (initial_pitch old, initial_pitch new) of
        _ | start >= end -> Nothing
        _ | event_instrument old /= event_instrument new ->
            Just "instruments differ"
        (Just (initial_old, _), Just (initial_new, _))
            | not (Signal.pitches_share in_decay start end
                initial_old (event_pitch old) initial_new (event_pitch new)) ->
                    Just $ "pitch signals incompatible: "
                        <> prettyt (event_pitch old) <> " /= "
                        <> prettyt (event_pitch new)
            | not c_equal ->
                Just $ "controls differ: "
                    <> prettyt (event_controls old)
                    <> " /= " <> prettyt (event_controls new)
            | otherwise -> Nothing
        _ -> Nothing
    where
    start = event_start new
    -- Note that I add the control_lead_time to the decay of the old note
    -- rather than subtracting it from the start of the new one.  Subtracting
    -- would cause 'Signal.pitches_share' to check the pitch signal before
    -- the start of the note, which is going to be 0 and mess up sharing.
    end = min (note_end new) (note_end old) + control_lead_time
    initial_pitch event = event_pitch_at (event_pb_range event)
        event (event_start event)
    -- If the overlap is in the decay of one or both notes, the rules are
    -- slightly different.
    in_decay = event_end new <= event_start old
        || event_end old <= event_start new
    c_equal = controls_equal (event_start new) (event_end old)
        (event_controls old) (event_controls new)

-- | Are the controls equal in the given range?
--
-- Notes with differing @c_aftertouch@ can always share, since they are
-- addressed by MIDI key.  If the key is the same, they already can't share.
--
-- Previously I insisted that the controls be identical, but now I check within
-- the overlapping range only.  What's more, I only check where the events
-- actually overlap, not including decay time.
--
-- Each event is supposed to only include the controls within its range.  So
-- given a series of notes with a changing control, each note includes a bit of
-- control, which then becomes constant as soon as the next note begins, since
-- the rest of the control belongs to the next note.  This means the two notes
-- can't share, because one has a flat signal during its decay while the other
-- has the moving signal.  But in practice this turns out to be inconvenient,
-- because it means that a series of notes with a crescendo will be divided
-- across multiple channels.  That's ok if there are enough channels, but if
-- there aren't, then this can introduce lots of bad-sounding channel stealing.
--
-- TODO However, not counting the decay means that very audible controls will
-- be shared and cause artifacts.  I think the real difference is that controls
-- like dyn and mod are not very audible during the decay, so it's ok to share
-- them.  But another control, like a filter cutoff, might be very obvious.
-- So perhaps there should be a per-control configuration, but I'll worry about
-- that only if it ever becomes a problem.
controls_equal :: RealTime -> RealTime -> ControlMap -> ControlMap -> Bool
controls_equal start end cs1 cs2 = start >= end || all eq pairs
    where
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controls require their own channel.
    relevant = Map.filterWithKey (\k _ -> Control.is_channel_control k)
    pairs = Map.pairs (relevant cs1) (relevant cs2)
    eq (_, Seq.Both sig1 sig2) =
        Signal.within start end sig1 == Signal.within start end sig2
    eq _ = False


-- * allot channels

-- | 'channelize' will assign channels based on whether the notes can coexist
-- without interfering with each other.  'allot' reduces those channels down
-- to the real midi channels assigned to the instrument, stealing if necessary.
--
-- allot assigns notes to the longest-unused channel, which means that even if
-- notes could coexist but don't have to, they will wind up on separate
-- channels.
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
    -- for plunder.  It also has the AllotKey so the previous allotment can be
    -- deleted.
    ast_available :: !(Map.Map Instrument.Addr (RealTime, AllotKey))
    -- | Map input channels to an instrument address in the allocated range.
    -- Once an (inst, chan) pair has been allotted to a particular Addr, it
    -- should keep going to that Addr, as long as voices remain.
    , ast_allotted :: !(Map.Map AllotKey Allotted)
    } deriving (Eq, Show)

-- | Channelize makes sure that a (inst, ichan) key identifies events that can
-- share channels.
type AllotKey = (Instrument.Instrument, Channel)

data Allotted = Allotted {
    allotted_addr :: !Instrument.Addr
    -- | End time for each allocated voice.
    , allotted_voices :: ![RealTime]
    -- | Maximum length for allotted_voices.
    , allotted_voice_count :: !Instrument.Voices
    } deriving (Eq, Show)

empty_allot_state :: AllotState
empty_allot_state = AllotState Map.empty Map.empty

-- | Try to find an Addr for the given Event.  If that's impossible, return
-- a log msg.
--
-- If channelize decided that two events have the same channel, then they can
-- go to the same addr, as long as it has voices left.  Otherwise, take over
-- another channel.
allot_event :: InstAddrs -> AllotState -> (Event, Channel)
    -> (AllotState, LEvent.LEvent (Event, Instrument.Addr))
allot_event inst_addrs state (event, ichan) =
    case expire_voices <$> Map.lookup (inst, ichan) (ast_allotted state) of
        -- If there is an already allotted addr with a free voice, add this
        -- event to it.
        Just (Allotted addr voices voice_count) | length voices < voice_count ->
            (update Nothing addr voices state, LEvent.Event (event, addr))
        -- Otherwise, steal the oldest already allotted voice.
        -- Delete the old (inst, chan) mapping.
        _ -> case steal_addr inst_addrs inst state of
            Just (addr, voice_count, old_key) ->
                (update (Just (voice_count, old_key)) addr [] state,
                    LEvent.Event (event, addr))
            -- This will return lots of msgs if an inst has no allocation.
            -- A higher level should filter out the duplicates.
            Nothing -> (state, LEvent.Log no_alloc)
    where
    -- log = LEvent.Log
    --     . Log.msg Log.Warn (Just (Stack.to_strings (event_stack event)))
    --     . (log_prefix event <>)
    expire_voices allotted = allotted
        { allotted_voices =
            filter (> event_start event) (allotted_voices allotted)
        }
    inst = event_instrument event
    update = update_allot_state (inst, ichan) (event_end event)
    no_alloc = event_warning event
        ("no allocation for " <> prettyt (Instrument.inst_score inst))

-- | Record this addr as now being allotted, and add its voice allocation.
update_allot_state :: (Instrument.Instrument, Channel) -> RealTime
    -> Maybe (Instrument.Voices, Maybe AllotKey) -> Instrument.Addr
    -> [RealTime] -> AllotState -> AllotState
update_allot_state inst_chan end maybe_new_allot addr voices state = state
    { ast_available = Map.insert addr (end, inst_chan) (ast_available state)
    , ast_allotted = case maybe_new_allot of
        Just (voice_count, old_key) ->
            Map.insert inst_chan (Allotted addr [end] voice_count) $
                maybe id Map.delete old_key (ast_allotted state)
        Nothing -> Map.adjust adjust inst_chan (ast_allotted state)
    }
    where adjust allotted = allotted { allotted_voices = end : voices }

-- | Steal the least recently used address for the given instrument, and return
-- how many voices it supports.
--
-- Nothing voices means no limit, and in this case it'll pick a big number.
-- I initially feared keeping track of voice allocation would be wasteful for
-- addrs with no limitation, but profiling revealed no detectable difference.
-- So either it's not important or my profiles are broken.
steal_addr :: InstAddrs -> Instrument.Instrument -> AllotState
    -> Maybe (Instrument.Addr, Instrument.Voices, Maybe AllotKey)
steal_addr inst_addrs inst state =
    case Map.lookup (Instrument.inst_score inst) inst_addrs of
        Just addr_voices ->
            let avail = zip addr_voices (map (mlookup . fst) addr_voices)
            in case Seq.minimum_on (fst . snd) avail of
                Just ((addr, voices), (_, maybe_inst_chan)) ->
                    Just (addr, fromMaybe 10000 voices, maybe_inst_chan)
                Nothing -> Nothing
        _ -> Nothing
    where
    mlookup addr = case Map.lookup addr (ast_available state) of
        Nothing -> (0, Nothing)
        Just (end, inst_chan) -> (end, Just inst_chan)


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

-- | Given an ordered list of note events, produce the appropriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: PerformState -> [LEvent.LEvent (Event, Instrument.Addr)]
    -> (MidiEvents, PerformState)
perform_notes state events =
    (Seq.merge_asc_lists levent_start midi_msgs, final_state)
    where
    (final_state, midi_msgs) = List.mapAccumL go state
        (zip events (drop 1 (List.tails events)))
    go state (LEvent.Log log, _) = (state, [LEvent.Log log])
    go state (LEvent.Event event@(_, addr), future) =
        _perform_note state (find_addr addr future) event
    find_addr addr =
        fmap (event_start . fst) . LEvent.find_event ((==addr) . snd)

_perform_note :: PerformState -> Maybe RealTime -> (Event, Instrument.Addr)
    -> (PerformState, MidiEvents)
_perform_note (addr_inst, note_off_map) next_note_on (event, addr) =
    ((addr_inst2, Map.insert addr note_off note_off_map), msgs)
    where
    (note_msgs, note_off) = perform_note
        (Map.findWithDefault 0 addr note_off_map) next_note_on event addr
    (chan_state_msgs, addr_inst2) = adjust_chan_state addr_inst addr event
    msgs = merge_events chan_state_msgs note_msgs

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
    -> Either Text [Midi.WriteMessage]
chan_state_msgs addr@(wdev, chan) start maybe_old_inst new_inst
    | not same_synth =
        Left $ "two synths on " <> showt addr <> ": " <> inst_desc
    | not same_inst = Left $ "program change not supported yet on "
        <> showt addr <> ": " <> inst_desc
    | not same_ks = Right $
        keyswitch_messages maybe_old_inst new_inst wdev chan start
    | otherwise = Right []
    where
    inst_desc = showt (fmap extract maybe_old_inst, extract new_inst)
    extract inst = (Instrument.inst_synth inst, Instrument.inst_score inst)

    same_synth = case maybe_old_inst of
        Nothing -> True
        Just o -> Instrument.inst_synth o == Instrument.inst_synth new_inst
    same_inst = case maybe_old_inst of
        Nothing -> True -- when pchange is supported I can assume false
        Just o -> Instrument.inst_name o == Instrument.inst_name new_inst
    -- No previous inst is the same as not having a keyswitch.
    same_ks = maybe [] Instrument.inst_keyswitch maybe_old_inst
        == Instrument.inst_keyswitch new_inst

-- TODO if the last note was a hold keyswitch, this will leave the keyswitch
-- down.  Technically I should clean that up, but it's a hassle because
-- I'd need to keep the keyswitch down state in the PerformState so
-- 'perform_notes' can clean them all up, or let 'adjust_chan_state' look into
-- the future so it knows if there will be another note.  But in practice,
-- all notes get turned off after playing so the keyswitch should be cleaned up
-- by that.
keyswitch_messages :: Maybe Instrument.Instrument -> Instrument.Instrument
    -> Midi.WriteDevice -> Midi.Channel -> RealTime -> [Midi.WriteMessage]
keyswitch_messages maybe_old_inst new_inst wdev chan start =
    prev_ks_off ++ new_ks_on
    where
    -- Hold keyswitches have to stay down for the for the duration they are in
    -- effect.  So they just emit a NoteOn.  If I am switching keyswitches
    -- and the previous one was a hold-keyswitch, then it must be down, and
    -- I have to emit a NoteOff for it.
    prev_ks_off = Maybe.fromMaybe [] $ do
        old <- maybe_old_inst
        guard (Instrument.inst_hold_keyswitch old)
        -- I apply the adjacent_note_gap to the ks note off too.  It's probably
        -- unnecessary, but this way the note and the ks go off at the same
        -- time.
        return $ concatMap (ks_off (start-adjacent_note_gap))
            (Instrument.inst_keyswitch old)

    new_ks = Instrument.inst_keyswitch new_inst
    is_hold = Instrument.inst_hold_keyswitch new_inst
    ks_start = start - keyswitch_gap - delta * fromIntegral (length new_ks - 1)
    ks_starts = iterate (+delta) ks_start
    delta = RealTime.milliseconds 2

    new_ks_on
        | is_hold = zipWith ks_on ks_starts new_ks
        | otherwise = concat [ks_on t ks : ks_off (t+delta) ks
            | (t, ks) <- zip ks_starts new_ks]

    ks_on ts ks = mkmsg ts $ case ks of
        Instrument.Keyswitch key -> Midi.NoteOn key 64
        Instrument.ControlSwitch cc val -> Midi.ControlChange cc val
    ks_off ts ks = map (mkmsg ts) $ case ks of
        Instrument.Keyswitch key -> [Midi.NoteOff key 64]
        Instrument.ControlSwitch {} -> []
    mkmsg ts msg = Midi.WriteMessage wdev ts (Midi.ChannelMessage chan msg)

-- ** perform note

-- | Emit MIDI for a single event.
perform_note :: RealTime -> Maybe RealTime -> Event -> Instrument.Addr
    -> (MidiEvents, RealTime) -- ^ (msgs, note_off)
perform_note prev_note_off next_note_on event addr =
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
    _control_msgs =
        perform_control_msgs prev_note_off next_note_cutoff event addr
    -- Drop msgs that would overlap with the next note on.
    -- The controls after the note off are clipped to make room for the next
    -- note's leading controls.  Lead time will get pushed forward if the
    -- note really is adjacent, but if it's supposedly off then it's lower
    -- priority and I can clip off its controls.  Otherwise, the lead-time
    -- controls get messed up by controls from the last note.
    next_note_cutoff = max (event_end event) . subtract control_lead_time <$>
        next_note_on

-- | Perform the note on and note off.
perform_note_msgs :: Event -> Instrument.Addr -> Midi.Key
    -> (MidiEvents, RealTime)
perform_note_msgs event (dev, chan) midi_nn = (events, note_off)
    where
    events = map LEvent.Event
        [ chan_msg note_on (Midi.NoteOn midi_nn on_vel)
        , chan_msg tweaked_note_off (Midi.NoteOff midi_nn off_vel)
        ]
        ++ map LEvent.Log warns
    note_on = event_start event
    note_off = event_end event
    -- Subtract the adjacent_note_gap, but leave a little bit of duration for
    -- 0-dur notes.
    tweaked_note_off = max (note_on + adjacent_note_gap)
        (note_off - adjacent_note_gap)
    (on_vel, off_vel, vel_clip_warns) = note_velocity event note_on note_off
    warns = make_clip_warnings event (Controls.velocity, vel_clip_warns)
    chan_msg pos msg = Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

-- | Perform control change messages.
perform_control_msgs :: RealTime -> Maybe RealTime -> Event -> Instrument.Addr
    -> Midi.Key -> MidiEvents
perform_control_msgs prev_note_off next_note_on event (dev, chan) midi_nn =
    map LEvent.Event (trim control_msgs) ++ map LEvent.Log warns
    where
    trim = maybe id (\t -> takeWhile ((<t) . Midi.wmsg_ts)) next_note_on
    control_msgs = merge_messages $
        map (map chan_msg) (pitch_pos_msgs : control_pos_msgs)
    control_sigs = Map.toList (event_controls event)
    cmap = Instrument.inst_control_map (event_instrument event)
    (control_pos_msgs, clip_warns) = unzip $
        map (perform_control cmap prev_note_off note_on midi_nn) control_sigs
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
    Control.pitch_to_midi pb_range $
        Pitch.NoteNumber $ Signal.at pos (event_pitch event)

note_velocity :: Event -> RealTime -> RealTime
    -> (Midi.Velocity, Midi.Velocity, [ClipRange])
note_velocity event note_on note_off =
    (clipped_vel on_sig, clipped_vel off_sig, clip_warns)
    where
    on_sig = fromMaybe default_velocity $
        control_at event Controls.velocity note_on
    off_sig = fromMaybe default_velocity $
        control_at event Controls.velocity note_off
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

make_clip_warnings :: Event -> (Score.Control, [ClipRange]) -> [Log.Msg]
make_clip_warnings event (control, clip_warns) =
    [event_warning event (prettyt control <> " clipped: "
        <> prettyt (s, e)) | (s, e) <- clip_warns]

control_at :: Event -> Score.Control -> RealTime -> Maybe Signal.Y
control_at event control pos = do
    sig <- Map.lookup control (event_controls event)
    return (Signal.at pos sig)

perform_pitch :: Control.PbRange -> Midi.Key -> RealTime
    -> RealTime -> Signal.NoteNumber -> [(RealTime, Midi.ChannelMessage)]
perform_pitch pb_range nn prev_note_off start sig =
    [(x, Midi.PitchBend (Control.pb_from_nn pb_range nn (Pitch.NoteNumber y)))
        | (x, y) <- pos_vals]
    where
    -- As per 'perform_control', there shouldn't be much to drop here.
    trim = dropWhile ((< start) . fst)
    pos_vals = create_leading_cc prev_note_off start sig $
        trim (Signal.unsignal sig)

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed control range, 0--1.
perform_control :: Control.ControlMap -> RealTime -> RealTime -> Midi.Key
    -> (Score.Control, Signal.Control)
    -> ([(RealTime, Midi.ChannelMessage)], [ClipRange])
perform_control cmap prev_note_off start midi_key (control, sig) =
    case Control.control_constructor cmap control midi_key of
        Nothing -> ([], []) -- TODO warn about a control not in the cmap
        Just ctor -> ([(x, ctor y) | (x, y) <- pos_vals], clip_warns)
    where
    -- The signal should already be trimmed to the event range, except that,
    -- as per the behaviour of Signal.drop_before, it may have a leading
    -- sample.  I can drop that since it's handled specially by
    -- 'create_leading_cc'.
    pos_vals = create_leading_cc prev_note_off start sig $
        trim (Signal.unsignal clipped)
    trim = dropWhile ((< start) . fst)
    (clipped, out_of_bounds) = Signal.clip_bounds 0 1 sig
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
    initial = (tweak start, Signal.at start sig)

-- * post process

type PostprocState = Map.Map Instrument.Addr AddrState

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState =
    (Maybe Midi.PitchBendValue, Map.Map Midi.Control Midi.ControlValue)

-- | Some context free post-processing on the midi stream.
post_process :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
post_process state = first resort . drop_dup_controls state

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

-- | Sort almost-sorted MidiEvents.  Events may be out of order by
-- as much as control_lead_time.  This happens because 'create_leading_cc' adds
-- events between 0--control_lead_time before the note, which can violate the
-- precondition of 'Seq.merge_asc_lists'.
--
-- I tried to come up with a way for the events to come out sorted even with
-- 'create_leading_cc', but creativity failed me, so I resorted to this hammer.
resort :: MidiEvents -> MidiEvents
resort = go mempty
    where
    go collect [] = map LEvent.Event collect
    go collect (LEvent.Log log : events) = LEvent.Log log : go collect events
    go collect (LEvent.Event event : events) =
        map LEvent.Event pre ++ go post events
        where
        -- In the common sorted case, this means copying 'collect' every single
        -- time.  Presumably I could go find a priority queue on hackage, but
        -- lists are pretty fast...
        (pre, post) = break ((> Midi.wmsg_ts event - interval) . Midi.wmsg_ts)
            (Seq.insert_on Midi.wmsg_ts event collect)
    interval = control_lead_time


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

instance Pretty.Pretty Event where
    format (Event inst start dur controls pitch stack) = Pretty.record_title
        "Event"
        [ ("instrument", Pretty.format (Instrument.inst_score inst))
        , ("start", Pretty.format start)
        , ("duration", Pretty.format dur)
        , ("controls", Pretty.format controls)
        , ("pitch", Pretty.format pitch)
        , ("stack", Pretty.format stack)
        ]

event_end :: Event -> RealTime
event_end event = event_start event + event_duration event

note_begin :: Event -> RealTime
note_begin event = event_start event - control_lead_time

-- | The end of an event after taking decay into account.  The note shouldn't
-- be sounding past this time.
note_end :: Event -> RealTime
note_end event =
    event_end event + Instrument.inst_decay (event_instrument event)

-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer
type ControlMap = Map.Map Score.Control Signal.Control


-- * util

-- | Merge an unsorted list of sorted lists of midi messages.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = Seq.merge_lists Midi.wmsg_ts

merge_events :: MidiEvents -> MidiEvents -> MidiEvents
merge_events = Seq.merge_on levent_start

levent_start :: LEvent.LEvent Midi.WriteMessage -> RealTime
levent_start (LEvent.Log _) = 0
levent_start (LEvent.Event msg) = Midi.wmsg_ts msg

-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.  The overlapping events are
-- passed in reverse order, so the most recently overlapping is first.
overlap_map :: [(Event, a)] -> ([(Event, a)] -> Event
    -> (a, [Log.Msg])) -> Events
    -> ([LEvent.LEvent (Event, a)], [(Event, a)])
    -- ^ (output for each event, final overlapping state)
overlap_map initial = go initial
    where
    go prev _ [] = ([], prev)
    go prev f (LEvent.Log log : events) = (LEvent.Log log : rest, final_state)
        where (rest, final_state) = go prev f events
    go prev f (LEvent.Event e : events) =
        (LEvent.Event (e, val) : log_events ++ vals, final_state)
        where
        start = note_begin e
        overlapping = takeWhile ((> start) . note_end . fst) prev
        (val, logs) = f overlapping e
        log_events = if logging then map LEvent.Log logs else []
        (vals, final_state) = go ((e, val) : overlapping) f events

event_warning :: Event -> Text -> Log.Msg
event_warning event msg =
    Log.msg Log.Warn (Just (Stack.to_strings (event_stack event)))
        ("Perform: " <> msg)

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
    midi events.
-}
module Perform.Midi.Perform (
    default_velocity
    , State(..), initial_state
    , Config, config, addrs_config
    , perform
    -- * types
    , MidiEvents
#ifdef TESTING
    , module Perform.Midi.Perform
#endif
) where
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text

import qualified Util.CallStack as CallStack
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.LEvent as LEvent
import qualified Derive.ScoreT as ScoreT
import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Midi.MSignal as MSignal
import qualified Perform.Midi.Patch as Patch
import qualified Perform.Midi.Types as T
import qualified Perform.Pitch as Pitch
import qualified Perform.RealTime as RealTime
import           Perform.RealTime (RealTime)

import           Global


-- I tried using HashMap in here, but aside from one score with somewhat
-- better performance, the result seemed to be the same or slightly worse.
-- In any case, performance in here is not that important, since it happens
-- lazily on demand and is already way faster than realtime.

-- * constants

-- | Turn on debug logging.  This is hardcoded because debugging can generate
-- lots of logs and performance has to be efficient.
logging :: Bool
logging = False

-- | This winds up being 100, which is loud but not too loud and
-- distinctive-looking.
default_velocity :: MSignal.Y
default_velocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.  Some synthesizers (kontakt at least) will occasionally not notice
-- a keyswitch that comes too close to its note.
keyswitch_lead_time :: RealTime
keyswitch_lead_time = RealTime.milliseconds 10

-- | Most synths don't respond to control change and pitch bend instantly, but
-- smooth it out, so if you set pitch bend immediately before playing the note
-- you will get a little sproing.  Put pitch bends before their notes by this
-- amount.
control_lead_time :: RealTime
control_lead_time = RealTime.milliseconds 100

-- | 'control_lead_time' can be flattened out if there isn't time for it.  This
-- happens when there is another note on the same previous channel that would
-- overlap it.  To avoid an audible artifact on the tail of the previous note,
-- I omit the lead time in that case.  However, I still need a minimum amount
-- of lead time because some MIDI patches use the state of the controls at
-- NoteOn time to configure the whole note.  A tiny gap should be enough to
-- make sure the control changes arrive first, but short enough that it's not
-- audible on the previous note.
--
-- The root of the problem, like so many problems with MIDI, is that it's
-- highly stateful, nothing happens simultaneously, and channels are precious.
min_control_lead_time :: RealTime
min_control_lead_time = RealTime.milliseconds 4

-- | Subtract this much from every NoteOff.  Some synthesizers don't handle
-- simultaneous note on and note off of the same pitch well.  I actually only
-- need the gap for a NoteOff followed by NoteOn of the same pitch, but it's
-- simpler to just subtract it from all notes.
adjacent_note_gap :: RealTime
adjacent_note_gap = RealTime.milliseconds 10

-- | Each note will have at least this duration.  The reason is that some
-- synthesizers (kontakt at least) will sometimes not notice a note which is
-- too short.  Usually these notes are percussion and come in with zero
-- duration.
--
-- Honestly nothing really surprises me about Kontakt anymore.
min_note_duration :: RealTime
min_note_duration = RealTime.milliseconds 20

-- * perform

type Events = [LEvent.LEvent T.Event]
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

instance Pretty State where
    format (State channelize allot perform) = Pretty.record "State"
        [ ("channelize", Pretty.format channelize)
        , ("allot", Pretty.format allot)
        , ("perform", Pretty.format perform)
        ]

initial_state :: State
initial_state = State [] empty_allot_state empty_perform_state

type Configs = Map ScoreT.Instrument Config
newtype Config = Config {
    _addrs :: [(Patch.Addr, Maybe Patch.Voices)]
    } deriving (Show, Eq)

empty_config :: Config
empty_config = Config []

config :: Patch.Config -> Config
config patch_config = Config
    { _addrs = Patch.config_allocation patch_config
    }

addrs_config :: [(Patch.Addr, Maybe Patch.Voices)] -> Config
addrs_config addrs = empty_config { _addrs = addrs }

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: State -> Configs -> Events -> (MidiEvents, State)
perform state _ [] = ([], state)
perform state configs events = (final_msgs, final_state)
    where
    final_state = State channelize_state allot_state perform_state
    (event_channels, channelize_state) =
        channelize (state_channelize state) configs events
    (event_allotted, allot_state) =
        allot (state_allot state) configs event_channels
    (msgs, perform_state) = perform_notes (state_perform state) event_allotted
    (final_msgs, _) = post_process mempty msgs

-- * channelize

-- | This isn't directly the midi channel, since it goes higher than 15, but
-- will later be mapped to midi channels.
type Channel = Integer

-- | Overlapping events and the channels they were given.
type ChannelizeState = [(T.Event, Channel)]

-- | Assign channels.  Events will be merged into the the lowest channel they
-- can coexist with.
--
-- A less aggressive policy would be to distribute the instrument among all of
-- its addrs and only share when out of channels, but it seems like this would
-- quickly eat up all the channels, forcing a new note that can't share to snag
-- a used one.
channelize :: ChannelizeState -> Configs -> Events
    -> ([LEvent.LEvent (T.Event, Channel)], ChannelizeState)
channelize overlapping configs events =
    overlap_map overlapping (channelize_event configs) events

-- | This doesn't pay any mind to instrument channel assignments, except as an
-- optimization for instruments with only a single channel.  Channels are
-- actually assigned later by 'allot'.
channelize_event :: Configs -> [(T.Event, Channel)] -> T.Event
    -> (Channel, [Log.Msg])
channelize_event configs overlapping event =
    case _addrs <$> Map.lookup inst_name configs of
        Just (_:_:_) -> (chan, [log])
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        _ -> (0, [])
    where
    inst_name = T.patch_name (T.event_patch event)
    -- If there's no shareable channel, make up a channel one higher than the
    -- maximum channel in use.
    chan = fromMaybe (maximum (-1 : map snd overlapping) + 1) maybe_chan
    (maybe_chan, reasons) = shareable_chan overlapping event
    log = Log.msg Log.Warn (Just (T.event_stack event)) $ Text.unlines $
        (T.show_short event <> ": found chan " <> showt maybe_chan
            <> ", picked " <> showt chan)
        : map mkmsg reasons
        ++ [showt c <> ": " <> T.show_short e | (e, c) <- overlapping]
    mkmsg (chan, reason) = "can't share with " <> showt chan <> ": " <> reason

-- | Find a channel from the list of overlapping (T.Event, Channel) all of whose
-- events can share with the given event.  Return the rest of the channels and
-- the reason why they can't be used.
shareable_chan :: [(T.Event, Channel)] -> T.Event
    -> (Maybe Channel, [(Channel, Text)])
shareable_chan overlapping event =
    ( fst <$> List.find (null . snd) unshareable_reasons
    , map (second (Text.intercalate "; ")) $
        filter (not . null . snd) unshareable_reasons
    )
    where
    unshareable_reasons = [(chan, reasons evts) | (evts, chan) <- by_chan]
    by_chan = Seq.group_snd overlapping
    reasons = mapMaybe (flip can_share_chan event)

-- | Can the two events coexist in the same channel without interfering?
-- The reason this is not commutative is so I can assume the start of @old@
-- is equal to or precedes the start of @new@ and save a little computation.
--
-- This is by far the most finicky function in the whole module, because
-- this is the core decision when multiplexing channels.
can_share_chan :: T.Event -> T.Event -> Maybe Text
can_share_chan old new = case (initial_pitch old, initial_pitch new) of
    _ | start >= end -> Nothing
    -- Previously I required that the whole Patch be equal, which caused
    -- notes with different keyswitches to not share channels.  However, they
    -- actually can share channels, though they still can't play
    -- simultaneously.  I need to be as aggressive as possible sharing
    -- channels, especially for instruments with long decays, because any
    -- channel stealing for pitch bends can be very audible.
    _ | inst_of old /= inst_of new -> Just "instruments differ"
    (Just (initial_old, _), Just (initial_new, _))
        | not (MSignal.pitches_share in_decay start end
            initial_old (T.event_pitch old) initial_new (T.event_pitch new)) ->
                Just $ "pitch signals incompatible: "
                    <> pretty (T.event_pitch old) <> " /= "
                    <> pretty (T.event_pitch new)
        | not c_equal ->
            Just $ "controls differ: " <> pretty (T.event_controls old)
                <> " /= " <> pretty (T.event_controls new)
        | otherwise -> Nothing
    _ -> Nothing
    where
    inst_of = T.patch_name . T.event_patch
    start = T.event_start new
    -- Note that I add the control_lead_time to the decay of the old note
    -- rather than subtracting it from the start of the new one.  Subtracting
    -- would cause 'MSignal.pitches_share' to check the pitch signal before
    -- the start of the note, which is going to be 0 and mess up sharing.
    end = min (note_end new) (note_end old) + control_lead_time
    initial_pitch event = event_pitch_at (event_pb_range event)
        event (T.event_start event)
    -- If the overlap is in the decay of one or both notes, the rules are
    -- slightly different.
    in_decay = T.event_end new <= T.event_start old
        || T.event_end old <= T.event_start new
    c_equal = controls_equal (T.event_start new) (T.event_end old)
        (T.event_controls old) (T.event_controls new)

{- | Are the controls equal in the given range?

    Notes with differing @c_aftertouch@ can always share, since they are
    addressed by MIDI key.  If the key is the same, they already can't share.

    Previously I insisted that the controls be identical, but now I check
    within the overlapping range only.  What's more, I only check where the
    events actually overlap, not including decay time.

    Each event is supposed to only include the controls within its range.  So
    given a series of notes with a changing control, each note includes a bit
    of control, which then becomes constant as soon as the next note begins,
    since the rest of the control belongs to the next note.  This means the two
    notes can't share, because one has a flat signal during its decay while the
    other has the moving signal.  But in practice this turns out to be
    inconvenient, because it means that a series of notes with a crescendo will
    be divided across multiple channels.  That's ok if there are enough
    channels, but if there aren't, then this can introduce lots of bad-sounding
    channel stealing.

    TODO However, not counting the decay means that very audible controls will
    be shared and cause artifacts.  I think the real difference is that
    controls like dyn and mod are not very audible during the decay, so it's ok
    to share them.  But another control, like a filter cutoff, might be very
    obvious.  So perhaps there should be a per-control configuration, but I'll
    worry about that only if it ever becomes a problem.
-}
controls_equal :: RealTime -> RealTime
    -> Map ScoreT.Control MSignal.Signal
    -> Map ScoreT.Control MSignal.Signal
    -> Bool
controls_equal start end cs1 cs2 = start >= end || all eq pairs
    where
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controls require their own channel.
    relevant = Map.filterWithKey (\k _ -> Control.is_channel_control k)
    pairs = Maps.pairs (relevant cs1) (relevant cs2)
    eq (_, Seq.Both sig1 sig2) =
        MSignal.within start end sig1 == MSignal.within start end sig2
    eq _ = False


-- * allot channels

-- | 'channelize' will assign channels based on whether the notes can coexist
-- without interfering with each other.  'allot' reduces those channels down
-- to the real midi channels assigned to the instrument, stealing if necessary.
-- It steals from the longest-unused channel.
--
-- Events with instruments that have no address allocation in the config
-- will be dropped.
allot :: AllotState -> Configs -> [LEvent.LEvent (T.Event, Channel)]
    -> ([LEvent.LEvent (T.Event, Patch.Addr)], AllotState)
allot state configs events = (event_addrs, final_state)
    where
    (final_state, event_addrs) = List.mapAccumL allot1 state events
    allot1 state (LEvent.Event e) = allot_event configs state e
    allot1 state (LEvent.Log log) = (state, LEvent.Log log)

data AllotState = AllotState {
    -- | Allocated addresses, and when they were last used.
    -- This is used by the voice stealer to figure out which voice is ripest
    -- for plunder.  It also has the AllotKey so the previous allotment can be
    -- deleted.
    ast_available :: !(Map Patch.Addr (RealTime, AllotKey))
    -- | Map input channels to an instrument address in the allocated range.
    -- Once an (inst, chan) pair has been allotted to a particular Addr, it
    -- should keep going to that Addr, as long as voices remain.
    , ast_allotted :: !(Map AllotKey Allotted)
    } deriving (Eq, Show)

instance Pretty AllotState where
    format (AllotState available allotted) = Pretty.record "AllotState"
        [ ("available", Pretty.format available)
        , ("allotted", Pretty.format allotted)
        ]

empty_allot_state :: AllotState
empty_allot_state = AllotState Map.empty Map.empty

-- | Channelize makes sure that a (inst, ichan) key identifies events that can
-- share channels.
type AllotKey = (ScoreT.Instrument, Channel)

data Allotted = Allotted {
    _allotted_addr :: !Patch.Addr
    -- | End time for each allocated voice.
    , allotted_voices :: ![RealTime]
    -- | Maximum length for allotted_voices.
    , _allotted_voice_count :: !Patch.Voices
    } deriving (Eq, Show)

instance Pretty Allotted where
    format (Allotted addr voices voice_count) = Pretty.record "Allotted"
        [ ("addr", Pretty.format addr)
        , ("voices", Pretty.format voices)
        , ("voice_count", Pretty.format voice_count)
        ]

-- | Try to find an Addr for the given Event.  If that's impossible, return
-- a log msg.
--
-- If channelize decided that two events have the same channel, then they can
-- go to the same addr, as long as it has voices left.  Otherwise, take over
-- another channel.
allot_event :: Configs -> AllotState -> (T.Event, Channel)
    -> (AllotState, LEvent.LEvent (T.Event, Patch.Addr))
allot_event configs state (event, ichan) =
    case expire_voices <$> Map.lookup (inst, ichan) (ast_allotted state) of
        -- If there is an already allotted addr with a free voice, add this
        -- event to it.
        Just (Allotted addr voices voice_count) | length voices < voice_count ->
            (update Nothing addr voices state, LEvent.Event (event, addr))
        -- Otherwise, steal the oldest already allotted voice.
        -- Delete the old (inst, chan) mapping.
        _ -> case steal_addr configs inst state of
            Just (addr, voice_count, old_key) ->
                (update (Just (voice_count, old_key)) addr [] state,
                    LEvent.Event (event, addr))
            -- This will return lots of msgs if an inst has no allocation.
            -- A higher level should filter out the duplicates.
            Nothing -> (state, LEvent.Log no_alloc)
    where
    -- Remove voices that have ended.
    expire_voices allotted = allotted
        { allotted_voices =
            filter (> T.event_start event) (allotted_voices allotted)
        }
    inst = T.patch_name $ T.event_patch event
    update = update_allot_state (inst, ichan) (T.event_end event)
    no_alloc = event_warning event ("no allocation for " <> pretty inst)

-- | Record this addr as now being allotted, and add its voice allocation.
update_allot_state :: (ScoreT.Instrument, Channel) -> RealTime
    -> Maybe (Patch.Voices, Maybe AllotKey) -> Patch.Addr
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
steal_addr :: Configs -> ScoreT.Instrument -> AllotState
    -> Maybe (Patch.Addr, Patch.Voices, Maybe AllotKey)
steal_addr configs inst state = case _addrs <$> Map.lookup inst configs of
    Just addr_voices -> case Seq.minimum_on (fst . snd) avail of
        Just ((addr, voices), (_, maybe_inst_chan)) ->
            Just (addr, fromMaybe 10000 voices, maybe_inst_chan)
        Nothing -> Nothing
        where avail = zip addr_voices (map (mlookup . fst) addr_voices)
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
type AddrInst = Map Patch.Addr T.Patch

-- | Map from an address to the last time a note was playing on that address.
-- This includes the last note's decay time, so the channel should be reusable
-- after this time.
--
-- Used to give leading cc times a little breathing room.
--
-- It only needs to be 'min cc_lead (now - note_off)'
type NoteOffMap = Map Patch.Addr RealTime

-- | Pass an empty AddrInst because I can't make any assumptions about the
-- state of the synthesizer.  The one from the wdev state might be out of
-- date by the time this performance is played.
empty_perform_state :: PerformState
empty_perform_state = (Map.empty, Map.empty)

-- | Given an ordered list of note events, produce the appropriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: PerformState -> [LEvent.LEvent (T.Event, Patch.Addr)]
    -> (MidiEvents, PerformState)
perform_notes state events =
    (Seq.merge_asc_lists merge_key midi_msgs, final_state)
    where
    (final_state, midi_msgs) = List.mapAccumL go state
        (zip events (drop 1 (List.tails events)))
    go state (LEvent.Log log, _) = (state, [LEvent.Log log])
    go state (LEvent.Event event@(_, addr), future) =
        perform_note_in_channel state (find_addr addr future) event
    find_addr addr =
        fmap (T.event_start . fst) . LEvent.find_event ((==addr) . snd)

-- | Emit msgs to set the channel state, and msgs for a single note.
perform_note_in_channel :: PerformState
    -> Maybe RealTime -- ^ next note with the same addr
    -> (T.Event, Patch.Addr) -> (PerformState, MidiEvents)
perform_note_in_channel (addr_inst, note_off_map) next_note_on (event, addr) =
    ((addr_inst2, Map.insert addr note_off note_off_map), msgs)
    where
    (note_msgs, note_off) = perform_note
        (Map.findWithDefault 0 addr note_off_map) next_note_on event addr
    (chan_state_msgs, addr_inst2) = adjust_chan_state addr_inst addr event
    msgs = merge_events chan_state_msgs note_msgs

{- | Figure out of any msgs need to be emitted to convert the channel state to
    the given event on the given addr.  This means keyswitches and program
    changes.

    If there's no chan state always emit msgs, since in general there's no way
    to know what state the synth is in.  If I do know (e.g. playback will
    pass the current addr_inst) I can filter out expensive messages like
    program change.
    TODO implement playback with addr_inst when I implement pchange

    Another strategy would be to always emit msgs and rely on playback filter,
    but that would triple the number of msgs, which seems excessive.
-}
adjust_chan_state :: AddrInst -> Patch.Addr -> T.Event -> (MidiEvents, AddrInst)
adjust_chan_state addr_inst addr event = case event_midi_key event of
    Nothing -> ([], new_addr_inst)
    Just midi_key ->
        case chan_state_msgs midi_key addr (T.event_start event) old inst of
            Left err -> ([LEvent.Log $ event_warning event err], new_addr_inst)
            Right msgs -> (map LEvent.Event msgs, new_addr_inst)
    where
    new_addr_inst = Map.insert addr inst addr_inst
    inst = T.event_patch event
    old = Map.lookup addr addr_inst

-- | TODO support program change, I'll have to get ahold of patch_initialize.
chan_state_msgs :: Midi.Key -> Patch.Addr -> RealTime
    -> Maybe T.Patch -> T.Patch
    -> Either Text [Midi.WriteMessage]
chan_state_msgs midi_key addr@(wdev, chan) start maybe_old_inst new_inst
    | not same_inst = Left $ "program change not supported yet on "
        <> pretty addr <> ": "
            <> pretty (T.patch_name <$> maybe_old_inst)
            <> " -> " <> pretty (T.patch_name new_inst)
    | not (same_keyswitches maybe_old_inst new_inst) = Right $
        keyswitch_messages midi_key maybe_old_inst new_inst wdev chan start
    | otherwise = Right []
    where
    same_inst = case maybe_old_inst of
        Nothing -> True -- when pchange is supported I can assume false
        Just o -> T.patch_name o == T.patch_name new_inst

same_keyswitches :: Maybe T.Patch -> T.Patch -> Bool
same_keyswitches maybe_old new =
    go (maybe [] T.patch_keyswitches maybe_old) (T.patch_keyswitches new)
    where
    go [] [] = True
    -- To actually get this right I'd have to either change the Instrument
    -- Keyswitch to have the MIDI key, or keep a map of the aftertouch state
    -- by key.  Both sound like a hassle, so I'll just emit possibly redundant
    -- msgs.
    go (Patch.Aftertouch _ : _) (Patch.Aftertouch _ : _) = False
    go (x : xs) (y : ys) = x == y && go xs ys
    go _ _ = False

{- | Emit keyswitch msgs to adjust the channel to the new instrument.

    TODO if the last note was a hold keyswitch, this will leave the keyswitch
    down.  Technically I should clean that up, but it's a hassle because I'd
    need to keep the keyswitch down state in the PerformState so
    'perform_notes' can clean them all up, or let 'adjust_chan_state' look into
    the future so it knows if there will be another note.  But in practice, all
    notes get turned off after playing so the keyswitch should be cleaned up by
    that.
-}
keyswitch_messages :: Midi.Key -> Maybe T.Patch
    -> T.Patch -> Midi.WriteDevice -> Midi.Channel -> RealTime
    -> [Midi.WriteMessage]
keyswitch_messages midi_key maybe_old_inst new_inst wdev chan start =
    prev_ks_off ++ new_ks_on
    where
    -- Hold keyswitches have to stay down for the for the duration they are in
    -- effect.  So they just emit a NoteOn.  If I am switching keyswitches
    -- and the previous one was a hold-keyswitch, then it must be down, and
    -- I have to emit a NoteOff for it.
    prev_ks_off = Maybe.fromMaybe [] $ do
        old <- maybe_old_inst
        guard (T.patch_hold_keyswitches old)
        -- I apply the adjacent_note_gap to the ks note off too.  It's probably
        -- unnecessary, but this way the note and the ks go off at the same
        -- time.
        return $ mapMaybe (ks_off (start-adjacent_note_gap))
            (T.patch_keyswitches old)

    new_ks = T.patch_keyswitches new_inst
    is_hold = T.patch_hold_keyswitches new_inst
    ks_start = start - keyswitch_lead_time

    new_ks_on
        | is_hold = map (ks_on ks_start) new_ks
        | otherwise = map (ks_on ks_start) new_ks
            ++ mapMaybe (ks_off (ks_start+min_note_duration)) new_ks
    ks_on ts = mkmsg ts . Patch.keyswitch_on midi_key
    ks_off ts = fmap (mkmsg ts) . Patch.keyswitch_off
    mkmsg ts msg = Midi.WriteMessage wdev ts (Midi.ChannelMessage chan msg)

-- ** perform note

-- | Emit MIDI for a single event.
perform_note :: RealTime -> Maybe RealTime -- ^ next note with the same addr
    -> T.Event -> Patch.Addr -> (MidiEvents, RealTime) -- ^ (msgs, note_off)
perform_note prev_note_off next_note_on event addr =
    case event_midi_key event of
        Nothing ->
            ( [LEvent.Log $ event_warning event "no pitch signal"]
            , prev_note_off
            )
        Just midi_key -> (merge_events control_msgs note_msgs, note_off)
            where
            (note_msgs, note_off) = _note_msgs midi_key
            control_msgs = _control_msgs note_off midi_key
    where
    -- 'perform_note_msgs' and 'perform_control_msgs' are really part of one
    -- big function.  Splitting it apart led to a bit of duplicated work but
    -- hopefully it's easier to understand this way.
    _note_msgs = perform_note_msgs event addr
    _control_msgs = perform_control_msgs prev_note_off next_note_on event addr

-- | Perform the note on and note off.
perform_note_msgs :: T.Event -> Patch.Addr -> Midi.Key -> (MidiEvents, RealTime)
perform_note_msgs event (dev, chan) midi_nn = (events, note_off)
    where
    events =
        [ LEvent.Event $ chan_msg note_on $ Midi.NoteOn midi_nn $
            -- NoteOn with 0 velocity is interpreted as NoteOff.  This messes
            -- up notes that are supposed to start from 0, e.g. via breath
            -- control.
            max 1 (Control.val_to_cval (T.event_start_velocity event))
        , LEvent.Event $ chan_msg note_off $ Midi.NoteOff midi_nn $
            Control.val_to_cval (T.event_end_velocity event)
        ]
    note_on = T.event_start event
    -- Subtract the adjacent_note_gap, but still have at least
    -- min_note_duration.
    note_off = max (note_on + min_note_duration)
        (T.event_end event - adjacent_note_gap)
    chan_msg pos msg = Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

-- | Perform control change messages.
perform_control_msgs :: RealTime -> Maybe RealTime -> T.Event -> Patch.Addr
    -> RealTime -> Midi.Key -> MidiEvents
perform_control_msgs prev_note_off next_note_on event (dev, chan) note_off
        midi_key =
    map LEvent.Event control_msgs ++ map LEvent.Log warns
    where
    control_msgs = merge_messages $
        map (map chan_msg) (pitch_pos_msgs : control_pos_msgs)
    control_sigs = Map.toList (T.event_controls event)
    cmap = T.patch_control_map (T.event_patch event)
    -- |===---
    --      -|===---
    -- Drop controls that would overlap with the next note on.
    -- The controls after the note off are clipped to make room for the next
    -- note's leading controls.  Lead time will get pushed forward if the
    -- note really is adjacent, but if it's supposedly off then it's lower
    -- priority and I can clip off its controls.  Otherwise, the lead-time
    -- controls get messed up by controls from the last note.
    control_end = case next_note_on of
        Nothing -> Nothing
        Just next -> Just $ max note_off (next - control_lead_time)

    (control_pos_msgs, clip_warns) = unzip $
        map (perform_control cmap prev_note_off note_on control_end midi_key)
            control_sigs
    pitch_pos_msgs = perform_pitch (event_pb_range event)
        midi_key prev_note_off note_on control_end (T.event_pitch event)
    note_on = T.event_start event

    warns = concatMap (make_clip_warnings event)
        (zip (map fst control_sigs) clip_warns)
    chan_msg (pos, msg) =
        Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

event_pb_range :: T.Event -> Control.PbRange
event_pb_range = T.patch_pitch_bend_range . T.event_patch

-- | Get pitch at the given point of the signal.
--
-- The pitch bend always tunes upwards from the tempered note.  It would be
-- slicker to use a negative offset if the note is eventually going above
-- unity, but that's too much work.
event_pitch_at :: Control.PbRange -> T.Event -> RealTime
    -> Maybe (Midi.Key, Midi.PitchBendValue)
event_pitch_at pb_range event pos =
    Control.pitch_to_midi pb_range $
        Pitch.NoteNumber $ MSignal.at pos (T.event_pitch event)

-- | Get the Midi.Key that will be used for the event, without pitch bend.
event_midi_key :: T.Event -> Maybe Midi.Key
event_midi_key event =
    fst <$> event_pitch_at (event_pb_range event) event (T.event_start event)

type ClipRange = (RealTime, RealTime)

make_clip_warnings :: T.Event -> (ScoreT.Control, [ClipRange]) -> [Log.Msg]
make_clip_warnings event (control, clip_warns) =
    [ event_warning event $ pretty control <> " clipped: "
        <> pretty s <> "--" <> pretty e
    | (s, e) <- clip_warns
    ]

perform_pitch :: Control.PbRange -> Midi.Key -> RealTime -> RealTime
    -> Maybe RealTime -> MSignal.Signal -> [(RealTime, Midi.ChannelMessage)]
perform_pitch pb_range nn prev_note_off start end sig =
    [ (x, Midi.PitchBend (Control.pb_from_nn pb_range nn (Pitch.NoteNumber y)))
    | (x, y) <- pos_vals
    ]
    where pos_vals = perform_signal prev_note_off start end sig

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed control range, 0--1.
perform_control :: Control.ControlMap -> RealTime -> RealTime -> Maybe RealTime
    -> Midi.Key -> (ScoreT.Control, MSignal.Signal)
    -> ([(RealTime, Midi.ChannelMessage)], [ClipRange])
perform_control cmap prev_note_off start end midi_key (control, sig) =
    case Control.control_constructor cmap control midi_key of
        Nothing -> ([], [])
        Just ctor -> ([(x, ctor y) | (x, y) <- pos_vals], clip_warns)
    where
    -- The signal should already be trimmed to the event range, except that,
    -- as per the behaviour of MSignal.drop_before, it may have a leading
    -- sample.  I can drop that since it's handled specially by
    -- 'perform_signal'.
    pos_vals = perform_signal prev_note_off start end clipped
    (clipped, out_of_bounds) = MSignal.clip_bounds 0 1 sig
    clip_warns = [(s, e) | (s, e) <- out_of_bounds]

-- | Trim a signal to the proper time range and emit (X, Y) pairs.  The proper
-- time range is complicated since there are two levels of priority.  Controls
-- within the note's start to end+decay are always emitted.  The end+decay is
-- put into the 'NoteOffMap' so the next note will yield 'control_lead_time' if
-- necessary.  Samples after end+decay are also emitted, but trimmed so they
-- won't overlap the next note's start - control_lead_time.
--
-- 'channelize' respects 'control_lead_time', so I expect msgs to be
-- scheduled on their own channels if possible.
--
-- If the signal has consecutive samples with the same value, this will emit
-- unnecessary CCs, but they will be eliminated by postprocessing.
perform_signal :: RealTime -> RealTime -> Maybe RealTime -> MSignal.Signal
    -> [(RealTime, MSignal.Y)]
perform_signal prev_note_off start end sig = initial : pairs
    where
    -- The signal should already be trimmed to the event start, except that
    -- it may have a leading sample, due to 'MSignal.drop_before'.
    pairs = Seq.drop_initial_dups fst $
        dropWhile ((<=start) . fst) $ MSignal.to_pairs $
        MSignal.drop_before start $ maybe id MSignal.drop_at_after end sig
    -- Don't go before the previous note, but don't go after the start of this
    -- note, in case the previous note ends after this one begins.
    tweaked_start = min (start - min_control_lead_time) $
        max (min prev_note_off start) (start - control_lead_time)
    initial = (tweaked_start, MSignal.at start sig)

-- * post process

type PostprocState = Map Patch.Addr AddrState

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState = (Maybe Midi.PitchBendValue, Map Midi.Control Midi.ControlValue)

-- | Some context free post-processing on the midi stream.
post_process :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
post_process state = first avoid_overlaps . drop_dup_controls state . resort

{- |
    Overlapping notes with the same key number are undefined in MIDI.
    I have two strategies for them.  One is that an overlapping NoteOn will
    cause a NoteOff, and then the next NoteOff will be suppressed, which is
    effectively moving the overlapping note's NoteOff to right before the
    NoteOn.  So where + is NoteOn and | is NoteOff:

    > 0   1   2   3   5   6   7   8
    > +---+---|---|
    > +--|+-------|

    The other is Patch.UseFinalNoteOff, which accumulates the NoteOffs to the
    end:

    > 0   1   2   3   5   6   7   8
    > +---+---|---|
    > +---+-------||

    This was originally for kontakt, which doesn't count NoteOns and turns the
    note off on the first NoteOff, but I think the first method makes this
    obsolete, so it's disabled.
-}
avoid_overlaps :: MidiEvents -> MidiEvents
avoid_overlaps events = concat $ snd $ LEvent.map_accum move initial events
    where
    initial :: Map (Midi.WriteDevice, Midi.Channel, Midi.Key) NoteOnState
    initial = Map.empty

    move state wmsg _ = case msg_key wmsg of
        Just (skey@(_, chan, key), msg) -> case msg of
            Midi.NoteOff {} ->
                (state2, map (make_msg wmsg . Midi.ChannelMessage chan) offs)
                where (state2, offs) = note_off skey msg state
            Midi.NoteOn {} -> (state2, cons_if emit_off off [wmsg])
                where
                off = make_msg wmsg $
                    Midi.ChannelMessage chan $ Midi.NoteOff key 100
                (state2, emit_off) = note_on skey state
            _ -> (state, [wmsg])
        Nothing -> (state, [wmsg])
    -- Overlapping notes with the same (dev, chan, key) are affected.
    msg_key wmsg = case Midi.wmsg_msg wmsg of
        Midi.ChannelMessage chan msg@(Midi.NoteOn key _) ->
            Just ((dev, chan, key), msg)
        Midi.ChannelMessage chan msg@(Midi.NoteOff key _) ->
            Just ((dev, chan, key), msg)
        _ -> Nothing
        where dev = Midi.wmsg_dev wmsg

    -- When I see NoteOn, emit NoteOff if a note with that key is already on.
    note_on skey state = case Map.lookup skey state of
        Just Playing -> (Map.insert skey SuppressNoteOff state, True)
        _ -> (Map.insert skey Playing state, False)

    -- When I see a NoteOff, drop it if SuppressNoteOff or there's nothing
    -- playing, otherwise emit it normally and update the state.
    note_off skey msg state = case Map.lookup skey state of
        Just SuppressNoteOff -> (Map.insert skey Playing state, [])
        Just NotPlaying -> (state, [])
        Just Playing -> (Map.delete skey state, [msg])
        Nothing -> (state, [])

    make_msg wmsg = Midi.WriteMessage (Midi.wmsg_dev wmsg) (Midi.wmsg_ts wmsg)

data NoteOnState = Playing | NotPlaying
    -- | This means the next NoteOff will get skipped, but the one after that
    -- emitted.
    | SuppressNoteOff
    deriving (Show)

cons_if :: Bool -> a -> [a] -> [a]
cons_if True x = (x:)
cons_if False _ = id

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
-- as much as control_lead_time.  This happens because 'perform_signal' adds
-- events between 0--control_lead_time before the note, which can violate the
-- precondition of 'Seq.merge_asc_lists'.
--
-- I tried to come up with a way for the events to come out sorted even with
-- 'perform_signal', but creativity failed me, so I resorted to this hammer.
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


-- * event

note_begin :: T.Event -> RealTime
note_begin event = T.event_start event - control_lead_time

-- | The end of an event after taking decay into account.  The note shouldn't
-- be sounding past this time.
note_end :: T.Event -> RealTime
note_end event = T.event_end event
    + fromMaybe T.default_decay (T.patch_decay (T.event_patch event))

-- * util

-- | Merge an unsorted list of sorted lists of midi messages.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = Seq.merge_lists Midi.wmsg_ts

merge_events :: MidiEvents -> MidiEvents -> MidiEvents
merge_events = Seq.merge_on merge_key

merge_key :: LEvent.LEvent Midi.WriteMessage -> RealTime
merge_key (LEvent.Log _) = 0
merge_key (LEvent.Event msg) = Midi.wmsg_ts msg


-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.  The overlapping events are
-- passed in reverse order, so the most recently overlapping is first.
overlap_map :: [(T.Event, a)] -> ([(T.Event, a)] -> T.Event
    -> (a, [Log.Msg])) -> Events
    -> ([LEvent.LEvent (T.Event, a)], [(T.Event, a)])
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

event_warning :: CallStack.Stack => T.Event -> Text -> Log.Msg
event_warning event = Log.msg Log.Warn (Just (T.event_stack event))

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{-# LANGUAGE StrictData #-}
{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
    midi events.
-}
module Perform.Midi.Perform (
    defaultVelocity
    , State(..), initialState
    , Config, config, addrsConfig
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

import           GHC.Stack (HasCallStack)

import qualified Util.Lists as Lists
import qualified Util.Log as Log
import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty

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
defaultVelocity :: MSignal.Y
defaultVelocity = 0.79

-- | A keyswitch gets this much lead time before the note it is meant to
-- apply to.  Some synthesizers (kontakt at least) will occasionally not notice
-- a keyswitch that comes too close to its note.
keyswitchLeadTime :: RealTime
keyswitchLeadTime = RealTime.milliseconds 10

-- | Most synths don't respond to control change and pitch bend instantly, but
-- smooth it out, so if you set pitch bend immediately before playing the note
-- you will get a little sproing.  Put pitch bends before their notes by this
-- amount.
controlLeadTime :: RealTime
controlLeadTime = RealTime.milliseconds 100

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
minControlLeadTime :: RealTime
minControlLeadTime = RealTime.milliseconds 4

-- | Subtract this much from every NoteOff.  Some synthesizers don't handle
-- simultaneous note on and note off of the same pitch well.  I actually only
-- need the gap for a NoteOff followed by NoteOn of the same pitch, but it's
-- simpler to just subtract it from all notes.
adjacentNoteGap :: RealTime
adjacentNoteGap = RealTime.milliseconds 10

-- | Each note will have at least this duration.  The reason is that some
-- synthesizers (kontakt at least) will sometimes not notice a note which is
-- too short.  Usually these notes are percussion and come in with zero
-- duration.
--
-- Honestly nothing really surprises me about Kontakt anymore.
minNoteDuration :: RealTime
minNoteDuration = RealTime.milliseconds 20

-- * perform

type Events = [LEvent.LEvent T.Event]
type MidiEvents = [LEvent.LEvent Midi.WriteMessage]

-- | Performance state.  This is a snapshot of the state of the various
-- functions in the performance pipeline.  You should be able to resume
-- performance at any point given a RealTime and a State.
--
-- I don't do that anymore, and this is left over from when I cached the
-- performance.  I removed the cache but left the state visible.
data State = State
    { stateChannelize :: ChannelizeState
    , stateAllot :: AllotState
    , statePerform :: PerformState
    } deriving (Eq, Show)

instance Pretty State where
    format (State channelize allot perform) = Pretty.record "State"
        [ ("channelize", Pretty.format channelize)
        , ("allot", Pretty.format allot)
        , ("perform", Pretty.format perform)
        ]

initialState :: State
initialState = State [] emptyAllotState emptyPerformState

type Configs = Map ScoreT.Instrument Config
newtype Config = Config {
    _addrs :: [(Patch.Addr, Maybe Patch.Voices)]
    } deriving (Show, Eq)

emptyConfig :: Config
emptyConfig = Config []

config :: Patch.Config -> Config
config patchConfig = Config
    { _addrs = Patch.configAllocation patchConfig
    }

addrsConfig :: [(Patch.Addr, Maybe Patch.Voices)] -> Config
addrsConfig addrs = emptyConfig { _addrs = addrs }

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event list, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: State -> Configs -> Events -> (MidiEvents, State)
perform state _ [] = ([], state)
perform state configs events = (finalMsgs, finalState)
    where
    finalState = State channelizeState allotState performState
    (eventChannels, channelizeState) =
        channelize (stateChannelize state) configs events
    (eventAllotted, allotState) =
        allot (stateAllot state) configs eventChannels
    (msgs, performState) = performNotes (statePerform state) eventAllotted
    (finalMsgs, _) = postProcess mempty msgs

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
    overlapMap overlapping (channelizeEvent configs) events

-- | This doesn't pay any mind to instrument channel assignments, except as an
-- optimization for instruments with only a single channel.  Channels are
-- actually assigned later by 'allot'.
channelizeEvent :: Configs -> [(T.Event, Channel)] -> T.Event
    -> (Channel, [Log.Msg])
channelizeEvent configs overlapping event =
    case _addrs <$> Map.lookup instName configs of
        Just (_:_:_) -> (chan, [log])
        -- If the event has 0 or 1 addrs I can just give a constant channel.
        -- 'allot' will assign the correct addr, or drop the event if there
        -- are none.
        _ -> (0, [])
    where
    instName = T.patchName (T.eventPatch event)
    -- If there's no shareable channel, make up a channel one higher than the
    -- maximum channel in use.
    chan = fromMaybe (maximum (-1 : map snd overlapping) + 1) maybeChan
    (maybeChan, reasons) = shareableChan overlapping event
    log = Log.msg Log.Warn (Just (T.eventStack event)) $ Text.unlines $
        (T.showShort event <> ": found chan " <> showt maybeChan
            <> ", picked " <> showt chan)
        : map mkmsg reasons
        ++ [showt c <> ": " <> T.showShort e | (e, c) <- overlapping]
    mkmsg (chan, reason) = "can't share with " <> showt chan <> ": " <> reason

-- | Find a channel from the list of overlapping (T.Event, Channel) all of whose
-- events can share with the given event.  Return the rest of the channels and
-- the reason why they can't be used.
shareableChan :: [(T.Event, Channel)] -> T.Event
    -> (Maybe Channel, [(Channel, Text)])
shareableChan overlapping event =
    ( fst <$> List.find (null . snd) unshareableReasons
    , map (second (Text.intercalate "; ")) $
        filter (not . null . snd) unshareableReasons
    )
    where
    unshareableReasons = [(chan, reasons evts) | (evts, chan) <- byChan]
    byChan = Lists.groupSnd overlapping
    reasons = mapMaybe (flip canShareChan event)

-- | Can the two events coexist in the same channel without interfering?
-- The reason this is not commutative is so I can assume the start of @old@
-- is equal to or precedes the start of @new@ and save a little computation.
--
-- This is by far the most finicky function in the whole module, because
-- this is the core decision when multiplexing channels.
canShareChan :: T.Event -> T.Event -> Maybe Text
canShareChan old new = case (initialPitch old, initialPitch new) of
    _ | start >= end -> Nothing
    -- Previously I required that the whole Patch be equal, which caused
    -- notes with different keyswitches to not share channels.  However, they
    -- actually can share channels, though they still can't play
    -- simultaneously.  I need to be as aggressive as possible sharing
    -- channels, especially for instruments with long decays, because any
    -- channel stealing for pitch bends can be very audible.
    _ | instOf old /= instOf new -> Just "instruments differ"
    (Just (initialOld, _), Just (initialNew, _))
        | not (MSignal.pitchesShare inDecay start end
            initialOld (T.eventPitch old) initialNew (T.eventPitch new)) ->
                Just $ "pitch signals incompatible: "
                    <> pretty (T.eventPitch old) <> " /= "
                    <> pretty (T.eventPitch new)
        | not cEqual ->
            Just $ "controls differ: " <> pretty (T.eventControls old)
                <> " /= " <> pretty (T.eventControls new)
        | otherwise -> Nothing
    _ -> Nothing
    where
    instOf = T.patchName . T.eventPatch
    start = T.eventStart new
    -- Note that I add the control_lead_time to the decay of the old note
    -- rather than subtracting it from the start of the new one.  Subtracting
    -- would cause 'MSignal.pitches_share' to check the pitch signal before
    -- the start of the note, which is going to be 0 and mess up sharing.
    end = min (noteEnd new) (noteEnd old) + controlLeadTime
    initialPitch event = eventPitchAt (eventPbRange event)
        event (T.eventStart event)
    -- If the overlap is in the decay of one or both notes, the rules are
    -- slightly different.
    inDecay = T.eventEnd new <= T.eventStart old
        || T.eventEnd old <= T.eventStart new
    cEqual = controlsEqual (T.eventStart new) (T.eventEnd old)
        (T.eventControls old) (T.eventControls new)

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
controlsEqual :: RealTime -> RealTime
    -> Map ScoreT.Control MSignal.Signal
    -> Map ScoreT.Control MSignal.Signal
    -> Bool
controlsEqual start end cs1 cs2 = start >= end || all eq pairs
    where
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controls require their own channel.
    relevant = Map.filterWithKey (\k _ -> Control.isChannelControl k)
    pairs = Maps.pairs (relevant cs1) (relevant cs2)
    eq (_, Lists.Both sig1 sig2) =
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
allot state configs events = (eventAddrs, finalState)
    where
    (finalState, eventAddrs) = List.mapAccumL allot1 state events
    allot1 state (LEvent.Event e) = allotEvent configs state e
    allot1 state (LEvent.Log log) = (state, LEvent.Log log)

data AllotState = AllotState
    -- | Allocated addresses, and when they were last used.
    -- This is used by the voice stealer to figure out which voice is ripest
    -- for plunder.  It also has the AllotKey so the previous allotment can be
    -- deleted.
    { astAvailable :: Map Patch.Addr (RealTime, AllotKey)
    -- | Map input channels to an instrument address in the allocated range.
    -- Once an (inst, chan) pair has been allotted to a particular Addr, it
    -- should keep going to that Addr, as long as voices remain.
    , astAllotted :: Map AllotKey Allotted
    } deriving (Eq, Show)

instance Pretty AllotState where
    format (AllotState available allotted) = Pretty.record "AllotState"
        [ ("available", Pretty.format available)
        , ("allotted", Pretty.format allotted)
        ]

emptyAllotState :: AllotState
emptyAllotState = AllotState Map.empty Map.empty

-- | Channelize makes sure that a (inst, ichan) key identifies events that can
-- share channels.
type AllotKey = (ScoreT.Instrument, Channel)

data Allotted = Allotted
    { _allottedAddr :: Patch.Addr
    -- | End time for each allocated voice.
    , allottedVoices :: [RealTime]
    -- | Maximum length for allotted_voices.
    , _allottedVoiceCount :: Patch.Voices
    } deriving (Eq, Show)

instance Pretty Allotted where
    format (Allotted addr voices voiceCount) = Pretty.record "Allotted"
        [ ("addr", Pretty.format addr)
        , ("voices", Pretty.format voices)
        , ("voice_count", Pretty.format voiceCount)
        ]

-- | Try to find an Addr for the given Event.  If that's impossible, return
-- a log msg.
--
-- If channelize decided that two events have the same channel, then they can
-- go to the same addr, as long as it has voices left.  Otherwise, take over
-- another channel.
allotEvent :: Configs -> AllotState -> (T.Event, Channel)
    -> (AllotState, LEvent.LEvent (T.Event, Patch.Addr))
allotEvent configs state (event, ichan) =
    case expireVoices <$> Map.lookup (inst, ichan) (astAllotted state) of
        -- If there is an already allotted addr with a free voice, add this
        -- event to it.
        Just (Allotted addr voices voiceCount) | length voices < voiceCount ->
            (update Nothing addr voices state, LEvent.Event (event, addr))
        -- Otherwise, steal the oldest already allotted voice.
        -- Delete the old (inst, chan) mapping.
        _ -> case stealAddr configs inst state of
            Just (addr, voiceCount, oldKey) ->
                (update (Just (voiceCount, oldKey)) addr [] state,
                    LEvent.Event (event, addr))
            -- This will return lots of msgs if an inst has no allocation.
            -- A higher level should filter out the duplicates.
            Nothing -> (state, LEvent.Log noAlloc)
    where
    -- Remove voices that have ended.
    expireVoices allotted = allotted
        { allottedVoices =
            filter (> T.eventStart event) (allottedVoices allotted)
        }
    inst = T.patchName $ T.eventPatch event
    update = updateAllotState (inst, ichan) (T.eventEnd event)
    noAlloc = eventWarning event ("no allocation for " <> pretty inst)

-- | Record this addr as now being allotted, and add its voice allocation.
updateAllotState :: (ScoreT.Instrument, Channel) -> RealTime
    -> Maybe (Patch.Voices, Maybe AllotKey) -> Patch.Addr
    -> [RealTime] -> AllotState -> AllotState
updateAllotState instChan end maybeNewAllot addr voices state = state
    { astAvailable = Map.insert addr (end, instChan) (astAvailable state)
    , astAllotted = case maybeNewAllot of
        Just (voiceCount, oldKey) ->
            Map.insert instChan (Allotted addr [end] voiceCount) $
                maybe id Map.delete oldKey (astAllotted state)
        Nothing -> Map.adjust adjust instChan (astAllotted state)
    }
    where adjust allotted = allotted { allottedVoices = end : voices }

-- | Steal the least recently used address for the given instrument, and return
-- how many voices it supports.
--
-- Nothing voices means no limit, and in this case it'll pick a big number.
-- I initially feared keeping track of voice allocation would be wasteful for
-- addrs with no limitation, but profiling revealed no detectable difference.
-- So either it's not important or my profiles are broken.
stealAddr :: Configs -> ScoreT.Instrument -> AllotState
    -> Maybe (Patch.Addr, Patch.Voices, Maybe AllotKey)
stealAddr configs inst state = case _addrs <$> Map.lookup inst configs of
    Just addrVoices -> case Lists.minimumOn (fst . snd) avail of
        Just ((addr, voices), (_, maybeInstChan)) ->
            Just (addr, fromMaybe 10000 voices, maybeInstChan)
        Nothing -> Nothing
        where avail = zip addrVoices (map (mlookup . fst) addrVoices)
    _ -> Nothing
    where
    mlookup addr = case Map.lookup addr (astAvailable state) of
        Nothing -> (0, Nothing)
        Just (end, instChan) -> (end, Just instChan)


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
emptyPerformState :: PerformState
emptyPerformState = (Map.empty, Map.empty)

-- | Given an ordered list of note events, produce the appropriate midi msgs.
-- The input events are ordered, but may overlap.
performNotes :: PerformState -> [LEvent.LEvent (T.Event, Patch.Addr)]
    -> (MidiEvents, PerformState)
performNotes state events =
    (Lists.mergeAscLists mergeKey midiMsgs, finalState)
    where
    (finalState, midiMsgs) = List.mapAccumL go state
        (zip events (drop 1 (List.tails events)))
    go state (LEvent.Log log, _) = (state, [LEvent.Log log])
    go state (LEvent.Event event@(_, addr), future) =
        performNoteInChannel state (findAddr addr future) event
    findAddr addr =
        fmap (T.eventStart . fst) . LEvent.findEvent ((==addr) . snd)

-- | Emit msgs to set the channel state, and msgs for a single note.
performNoteInChannel :: PerformState
    -> Maybe RealTime -- ^ next note with the same addr
    -> (T.Event, Patch.Addr) -> (PerformState, MidiEvents)
performNoteInChannel (addrInst, noteOffMap) nextNoteOn (event, addr) =
    ((addrInst2, Map.insert addr noteOff noteOffMap), msgs)
    where
    (noteMsgs, noteOff) = performNote
        (Map.findWithDefault 0 addr noteOffMap) nextNoteOn event addr
    (chanStateMsgs, addrInst2) = adjustChanState addrInst addr event
    msgs = mergeEvents chanStateMsgs noteMsgs

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
adjustChanState :: AddrInst -> Patch.Addr -> T.Event -> (MidiEvents, AddrInst)
adjustChanState addrInst addr event = case eventMidiKey event of
    Nothing -> ([], newAddrInst)
    Just midiKey ->
        case chanStateMsgs midiKey addr (T.eventStart event) old inst of
            Left err -> ([LEvent.Log $ eventWarning event err], newAddrInst)
            Right msgs -> (map LEvent.Event msgs, newAddrInst)
    where
    newAddrInst = Map.insert addr inst addrInst
    inst = T.eventPatch event
    old = Map.lookup addr addrInst

-- | TODO support program change, I'll have to get ahold of patch_initialize.
chanStateMsgs :: Midi.Key -> Patch.Addr -> RealTime
    -> Maybe T.Patch -> T.Patch
    -> Either Text [Midi.WriteMessage]
chanStateMsgs midiKey addr@(wdev, chan) start maybeOldInst newInst
    | not sameInst = Left $ "program change not supported yet on "
        <> pretty addr <> ": "
            <> pretty (T.patchName <$> maybeOldInst)
            <> " -> " <> pretty (T.patchName newInst)
    | not (sameKeyswitches maybeOldInst newInst) = Right $
        keyswitchMessages midiKey maybeOldInst newInst wdev chan start
    | otherwise = Right []
    where
    sameInst = case maybeOldInst of
        Nothing -> True -- when pchange is supported I can assume false
        Just o -> T.patchName o == T.patchName newInst

sameKeyswitches :: Maybe T.Patch -> T.Patch -> Bool
sameKeyswitches maybeOld new =
    go (maybe [] T.patchKeyswitches maybeOld) (T.patchKeyswitches new)
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
keyswitchMessages :: Midi.Key -> Maybe T.Patch
    -> T.Patch -> Midi.WriteDevice -> Midi.Channel -> RealTime
    -> [Midi.WriteMessage]
keyswitchMessages midiKey maybeOldInst newInst wdev chan start =
    prevKsOff ++ newKsOn
    where
    -- Hold keyswitches have to stay down for the for the duration they are in
    -- effect.  So they just emit a NoteOn.  If I am switching keyswitches
    -- and the previous one was a hold-keyswitch, then it must be down, and
    -- I have to emit a NoteOff for it.
    prevKsOff = Maybe.fromMaybe [] $ do
        old <- maybeOldInst
        guard (T.patchHoldKeyswitches old)
        -- I apply the adjacent_note_gap to the ks note off too.  It's probably
        -- unnecessary, but this way the note and the ks go off at the same
        -- time.
        pure $ mapMaybe (ksOff (start-adjacentNoteGap))
            (T.patchKeyswitches old)

    newKs = T.patchKeyswitches newInst
    isHold = T.patchHoldKeyswitches newInst
    ksStart = start - keyswitchLeadTime

    newKsOn
        | isHold = map (ksOn ksStart) newKs
        | otherwise = map (ksOn ksStart) newKs
            ++ mapMaybe (ksOff (ksStart+minNoteDuration)) newKs
    ksOn ts = mkmsg ts . Patch.keyswitchOn midiKey
    ksOff ts = fmap (mkmsg ts) . Patch.keyswitchOff
    mkmsg ts msg = Midi.WriteMessage wdev ts (Midi.ChannelMessage chan msg)

-- ** perform note

-- | Emit MIDI for a single event.
performNote :: RealTime -> Maybe RealTime -- ^ next note with the same addr
    -> T.Event -> Patch.Addr -> (MidiEvents, RealTime) -- ^ (msgs, note_off)
performNote prevNoteOff nextNoteOn event addr =
    case eventMidiKey event of
        Nothing ->
            ( [LEvent.Log $ eventWarning event "event has no valid pitches"]
            , prevNoteOff
            )
        Just midiKey -> (mergeEvents controlMsgs noteMsgs, noteOff)
            where
            (noteMsgs, noteOff) = _noteMsgs midiKey
            controlMsgs = _controlMsgs noteOff midiKey
    where
    -- 'perform_note_msgs' and 'perform_control_msgs' are really part of one
    -- big function.  Splitting it apart led to a bit of duplicated work but
    -- hopefully it's easier to understand this way.
    _noteMsgs = performNoteMsgs event addr
    _controlMsgs = performControlMsgs prevNoteOff nextNoteOn event addr

-- | Perform the note on and note off.
performNoteMsgs :: T.Event -> Patch.Addr -> Midi.Key -> (MidiEvents, RealTime)
performNoteMsgs event (dev, chan) midiNn = (events, noteOff)
    where
    events =
        [ LEvent.Event $ chanMsg noteOn $ Midi.NoteOn midiNn $
            -- NoteOn with 0 velocity is interpreted as NoteOff.  This messes
            -- up notes that are supposed to start from 0, e.g. via breath
            -- control.
            max 1 (Control.valToCval (T.eventStartVelocity event))
        , LEvent.Event $ chanMsg noteOff $ Midi.NoteOff midiNn $
            Control.valToCval (T.eventEndVelocity event)
        ]
    noteOn = T.eventStart event
    -- Subtract the adjacent_note_gap, but still have at least
    -- min_note_duration.
    noteOff = max (noteOn + minNoteDuration)
        (T.eventEnd event - adjacentNoteGap)
    chanMsg pos msg = Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

-- | Perform control change messages.
performControlMsgs :: RealTime -> Maybe RealTime -> T.Event -> Patch.Addr
    -> RealTime -> Midi.Key -> MidiEvents
performControlMsgs prevNoteOff nextNoteOn event (dev, chan) noteOff
        midiKey =
    map LEvent.Event controlMsgs ++ map LEvent.Log warns
    where
    controlMsgs = mergeMessages $
        map (map chanMsg) (pitchPosMsgs : controlPosMsgs)
    controlSigs = Map.toList (T.eventControls event)
    cmap = T.patchControlMap (T.eventPatch event)
    -- |===---
    --      -|===---
    -- Drop controls that would overlap with the next note on.
    -- The controls after the note off are clipped to make room for the next
    -- note's leading controls.  Lead time will get pushed forward if the
    -- note really is adjacent, but if it's supposedly off then it's lower
    -- priority and I can clip off its controls.  Otherwise, the lead-time
    -- controls get messed up by controls from the last note.
    controlEnd = case nextNoteOn of
        Nothing -> Nothing
        Just next -> Just $ max noteOff (next - controlLeadTime)

    (controlPosMsgs, clipWarns) = unzip $
        map (performControl cmap prevNoteOff noteOn controlEnd midiKey)
            controlSigs
    pitchPosMsgs = performPitch (eventPbRange event)
        midiKey prevNoteOff noteOn controlEnd (T.eventPitch event)
    noteOn = T.eventStart event

    warns = concatMap (makeClipWarnings event)
        (zip (map fst controlSigs) clipWarns)
    chanMsg (pos, msg) =
        Midi.WriteMessage dev pos (Midi.ChannelMessage chan msg)

eventPbRange :: T.Event -> Control.PbRange
eventPbRange = T.patchPitchBendRange . T.eventPatch

-- | Get pitch at the given point of the signal.
--
-- The pitch bend always tunes upwards from the tempered note.  It would be
-- slicker to use a negative offset if the note is eventually going above
-- unity, but that's too much work.
eventPitchAt :: Control.PbRange -> T.Event -> RealTime
    -> Maybe (Midi.Key, Midi.PitchBendValue)
eventPitchAt pbRange event pos =
    Control.pitchToMidi pbRange $
        Pitch.NoteNumber $ MSignal.at pos (T.eventPitch event)

-- | Get the Midi.Key that will be used for the event, without pitch bend.
eventMidiKey :: T.Event -> Maybe Midi.Key
eventMidiKey event =
    fst <$> eventPitchAt (eventPbRange event) event (T.eventStart event)

type ClipRange = (RealTime, RealTime)

makeClipWarnings :: T.Event -> (ScoreT.Control, [ClipRange]) -> [Log.Msg]
makeClipWarnings event (control, clipWarns) =
    [ eventWarning event $ pretty control <> " clipped: "
        <> pretty s <> "--" <> pretty e
    | (s, e) <- clipWarns
    ]

performPitch :: Control.PbRange -> Midi.Key -> RealTime -> RealTime
    -> Maybe RealTime -> MSignal.Signal -> [(RealTime, Midi.ChannelMessage)]
performPitch pbRange nn prevNoteOff start end sig =
    [ (x, Midi.PitchBend (Control.pbFromNn pbRange nn (Pitch.NoteNumber y)))
    | (x, y) <- posVals
    ]
    where posVals = performSignal prevNoteOff start end sig

-- | Return the (pos, msg) pairs, and whether the signal value went out of the
-- allowed control range, 0--1.
performControl :: Control.ControlMap -> RealTime -> RealTime -> Maybe RealTime
    -> Midi.Key -> (ScoreT.Control, MSignal.Signal)
    -> ([(RealTime, Midi.ChannelMessage)], [ClipRange])
performControl cmap prevNoteOff start end midiKey (control, sig) =
    case Control.controlConstructor cmap control midiKey of
        Nothing -> ([], [])
        Just ctor -> ([(x, ctor y) | (x, y) <- posVals], clipWarns)
    where
    -- The signal should already be trimmed to the event range, except that,
    -- as per the behaviour of MSignal.drop_before, it may have a leading
    -- sample.  I can drop that since it's handled specially by
    -- 'perform_signal'.
    posVals = performSignal prevNoteOff start end clipped
    (clipped, outOfBounds) = MSignal.clipBounds 0 1 sig
    clipWarns = [(s, e) | (s, e) <- outOfBounds]

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
performSignal :: RealTime -> RealTime -> Maybe RealTime -> MSignal.Signal
    -> [(RealTime, MSignal.Y)]
performSignal prevNoteOff start end sig = initial : pairs
    where
    -- The signal should already be trimmed to the event start, except that
    -- it may have a leading sample, due to 'MSignal.drop_before'.
    pairs = Lists.dropInitialDups fst $
        dropWhile ((<=start) . fst) $ MSignal.toPairs $
        MSignal.dropBefore start $ maybe id MSignal.dropAtAfter end sig
    -- Don't go before the previous note, but don't go after the start of this
    -- note, in case the previous note ends after this one begins.
    tweakedStart = min (start - minControlLeadTime) $
        max (min prevNoteOff start) (start - controlLeadTime)
    initial = (tweakedStart, MSignal.at start sig)

-- * post process

type PostprocState = Map Patch.Addr AddrState

-- | Keep a running state for each channel and drop duplicate msgs.
type AddrState = (Maybe Midi.PitchBendValue, Map Midi.Control Midi.ControlValue)

-- | Some context free post-processing on the midi stream.
postProcess :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
postProcess state = first avoidOverlaps . dropDupControls state . resort

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
avoidOverlaps :: MidiEvents -> MidiEvents
avoidOverlaps events = concat $ snd $ LEvent.mapAccum move initial events
    where
    initial :: Map (Midi.WriteDevice, Midi.Channel, Midi.Key) NoteOnState
    initial = Map.empty

    move state wmsg _ = case msgKey wmsg of
        Just (skey@(_, chan, key), msg) -> case msg of
            Midi.NoteOff {} ->
                (state2, map (makeMsg wmsg . Midi.ChannelMessage chan) offs)
                where (state2, offs) = noteOff skey msg state
            Midi.NoteOn {} -> (state2, consIf emitOff off [wmsg])
                where
                off = makeMsg wmsg $
                    Midi.ChannelMessage chan $ Midi.NoteOff key 100
                (state2, emitOff) = noteOn skey state
            _ -> (state, [wmsg])
        Nothing -> (state, [wmsg])
    -- Overlapping notes with the same (dev, chan, key) are affected.
    msgKey wmsg = case Midi.wmsgMsg wmsg of
        Midi.ChannelMessage chan msg@(Midi.NoteOn key _) ->
            Just ((dev, chan, key), msg)
        Midi.ChannelMessage chan msg@(Midi.NoteOff key _) ->
            Just ((dev, chan, key), msg)
        _ -> Nothing
        where dev = Midi.wmsgDev wmsg

    -- When I see NoteOn, emit NoteOff if a note with that key is already on.
    noteOn skey state = case Map.lookup skey state of
        Just Playing -> (Map.insert skey SuppressNoteOff state, True)
        _ -> (Map.insert skey Playing state, False)

    -- When I see a NoteOff, drop it if SuppressNoteOff or there's nothing
    -- playing, otherwise emit it normally and update the state.
    noteOff skey msg state = case Map.lookup skey state of
        Just SuppressNoteOff -> (Map.insert skey Playing state, [])
        Just NotPlaying -> (state, [])
        Just Playing -> (Map.delete skey state, [msg])
        Nothing -> (state, [])

    makeMsg wmsg = Midi.WriteMessage (Midi.wmsgDev wmsg) (Midi.wmsgTs wmsg)

data NoteOnState = Playing | NotPlaying
    -- | This means the next NoteOff will get skipped, but the one after that
    -- emitted.
    | SuppressNoteOff
    deriving (Show)

consIf :: Bool -> a -> [a] -> [a]
consIf True x = (x:)
consIf False _ = id

-- | Having to deal with Log is ugly... can't I get that out with fmap?
dropDupControls :: PostprocState -> MidiEvents -> (MidiEvents, PostprocState)
dropDupControls state [] = ([], state)
dropDupControls state (log@(LEvent.Log _) : events) =
    let (rest, finalState) = dropDupControls state events
    in (log : rest, finalState)
dropDupControls state (event@(LEvent.Event wmsg) : wmsgs) = case wmsg of
    Midi.WriteMessage dev _ (Midi.ChannelMessage chan cmsg) ->
        let addr = (dev, chan)
            addrState = Map.lookup addr state
            (keep, addrState2) = analyzeMsg addrState cmsg
            state2 = maybe state (\s -> Map.insert addr s state) addrState2
            (rest, finalState) = dropDupControls state2 wmsgs
        in (if keep then event : rest else rest, finalState)
    _ -> dropDupControls state wmsgs

analyzeMsg :: Maybe AddrState -> Midi.ChannelMessage -> (Bool, Maybe AddrState)
analyzeMsg Nothing msg = case msg of
    Midi.PitchBend v -> (True, Just (Just v, Map.empty))
    Midi.ControlChange c v -> (True, Just (Nothing, Map.singleton c v))
    _ -> (True, Nothing)
analyzeMsg (Just (pbVal, cmap)) msg = case msg of
    Midi.PitchBend v
        | Just v == pbVal -> (False, Nothing)
        | otherwise -> (True, Just (Just v, cmap))
    Midi.ControlChange c v
        | Just v == Map.lookup c cmap -> (False, Nothing)
        | otherwise -> (True, Just (pbVal, Map.insert c v cmap))
    _ -> (True, Nothing)

-- | Sort almost-sorted MidiEvents.  Events may be out of order by
-- as much as control_lead_time.  This happens because 'perform_signal' adds
-- events between 0--control_lead_time before the note, which can violate the
-- precondition of 'Lists.mergeAscLists'.
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
        (pre, post) = break ((> Midi.wmsgTs event - interval) . Midi.wmsgTs)
            (Lists.insertOn Midi.wmsgTs event collect)
    interval = controlLeadTime


-- * event

noteBegin :: T.Event -> RealTime
noteBegin event = T.eventStart event - controlLeadTime

-- | The end of an event after taking decay into account.  The note shouldn't
-- be sounding past this time.
noteEnd :: T.Event -> RealTime
noteEnd event = T.eventEnd event
    + fromMaybe T.defaultDecay (T.patchDecay (T.eventPatch event))

-- * util

-- | Merge an unsorted list of sorted lists of midi messages.
mergeMessages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
mergeMessages = Lists.mergeLists Midi.wmsgTs

mergeEvents :: MidiEvents -> MidiEvents -> MidiEvents
mergeEvents = Lists.mergeOn mergeKey

mergeKey :: LEvent.LEvent Midi.WriteMessage -> RealTime
mergeKey (LEvent.Log _) = 0
mergeKey (LEvent.Event msg) = Midi.wmsgTs msg


-- | Map the given function across the events, passing it previous events it
-- overlaps with.  The previous events passed to the function are paired with
-- its previous return values on those events.  The overlapping events are
-- passed in reverse order, so the most recently overlapping is first.
overlapMap :: [(T.Event, a)] -> ([(T.Event, a)] -> T.Event
    -> (a, [Log.Msg])) -> Events
    -> ([LEvent.LEvent (T.Event, a)], [(T.Event, a)])
    -- ^ (output for each event, final overlapping state)
overlapMap initial = go initial
    where
    go prev _ [] = ([], prev)
    go prev f (LEvent.Log log : events) = (LEvent.Log log : rest, finalState)
        where (rest, finalState) = go prev f events
    go prev f (LEvent.Event e : events) =
        (LEvent.Event (e, val) : logEvents ++ vals, finalState)
        where
        start = noteBegin e
        overlapping = takeWhile ((> start) . noteEnd . fst) prev
        (val, logs) = f overlapping e
        logEvents = if logging then map LEvent.Log logs else []
        (vals, finalState) = go ((e, val) : overlapping) f events

eventWarning :: HasCallStack => T.Event -> Text -> Log.Msg
eventWarning event = Log.msg Log.Warn (Just (T.eventStack event))

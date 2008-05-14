{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
midi events.
-}
module Perform.Midi.Perform where
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Util.Seq as Seq
import qualified Util.Data

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event lists, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: Instrument.Config -> [Event] -> [Midi.WriteMessage]
perform config = perform_notes . allot config . channelize


-- * perform notes

-- | Given an ordered list of note events, produce the apprapriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: [(Event, Instrument.Addr)] -> [Midi.WriteMessage]
perform_notes events = post_process $
    _perform_notes (Timestamp.Timestamp 0) [] events

-- TODO use DList
_perform_notes _ overlapping [] = overlapping
_perform_notes ts overlapping ((event, addr):events) =
    play ++ _perform_notes first_ts not_yet events
    where
    -- This find could demand lots or all of events if the
    -- (instrument, chan) doesn't play for a long time.  It only happens once
    -- at gaps though, but if it's a problem I can abort after n seconds, which
    -- would possibly save generating controllers there, at the cost of
    -- assuming decays are < n sec.
    next_on_ts = case List.find ((==addr) . snd) events of
        Nothing -> event_end event -- TODO plus decay_time?
        Just (evt, _chan) -> event_start evt
    msgs = perform_note next_on_ts event addr
    -- TODO unprotected head
    first_ts = Midi.wmsg_ts (head msgs)
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

-- Bubblesort.  It's lazy and sorts almost-sorted stuff well.
sort2 cmp (a:b:zs) = case cmp a b of
    GT -> b : sort2 cmp (a:zs)
    _ -> a : sort2 cmp (b:zs)
sort2 cmp xs = List.sortBy cmp xs


perform_note :: Timestamp.Timestamp -> Event -> Instrument.Addr
    -> [Midi.WriteMessage]
perform_note next_on_ts event (dev, chan) =
    -- If a msg and controller have the same ts, the controller goes first.
    merge_messages [controller_msgs2, note_msgs]
    where
    note_msgs = [chan_msg on_ts (Midi.NoteOn midi_nn on_vel),
        chan_msg off_ts (Midi.NoteOff midi_nn off_vel)]
    chan_msg ts msg = Midi.WriteMessage dev ts (Midi.ChannelMessage chan msg)
    on_ts = event_start event
    off_ts = on_ts + event_duration event
    on_vel = 100
    off_vel = 100
    midi_nn = Pitch.midi_nn (event_pitch event)
    -- Cents are always measure upwards from the tempered note.  It would be
    -- slicker to use a negative offset if the note is eventually going above
    -- unity, but that's too much work.
    pb_range = Instrument.inst_pitch_bend_range (event_instrument event)
    pb_offset = Controller.cents_to_pb_val pb_range
        (Pitch.cents (event_pitch event))

    mkmsg (ts, msg) = Midi.WriteMessage dev ts msg
    controller_msgs = merge_messages $ map (map mkmsg) $
        map (perform_controller on_ts next_on_ts chan)
            (Map.assocs (event_controls event))
    controller_msgs2 = map (add_pitch_bend pb_offset) controller_msgs

-- | Add offset to pitch bend msgs, passing others through.
add_pitch_bend :: Int -> Midi.WriteMessage -> Midi.WriteMessage
add_pitch_bend offset msg = msg { Midi.wmsg_msg = msg' }
    where
    msg' = case (Midi.wmsg_msg msg) of
        Midi.ChannelMessage chan (Midi.PitchBend n) ->
            -- TODO warn about overflow
            Midi.ChannelMessage chan (Midi.PitchBend (n+offset))
        msg -> msg

-- TODO filter duplicate msgs
-- It would be more general to do this at a higher level, but I have to do it
-- when I have a definite bound on the output, so I don't wait forever on the
-- last sample.
perform_controller :: Timestamp.Timestamp -> Timestamp.Timestamp -> Midi.Channel
    -> (Controller.Controller, Signal.Signal)
    -> [(Timestamp.Timestamp, Midi.Message)]
perform_controller start_ts end_ts chan (controller, sig) =
    case Controller.controller_constructor controller of
        Nothing -> []
        Just ctor -> [(ts, Midi.ChannelMessage chan (ctor val))
            | (ts, val) <- ts_vals]
    where
    ts_vals = Signal.sample sig start_ts end_ts

-- | Merge the sorted midi messages into a single sorted list.
merge_messages :: [[Midi.WriteMessage]] -> [Midi.WriteMessage]
merge_messages = foldr (Seq.merge_by (compare `on` Midi.wmsg_ts)) []

-- * channelize

-- | Assign channels.  Events will be merged into the same channel where they
-- can be.
channelize :: [Event] -> [(Event, Channel)]
channelize events = overlap_map channelize_event events

channelize_event :: [(Event, Channel)] -> Event -> Channel
channelize_event overlapping event = chan
    where
    chan_of = snd
    chan = maybe (maximum ((-1) : map chan_of overlapping) + 1) id
        (shareable_chan overlapping event)

-- Find a channel, all of whose events can share.
shareable_chan :: [(Event, Channel)] -> Event -> Maybe Channel
shareable_chan overlapping event = fmap fst (List.find all_share by_chan)
    where
    by_chan = Seq.keyed_group_with snd overlapping
    all_share (chan, evt_chans) = all (can_share_chan event) (map fst evt_chans)

-- | Can the two events coexist in the same channel without interfering?
can_share_chan :: Event -> Event -> Bool
can_share_chan event1 event2 =
    event_instrument event1 == event_instrument event2
    -- Two events with the same pitch can never go on the same channel.
    && event_pitch event1 /= event_pitch event2
    -- If they have different tunings, they'll be pitch-bent by
    -- perform_note.
    && cents event1 == cents event2
    && controls_equal (relevant event1) (relevant event2)
    where
    cents = Pitch.cents . event_pitch
    -- Velocity and aftertouch are per-note addressable in midi, but the rest
    -- of the controllers require their own channel.
    relevant event = filter (Controller.is_channel_controller . fst)
        (Map.assocs (event_controls event))

-- TODO make this more efficient?  I only actually need to compare the areas
-- covered by (event_on_ts, next_on_ts)
-- implement! undefined
controls_equal :: [(Controller.Controller, Signal.Signal)]
    -> [(Controller.Controller, Signal.Signal)] -> Bool
controls_equal c1 c2 = {- trace ("\n*trace: "++show (c1, c2)++"\n") $-} c1 == c2


-- * allot channels

-- |
-- Events with instruments that have no address allocation in the config
-- will be silently dropped.  A higher level should have warned about those.
allot :: Instrument.Config -> [(Event, Channel)] -> [(Event, Instrument.Addr)]
allot config events = Maybe.catMaybes $
    map_state allot_event (initial_allot_state config) events

data AllotState = AllotState {
    -- | Allocated addresses, and when they were last used.
    ast_available :: Map.Map Instrument.Addr Timestamp.Timestamp
    -- | Map arbitrary input channels to an instrument address in the allocated
    -- range.
    , ast_map :: Map.Map (Instrument.Instrument, Channel) Instrument.Addr
    -- | Addresses allocated to each instrument.
    , ast_alloc :: Map.Map Instrument.Instrument [Instrument.Addr]
    } deriving (Show)
initial_allot_state config = AllotState Map.empty Map.empty
    (Util.Data.invert_map (Instrument.config_alloc config))

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
steal_addr inst state = case Map.lookup inst (ast_alloc state) of
    Just addrs -> let avail = zip addrs (map mlookup addrs)
        in if null avail then Nothing -- no addrs assigned to this instrument
            else let (addr, _) = List.minimumBy (compare `on` snd) avail
            in Just addr
    _ -> Nothing
    where
    mlookup addr = Map.findWithDefault
        (Timestamp.Timestamp 0) addr (ast_available state)

-- * data

data Event = Event {
    event_instrument :: Instrument.Instrument
    , event_start :: Timestamp.Timestamp
    , event_duration :: Timestamp.Timestamp
    , event_pitch :: Pitch.Pitch
    , event_controls :: Map.Map Controller.Controller Signal.Signal
    -- original (TrackId, TrackPos) for errors
    -- event_stack :: [(Track.TrackId, TrackPos)]
    } -- deriving (Show)

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
        overlapping = takeWhile ((> start) . event_end . fst) prev
        val = f overlapping e

map_state :: (st -> a -> (st, b)) -> st -> [a] -> [b]
map_state _ _ [] = []
map_state f state (x:xs) = let (state', v) = f state x
    in v : map_state f state' xs

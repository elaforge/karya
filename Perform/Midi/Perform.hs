{- | Main entry point for Perform.Midi.  Render Deriver output down to actual
midi events.
-}
module Perform.Midi.Perform where
import Data.Function
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Util.Seq as Seq

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Timestamp as Timestamp

import qualified Perform.Midi.Controller as Controller
import qualified Perform.Midi.Instrument as Instrument

import Debug.Trace

{-
    Can the deriver group by instrument and still remain lazy?  I guess the
    list of allowed instruments is known in advance since there must be
    a Instrument -> WriteDevice mapping.  On the other hand, this would make
    loading other blocks tricky because you need to somehow join the instrument
    maps.  Also, instrument device mapping really is static... but the channel
    allocation probably isn't.

    I guess it's ok to decide that channel allocation will be manual and it
    will throw a warning when it hits an instrument that has no allocation
    (this could also be a reasonable way to mute an instrument) and not send it
    to Perform.

    I may want something more flexible where I can change channel allocation
    within the piece and have initialization sent automatically, but it sounds
    complicated and I don't need that yet.

    TODO: need a mechanism to emit warnings
    perform_note does pitch bend, warns if it's out of range
    perform_instrument has an error if no dev for instrument
    or I could possibly stick this all at the Derive level
-}

-- | Render instrument tracks down to midi messages, sorted in timestamp order.
-- This should be non-strict on the event lists, so that it can start producing
-- MIDI output as soon as it starts processing Events.
perform :: Instrument.Config -> [(Instrument.Instrument, [Event])]
    -> [Midi.WriteMessage]
perform config inst_events = undefined


perform_instrument :: Instrument.Config -> Instrument.Instrument
    -> [Event] -> [Midi.WriteMessage]
perform_instrument config inst events = [(dev, ts, msg) | (ts, msg) <- msgs]
    where
    chans = Map.keys (Map.filter (==inst) (Instrument.config_channels config))
    event_chans = allot_channels chans (channelize events)
    msgs = perform_notes (Instrument.inst_pitch_bend_range inst) event_chans
    -- TODO
    Just dev = Map.lookup inst (Instrument.config_devices config)


-- * perform notes

type TsMessage = (Timestamp.Timestamp, Midi.Message)

-- | Given an ordered list of note events, produce the apprapriate midi msgs.
-- The input events are ordered, but may overlap.
perform_notes :: Instrument.PbRange -> [(Event, Midi.Channel)] -> [TsMessage]
perform_notes pb events = post_process $
    _perform_notes pb (Timestamp.Timestamp 0) [] events

-- TODO use DList
_perform_notes _ _ overlapping [] = overlapping
_perform_notes pb_range ts overlapping ((event, chan):events) =
    play ++ _perform_notes pb_range first_ts not_yet events
    where
    next_on_ts = case List.find ((==chan) . snd) events of
        Nothing -> event_end event -- TODO plus decay_time?
        Just (evt, _chan) -> event_start evt
    msgs = perform_note pb_range next_on_ts event chan
    first_ts = ts_of (head msgs)
    (play, not_yet) = List.partition ((<= first_ts) . ts_of)
        (merge_messages [overlapping, msgs])
    ts_of = fst

post_process = reorder_control_messages . drop_duplicates (==)

drop_duplicates = Seq.drop_dups

-- | If a control message and a note message happen at the same time, the
-- control should go first, just to make sure the synth doesn't make a popping
-- noise.
reorder_control_messages :: [TsMessage] -> [TsMessage]
reorder_control_messages = sort2 (compare `on` msg_key)
    where
    msg_key (ts, (Midi.ChannelMessage _ (Midi.ControlChange _ _))) = (ts, 0)
    msg_key (ts, _) = (ts, 1)

-- Bubblesort.  It's lazy and sorts almost-sorted stuff well.
sort2 cmp (a:b:zs) = case cmp a b of
    GT -> b : sort2 cmp (a:zs)
    _ -> a : sort2 cmp (b:zs)
sort2 cmp xs = List.sortBy cmp xs


perform_note :: Instrument.PbRange -> Timestamp.Timestamp -> Event
    -> Midi.Channel -> [TsMessage]
perform_note pb_range next_on_ts event chan =
    -- If a msg and controller have the same ts, the controller goes first.
    merge_messages [controller_msgs2, note_msgs]
    where
    note_msgs = [(on_ts, chan_msg (Midi.NoteOn midi_nn on_vel)),
        (off_ts, chan_msg (Midi.NoteOff midi_nn off_vel))]
    chan_msg = Midi.ChannelMessage chan
    on_ts = event_start event
    off_ts = on_ts + event_duration event
    on_vel = 100
    off_vel = 100
    midi_nn = Pitch.midi_nn (event_pitch event)
    -- Cents are always measure upwards from the tempered note.  It would be
    -- slicker to use a negative offset if the note is eventually going above
    -- unity, but that's too much work.
    pb_offset = Controller.cents_to_pb_val pb_range
        (Pitch.cents (event_pitch event))
    controller_msgs = merge_messages $
        map (perform_controller on_ts next_on_ts chan)
            (Map.assocs (event_controls event))
    controller_msgs2 = map (add_pitch_bend pb_offset) controller_msgs

-- | Add offset to pitch bend msgs, passing others through.
add_pitch_bend :: Int -> TsMessage -> TsMessage
add_pitch_bend offset (ts, msg) = (ts, msg')
    where
    msg' = case msg of
        Midi.ChannelMessage chan (Midi.PitchBend n) ->
            -- TODO warn about overflow
            Midi.ChannelMessage chan (Midi.PitchBend (n+offset))
        _ -> msg

-- TODO filter duplicate msgs
-- It would be more general to do this at a higher level, but I have to do it
-- when I have a definite bound on the output, so I don't wait forever on the
-- last sample.
perform_controller :: Timestamp.Timestamp -> Timestamp.Timestamp -> Midi.Channel
    -> (Controller.Controller, Signal.Signal) -> [TsMessage]
perform_controller start_ts end_ts chan (controller, sig) =
    case Controller.controller_constructor controller of
        Nothing -> []
        Just ctor -> [(ts, Midi.ChannelMessage chan (ctor val))
            | (ts, val) <- ts_vals]
    where
    ts_vals = Signal.sample sig start_ts end_ts

merge_messages :: [[TsMessage]] -> [TsMessage]
merge_messages = foldr (Seq.merge_by (compare `on` fst)) []

-- * channelize

-- | Merge channels where they can be.
channelize :: [Event] -> [(Event, Channel)]
channelize events = overlap_map channelize_event fst [] events

channelize_event :: [(Event, Channel)] -> Event -> (Event, Channel)
channelize_event overlapping event = (event, chan)
    where
    chan_of = snd
    chan = maybe (maximum (0 : map chan_of overlapping) + 1) id
        (fmap chan_of (List.find ((can_share_chan event) . fst) overlapping))

-- | Can the two events coexist in the same channel without interfering?
can_share_chan :: Event -> Event -> Bool
can_share_chan event1 event2 =
    -- Two events with the same pitch can never go on the same channel.
    event_pitch event1 /= event_pitch event2
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

allot_channels :: [Midi.Channel] -> [(Event, Channel)]
    -> [(Event, Midi.Channel)]
allot_channels chans events = map_state allot state events
    where
    state = (Map.fromList [(chan, ts0) | chan <- chans], Map.empty)
    ts0 = Timestamp.Timestamp 0

-- (ochan -> ts, ichan -> ochan)
type AllotState =
    (Map.Map Midi.Channel Timestamp.Timestamp, Map.Map Channel Midi.Channel)
allot :: AllotState -> (Event, Channel) -> (AllotState, (Event, Midi.Channel))
allot (available, chan_map) (event, ichan) = case Map.lookup ichan chan_map of
    Just ochan -> ((update_ts ochan, chan_map), (event, ochan))
    Nothing ->
        let oldest = fst $ Seq.mhead (error "allot to 0 channels") $
                List.sortBy (compare `on` snd) (Map.assocs available)
        in ((update_ts oldest, Map.insert ichan oldest chan_map),
            (event, oldest))
    where
    update_ts chan = Map.insert chan (event_end event) available


-- * data

-- TODO: controls are also events, with start and dur
-- TODO: events have IDs
data Event = Event {
    event_start :: Timestamp.Timestamp
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

overlap_map :: ([a] -> Event -> a) -> (a -> Event) -> [a] -> [Event] -> [a]
overlap_map _ _ _ [] = []
overlap_map f event_of prev (event:events) =
    val : overlap_map f event_of (val:prev) events
    where
    start = event_start event
    overlapping = takeWhile ((> start) . event_end . event_of) prev
    val = f prev event

map_state :: (st -> a -> (st, b)) -> st -> [a] -> [b]
map_state _ _ [] = []
map_state f state (x:xs) = let (state', v) = f state x
    in v : map_state f state' xs

{- | Convert incoming MIDI (and other inputs) to the internal note
    representation.  This has two purposes: note entry, and midi thru.

    This module is, in a way, dual to "Perform.Midi.Perform".  It takes MIDI
    input to the internal 'Input' representation and back again, while Perform
    takes the internal representation, in the form of
    'Perform.Midi.Perform.Event', to MIDI output.

    The overlapping part is that this module maps 'Derive.Score.Controller's to
    and from MIDI while Play uses 'Perform.Midi.Controller.Controller'.  They
    use the same controller names, though, so I can reuse code from Controller.

    One significant difference between 'Input' and MIDI is that MIDI supports
    two levels of controller addressing: note and channel, while 'Input' can
    only represent note addressing through 'NoteId'.  MIDI controllers almost
    all apply at the channel level, but of course these controllers all apply
    at the note level.  The result is that a MIDI controller that on a keyboard
    affects the whole channel will only affect the last played note here.  When
    the input is converted back to MIDI it may wind up sharing a channel
    anyway, at which point the controller will go back to being channel global,
    but if the instrument has multiple channels, I try to distribute between
    them to keep note_ids on separate channels.  This way, multiple channel
    emitting controllers can be mapped to multiple channel using instruments.

    This is basically a simplified version of the channel allocation algorithm
    in "Perform.Midi.Perform".  It's hard to reuse that algorithm directly
    because this one has to operate in realtime and can't see which controllers
    the note is going to use.
-}
module Cmd.InputNote where
import qualified Data.Map as Map
import qualified Util.Map as Map
import qualified Midi.Midi as Midi

import qualified Derive.Score as Score
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal
import qualified Perform.Midi.Controller as Controller


data Input =
    -- | The InputKey and Val (velocity) could be sent separately, but that
    -- would make converting this back into midi for thru harder.
    NoteOn NoteId Pitch.InputKey Signal.Val
    | NoteOff NoteId Signal.Val
    | Control NoteId Score.Controller Signal.Val
    -- | Pitch could also be a Control, but this way the pitch is typed.
    | PitchChange NoteId Pitch.InputKey
    deriving (Eq, Show)

input_id :: Input -> NoteId
input_id input = case input of
    NoteOn note_id _ _ -> note_id
    NoteOff note_id _ -> note_id
    Control note_id _ _ -> note_id
    PitchChange note_id _ -> note_id

newtype NoteId = NoteId Int deriving (Eq, Ord, Show)
key_to_id :: Midi.Key -> NoteId
key_to_id = NoteId . fromIntegral
id_to_key :: NoteId -> Midi.Key
id_to_key (NoteId key) = fromIntegral key

-- * from midi

type Addr = (Midi.ReadDevice, Midi.Channel)

data ControllerState = ControllerState
    { state_note_id :: Map.Map Addr NoteId -- ^ last note_id
    , state_pb :: Map.Map Addr Midi.PitchBendValue -- ^ last pb
    , state_pb_range :: Controller.PbRange
    } deriving (Eq, Show)

empty_state :: Controller.PbRange -> ControllerState
empty_state pb_range = ControllerState Map.empty Map.empty pb_range

from_midi :: ControllerState -> Midi.ReadDevice -> Midi.Message
    -> Maybe (Input, ControllerState)
from_midi state rdev (Midi.ChannelMessage chan chan_msg) = case maybe_input of
        Nothing -> Nothing
        Just input -> Just (input, update_state addr chan_msg state)
    where
    addr = (rdev, chan)
    last_pb = Map.get 0 addr (state_pb state)
    with_last_id f = fmap f (Map.lookup addr (state_note_id state))
    maybe_input = case chan_msg of
        Midi.NoteOn key vel -> Just $
            NoteOn (key_to_id key) (to_pitch last_pb key) (to_val vel)
        Midi.NoteOff key vel -> Just $
            NoteOff (key_to_id key) (to_val vel)
        Midi.ControlChange cc val -> with_last_id $ \last_id ->
            Control last_id (cc_to_controller cc) (to_val val)
        Midi.PitchBend val -> with_last_id $ \last_id ->
            PitchChange last_id (to_pitch val (id_to_key last_id))
        Midi.Aftertouch key val -> Just $
            Control (key_to_id key) c_poly_aftertouch (to_val val)
        Midi.ChannelPressure val -> with_last_id $ \last_id ->
            Control last_id c_aftertouch (to_val val)
        _ -> Nothing
    to_pitch = pb_to_input (state_pb_range state)
    to_val v = fromIntegral v / 127
from_midi _ _ _ = Nothing

update_state :: Addr -> Midi.ChannelMessage -> ControllerState
    -> ControllerState
update_state addr chan_msg state = state
    { state_note_id = case id_of chan_msg of
        Nothing -> state_note_id state
        Just note_id -> Map.insert addr note_id (state_note_id state)
    , state_pb = case pb_of chan_msg of
        Nothing -> state_pb state
        Just pb -> Map.insert addr pb (state_pb state)
    }
    where
    pb_of (Midi.PitchBend val) = Just val
    pb_of _ = Nothing
    id_of (Midi.NoteOn key _) = Just (key_to_id key)
    id_of (Midi.NoteOff key _) = Just (key_to_id key)
    id_of _ = Nothing

pb_to_input :: Controller.PbRange -> Midi.PitchBendValue -> Midi.Key
    -> Pitch.InputKey
pb_to_input (low, high) pb key =
    Pitch.InputKey (realToFrac (fromIntegral key + offset))
    where
    offset = if pb < 0 then -pb * fromIntegral low else pb * fromIntegral high

cc_to_controller :: Midi.Controller -> Score.Controller
cc_to_controller cc = maybe (Score.Controller ("cc" ++ show cc)) id
    (Map.lookup cc cc_controller)

controller_to_cc :: Score.Controller -> Maybe Midi.Controller
controller_to_cc = flip Map.lookup controller_cc

cc_controller =
    Map.fromList [(cc, Score.Controller c) | (cc, c) <- Controller.cc_map]
controller_cc =
    Map.fromList [(Score.Controller c, cc) | (cc, c) <- Controller.cc_map]

c_poly_aftertouch = convert_controller Controller.c_poly_aftertouch
c_aftertouch = convert_controller Controller.c_aftertouch

convert_controller :: Controller.Controller -> Score.Controller
convert_controller (Controller.Controller s) = Score.Controller s

-- Just for testing.
c_mod = Score.Controller Controller.c_mod

-- * from key

from_key :: Pitch.Octave -> Bool -> Pitch.InputKey -> Input
from_key oct down (Pitch.InputKey nn)
    | down = NoteOn note_id (Pitch.InputKey nn2) vel
    | otherwise = NoteOff note_id vel
    where
    nn2 = nn + 12 * fromIntegral oct
    note_id = NoteId (floor nn2)
    vel = 100 / 127

-- * to midi

-- | Convert an Input to MIDI.
to_midi :: Controller.PbRange -> Midi.PitchBendValue
    -> Map.Map NoteId Midi.Key  -> Input
    -> ([Midi.ChannelMessage], Map.Map NoteId Midi.Key)
to_midi pb_range prev_pb id_to_key input = case input of
        NoteOn note_id (Pitch.InputKey nn) vel ->
            let (key, pb) = Controller.pitch_to_midi pb_range nn
                n = Midi.NoteOn key (from_val vel)
                msgs = if prev_pb == pb then [n] else [Midi.PitchBend pb, n]
            in (msgs, Map.insert note_id key id_to_key)
        NoteOff note_id vel -> with_key note_id $ \key ->
            ([Midi.NoteOff key (from_val vel)], Map.delete note_id id_to_key)
        PitchChange note_id (Pitch.InputKey nn) -> with_key note_id $ \key ->
            let pb = Controller.pb_from_nn pb_range key nn
            in (if prev_pb == pb then [] else [Midi.PitchBend pb], id_to_key)
        Control _ controller val -> case controller_to_cc controller of
            Nothing -> ([], id_to_key)
            Just cc -> ([Midi.ControlChange cc (from_val val)], id_to_key)
    where
    with_key note_id f = case Map.lookup note_id id_to_key of
        Nothing -> ([], id_to_key)
        Just key -> f key
    from_val val = floor (val * 127)

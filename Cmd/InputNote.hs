-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{- | Convert incoming MIDI (and other inputs) to the internal note
    representation.  This has two purposes: note entry, and midi thru.

    This module is, in a way, dual to "Perform.Midi.Perform".  It takes MIDI
    input to the internal 'Input' representation and back again, while Perform
    takes the internal representation, in the form of
    'Perform.Midi.Perform.Event', to MIDI output.

    The overlapping part is that this module maps 'Derive.Score.Control's to
    and from MIDI while Play uses 'Perform.Midi.Control.Control'.  They
    use the same control names, though, so I can reuse code from Control.

    One significant difference between 'Input' and MIDI is that MIDI supports
    two levels of control addressing: note and channel, while 'Input' can
    only represent note addressing through 'NoteId'.  MIDI controls almost
    all apply at the channel level, but of course these controls all apply
    at the note level.  The result is that a MIDI control that on a keyboard
    affects the whole channel will only affect the last played note here.  When
    the input is converted back to MIDI it may wind up sharing a channel
    anyway, at which point the control will go back to being channel global,
    but if the instrument has multiple channels, I try to distribute between
    them to keep note_ids on separate channels.  This way, multiple channel
    emitting controls can be mapped to multiple channel using instruments.

    This is basically a simplified version of the channel allocation algorithm
    in "Perform.Midi.Perform".  It's hard to reuse that algorithm directly
    because this one has to operate in realtime and can't see which controls
    the note is going to use.
-}
module Cmd.InputNote where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Util.Num as Num

import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Score as Score
import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal


data Input =
    -- | The InputKey and val (velocity) could be sent separately, but that
    -- would make converting this back into midi for thru harder.
    NoteOn NoteId Pitch.InputKey Signal.Y
    | NoteOff NoteId Signal.Y
    -- | Controls coming from MIDI are mapped to control names, since this is
    -- a superset of MIDI CC numbers, and may include non-MIDI as well.  But
    -- for MidiThru to map back to a CC number, I need 1:1 mapping between
    -- Score.Controls and CC numbers.  This is what 'cc_to_control' and
    -- 'control_to_cc' provide.
    | Control NoteId Score.Control Signal.Y
    -- | Pitch could also be a Control, but this way the pitch is typed.
    | PitchChange NoteId Pitch.InputKey
    deriving (Eq, Show)

input_id :: Input -> NoteId
input_id input = case input of
    NoteOn note_id _ _ -> note_id
    NoteOff note_id _ -> note_id
    Control note_id _ _ -> note_id
    PitchChange note_id _ -> note_id

-- | In theory, NoteId is an arbitrary ID, but in practice it's the same as
-- the initial note on Midi.Key.  The reason is that pitch bend needs to
-- know the original key so it knows what the pitch bend is relative to.  I
-- could store the original key separately, but it's convenient to put them
-- both into NoteId, and I can't think of any instances where I'd want them
-- to be different.
newtype NoteId = NoteId Int deriving (Eq, Ord, Show)
key_to_id :: Midi.Key -> NoteId
key_to_id = NoteId . Midi.from_key
id_to_key :: NoteId -> Midi.Key
id_to_key (NoteId key) = Midi.to_key key

-- * from midi

type Addr = (Midi.ReadDevice, Midi.Channel)

data ControlState = ControlState
    { state_note_id :: Map.Map Addr NoteId -- ^ last note_id
    , state_pb :: Map.Map Addr Midi.PitchBendValue -- ^ last pb
    , state_pb_range :: Control.PbRange
    } deriving (Eq, Show)

empty_state :: Control.PbRange -> ControlState
empty_state = ControlState Map.empty Map.empty

from_midi :: ControlState -> Midi.ReadDevice -> Midi.Message
    -> Maybe (Input, ControlState)
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
            Control last_id (cc_to_control cc) (to_val val)
        Midi.PitchBend val -> with_last_id $ \last_id ->
            PitchChange last_id (to_pitch val (id_to_key last_id))
        Midi.Aftertouch key val -> Just $
            Control (key_to_id key) Controls.aftertouch (to_val val)
        Midi.ChannelPressure val -> with_last_id $ \last_id ->
            Control last_id Controls.pressure (to_val val)
        _ -> Nothing
    to_pitch = pb_to_input (state_pb_range state)
    to_val v = fromIntegral v / 127
from_midi _ _ _ = Nothing

update_state :: Addr -> Midi.ChannelMessage -> ControlState -> ControlState
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

pb_to_input :: Control.PbRange -> Midi.PitchBendValue -> Midi.Key
    -> Pitch.InputKey
pb_to_input (low, high) pb key =
    Pitch.InputKey (Num.f2d (Midi.from_key key + offset))
    where
    offset = if pb < 0 then -pb * fromIntegral low else pb * fromIntegral high

cc_to_control :: Midi.Control -> Score.Control
cc_to_control cc = fromMaybe (Score.Control ("cc" <> showt cc))
    (Map.lookup cc cc_control)

control_to_cc :: Score.Control -> Maybe Midi.Control
control_to_cc = flip Map.lookup control_cc

cc_control :: Map.Map Midi.Control Score.Control
cc_control = Map.invert control_cc

control_cc :: Map.Map Score.Control Midi.Control
control_cc = Control.universal_control_map

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
to_midi :: Control.PbRange -> Midi.PitchBendValue
    -> Map.Map NoteId Midi.Key -> Input
    -> ([Midi.ChannelMessage], Map.Map NoteId Midi.Key)
to_midi pb_range prev_pb id_to_key input = case input of
        NoteOn note_id (Pitch.InputKey nn) vel -> note_on note_id nn vel
        NoteOff note_id vel -> with_key note_id $ \key ->
            ([Midi.NoteOff key (from_val vel)], Map.delete note_id id_to_key)
        PitchChange note_id (Pitch.InputKey nn) -> with_key note_id $ \key ->
            (cons_pb (Control.pb_from_nn pb_range key nn) [], id_to_key)
        Control _ control val -> case control_to_cc control of
            Nothing -> ([], id_to_key)
            Just cc -> ([Midi.ControlChange cc (from_val val)], id_to_key)
    where
    with_key note_id f = case Map.lookup note_id id_to_key of
        Nothing -> ([], id_to_key)
        Just key -> f key
    from_val val = floor (val * 127)
    note_on note_id nn vel = case Control.pitch_to_midi pb_range nn of
        Nothing -> ([], id_to_key)
        Just (key, pb) ->
            (cons_pb pb [Midi.NoteOn key (from_val vel)],
                Map.insert note_id key id_to_key)
    cons_pb pb
        | prev_pb /= pb = (Midi.PitchBend pb:)
        | otherwise = id

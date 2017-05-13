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
import qualified Data.Text as Text

import qualified Util.Map as Map
import qualified Util.Num as Num
import qualified Midi.Midi as Midi
import qualified Derive.Controls as Controls
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Score as Score

import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import qualified App.Config as Config
import Global


-- | Since the ASCII keyboard isn't pressure sensitive, this is the default
-- velocity.  Hopefully it's strong but not so strong as to be hard on the
-- ears.
keyboard_velocity :: Signal.Y
keyboard_velocity = 0.75

type Input = GenericInput Pitch.Input

-- | An input with a plain NoteNumber pitch instead of a 'Pitch.Input'.
type InputNn = GenericInput Pitch.NoteNumber

data GenericInput pitch =
    -- | The Input and val (velocity) could be sent separately, but that
    -- would make converting this back into midi for thru harder.
    NoteOn NoteId pitch Signal.Y
    | NoteOff NoteId Signal.Y
    -- | Controls coming from MIDI are mapped to control names, since this is
    -- a superset of MIDI CC numbers, and may include non-MIDI as well.  But
    -- for MidiThru to map back to a CC number, I need 1:1 mapping between
    -- Score.Controls and CC numbers.  This is what 'cc_to_control' and
    -- 'control_to_cc' provide.
    | Control NoteId Score.Control Signal.Y
    -- | Pitch could also be a Control, but this way the pitch is typed.
    | PitchChange NoteId pitch
    deriving (Eq, Show)

instance (Show pitch, Pretty pitch) => Pretty (GenericInput pitch) where
    pretty (NoteOn id pitch vel) = Text.unwords
        ["NoteOn", "(" <> showt id <> ")", pretty pitch, pretty vel]
    pretty input = showt input

input_id :: GenericInput x -> NoteId
input_id input = case input of
    NoteOn note_id _ _ -> note_id
    NoteOff note_id _ -> note_id
    Control note_id _ _ -> note_id
    PitchChange note_id _ -> note_id

-- | Modify the NodeId so that it won't collide with other NodeIds.
--
-- NoteIds are supposed to be unique for each Input.  However, in practice they
-- wind up being the MIDI NoteOn 'Midi.Key', for reasons described in 'NoteId'.
-- So if you want to emit MIDI thru for two notes with the same pitch (e.g.
-- dispatch a single pitch to two instruments), you need to give them different
-- NoteIds.  This function multiplies them such that they won't collide.
--
-- TODO This is a grody hack.  A better solution might be to make NoteId into
-- a (Channel, Int) pair.
multiply_note_id :: Int -> GenericInput x -> GenericInput x
multiply_note_id multiplier input = case input of
    NoteOn note_id pitch vel -> NoteOn (modify note_id) pitch vel
    NoteOff note_id vel -> NoteOff (modify note_id) vel
    Control note_id control val -> Control (modify note_id) control val
    PitchChange note_id pitch -> PitchChange (modify note_id) pitch
    where
    modify (NoteId n) = NoteId (n + 1000 * multiplier)

-- | In theory, NoteId is an arbitrary ID, but in practice it's the same as
-- the initial note on Midi.Key.  The reason is that pitch bend needs to
-- know the original key so it knows what the pitch bend is relative to.  I
-- could store the original key separately, but it's convenient to put them
-- both into NoteId, and I can't think of any instances where I'd want them
-- to be different.
--
-- In addition, when a MIDI NoteOff comes in I have to know what NoteId it
-- applies to.  Since MIDI's NoteId is the key number, I have no choice but to
-- use that.
newtype NoteId = NoteId Int deriving (Eq, Ord, Show)

key_to_id :: Midi.Key -> NoteId
key_to_id = NoteId . Midi.from_key

id_to_key :: NoteId -> Midi.Key
id_to_key (NoteId key) = Midi.to_key key

-- * from midi

type Addr = (Midi.ReadDevice, Midi.Channel)

-- | Keep track of the state of each 'Midi.ReadDevice'.
newtype ReadDeviceState = ReadDeviceState (Map Midi.ReadDevice ControlState)
    deriving (Eq, Show)

empty_rdev_state :: ReadDeviceState
empty_rdev_state = ReadDeviceState mempty

next_state :: Midi.ReadDevice -> ControlState -> ReadDeviceState
    -> ReadDeviceState
next_state rdev cstate (ReadDeviceState state) =
    ReadDeviceState $ Map.insert rdev cstate state

-- | The state of one 'Midi.ReadDevice'.
data ControlState = ControlState
    { state_note_id :: Map Addr NoteId -- ^ last note_id
    , state_pb :: Map Addr Midi.PitchBendValue -- ^ last pb
    , state_pb_range :: Control.PbRange
    } deriving (Eq, Show)

empty_control_state :: Control.PbRange -> ControlState
empty_control_state pb_range = ControlState
    { state_note_id = Map.empty
    , state_pb = Map.empty
    , state_pb_range = pb_range
    }

from_midi :: ReadDeviceState -> Midi.ReadDevice -> Midi.Message
    -> Maybe (Input, ReadDeviceState)
from_midi (ReadDeviceState state) rdev (Midi.ChannelMessage chan chan_msg) =
    case maybe_input of
        Nothing -> Nothing
        Just input -> Just (input, next_state)
    where
    next_state = ReadDeviceState $
        Map.insert rdev (update_control_state addr chan_msg cstate) state
    cstate = Map.findWithDefault
        (empty_control_state Config.read_device_pb_range) rdev state
    addr = (rdev, chan)
    last_pb = Map.get 0 addr (state_pb cstate)
    with_last_id f = fmap f (Map.lookup addr (state_note_id cstate))
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
    to_pitch = pb_to_input (state_pb_range cstate)
    to_val v = fromIntegral v / 127
from_midi _ _ _ = Nothing

update_control_state :: Addr -> Midi.ChannelMessage -> ControlState
    -> ControlState
update_control_state addr chan_msg state = state
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

pb_to_input :: Control.PbRange -> Midi.PitchBendValue -> Midi.Key -> Pitch.Input
pb_to_input (low, high) pb key =
    nn_to_input $ Midi.from_key key + Pitch.nn offset
    where
    offset = Num.f2d $
        if pb < 0 then -pb * fromIntegral low else pb * fromIntegral high

nn_to_input :: Pitch.NoteNumber -> Pitch.Input
nn_to_input nn = Pitch.Input Pitch.PianoKbd pitch frac
    where
    pitch = Theory.semis_to_pitch_sharps Theory.piano_layout $
        Theory.nn_to_semis key
    (key, frac) = properFraction (Pitch.nn_to_double nn)

input_to_nn :: Pitch.Input -> Pitch.NoteNumber
input_to_nn (Pitch.Input _ pitch frac) = nn + Pitch.nn frac
    where nn = fromIntegral (pitch_to_nn pitch)

cc_to_control :: Midi.Control -> Score.Control
cc_to_control cc = fromMaybe (Score.unchecked_control ("cc" <> showt cc))
    (Map.lookup cc cc_control)

control_to_cc :: Score.Control -> Maybe Midi.Control
control_to_cc = flip Map.lookup control_cc

cc_control :: Map Midi.Control Score.Control
cc_control = Map.invert control_cc

control_cc :: Map Score.Control Midi.Control
control_cc = Control.universal_control_map

-- * from ascii

-- | Create an Input from an ascii keyboard Pitch.
from_ascii :: Bool -> Pitch.Pitch -> Input
from_ascii down pitch
    | down = NoteOn note_id (Pitch.Input Pitch.AsciiKbd pitch 0)
        keyboard_velocity
    | otherwise = NoteOff note_id keyboard_velocity
    where note_id = NoteId (pitch_to_nn pitch)

pitch_to_nn :: Pitch.Pitch -> Int
pitch_to_nn = Theory.semis_to_nn . Theory.pitch_to_semis Theory.piano_layout

-- * to midi

-- | Convert an InputNn to MIDI.
to_midi :: Control.PbRange -> Midi.PitchBendValue
    -> Map NoteId Midi.Key -> InputNn
    -> ([Midi.ChannelMessage], Map NoteId Midi.Key)
to_midi pb_range prev_pb id_to_key input_nn = case input_nn of
    NoteOn note_id nn vel -> note_on note_id nn vel
    NoteOff note_id vel -> with_key note_id $ \key ->
        ([Midi.NoteOff key (from_val vel)], Map.delete note_id id_to_key)
    PitchChange note_id nn -> with_key note_id $ \key ->
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
            ( cons_pb pb [Midi.NoteOn key (from_val vel)]
            , Map.insert note_id key id_to_key
            )
    cons_pb pb
        | prev_pb /= pb = (Midi.PitchBend pb:)
        | otherwise = id

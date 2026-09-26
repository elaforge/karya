-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

{-# LANGUAGE CPP #-}
{- | Convert incoming MIDI (and other inputs) to the internal note
    representation.  This has two purposes: note entry, and midi thru.

    This module is, in a way, dual to "Perform.Midi.Perform".  It takes MIDI
    input to the internal 'Input' representation and back again, while Perform
    takes the internal representation, in the form of
    'Perform.Midi.Perform.Event', to MIDI output.

    The overlapping part is that this module maps 'Derive.ScoreT.Control's to
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
module Cmd.InputNote (
    keyboardVelocity
    , Input
    , InputNn
    , GenericInput(..)
    , inputId
    , offsetNoteId
    , NoteId(..)

    -- * from_midi
    , ReadDeviceState
    , emptyRdevState
    , fromMidi
    , nnToInput
    -- * from_ascii
    , fromAscii
    -- * to_midi
    , toMidi
#ifdef TESTING
    , module Cmd.InputNote
#endif
) where
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Maps as Maps
import qualified Util.Num as Num
import qualified App.Config as Config
import qualified Derive.Controls as Controls
import qualified Derive.Scale.Theory as Theory
import qualified Derive.ScoreT as ScoreT

import qualified Midi.Midi as Midi
import qualified Perform.Midi.Control as Control
import qualified Perform.Pitch as Pitch
import qualified Perform.Signal as Signal

import           Global


-- | Since the ASCII keyboard isn't pressure sensitive, this is the default
-- velocity.  Hopefully it's strong but not so strong as to be hard on the
-- ears.
keyboardVelocity :: Signal.Y
keyboardVelocity = 0.75

type Input = GenericInput Pitch.Input

-- | An input with a plain NoteNumber pitch instead of a 'Pitch.Input'.
type InputNn = GenericInput Pitch.NoteNumber

data GenericInput pitch
    -- | The Input and val (velocity) could be sent separately, but that
    -- would make converting this back into midi for thru harder.
    = NoteOn NoteId pitch Signal.Y
    | NoteOff NoteId Signal.Y
    -- | Controls coming from MIDI are mapped to control names, since this is
    -- a superset of MIDI CC numbers, and may include non-MIDI as well.  But
    -- for MidiThru to map back to a CC number, I need 1:1 mapping between
    -- ScoreT.Controls and CC numbers.  This is what 'cc_to_control' and
    -- 'control_to_cc' provide.
    | Control NoteId ScoreT.Control Signal.Y
    -- | Pitch could also be a Control, but this way the pitch is typed.
    | PitchChange NoteId pitch
    deriving (Eq, Show)

instance (Show pitch, Pretty pitch) => Pretty (GenericInput pitch) where
    pretty (NoteOn id pitch vel) = Text.unwords
        ["NoteOn", "(" <> showt id <> ")", pretty pitch, pretty vel]
    pretty input = showt input

inputId :: GenericInput x -> NoteId
inputId input = case input of
    NoteOn noteId _ _ -> noteId
    NoteOff noteId _ -> noteId
    Control noteId _ _ -> noteId
    PitchChange noteId _ -> noteId

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
offsetNoteId :: Int -> GenericInput x -> GenericInput x
offsetNoteId multiplier input = case input of
    NoteOn noteId pitch vel -> NoteOn (modify noteId) pitch vel
    NoteOff noteId vel -> NoteOff (modify noteId) vel
    Control noteId control val -> Control (modify noteId) control val
    PitchChange noteId pitch -> PitchChange (modify noteId) pitch
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

keyToId :: Midi.Key -> NoteId
keyToId = NoteId . Midi.fromKey

idToKey :: NoteId -> Midi.Key
idToKey (NoteId key) = Midi.toKey key

-- * from midi

type Addr = (Midi.ReadDevice, Midi.Channel)

-- | Keep track of the state of each 'Midi.ReadDevice'.
newtype ReadDeviceState = ReadDeviceState (Map Midi.ReadDevice ControlState)
    deriving (Eq, Show)

emptyRdevState :: ReadDeviceState
emptyRdevState = ReadDeviceState mempty

nextState :: Midi.ReadDevice -> ControlState -> ReadDeviceState
    -> ReadDeviceState
nextState rdev cstate (ReadDeviceState state) =
    ReadDeviceState $ Map.insert rdev cstate state

-- | The state of one 'Midi.ReadDevice'.
data ControlState = ControlState
    { stateNoteId :: Map Addr NoteId -- ^ last note_id
    , statePb :: Map Addr Midi.PitchBendValue -- ^ last pb
    , statePbRange :: Control.PbRange
    } deriving (Eq, Show)

emptyControlState :: Control.PbRange -> ControlState
emptyControlState pbRange = ControlState
    { stateNoteId = Map.empty
    , statePb = Map.empty
    , statePbRange = pbRange
    }

fromMidi :: ReadDeviceState -> Midi.ReadDevice -> Midi.Message
    -> Maybe (Input, ReadDeviceState)
fromMidi (ReadDeviceState state) rdev (Midi.ChannelMessage chan chanMsg) =
    case maybeInput of
        Nothing -> Nothing
        Just input -> Just (input, nextState)
    where
    nextState = ReadDeviceState $
        Map.insert rdev (updateControlState addr chanMsg cstate) state
    cstate = Map.findWithDefault
        (emptyControlState Config.readDevicePbRange) rdev state
    addr = (rdev, chan)
    lastPb = Map.findWithDefault 0 addr (statePb cstate)
    withLastId f = fmap f (Map.lookup addr (stateNoteId cstate))
    maybeInput = case chanMsg of
        Midi.NoteOn key vel -> Just $
            NoteOn (keyToId key) (toPitch lastPb key) (toVal vel)
        Midi.NoteOff key vel -> Just $
            NoteOff (keyToId key) (toVal vel)
        Midi.ControlChange cc val -> withLastId $ \lastId ->
            Control lastId (ccToControl cc) (toVal val)
        Midi.PitchBend val -> withLastId $ \lastId ->
            PitchChange lastId (toPitch val (idToKey lastId))
        Midi.Aftertouch key val -> Just $
            Control (keyToId key) Controls.aftertouch (toVal val)
        Midi.ChannelPressure val -> withLastId $ \lastId ->
            Control lastId Controls.pressure (toVal val)
        _ -> Nothing
    toPitch = pbToInput (statePbRange cstate)
    toVal v = fromIntegral v / 127
fromMidi _ _ _ = Nothing

updateControlState :: Addr -> Midi.ChannelMessage -> ControlState
    -> ControlState
updateControlState addr chanMsg state = state
    { stateNoteId = case idOf chanMsg of
        Nothing -> stateNoteId state
        Just noteId -> Map.insert addr noteId (stateNoteId state)
    , statePb = case pbOf chanMsg of
        Nothing -> statePb state
        Just pb -> Map.insert addr pb (statePb state)
    }
    where
    pbOf (Midi.PitchBend val) = Just val
    pbOf _ = Nothing
    idOf (Midi.NoteOn key _) = Just (keyToId key)
    idOf (Midi.NoteOff key _) = Just (keyToId key)
    idOf _ = Nothing

pbToInput :: Control.PbRange -> Midi.PitchBendValue -> Midi.Key -> Pitch.Input
pbToInput (low, high) pb key =
    nnToInput $ Midi.fromKey key + Pitch.nn offset
    where
    offset = Num.f2d $
        if pb < 0 then -pb * fromIntegral low else pb * fromIntegral high

nnToInput :: Pitch.NoteNumber -> Pitch.Input
nnToInput nn = Pitch.Input Pitch.PianoKbd pitch frac
    where
    pitch = Theory.semisToPitchSharps Theory.pianoLayout $
        Theory.nnToSemis key
    (key, frac) = properFraction (Pitch.nnToDouble nn)

inputToNn :: Pitch.Input -> Pitch.NoteNumber
inputToNn (Pitch.Input _ pitch frac) = nn + Pitch.nn frac
    where nn = fromIntegral (pitchToNn pitch)

ccToControl :: Midi.Control -> ScoreT.Control
ccToControl cc = fromMaybe (ScoreT.Control ("cc" <> showt cc))
    (Map.lookup cc ccControl)

controlToCc :: ScoreT.Control -> Maybe Midi.Control
controlToCc = flip Map.lookup controlCc

ccControl :: Map Midi.Control ScoreT.Control
ccControl = Maps.invert controlCc

controlCc :: Map ScoreT.Control Midi.Control
controlCc = Control.universalControlMap

-- * from ascii

-- | Create an Input from an ascii keyboard Pitch.
fromAscii :: Bool -> Pitch.Pitch -> Input
fromAscii down pitch
    | down = NoteOn noteId (Pitch.Input Pitch.AsciiKbd pitch 0)
        keyboardVelocity
    | otherwise = NoteOff noteId keyboardVelocity
    where noteId = NoteId (pitchToNn pitch)

pitchToNn :: Pitch.Pitch -> Int
pitchToNn = Theory.semisToNn . Theory.pitchToSemis Theory.pianoLayout

-- * to midi

-- | Convert an InputNn to MIDI.
toMidi :: Control.PbRange -> Map NoteId Midi.Key -> InputNn
    -> ([Midi.ChannelMessage], Map NoteId Midi.Key)
toMidi pbRange idToKey = \case
    NoteOn noteId nn vel -> noteOn noteId nn vel
    NoteOff noteId vel -> withKey noteId $ \key ->
        ([Midi.NoteOff key (fromVal vel)], Map.delete noteId idToKey)
    PitchChange noteId nn -> withKey noteId $ \key ->
        ([Midi.PitchBend (Control.pbFromNn pbRange key nn)], idToKey)
    Control _ control val -> case controlToCc control of
        Nothing -> ([], idToKey)
        Just cc -> ([Midi.ControlChange cc (fromVal val)], idToKey)
    where
    withKey noteId f = case Map.lookup noteId idToKey of
        Nothing -> ([], idToKey)
        Just key -> f key
    fromVal val = floor (val * 127)
    noteOn noteId nn vel = case Control.pitchToMidi pbRange nn of
        Nothing -> ([], idToKey)
        Just (key, pb) ->
            ( [Midi.PitchBend pb, Midi.NoteOn key (fromVal vel)]
            , Map.insert noteId key idToKey
            )

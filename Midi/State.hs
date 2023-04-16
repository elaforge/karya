-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Simulate the state of a MIDI synthesizer.
--
-- "Midi.Synth" is different in that it wants to convert MIDI messages to
-- higher level notes, but similar in that it's also simulating a synthesizer.
-- This module focuses on the instantaneous state of the synth.
module Midi.State (
    State(..), empty, Channel(..), get_channel, empty_channel
    , Control(..), Addr
    , Message, convert
    , play, process, diff
) where
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import qualified Util.Maps as Maps
import qualified Util.Pretty as Pretty
import qualified Util.Lists as Lists

import qualified Midi.Midi as Midi
import Global


newtype State = State (Map Addr Channel)
    deriving (Eq, Show, Pretty)

empty :: State
empty = State Map.empty

data Channel = Channel {
    chan_notes :: !(Map Midi.Key Midi.Velocity)
    , chan_pb :: !Midi.PitchBendValue
    , chan_controls :: !(Map Control Midi.ControlValue)
    } deriving (Eq, Show)

empty_channel :: Channel
empty_channel = Channel Map.empty 0 Map.empty

get_channel :: Addr -> State -> Channel
get_channel addr (State chans) = Map.findWithDefault empty_channel addr chans

instance Pretty Channel where
    format (Channel notes pb controls) = Pretty.record "Channel"
        [ ("notes", Pretty.format notes)
        , ("pb", Pretty.format pb)
        , ("controls", Pretty.format controls)
        ]

data Control = CC Midi.Control | Aftertouch Midi.Key | Pressure
    deriving (Eq, Ord, Show)
type Addr = (Midi.WriteDevice, Midi.Channel)
type Message = (Midi.WriteDevice, Midi.Message)

instance Pretty Control where
    pretty (CC cc) = pretty cc
    pretty (Aftertouch key) = "at:" <> pretty key
    pretty Pressure = "pressure"

cc_msg :: Control -> Midi.ControlValue -> Midi.ChannelMessage
cc_msg (CC cc) val = Midi.ControlChange cc val
cc_msg (Aftertouch key) val = Midi.Aftertouch key val
cc_msg Pressure val = Midi.ChannelPressure val

convert :: Midi.WriteMessage -> Message
convert msg = (Midi.wmsg_dev msg, Midi.wmsg_msg msg)

-- * play

play :: [Message] -> State -> State
play msgs state = foldl' process state msgs

process :: State -> Message -> State
process state (dev, Midi.ChannelMessage chan msg) = case msg of
    Midi.NoteOff key _ -> notes (Map.delete key)
    Midi.NoteOn key vel
        | vel == 0 -> notes (Map.delete key)
        | otherwise -> notes (Map.insert key vel)
    Midi.Aftertouch key val -> controls (Map.insert (Aftertouch key) val)
    Midi.ControlChange cc val -> controls (Map.insert (CC cc) val)
    Midi.ProgramChange _pgm -> state -- I don't track current patch
    Midi.ChannelPressure val -> controls (Map.insert Pressure val)
    Midi.PitchBend val -> modify $ \chan -> chan { chan_pb = val }
    Midi.AllSoundOff -> notes (const Map.empty)
    Midi.ResetAllControls -> controls (const Map.empty)
    Midi.AllNotesOff -> notes (const Map.empty)
    _ -> state
    where
    notes f = modify $ \chan -> chan { chan_notes = f (chan_notes chan) }
    controls f = modify $ \chan ->
        chan { chan_controls = f (chan_controls chan) }
    modify = modify_addr state (dev, chan)
process state _ = state

modify_addr :: State -> Addr -> (Channel -> Channel) -> State
modify_addr (State chans) addr f =
    State (Map.alter (Just . f . Maybe.fromMaybe empty_channel) addr chans)


-- * diff

-- | Emit msgs needed to take one State to another.
diff :: State -> State -> [Message]
diff (State chans1) (State chans2) =
    diff_map chans1 chans2 empty_channel diff_chan

diff_chan :: Addr -> Channel -> Channel -> [Message]
diff_chan (dev, chan) (Channel notes1 pb1 controls1)
        (Channel notes2 pb2 controls2) =
    map (\m -> (dev, Midi.ChannelMessage chan m)) $
    (if pb1 == pb2 then [] else [Midi.PitchBend pb2])
    ++ diff_map controls1 controls2 0 diff_control
    ++ concatMap (uncurry diff_note) (Maps.pairs notes1 notes2)

diff_control :: Control -> Midi.ControlValue -> Midi.ControlValue
    -> [Midi.ChannelMessage]
diff_control control v1 v2
    | v1 == v2 = []
    | otherwise = [cc_msg control v2]

diff_note :: Midi.Key -> Lists.Paired Midi.Velocity Midi.Velocity
    -> [Midi.ChannelMessage]
diff_note key (Lists.First _) = [Midi.NoteOff key 0]
diff_note key (Lists.Second vel) = [Midi.NoteOn key vel]
diff_note key (Lists.Both v1 v2)
    | v1 == v2 = []
    | otherwise = [Midi.NoteOff key 0, Midi.NoteOn key v2]

diff_map :: Ord k => Map k a -> Map k a -> a -> (k -> a -> a -> [b]) -> [b]
diff_map m1 m2 deflt f = concatMap go (Maps.pairs m1 m2)
    where
    go (k, Lists.Both v1 v2) = f k v1 v2
    go (k, Lists.First v1) = f k v1 deflt
    go (k, Lists.Second v2) = f k deflt v2

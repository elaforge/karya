module Interface.MidiMsg where

data MidiMsg = MidiMsg
    deriving (Show)

take_midi_msgs :: IO [MidiMsg]
take_midi_msgs = return []

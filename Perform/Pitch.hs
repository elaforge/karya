module Perform.Pitch where
import Util.Pretty
import qualified Midi.Midi as Midi

-- frequency = 440 * 2^((pitch - 69)/12)
data Pitch = Pitch String NoteNumber deriving (Eq, Ord, Show)
newtype Hz = Hz Double deriving (Eq, Ord, Show)
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show)

-- | Convert NoteNumber to a MIDI key number.
cents :: Pitch -> Int
cents (Pitch _ (NoteNumber nn)) = floor (nn * 100) `mod` 100

-- TODO do I want the same range as MIDI?
midi_nn :: Pitch -> Midi.Key
midi_nn (Pitch _ (NoteNumber nn)) = floor nn

from_midi_nn :: String -> Int -> Pitch
from_midi_nn name nn = Pitch name (NoteNumber (fromIntegral nn))

instance Pretty Pitch where
    pretty (Pitch s _) = show s

module Perform.Pitch where
import Util.Pretty
import qualified Midi.Midi as Midi

-- frequency = 440 * 2^((pitch - 69)/12)
data Pitch = Pitch String NoteNumber deriving (Eq, Ord, Show)

-- newtype Hz = Hz Double deriving (Eq, Ord, Show)

-- | This is the same note range as MIDI, so MIDI note 0 is NoteNumber 0,
-- at 8.176 Hz.  Middle C is octave NoteNumber 60, octave 5.
newtype NoteNumber = NoteNumber Double deriving (Eq, Ord, Show)

-- | Convert NoteNumber to a MIDI key number.  Rounds down.
midi_nn :: Pitch -> Midi.Key
midi_nn (Pitch _ (NoteNumber nn)) = floor nn

-- | Number of cents the given Pitch is above its equal tempered pitch.
cents :: Pitch -> Int
cents (Pitch _ (NoteNumber nn)) = floor (nn * 100) `mod` 100

from_midi_nn :: Integral a => String -> a -> Pitch
from_midi_nn name nn = Pitch name (NoteNumber (fromIntegral nn))

instance Pretty Pitch where
    pretty (Pitch s _) = show s

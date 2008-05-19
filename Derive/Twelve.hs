{- | Dealing with the western tempered 12 note scale.

TODO: nail down the pitch range, for now I'm using the midi range

TODO: this doesn't have any support for enharmonics, but I do want to support
them for scale sensitive instruments and tunings.
-}
module Derive.Twelve where
import qualified Data.Array.IArray as IArray
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Midi.Midi as Midi

import qualified Perform.Pitch as Pitch

import qualified Derive.Derive as Derive
import qualified Derive.Score as Score

-- * deriver

twelve events = Derive.map_events () realize_note id events
realize_note _ event = case event_pitch2 (Score.event_text event) of
    Nothing -> Derive.throw $
        "can't realize event " ++ show (Score.event_text event)
    Just pitch -> return (event { Score.event_pitch = Just pitch })

-- * implementation

-- | Represent a note from the tempered 12 note scale.  In integers, starting
-- at c? (?? hz).
newtype Pitch = Pitch { pitch_nn :: Int }
    deriving (Show)

-- | Pitch as a string that can be an event's text.  I use a non-traditional
-- format that goes "octave note sharp" instead of "note sharp octave".
pitch_event :: Pitch -> String
pitch_event (Pitch pitch) = show octave ++ pitch_notes IArray.! p
    where
    octave = pitch `div` 12
    p = pitch `mod` 12

-- | Convert a string, which should have been produced by 'pitch_event', back
-- into a Pitch.
event_pitch :: String -> Maybe Pitch
event_pitch text = do
    (octave:pitch) <- return text
    oct <- if '0' <= octave && octave <= '9'
        then Just (Char.digitToInt octave) else Nothing
    p <- List.elemIndex pitch (IArray.elems pitch_notes)
    return (Pitch (oct * 12 + p))

-- TODO replace event_pitch when I get rid of the old Render
event_pitch2 text =
    fmap (Pitch.from_midi_nn text . pitch_nn) (event_pitch text)

pitch_notes :: IArray.Array Int String
pitch_notes = IArray.listArray (0, 11)
    ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]


to_midi_nn :: Pitch -> Midi.Key
-- TODO range checking
to_midi_nn (Pitch pitch) = fromIntegral pitch

from_midi_nn :: Midi.Key -> Pitch
from_midi_nn key = Pitch (fromIntegral key)

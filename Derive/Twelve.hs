{- | Dealing with the western tempered 12 note scale.

TODO: nail down the pitch range, for now I'm using the midi range

TODO: this doesn't have any support for enharmonics, but I do want to support
them for scale sensitive instruments and tunings.
-}
module Derive.Twelve where
import qualified Data.Array.IArray as IArray
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Perform.Pitch as Pitch


-- * implementation

-- | Pitch as a string that can be an event's text.  I use a non-traditional
-- format that goes "octave note sharp" instead of "note sharp octave".
-- Any cents the pitch may have are lost.
pitch_event :: Pitch.Pitch -> String
pitch_event (Pitch.Pitch _ (Pitch.NoteNumber nn)) =
    show octave ++ pitch_notes IArray.! p
    where
    octave = (floor nn) `div` 12
    p = (floor nn) `mod` 12

-- | Convert a string, which should have been produced by 'pitch_event', back
-- into a Pitch.
event_pitch :: String -> Maybe Pitch.Pitch
event_pitch text = do
    (octave:pitch) <- return text
    oct <- if '0' <= octave && octave <= '9'
        then Just (Char.digitToInt octave) else Nothing
    p <- List.elemIndex pitch (IArray.elems pitch_notes)
    return $ Pitch.from_midi_nn text (oct*12 + p)

pitch_notes :: IArray.Array Int String
pitch_notes = IArray.listArray (0, 11)
    ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]

-- TODO this should go to a general place like Derive.Scale
data Scale = Scale {
    scale_name :: String
    , scale_to_pitch :: String -> Maybe Pitch.Pitch
    , scale_from_pitch :: Pitch.Pitch -> String
    }

twelve_scale = Scale "twelve: [0-8][a-g][#-]" event_pitch pitch_event

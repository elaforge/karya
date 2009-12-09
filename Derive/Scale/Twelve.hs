{- | The western tempered 12 note scale.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.

    This, along with pengisep and pengumbang, will probably require that
    scale_note_to_nn and scale_input_to_note be passed performance and input
    context respectively.  And I'll need a flip enharmonic command here.

    Middle c = c-4, and I limit the range to the midi range.

    nn 127 = 9g
    nn 120 = 9c
    middle c = nn 60 = 4c
    nn 24 = 1c
    nn 12 = 0c
    nn 0 = -1c
-}
module Derive.Scale.Twelve where
import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.List as List

import qualified Util.Map as Map

import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[-1-9][a-g]#?"
    , Pitch.scale_octave = 12
    , Pitch.scale_note_to_generic = note_to_generic
    , Pitch.scale_input_to_note = input_to_note
    , Pitch.scale_input_to_nn = input_to_nn
    , Pitch.scale_generic_to_nn = generic_to_nn
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

note_to_generic :: Pitch.Note -> Maybe Pitch.Generic
note_to_generic note = do
    (degree, frac) <- Util.split_note note
    g <- Map.lookup degree degree_to_generic
    return (Pitch.Generic (fromIntegral g + frac))

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key_nn) = do
    let (int, cents) = properFraction key_nn
    degree <- Map.lookup int generic_to_degree
    return $ Util.join_note degree cents

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

generic_to_nn :: Pitch.Generic -> Maybe Pitch.NoteNumber
generic_to_nn (Pitch.Generic n) = Just (Pitch.NoteNumber n)

-- * implementation

degree_to_generic :: Map.Map String Util.IntGeneric
degree_to_generic = Map.fromList $ zip degrees [0..127]
    where degrees = [show o ++ d | o <- [-1..9], d <- note_degrees]
generic_to_degree = Map.invert degree_to_generic

note_degrees :: [String]
note_degrees = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

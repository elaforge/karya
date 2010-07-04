{- | The western tempered 12 note scale.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.

    This, along with pengisep and pengumbang, will probably require that
    scale_note_to_nn and scale_input_to_note be passed performance and input
    context respectively.  And I'll need a flip enharmonic command here.

    Middle c = c-4, and I limit the range to the midi range.

    > nn 127 = 9g
    > nn 120 = 9c
    > middle c = nn 60 = 4c
    > nn 24 = 1c
    > nn 12 = 0c
    > nn 0 = -1c
-}
module Derive.Scale.Twelve where
import qualified Data.Map as Map
import qualified Util.Map as Map

import qualified Derive.Derive as Derive
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[-1-9][a-g]#?"
    , Pitch.scale_octave = 12
    , Pitch.scale_note_to_call = note_to_call
    , Pitch.scale_input_to_note = input_to_note
    , Pitch.scale_input_to_nn = input_to_nn
    , Pitch.scale_degree_to_nn = degree_to_nn
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = case Map.lookup note note_to_degree of
        Nothing -> Nothing
        Just int_degree -> Just $
            Call.Pitch.degree_call note
                (Pitch.Degree (fromIntegral int_degree)) add_hz
    where
    add_hz (Pitch.Degree degree) hz =
        to_degree $ Pitch.add_hz hz (Pitch.NoteNumber degree)
    to_degree (Pitch.NoteNumber n) = Pitch.Degree n

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key_nn) = do
    let (int, cents) = properFraction key_nn
    note <- Map.lookup int degree_to_note
    return $ Pitch.Note $ Call.Pitch.note_call note cents

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

degree_to_nn :: Pitch.Degree -> Maybe Pitch.NoteNumber
degree_to_nn (Pitch.Degree n) = Just (Pitch.NoteNumber n)

-- * implementation

note_to_degree :: Map.Map Pitch.Note Util.IntDegree
note_to_degree = Map.fromList $ zip notes [0..127]
    where notes = map Pitch.Note [show o ++ d | o <- [-1..9], d <- note_degrees]
degree_to_note = Map.invert note_to_degree

note_degrees :: [String]
note_degrees = ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

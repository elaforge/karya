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

import qualified Util.Data as Data

import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[-1-9][a-g]#?"
    , Pitch.scale_note_to_nn = note_to_nn
    , Pitch.scale_input_to_note = input_to_note
    , Pitch.scale_input_to_nn = input_to_nn
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*12)
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

note_to_nn :: Pitch.Note -> Maybe Pitch.NoteNumber
note_to_nn note = do
    (degree, frac, hz) <- Util.split_note note
    nn <- Map.lookup degree degree_to_nn
    return $ Pitch.add_hz (fromIntegral hz) (nn + Pitch.nn frac)

degree_to_nn :: Map.Map String Pitch.NoteNumber
degree_to_nn = Map.fromList $ zip degrees (map Pitch.NoteNumber [0..127])
    where degrees = [show o ++ d | o <- [-1..9], d <- note_degrees]
nn_to_degree = Data.invert_map degree_to_nn

note_degrees :: [String]
note_degrees =
    ["c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key_nn) = do
    let (nn, cents) = split_nn (Pitch.NoteNumber key_nn)
    degree <- Map.lookup nn nn_to_degree
    return $ Util.join_note degree cents 0

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

transpose :: Double -> Pitch.Note -> Either Pitch.Error Pitch.Note
transpose n note = do
    (degree, frac, hz) <-
        maybe (Left Pitch.NotInScale) Right (Util.split_note note)
    nn <- maybe (Left Pitch.NotInScale) Right (Map.lookup degree degree_to_nn)
    let (nn2, frac2) = split_nn $ Pitch.nn n + nn + Pitch.nn frac
    degree2 <- maybe (Left Pitch.OutOfRange) Right (Map.lookup nn2 nn_to_degree)
    return $ Util.join_note degree2 frac2 hz

-- * implementation

split_nn :: Pitch.NoteNumber -> (Pitch.NoteNumber, Util.Frac)
split_nn (Pitch.NoteNumber nn) = (Pitch.nn n, frac)
    where (n, frac) = properFraction nn

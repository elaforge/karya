{- | The western tempered 12 note scale.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.
-}
module Derive.Twelve where
import Control.Monad.Error () -- Either instance
import qualified Data.Char as Char
import qualified Data.List as List

import Util.Control () -- Monad Either instance
import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[0-9][a-g][#-]"
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*12)
    , Pitch.scale_to_nn = note_to_nn
    , Pitch.scale_from_nn = nn_to_note
    , Pitch.scale_key_to_note = key_to_note
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = "twelve"

note_to_nn :: Pitch.Note -> Maybe Pitch.NoteNumber
note_to_nn (Pitch.Note note) = do
    octave:pitch <- return note
    oct <- if '0' <= octave && octave <= '9'
        then Just (Char.digitToInt octave) else Nothing
    p <- List.elemIndex pitch notes
    return $ Pitch.nn (oct*12 + p)

nn_to_note :: Pitch.NoteNumber -> Either String Pitch.Note
nn_to_note (Pitch.NoteNumber nn)
    | octave > 9 = Left $ "note number " ++ show nn ++ " is out of range"
    | otherwise = Right $ Pitch.Note (show octave ++ notes !! p)
    where (octave, p) = floor nn `divMod` 12

transpose :: Int -> Pitch.Note -> Either String Pitch.Note
transpose n note = do
    nn <- case note_to_nn note of
        Nothing -> Left $ "can't parse note " ++ show note
        Just (Pitch.NoteNumber nn) -> return nn
    nn_to_note (Pitch.nn (nn + fromIntegral n))

key_to_note :: Pitch.KeyNumber -> Either String Pitch.Note
key_to_note (oct, num) = nn_to_note (Pitch.nn (oct*12 + num))

notes = ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]

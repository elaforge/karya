{- | The western tempered 12 note scale.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.
-}
module Derive.Scale.Twelve where
import qualified Data.Char as Char
import qualified Data.List as List

import Util.Control () -- Monad Either instance
import qualified Perform.Pitch as Pitch


scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[0-9][a-g][#-]"
    , Pitch.scale_to_nn = note_to_nn
    , Pitch.scale_key_to_note = key_to_note
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*12)
    , Pitch.scale_set_pitch_bend = False
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

note_to_nn :: Pitch.Note -> Maybe Pitch.NoteNumber
note_to_nn (Pitch.Note note) = do
    octave:pitch <- return note
    oct <- if '0' <= octave && octave <= '9'
        then Just (Char.digitToInt octave) else Nothing
    p <- List.elemIndex pitch notes
    return $ Pitch.nn (oct*12 + p)

key_to_note :: Pitch.KeyNumber -> Maybe Pitch.Note
key_to_note (oct, num) = nn_to_note (Pitch.nn (oct*12 + num))

transpose :: Int -> Pitch.Note -> Either Pitch.Error Pitch.Note
transpose n note = do
    Pitch.NoteNumber nn <- maybe (Left Pitch.NotInScale) Right (note_to_nn note)
    maybe (Left Pitch.OutOfRange) Right
        (nn_to_note (Pitch.nn (nn + fromIntegral n)))

-- * implementation

nn_to_note :: Pitch.NoteNumber -> Maybe Pitch.Note
nn_to_note (Pitch.NoteNumber nn)
        -- This range could be expanded if necessary.
    | 0 > octave || octave > 9 = Nothing
    | otherwise = Just $ Pitch.Note (show octave ++ notes !! p)
    where (octave, p) = floor nn `divMod` 12

notes = ["c-", "c#", "d-", "d#", "e-", "f-", "f#", "g-", "g#", "a-", "a#", "b-"]

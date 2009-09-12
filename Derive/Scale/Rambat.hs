-- | Saih gender rambat.
module Derive.Scale.Rambat where
import qualified Data.Char as Char

import qualified Perform.Pitch as Pitch
import Util.Control () -- Monad Either instance

scale = Pitch.Scale {
    Pitch.scale_id = scale_id
    , Pitch.scale_pattern = "[12356](\\.*|\\^*)"
    , Pitch.scale_to_nn = note_to_nn
    , Pitch.scale_key_to_note = key_to_note
    , Pitch.scale_transpose = transpose
    , Pitch.scale_transpose_octave = transpose . (*5)
    , Pitch.scale_set_pitch_bend = True
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "rambat"

transpose_keynum :: Pitch.KeyNumber -> Int -> Pitch.KeyNumber
transpose_keynum (oct, degree) n = (oct*5 + degree + n) `divMod` 5

note_to_nn :: Pitch.Note -> Maybe Pitch.NoteNumber
note_to_nn note = lookup note note_nn

key_to_note :: Pitch.KeyNumber -> Maybe Pitch.Note
key_to_note keynum = lookup keynum keynum_note

transpose :: Int -> Pitch.Note -> Either Pitch.Error Pitch.Note
transpose n note = do
    keynum <- maybe (Left Pitch.NotInScale) Right (lookup note note_keynum)
    let trans_keynum = transpose_keynum keynum n
    maybe (Left Pitch.OutOfRange) Right (lookup trans_keynum keynum_note)

-- * implementation

note_keynum = zip (map (Pitch.Note . keynum_to_str) degrees)
    [(oct, if degree >= 5 then degree-1 else degree) | (oct, degree) <- degrees]
keynum_note = [(b, a) | (a, b) <- note_keynum]
note_nn = [(note, nn) | ((note, _), nn) <- zip note_keynum freqs]

keynum_to_str :: Pitch.KeyNumber -> String
keynum_to_str (oct, degree)
    | oct < 0 = show degree ++ replicate (-oct) '.'
    | otherwise = show degree ++ replicate oct '^'

degrees = (-2, 6) : [(oct, degree) | oct <- [-1..1], degree <- [1, 2, 3, 5, 6]]
freqs = map Pitch.nn [56.82,
    -- nding (-1, 1)
    60.73, 62.8, 63.35, 67.7, 68.2,
    -- nding (0, 1)
    72.46, 73.9, 75.5, 79.4, 80.5,
    -- nding (1, 1)
    84.46, 86, 91.74]

octave_map = [("..", -2), (".", -1), ("", 0), ("^", 1), ("^^", 2)]

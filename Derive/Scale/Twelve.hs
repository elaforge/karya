{- | The western equal tempered 12 note scale, aka 12TET.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".  General to specific is more
    aesthetically appealing.

    TODO: this doesn't have any support for enharmonics, but I do want to
    support them for scale sensitive instruments and tunings.

    This, along with pengisep and pengumbang, will probably require that
    scale_note_to_nn and scale_input_to_note be passed performance and input
    context respectively.  And I'll need a flip enharmonic command here.

    4c is middle C, and the range is limited to the midi range.  Since
    'Pitch.NoteNumber's also use midi numbering, conversions are trivial.

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
import qualified Ui.Track as Track
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale = Scale.Scale {
    Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-g]#?"
    , Scale.scale_map =
        Track.make_scale_map [(Pitch.note_text n, fromIntegral d)
            | (n, d) <- Map.assocs note_to_degree]
    , Scale.scale_symbols = [] -- later maybe I can use fancy sharps and flats
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn = input_to_nn
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

transpose :: Derive.Transpose
transpose octaves degrees note = do
    d <- Map.lookup note note_to_degree
    Map.lookup (d + Pitch.Degree (octaves*12) + degrees) degree_to_note

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = case Map.lookup note note_to_degree of
        Nothing -> Nothing
        Just degree -> Just $ Call.Pitch.note_call note (note_number degree)
    where
    note_number (Pitch.Degree degree) (Pitch.Chromatic chrom)
            (Pitch.Diatonic _dia) _key -- TODO unimplemented
        | 0 < nn && nn > 127 = Nothing
        | otherwise = Just nn
        where nn = Pitch.NoteNumber $ fromIntegral degree + chrom

input_to_note :: Pitch.InputKey -> Maybe Pitch.Note
input_to_note (Pitch.InputKey key_nn) = do
    let (int, cents) = properFraction key_nn
    note <- Map.lookup int degree_to_note
    return $ Pitch.Note $ Call.Pitch.note_expr note cents

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

-- * constants

-- middle_c :: Pitch.Degree
-- middle_c = c4
--
-- c3, d3, e3, f3, g3, a3, b3 :: Pitch.Degree
-- (c3, d3, e3, f3, g3, a3, b3) = (48, 50, 52, 53, 55, 57, 59)
--
-- c4, d4, e4, f4, g4, a4, b4 :: Pitch.Degree
-- (c4, d4, e4, f4, g4, a4, b4) = (60, 62, 64, 65, 67, 69, 71)
--
-- c5, d5, e5, f5, g5, a5, b5 :: Pitch.Degree
-- (c5, d5, e5, f5, g5, a5, b5) = (72, 74, 76, 77, 79, 81, 83)
--
-- c6, d6, e6, f6, g6, a6, b6 :: Pitch.Degree
-- (c6, d6, e6, f6, g6, a6, b6) = (84, 86, 88, 89, 91, 93, 95)

-- * implementation

note_to_degree :: Map.Map Pitch.Note Pitch.Degree
note_to_degree = Map.fromList $ zip notes [1..127]
    where
    notes = drop 1 $
        map Pitch.Note [show o ++ d | o <- [-1..9], d <- note_degrees]

degree_to_note :: Map.Map Pitch.Degree Pitch.Note
degree_to_note = Map.invert note_to_degree

-- | I could use `sharp` in here and look a little nicer, but it's simpler to
-- have plain text if possible.
note_degrees :: [String]
note_degrees =
    [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"]

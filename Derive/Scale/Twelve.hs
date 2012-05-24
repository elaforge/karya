{- | The western equal tempered 12 note scale, aka 12TET.

    For the note text, I use a non-traditional format that goes "octave note
    sharp" instead of "note sharp octave".  General to specific is more
    aesthetically appealing.

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
import qualified Data.Maybe as Maybe

import qualified Util.Num as Num
import qualified Ui.Track as Track
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Util as Util

import qualified Perform.Pitch as Pitch


scale :: Scale.Scale
scale = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[-1-9][a-g]#?"
    , Scale.scale_map =
        Track.make_scale_map [(Pitch.note_text n, fromIntegral d)
            | (n, (_, d)) <- Map.toList note_to_degree]
    , Scale.scale_symbols = [] -- later maybe I can use fancy sharps and flats
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = transpose
    , Scale.scale_enharmonics = enharmonics
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn = input_to_nn
    }

scale_id :: Pitch.ScaleId
scale_id = Pitch.ScaleId "twelve"

transpose :: Derive.Transpose
transpose maybe_key octaves steps note = do
    key <- maybe (return default_key) Theory.read_key maybe_key
    pitch <- Theory.read_pitch (Theory.key_layout key) (Pitch.note_text note)
    let pitch2 = pitch
            { Theory.pitch_octave = Theory.pitch_octave pitch + octaves }
    case steps of
        Pitch.Chromatic steps ->
            pitch_note $ Theory.transpose_chromatic key (floor steps) pitch2
        Pitch.Diatonic steps ->
            pitch_note $ Theory.transpose_diatonic key (floor steps) pitch2

enharmonics :: Derive.Enharmonics
enharmonics maybe_key note = do
    key <- read_key maybe_key
    pitch <- Theory.read_pitch (Theory.key_layout key) (Pitch.note_text note)
    return $ Maybe.mapMaybe pitch_note $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: Pitch.Note -> Maybe Derive.ValCall
note_to_call note = case Map.lookup note note_to_degree of
    Nothing -> Nothing
    Just (pitch, degree) ->
        Just $ Call.Pitch.note_call note (note_number pitch degree)
    where
    note_number :: Theory.Pitch -> Pitch.Degree -> Scale.GetNoteNumber
    note_number pitch (Pitch.Degree degree) chromatic diatonic mb_str_key = do
        dsteps <- if diatonic == 0 then Right 0 else do
            str_key <- maybe (Left Scale.KeyNeeded) Right mb_str_key
            key <- maybe (Left Scale.UnparseableKey) Right
                (Theory.read_key str_key)
            return $ Theory.diatonic_to_chromatic key
                (Theory.pitch_note pitch) diatonic
        let nn = Pitch.NoteNumber $ fromIntegral degree + chromatic + dsteps
        if Num.in_range 1 127 nn then Right nn
            else Left Scale.InvalidTransposition

input_to_note :: Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note maybe_key (Pitch.InputKey key_nn) = do
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    let key = Maybe.fromMaybe default_key $ Theory.read_key =<< maybe_key
        (degree, cents) = properFraction key_nn
    note <- pitch_note $ Theory.semis_to_pitch key degree
    return $ Pitch.Note $ Call.Pitch.note_expr note cents

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)

-- * constants

middle_c :: Pitch.Degree
middle_c = c4

c3, d3, e3, f3, g3, a3, b3 :: Pitch.Degree
(c3, d3, e3, f3, g3, a3, b3) = (48, 50, 52, 53, 55, 57, 59)

c4, d4, e4, f4, g4, a4, b4 :: Pitch.Degree
(c4, d4, e4, f4, g4, a4, b4) = (60, 62, 64, 65, 67, 69, 71)

c5, d5, e5, f5, g5, a5, b5 :: Pitch.Degree
(c5, d5, e5, f5, g5, a5, b5) = (72, 74, 76, 77, 79, 81, 83)

c6, d6, e6, f6, g6, a6, b6 :: Pitch.Degree
(c6, d6, e6, f6, g6, a6, b6) = (84, 86, 88, 89, 91, 93, 95)


-- * implementation

note_to_degree :: Map.Map Pitch.Note (Theory.Pitch, Pitch.Degree)
note_to_degree = Map.fromList $ filter in_range $ concat $
    [[note "#" "x" "b" "bb" p, note "`#`" "`##`" "`b`" "`bb`" p]
        | p <- Theory.piano_pitches]
    where
    note s s2 f f2 p = (Pitch.Note $ Theory.show_pitch s s2 f f2 p,
        (p, Pitch.Degree $ Theory.pitch_to_semis Theory.piano_layout p))
    in_range = Num.in_range 1 128 . snd . snd

-- | Don't emit a 'Pitch.Note' that's out of range, because it won't be
-- recognized when it comes time to play it back.
pitch_note :: Theory.Pitch -> Maybe Pitch.Note
pitch_note pitch
    | Map.member n note_to_degree = Just n
    | otherwise = Nothing
    where n = Pitch.Note $ Theory.show_pitch "#" "x" "b" "bb" pitch

default_key :: Theory.Key
Just default_key = Theory.read_key (Pitch.Key "c-maj")

read_key :: Maybe Pitch.Key -> Maybe Theory.Key
read_key Nothing = Just default_key
read_key (Just key) = Theory.read_key key

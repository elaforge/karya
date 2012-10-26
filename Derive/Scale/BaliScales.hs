-- | Utilities for Balinese scales.  Mostly that means dealing with umbang and
-- isep.
module Derive.Scale.BaliScales where
import qualified Data.Map as Map

import Util.Control
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Symbols as Symbols
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


data ScaleMap = ScaleMap {
    scale_degree_map :: !Util.DegreeMap
    , scale_input_map :: !Util.InputMap
    , scale_nn_map :: !NoteNumberMap
    } deriving (Show)

scale_map :: Int -> [Pitch.NoteNumber] -> [Pitch.NoteNumber] -> ScaleMap
scale_map align_with_ding umbang isep = ScaleMap
    { scale_degree_map = Util.degree_map (align all_notes)
    , scale_input_map = Map.fromList (zip (align all_inputs) [0..])
    , scale_nn_map = make_nn_map umbang isep
    }
    -- Line a list starting with ding up with the note numbers.
    where
    align = take (length umbang) . drop align_with_ding

all_notes :: [Pitch.Note]
all_notes = map Symbols.dotted_number
    [(num, oct) | oct <- [-2..2], num <- [1, 2, 3, 5, 6]]

all_inputs :: [Pitch.InputKey]
all_inputs =
    [Pitch.middle_c + fromIntegral (o*12) + d | o <- [-2..2], d <- keys]
    where keys = [Util.i_c, Util.i_d, Util.i_e, Util.i_f, Util.i_g]

scale :: Pitch.ScaleId -> ScaleMap -> Scale.Scale
scale scale_id (ScaleMap degree_map input_map nn_map) = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = "[12356](\\.*|\\^*)"
    , Scale.scale_map = Util.track_scale_map degree_map
    -- loaded from Derive.Scale.Symbols
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Util.standard_transposers
    , Scale.scale_transpose = Util.transpose degree_map 5
    , Scale.scale_enharmonics = Util.no_enharmonics
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn =
        Util.computed_input_to_nn input_to_note note_to_call
    , Scale.scale_call_doc = Util.call_doc degree_map input_map doc
    }
    where
    note_to_call = Util.note_to_call degree_map (degree_to_nn nn_map)
    input_to_note = Util.input_to_note input_map degree_map
    doc =
        "Balinese scales come in detuned pairs. They use the `tuning` env var"
        <> " to select between pengumbang and pengisep tuning. The env var"
        <> " should be set to either `'umbang'` or `'isep'`, and if it's not"
        <> " set, `'umbang'` is assumed. Normally the umbang and isep"
        <> " frequencies are hardcoded according to the scale, but if the "
        <> ShowVal.show_val c_ombak <> " control is present, they will be tuned"
        <> " that many hz apart."

data Tuning = Umbang | Isep deriving (Show)

read_tuning :: String -> Maybe Tuning
read_tuning "umbang" = Just Umbang
read_tuning "isep" = Just Isep
read_tuning _ = Nothing

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
c_ombak :: Score.Control
c_ombak = Score.Control "ombak"

type NoteNumberMap =
    (Util.NoteNumberMap, Util.NoteNumberMap, Util.NoteNumberMap)

make_nn_map :: [Pitch.NoteNumber] -> [Pitch.NoteNumber] -> NoteNumberMap
make_nn_map umbang isep =
    (Util.note_number_map umbang, Util.note_number_map isep,
        Util.note_number_map center)
    where center = zipWith (\a b -> (a+b) / 2) umbang isep

degree_to_nn :: NoteNumberMap
    -> TrackLang.Environ -> PitchSignal.Controls -> Pitch.Degree
    -> Either Scale.ScaleError Pitch.NoteNumber
degree_to_nn (umbang_nns, isep_nns, center_nns) = \env controls degree -> do
    tuning <- Util.read_environ read_tuning Umbang TrackLang.v_tuning env
    let lookup_nn =
            maybe (Left Scale.InvalidTransposition) Right . Map.lookup degree
    case Map.lookup c_ombak controls of
        Nothing -> case tuning of
            Umbang -> lookup_nn umbang_nns
            Isep -> lookup_nn isep_nns
        Just ombak -> do
            nn <- lookup_nn center_nns
            return $ case tuning of
                Umbang -> Pitch.add_hz (- (ombak / 2)) nn
                Isep -> Pitch.add_hz (ombak / 2) nn

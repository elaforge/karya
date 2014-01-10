-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Balinese scales.  Mostly that means dealing with umbang and
-- isep.
module Derive.Scale.BaliScales where
import qualified Data.Map as Map

import Util.Control
import qualified Util.Map as Map
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Symbols as Symbols
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch


data ScaleMap = ScaleMap {
    scale_degree_map :: !Scales.DegreeMap
    , scale_nn_map :: !NoteNumberMap
    }

-- | umbang, isep
data NoteNumberMap = NoteNumberMap !ToNn !ToNn deriving (Show)
type ToNn = Map.Map Pitch.Pitch Pitch.NoteNumber

scale_map :: Pitch.PitchClass -> Pitch.Octave -> Pitch.PitchClass
    -> [Pitch.Note] -> [Pitch.NoteNumber] -> [Pitch.NoteNumber] -> ScaleMap
scale_map per_octave start_octave start_pc notes umbang isep = ScaleMap
    { scale_degree_map =
        Scales.degree_map per_octave start_octave start_pc
            (drop (fromIntegral start_pc) notes) avg
    , scale_nn_map = NoteNumberMap (make_to_nn umbang) (make_to_nn isep)
    }
    where
    avg = zipWith (\a b -> (a+b) / 2) umbang isep
    make_to_nn = Map.fromList
        . zip (Scales.pitches_from per_octave start_octave start_pc)

scale_top :: ScaleMap -> Maybe Pitch.Pitch
scale_top = fmap fst . Map.max . (\(NoteNumberMap n _) -> n) . scale_nn_map

scale_bottom :: ScaleMap -> Maybe Pitch.Pitch
scale_bottom = fmap fst . Map.min . (\(NoteNumberMap n _) -> n) . scale_nn_map

dotted12356 :: [Pitch.Note]
dotted12356 =
    [Symbols.dotted_number num oct | oct <- [-2..2], num <- [1, 2, 3, 5, 6]]

octave12356 :: [Pitch.Note]
octave12356 = [Pitch.Note $ showt oct <> "-" <> showt d
    | oct <- [1..], d <- [1, 2, 3, 5, 6]]

ioeua :: [Pitch.Note]
ioeua = [Pitch.Note $ showt oct <> vowel
    | oct <- [1..], vowel <- ["i", "o", "e", "u", "a"]]

-- | I would prefer to leave the middle octave without an octave mark, but
-- without one they conflict with common pitch calls.  Caps would also work,
-- but they look ugly and I'd have to do it for the absolute notation too for
-- consistency.
dotted_ioeua :: [Pitch.Note]
dotted_ioeua = [Pitch.Note $ vowel <> oct
    | oct <- ["_", "-", "^"], vowel <- ["i", "o", "e", "u", "a"]]

make_scale :: Text -> Pitch.ScaleId -> ScaleMap -> Scale.Scale
make_scale scale_pattern scale_id (ScaleMap dmap nn_map) = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = scale_pattern
    -- loaded from Derive.Scale.Symbols
    , Scale.scale_symbols = []
    -- I don't put %ombak into the transposers, this means changes in ombak
    -- take effect at the beginning of each note, but won't retune a sounding
    -- one.
    , Scale.scale_transposers = Scales.standard_transposers
    , Scale.scale_read = const $ Scales.read_note dmap
    , Scale.scale_show = const $ Scales.show_pitch dmap
    , Scale.scale_layout =
        Scale.diatonic_layout (fromIntegral (Scales.dm_per_octave dmap))
    , Scale.scale_transpose = Scales.transpose dmap
    , Scale.scale_enharmonics = Scales.no_enharmonics
    , Scale.scale_note_to_call = note_to_call
    , Scale.scale_input_to_note = input_to_note
    , Scale.scale_input_to_nn =
        Scales.computed_input_to_nn input_to_note note_to_call
    , Scale.scale_call_doc =
        Scales.call_doc Scales.standard_transposers dmap doc
    }
    where
    note_to_call = Scales.note_to_call scale dmap $
        degree_to_nn (Scales.dm_to_nn dmap) nn_map
    input_to_note = Scales.input_to_note dmap
    scale = PitchSignal.Scale scale_id Scales.standard_transposers
    doc =
        "Balinese scales come in detuned pairs. They use the `tuning` env var\
        \ to select between pengumbang and pengisep tuning. The env var\
        \ should be set to either `umbang` or `isep`, and if it's not\
        \ set, `umbang` is assumed. Normally the umbang and isep\
        \ frequencies are hardcoded according to the scale, but if the "
        <> ShowVal.doc_val c_ombak
        <> " control is present, they will be tuned that many hz apart."

data Tuning = Umbang | Isep deriving (Show)

read_tuning :: Text -> Maybe Tuning
read_tuning t
    | t == "umbang" = Just Umbang
    | t == "isep" = Just Isep
    | otherwise = Nothing

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
c_ombak :: Score.Control
c_ombak = "ombak"

degree_to_nn :: ToNn -> NoteNumberMap -> Scales.PitchToNoteNumber
degree_to_nn to_avg (NoteNumberMap to_umbang to_isep) =
    \(PitchSignal.PitchConfig env controls) pitch -> do
        tuning <- Scales.read_environ read_tuning Umbang Environ.tuning env
        let lookup_nn = maybe (Left Scale.InvalidTransposition) Right
                . Map.lookup pitch
        case Map.lookup c_ombak controls of
            Nothing -> case tuning of
                Umbang -> lookup_nn to_umbang
                Isep -> lookup_nn to_isep
            Just ombak -> do
                nn <- lookup_nn to_avg
                return $ case tuning of
                    Umbang -> Pitch.add_hz (- (ombak / 2)) nn
                    Isep -> Pitch.add_hz (ombak / 2) nn

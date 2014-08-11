-- Copyright 2014 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for Balinese scales.  Mostly that means dealing with umbang and
-- isep.
--
-- They're implemented as a modification of "ChromaticScales" because a saih
-- pitu or pelog scale requires a key or pathet, which winds up being similar
-- to a simplified chromatic scale.
module Derive.Scale.BaliScales where
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector ((!?))

import Util.Control
import qualified Util.Num as Num
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch


-- | Top level scale constructor.
make_scale :: Pitch.ScaleId -> ScaleMap -> Scale.Scale
make_scale scale_id smap =
    (ChromaticScales.make_scale scale_id (smap_chromatic smap) doc)
    { Scale.scale_enharmonics = Scales.no_enharmonics
    , Scale.scale_note_to_call = note_to_call scale smap
    , Scale.scale_input_to_note = input_to_note smap
    , Scale.scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note smap) (note_to_call scale smap)
    }
    where
    scale = PitchSignal.Scale scale_id Scales.standard_transposers
    doc = "Balinese scales come in detuned pairs. They use the `tuning` env var\
        \ to select between pengumbang and pengisep tuning. The env var\
        \ should be set to either `umbang` or `isep`, and if it's not\
        \ set, `umbang` is assumed. Normally the umbang and isep\
        \ frequencies are hardcoded according to the scale, but if the "
        <> ShowVal.doc_val c_ombak
        <> " control is present, they will be tuned that many hz apart."

data ScaleMap = ScaleMap {
    smap_chromatic :: !ChromaticScales.ScaleMap
    , smap_note_numbers :: !NoteNumbers
    }

scale_map :: Theory.Layout -> TheoryFormat.Format -> ChromaticScales.Keys
    -> Theory.Key -> NoteNumbers -> ScaleMap
scale_map layout fmt all_keys default_key note_numbers = ScaleMap
    { smap_chromatic =
        (ChromaticScales.scale_map layout fmt all_keys default_key)
        { ChromaticScales.smap_show_pitch = show_pitch layout fmt note_numbers }
    , smap_note_numbers = note_numbers
    }

data NoteNumbers = NoteNumbers {
    nn_umbang :: !(Vector.Vector Pitch.NoteNumber)
    , nn_isep :: !(Vector.Vector Pitch.NoteNumber)
    , nn_offset :: !Offset
    } deriving (Show)

-- | Not all scales start at octave 0, PitchClass 0.
data Offset = Offset {
    offset_pc_per_octave :: !Pitch.PitchClass
    , offset_octave :: !Pitch.Octave
    , offset_pc :: !Pitch.PitchClass
    } deriving (Show)

note_numbers :: Theory.Layout -> Pitch.Octave -> Pitch.PitchClass
    -> [Pitch.NoteNumber] -> [Pitch.NoteNumber] -> NoteNumbers
note_numbers layout start_octave start_pc umbang isep = NoteNumbers
    { nn_umbang = Vector.fromList umbang
    , nn_isep = Vector.fromList isep
    , nn_offset = Offset
        { offset_pc_per_octave = Theory.layout_pc_per_octave layout
        , offset_octave = start_octave
        , offset_pc = start_pc
        }
    }

add_offset :: Offset -> Pitch.Pitch -> Pitch.Pitch
add_offset (Offset per_oct start_oct start_pc) =
    Pitch.add_pc per_oct start_pc . Pitch.add_octave start_oct

remove_offset :: Offset -> Pitch.Pitch -> Pitch.Pitch
remove_offset (Offset per_oct start_oct start_pc) =
    Pitch.add_pc per_oct (-start_pc) . Pitch.add_octave (-start_oct)

-- | Top pitch of the scale.
scale_top :: ScaleMap -> Pitch.Pitch
scale_top smap =
    Theory.semis_to_pitch_sharps layout $
        Theory.pitch_to_semis layout bottom + semis
    where
    -- Number of semi steps in between each scale degree.
    semis = Vector.length (nn_umbang $ smap_note_numbers smap) - 1
    bottom = scale_bottom smap
    layout = ChromaticScales.smap_layout (smap_chromatic smap)

scale_bottom :: ScaleMap -> Pitch.Pitch
scale_bottom smap = Pitch.Pitch oct (Pitch.Degree pc 0)
    where Offset _ oct pc = nn_offset $ smap_note_numbers smap

-- * Format

ioeua :: TheoryFormat.Degrees
ioeua = TheoryFormat.make_degrees ["i", "o", "e", "u", "a"]

ioeua_relative :: Bool -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
ioeua_relative chromatic default_key keys =
    TheoryFormat.make_relative_format
        ("[1-9][ioeua]" <> if chromatic then "#?" else "")
        ioeua (ChromaticScales.relative_fmt default_key keys)

ioeua_relative_dotted :: Pitch.Octave -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
ioeua_relative_dotted center chromatic default_key keys =
    TheoryFormat.make_relative_format_config (dotted_octaves center)
        ("[ioeua]" <> (if chromatic then "#?" else "") <> "[_^-]")
        ioeua (ChromaticScales.relative_fmt default_key keys)

ioeua_absolute :: TheoryFormat.Format
ioeua_absolute = TheoryFormat.make_absolute_format "[1-9][ioeua]" ioeua

ioeua_absolute_dotted :: Pitch.Octave -> TheoryFormat.Format
ioeua_absolute_dotted center = TheoryFormat.make_absolute_format_config
    (dotted_octaves center) TheoryFormat.ascii_accidentals "[ioeua][_^-]" ioeua

dotted_octaves :: Pitch.Octave -> TheoryFormat.OctaveFormat
dotted_octaves center = (show_octave, parse_octave)
    where
    show_octave oct
        | oct > center = (<> Text.replicate (oct-center) "^")
        | oct < center = (<> Text.replicate (center-oct) "_")
        | otherwise = (<>"-")
    parse_octave p_degree = do
        degree <- p_degree
        octs <- A.many1 $ A.satisfy $ \c -> c == '_' || c == '-' || c == '^'
        let oct | "_" `List.isPrefixOf` octs = -(length octs)
                | "-" `List.isPrefixOf` octs = 0
                | otherwise = length octs
        return $ Pitch.Pitch (center + oct) degree

-- * keys

make_keys :: Theory.Layout -> [(Text, Pitch.Semi, [Pitch.Semi])]
    -> ChromaticScales.Keys
make_keys layout keys = Map.fromList
    [ (Pitch.Key name, Theory.key (to_degree tonic) name intervals layout)
    | (name, tonic, intervals) <- keys
    ]
    where
    to_degree = Pitch.pitch_degree . Theory.semis_to_pitch_sharps layout

-- * implementation

note_to_call :: PitchSignal.Scale -> ScaleMap -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call scale (ScaleMap smap nns) note =
    case TheoryFormat.read_unadjusted_pitch fmt note of
        Left _ -> Nothing
        Right pitch -> Just $ ScaleDegree.scale_degree scale
            (ChromaticScales.pitch_nn smap (semis_to_nn nns)
                (remove_offset (nn_offset nns) pitch))
            (ChromaticScales.pitch_note smap pitch)
    where fmt = ChromaticScales.smap_fmt smap

-- | This is like 'ChromaticScales.input_to_note', but no enharmonics, and
-- no flats.
input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap maybe_key (Pitch.Input kbd_type pitch frac) = do
    pitch <- Scales.kbd_to_scale kbd_type pc_per_octave
        (ChromaticScales.key_tonic key) pitch
    note <- ChromaticScales.smap_show_pitch csmap maybe_key pitch
    return $ ScaleDegree.pitch_expr frac note
    where
    pc_per_octave = Theory.layout_pc_per_octave $
        ChromaticScales.smap_layout csmap
    key = fromMaybe (ChromaticScales.smap_default_key csmap) $
        flip Map.lookup (ChromaticScales.smap_keys csmap) =<< maybe_key
    csmap = smap_chromatic smap

show_pitch :: Theory.Layout -> TheoryFormat.Format -> NoteNumbers
    -> Maybe Pitch.Key -> Pitch.Pitch -> Either Scale.ScaleError Pitch.Note
show_pitch layout fmt nns maybe_key pitch
    | nn_umbang nns !? semis == Nothing = Left Scale.InvalidTransposition
    | otherwise = Right $ TheoryFormat.show_pitch fmt maybe_key pitch
    where
    offset = nn_offset nns
    semis = Theory.pitch_to_semis layout (remove_offset offset pitch)

-- * tuning

data Tuning = Umbang | Isep deriving (Show)

read_tuning :: Text -> Maybe Tuning
read_tuning t
    | t == Environ.umbang = Just Umbang
    | t == Environ.isep = Just Isep
    | otherwise = Nothing

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
c_ombak :: Score.Control
c_ombak = "ombak"

-- | Convert 'Pitch.FSemi' to 'Pitch.NoteNumber'.
semis_to_nn :: NoteNumbers -> ChromaticScales.SemisToNoteNumber
semis_to_nn nns = \(PitchSignal.PitchConfig env controls) fsemis -> do
    tuning <- Scales.read_environ read_tuning Umbang Environ.tuning env
    let to_either = maybe (Left Scale.InvalidTransposition) Right
    to_either $ case Map.lookup c_ombak controls of
        Nothing -> case tuning of
            Umbang -> get_nn (nn_umbang nns) fsemis
            Isep -> get_nn (nn_isep nns) fsemis
        Just ombak -> do
            umbang <- get_nn (nn_umbang nns) fsemis
            isep <- get_nn (nn_isep nns) fsemis
            let avg = (Pitch.nn_to_hz umbang + Pitch.nn_to_hz isep) / 2
            return $ Pitch.hz_to_nn $ case tuning of
                Umbang -> avg - ombak / 2
                Isep -> avg + ombak / 2

get_nn :: Vector.Vector Pitch.NoteNumber -> Pitch.FSemi
    -> Maybe Pitch.NoteNumber
get_nn nns fsemis
    | frac == 0 = nns !? semis
    | otherwise = do
        low <- nns !? semis
        high <- nns !? (semis + 1)
        return $ Num.scale low high (Pitch.nn frac)
    where (semis, frac) = properFraction fsemis

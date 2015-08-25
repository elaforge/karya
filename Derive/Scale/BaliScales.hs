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

import qualified Util.Num as Num
import qualified Util.Seq as Seq
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

import qualified Perform.Pitch as Pitch
import Global


-- | Top level scale constructor.
make_scale :: Pitch.ScaleId -> ScaleMap -> Scale.Scale
make_scale scale_id smap =
    (ChromaticScales.make_scale scale_id (smap_chromatic smap) doc)
    { Scale.scale_enharmonics = Scales.no_enharmonics }
    where
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
    -> Theory.Key -> NoteNumbers -> (Pitch.Semi, Pitch.Semi) -> ScaleMap
scale_map layout fmt all_keys default_key note_numbers range = ScaleMap
    { smap_chromatic =
        (ChromaticScales.scale_map layout fmt all_keys default_key)
        { ChromaticScales.smap_semis_to_nn = semis_to_nn offset note_numbers
        -- Convert range to absolute.
        , ChromaticScales.smap_range = ((+offset) *** (+offset)) range
        }
    , smap_note_numbers = note_numbers
    }
    where
    -- Scales start at octave 1.
    offset = Theory.layout_semis_per_octave layout

data NoteNumbers = NoteNumbers {
    nn_umbang :: !(Vector.Vector Pitch.NoteNumber)
    , nn_isep :: !(Vector.Vector Pitch.NoteNumber)
    } deriving (Show)

scale_range :: ScaleMap -> (Pitch.Pitch, Pitch.Pitch)
scale_range smap = (to_pitch bottom, to_pitch top)
    where
    (bottom, top) = ChromaticScales.smap_range $ smap_chromatic smap
    to_pitch = Theory.semis_to_pitch_sharps
        (ChromaticScales.smap_layout (smap_chromatic smap))

-- * Format

ioeua :: TheoryFormat.Degrees
ioeua = TheoryFormat.make_degrees ["i", "o", "e", "u", "a"]

ioeua_relative :: Bool -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
ioeua_relative chromatic default_key keys =
    TheoryFormat.make_relative_format
        ("[1-9][ioeua]" <> if chromatic then "#?" else "")
        ioeua (ChromaticScales.relative_fmt default_key keys)

ioeua_relative_arrow :: Pitch.Octave -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
ioeua_relative_arrow center chromatic default_key keys =
    TheoryFormat.make_relative_format_config (arrow_octaves center)
        ("[ioeua]" <> (if chromatic then "#?" else "") <> "[_^-]")
        ioeua (ChromaticScales.relative_fmt default_key keys)

ioeua_absolute :: TheoryFormat.Format
ioeua_absolute = TheoryFormat.make_absolute_format "[1-9][ioeua]" ioeua

ioeua_absolute_arrow :: Pitch.Octave -> TheoryFormat.Format
ioeua_absolute_arrow center = TheoryFormat.make_absolute_format_config
    (arrow_octaves center) TheoryFormat.ascii_accidentals "[ioeua][_^-]" ioeua

arrow_octaves :: Pitch.Octave -> TheoryFormat.OctaveFormat
arrow_octaves center = (show_octave, parse_octave)
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

cipher_relative_dotted :: Pitch.Octave -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
cipher_relative_dotted center default_key keys =
    TheoryFormat.make_relative_format_config (dotted_octaves center)
        "[12356]|`[12356][.^]*`" cipher5
        (ChromaticScales.relative_fmt default_key keys)

cipher5 :: TheoryFormat.Degrees
cipher5 = TheoryFormat.make_degrees ["1", "2", "3", "5", "6"]

dotted_octaves :: Pitch.Octave -> TheoryFormat.OctaveFormat
dotted_octaves center = (show_octave, parse_octave)
    where
    show_octave oct d
        | oct == center = d
        | otherwise = "`" <> d
            <> (if oct >= center then Text.replicate (oct-center) "^"
                else Text.replicate (center-oct) ".")
            <> "`"
    parse_octave p_degree =
        Pitch.Pitch center <$> p_degree <|> with_octave p_degree
    with_octave p_degree = do
        A.char '`'
        degree <- p_degree
        octs <- A.many' $ A.satisfy $ \c -> c == '.' || c == '^'
        A.char '`'
        let oct = Seq.count '^' octs - Seq.count '.' octs
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

-- * tuning

data Tuning = Umbang | Isep deriving (Show)

read_tuning :: Text -> Maybe Tuning
read_tuning t
    | t == EnvKey.umbang = Just Umbang
    | t == EnvKey.isep = Just Isep
    | otherwise = Nothing

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
c_ombak :: Score.Control
c_ombak = "ombak"

-- | Convert 'Pitch.FSemi' to 'Pitch.NoteNumber'.
semis_to_nn :: Pitch.Semi -> NoteNumbers -> ChromaticScales.SemisToNoteNumber
semis_to_nn offset nns = \(PSignal.PitchConfig env controls) fsemis_ -> do
    let fsemis = fsemis_ - fromIntegral offset
    tuning <- Scales.read_environ read_tuning (Just Umbang) EnvKey.tuning env
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

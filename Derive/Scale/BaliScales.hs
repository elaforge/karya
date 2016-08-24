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
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq
import qualified Util.TextUtil as TextUtil

import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.EnvKey as EnvKey
import qualified Derive.PSignal as PSignal
import qualified Derive.RestrictedEnviron as RestrictedEnviron
import qualified Derive.Scale as Scale
import qualified Derive.Scale.ChromaticScales as ChromaticScales
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal
import qualified Derive.Typecheck as Typecheck

import qualified Perform.Pitch as Pitch
import Global


-- | Top level scale constructor.
make_scale :: Pitch.ScaleId -> ScaleMap -> Scale.Scale
make_scale scale_id smap =
    (ChromaticScales.make_scale scale_id (smap_chromatic smap) doc)
    { Scale.scale_enharmonics = Scales.no_enharmonics }
    where
    doc = "Balinese scales come in detuned pairs. They use the "
        <> ShowVal.doc EnvKey.tuning <> " env var to select between pengumbang\
        \ and pengisep. The env var should be set to either `umbang` or `isep`,\
        \ defaulting to `umbang`. Normally the umbang and isep\
        \ frequencies are hardcoded according to the scale, but if the "
        <> ShowVal.doc c_ombak
        <> " control is present, they will be tuned that many hz apart.\
        \\nThe " <> ShowVal.doc saih_key <> " env var chooses between different\
        \ versions of the same scale.  It defaults to "
        <> ShowVal.doc (smap_default_saih smap)
        <> ". Saihs:\n"
        <> TextUtil.list
            [ ShowVal.doc name <> " - " <> saih_doc saih
            | (name, saih) <- Map.toList (smap_saihs smap)
            ]

data ScaleMap = ScaleMap {
    smap_chromatic :: !ChromaticScales.ScaleMap
    , smap_saihs :: !SaihMap
    , smap_default_saih :: !Text
    }

scale_map :: Theory.Layout -> TheoryFormat.Format -> Pitch.Octave
    -- ^ The octave where the saih starts.  It should be such that
    -- octave 4 is close to middle C.
    -> ChromaticScales.Keys
    -> Theory.Key -> SaihMap -> Text -> Maybe (Pitch.Semi, Pitch.Semi)
    -- ^ If not given, use the complete range of the saih.
    -> ScaleMap
scale_map layout fmt base_oct all_keys default_key saihs default_saih
        maybe_range =
    ScaleMap
        { smap_chromatic =
            (ChromaticScales.scale_map layout fmt all_keys default_key)
            { ChromaticScales.smap_semis_to_nn =
                semis_to_nn offset saihs default_saih
            -- Convert range to absolute.
            , ChromaticScales.smap_range = ((+offset) *** (+offset)) range
            }
        , smap_saihs = saihs
        , smap_default_saih = default_saih
        }
    where
    offset = Theory.layout_semis_per_octave layout * base_oct
    range = fromMaybe (0, top) maybe_range
    top = maybe 0 (subtract 1 . Vector.length . saih_umbang) $
        Seq.head (Map.elems saihs)

-- | This is a specialized version of 'scale_map' that uses octave and
-- 'Pitch.Semi' offsets to compute the range.
instrument_scale_map :: Theory.Layout -> ChromaticScales.Keys -> Theory.Key
    -> SaihMap -> Text -> Pitch.Octave -> Pitch.Octave
    -> (Pitch.Octave, Pitch.Semi) -> (Pitch.Octave, Pitch.Semi)
    -> ScaleMap
instrument_scale_map layout all_keys default_key saihs default_saih
        base_oct center_oct (low_oct, low_pc) (high_oct, high_pc) =
    scale_map layout fmt base_oct all_keys default_key saihs default_saih $
        Just
            ( (low_oct-base_oct) * per_oct + low_pc
            , (high_oct-base_oct) * per_oct + high_pc
            )
    where
    fmt = ioeua_relative_arrow center_oct True default_key all_keys
    per_oct = Theory.layout_semis_per_octave layout

scale_range :: ScaleMap -> Scale.Range
scale_range smap = Scale.Range (to_pitch bottom) (to_pitch top)
    where
    (bottom, top) = ChromaticScales.smap_range $ smap_chromatic smap
    to_pitch = Theory.semis_to_pitch_sharps
        (ChromaticScales.smap_layout (smap_chromatic smap))

-- * Saih

type SaihMap = Map.Map Text Saih

data Saih = Saih {
    saih_doc :: ShowVal.Doc
    , saih_umbang :: Vector.Vector Pitch.NoteNumber
    , saih_isep :: Vector.Vector Pitch.NoteNumber
    } deriving (Show)

saih :: ([Pitch.NoteNumber] -> [Pitch.NoteNumber])
    -> ShowVal.Doc -> [(Pitch.NoteNumber, Pitch.NoteNumber)] -> Saih
saih extend doc nns = Saih
    { saih_doc = doc
    , saih_umbang = Vector.fromList (extend umbang)
    , saih_isep = Vector.fromList (extend isep)
    }
    where (umbang, isep) = unzip nns

-- * Format

ioeua :: TheoryFormat.Degrees
ioeua = TheoryFormat.make_degrees ["i", "o", "e", "u", "a"]

ioeua_relative :: Bool -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
ioeua_relative chromatic default_key keys =
    TheoryFormat.make_relative_format
        ("[1-9][ioeua]" <> if chromatic then "#?" else "") ioeua fmt
    where
    fmt = ChromaticScales.relative_fmt default_key keys

ioeua_relative_arrow :: Pitch.Octave -> Bool -> Theory.Key
    -> ChromaticScales.Keys -> TheoryFormat.Format
ioeua_relative_arrow center chromatic default_key keys =
    TheoryFormat.make_relative_format
        ("[ioeua]" <> (if chromatic then "#?" else "") <> "[_^-]") ioeua fmt
    where
    fmt = with_octaves (arrow_octaves center) $
        ChromaticScales.relative_fmt default_key keys

ioeua_absolute :: TheoryFormat.Format
ioeua_absolute = TheoryFormat.make_absolute_format "[1-9][ioeua]" ioeua

ioeua_absolute_arrow :: Pitch.Octave -> TheoryFormat.Format
ioeua_absolute_arrow center = TheoryFormat.make_absolute_format_config
    (arrow_octaves center TheoryFormat.default_config) "[ioeua][_^-]" ioeua

with_octaves :: (TheoryFormat.Config -> TheoryFormat.Config)
    -> TheoryFormat.RelativeFormat key -> TheoryFormat.RelativeFormat key
with_octaves f config = config
    { TheoryFormat.rel_config = f (TheoryFormat.rel_config config) }

arrow_octaves :: Pitch.Octave -> TheoryFormat.Config -> TheoryFormat.Config
arrow_octaves center = TheoryFormat.set_octave show_octave parse_octave
    where
    show_octave oct
        | oct > center = (<> Text.replicate (oct-center) "^")
        | oct < center = (<> Text.replicate (center-oct) "_")
        | otherwise = (<>"-")
    parse_octave p_degree = do
        (pc, acc) <- p_degree
        octs <- A.many1 $ A.satisfy $ \c -> c == '_' || c == '-' || c == '^'
        let oct | "_" `List.isPrefixOf` octs = -(length octs)
                | "-" `List.isPrefixOf` octs = 0
                | otherwise = length octs
        return $ TheoryFormat.RelativePitch (center + oct) pc acc

cipher_relative_dotted :: Pitch.Octave -> Theory.Key -> ChromaticScales.Keys
    -> TheoryFormat.Format
cipher_relative_dotted center default_key keys =
    TheoryFormat.make_relative_format "[12356]|`[12356][.^]*`" cipher5 fmt
    where
    fmt = with_octaves (dotted_octaves center) $
        ChromaticScales.relative_fmt default_key keys

cipher5 :: TheoryFormat.Degrees
cipher5 = TheoryFormat.make_degrees ["1", "2", "3", "5", "6"]

dotted_octaves :: Pitch.Octave -> TheoryFormat.Config -> TheoryFormat.Config
dotted_octaves center = TheoryFormat.set_octave show_octave parse_octave
    where
    show_octave oct d
        | oct == center = d
        | otherwise = "`" <> d
            <> (if oct >= center then Text.replicate (oct-center) "^"
                else Text.replicate (center-oct) ".")
            <> "`"
    parse_octave p_degree =
        uncurry (TheoryFormat.RelativePitch center) <$> p_degree
            <|> with_octave p_degree
    with_octave p_degree = do
        A.char '`'
        (pc, acc) <- p_degree
        octs <- A.many' $ A.satisfy $ \c -> c == '.' || c == '^'
        A.char '`'
        let oct = Seq.count (=='^') octs - Seq.count (=='.') octs
        return $ TheoryFormat.RelativePitch (center + oct) pc acc

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

data Tuning = Umbang | Isep deriving (Enum, Bounded, Show)

instance Pretty.Pretty Tuning where pretty = showt
instance Typecheck.Typecheck Tuning
instance Typecheck.TypecheckSymbol Tuning
instance RestrictedEnviron.ToVal Tuning
instance ShowVal.ShowVal Tuning where
    show_val = Typecheck.enum_show_val

-- | If ombak is unset, use the hardcoded tunings.  Otherwise, create new
-- umbang and isep tunings based on the given number.
c_ombak :: Score.Control
c_ombak = "ombak"

-- | Convert 'Pitch.FSemi' to 'Pitch.NoteNumber'.
semis_to_nn :: Pitch.Semi -> SaihMap -> Text
    -> ChromaticScales.SemisToNoteNumber
semis_to_nn offset saihs default_saih =
    \(PSignal.PitchConfig env controls) fsemis_ -> do
        let fsemis = fsemis_ - fromIntegral offset
        tuning <- Scales.parse_environ (Just Umbang) EnvKey.tuning env
        saih <- Scales.read_environ_default (\v -> Map.lookup v saihs)
            (Just default_saih) saih_key env
        let to_either = maybe (Left BaseTypes.out_of_range) Right
        to_either $ case Map.lookup c_ombak controls of
            Nothing -> case tuning of
                Umbang -> get_nn (saih_umbang saih) fsemis
                Isep -> get_nn (saih_isep saih) fsemis
            Just ombak -> do
                umbang <- get_nn (saih_umbang saih) fsemis
                isep <- get_nn (saih_isep saih) fsemis
                let avg = (Pitch.nn_to_hz umbang + Pitch.nn_to_hz isep) / 2
                return $ Pitch.hz_to_nn $ case tuning of
                    Umbang -> avg - ombak / 2
                    Isep -> avg + ombak / 2

-- | VSymbol: Select saih tuning.
saih_key :: BaseTypes.Key
saih_key = "saih"

get_nn :: Vector.Vector Pitch.NoteNumber -> Pitch.FSemi
    -> Maybe Pitch.NoteNumber
get_nn nns fsemis
    | frac == 0 = nns !? semis
    | otherwise = do
        low <- nns !? semis
        high <- nns !? (semis + 1)
        return $ Num.scale low high (Pitch.nn frac)
    where (semis, frac) = properFraction fsemis

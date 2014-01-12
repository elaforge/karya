-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for equal-tempered chromatic scales with keys and modes.
module Derive.Scale.ChromaticScales where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- | This contains all that is needed to define a western-like key system.
-- It fills a similar role to 'Scales.ScaleMap' for non-keyed scales.
data ScaleMap = ScaleMap {
    smap_fmt :: !TheoryFormat.Format
    , smap_keys :: !Keys
    , smap_default_key :: !Theory.Key
    , smap_layout :: !Theory.Layout
    }

twelve_doc :: Text
twelve_doc = "Scales in the \"twelve\" family use western style note naming.\
    \ That is, note names look like octave-letter-accidentals like \"4c#\".\
    \ They have a notion of a \"layout\", which is a pattern of half and\
    \ whole steps, e.g. the piano layout, and a key, which is a subset of\
    \ notes from the scale along with a preferred spelling for them. The\
    \ rules of how enharmonic spelling works are complicated, and documented\
    \ in 'Derive.Scale.Theory'. The key is read from the `key` env var, and\
    \ each scale has a list of keys it will accept."

scale_map :: Theory.Layout -> TheoryFormat.Format -> Keys -> Theory.Key
    -> ScaleMap
scale_map layout fmt keys default_key = ScaleMap
    { smap_fmt = fmt
    , smap_keys = keys
    , smap_default_key = default_key
    , smap_layout = layout
    }

type Keys = Map.Map Pitch.Key Theory.Key

make_keys :: TheoryFormat.Format -> [Theory.Key] -> Keys
make_keys fmt keys =
    Map.fromList $ zip (map (TheoryFormat.show_key fmt) keys) keys

make_scale :: ScaleMap -> Pitch.ScaleId -> Text -> Scale.Scale
make_scale scale_map scale_id doc = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = TheoryFormat.fmt_pattern (smap_fmt scale_map)
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Scales.standard_transposers
    , Scale.scale_read = read_pitch scale_map
    , Scale.scale_show = show_pitch scale_map
    , Scale.scale_layout = Theory.layout_intervals (smap_layout scale_map)
    , Scale.scale_transpose = transpose scale_map
    , Scale.scale_enharmonics = enharmonics scale_map
    , Scale.scale_note_to_call = note_to_call scale scale_map
    , Scale.scale_input_to_note = input_to_note scale_map
    , Scale.scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note scale_map) (note_to_call scale scale_map)
    , Scale.scale_call_doc = call_doc Scales.standard_transposers scale_map doc
    }
    where scale = PitchSignal.Scale scale_id Scales.standard_transposers

-- * functions

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition maybe_key steps pitch = do
    key <- read_key smap maybe_key
    return $ t key steps pitch
    where
    t = case transposition of
        Scale.Chromatic -> Theory.transpose_chromatic
        Scale.Diatonic -> Theory.transpose_diatonic

enharmonics :: ScaleMap -> Derive.Enharmonics
enharmonics smap maybe_key note = do
    pitch <- read_pitch smap maybe_key note
    key <- read_key smap maybe_key
    return $ Either.rights $ map (show_pitch smap maybe_key) $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: PitchSignal.Scale -> ScaleMap -> Pitch.Note
    -> Maybe Derive.ValCall
note_to_call scale smap note =
    case TheoryFormat.read_unadjusted_pitch (smap_fmt smap) note of
        Left _ -> Nothing
        Right pitch -> Just $ ScaleDegree.scale_degree scale
            (pitch_nn smap degree_to_nn pitch) (pitch_note smap pitch)
    where degree_to_nn _config degree = Pitch.NoteNumber degree + 12
    -- Add an octave becasue of NOTE [middle-c].

pitch_note :: ScaleMap -> Pitch.Pitch -> Scale.PitchNote
pitch_note smap pitch (PitchSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error diatonic chromatic $ do
        let d = round diatonic
            c = round chromatic
        show_pitch smap (Scales.lookup_key env) =<< if d == 0 && c == 0
            then return pitch
            else do
                key <- read_env_key smap env
                return $ Theory.transpose_chromatic key c $
                    Theory.transpose_diatonic key d pitch
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

pitch_nn :: ScaleMap
    -> (PitchSignal.PitchConfig -> Double -> Pitch.NoteNumber)
    -> Pitch.Pitch -> Scale.PitchNn
pitch_nn smap degree_to_nn pitch config@(PitchSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error diatonic chromatic $ do
        pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap)
            (Scales.lookup_key env) pitch
        dsteps <- if diatonic == 0 then Right 0 else do
            key <- read_env_key smap env
            return $ Theory.diatonic_to_chromatic key
                (Pitch.pitch_degree pitch) diatonic
        let semis = Theory.pitch_to_semis (smap_layout smap) pitch
            degree = fromIntegral semis + chromatic + dsteps
            nn = degree_to_nn config degree
        if 1 <= nn && nn <= 127 then Right nn
            else Left Scale.InvalidTransposition
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap maybe_key (Pitch.Input kbd_type pitch frac) = do
    pitch <- Scales.kbd_to_scale kbd_type pc_per_octave tonic pitch
    -- Relative scales don't need to figure out enharmonic spelling, and
    -- besides it would be wrong since it assumes Pitch 0 0 is C.
    let pick_enharmonic = if TheoryFormat.fmt_relative (smap_fmt smap) then id
            else Theory.pick_enharmonic key
    note <- either (const Nothing) Just $ show_pitch smap Nothing $
        pick_enharmonic pitch
    return $ ScaleDegree.pitch_expr frac note
    where
    pc_per_octave = Theory.layout_pc_per_octave (smap_layout smap)
    tonic = Pitch.degree_pc (Theory.key_tonic key)
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (smap_default_key smap) $
        flip Map.lookup (smap_keys smap) =<< maybe_key

call_doc :: Set.Set Score.Control -> ScaleMap -> Text -> Derive.DocumentedCall
call_doc transposers smap doc =
    Scales.annotate_call_doc transposers extra_doc fields $
        Derive.extract_val_doc call
    where
    call = ScaleDegree.scale_degree PitchSignal.no_scale err err
        where err _ = Left $ PitchSignal.PitchError "it was just an example!"
    extra_doc = doc <> twelve_doc
    fields =
        [ ("default key", Pretty.prettytxt $
            TheoryFormat.show_key (smap_fmt smap) (smap_default_key smap))
        , ("keys", format_keys $ Map.keys (smap_keys smap))
        ]

format_keys :: [Pitch.Key] -> Text
format_keys keys
    | any (("-" `Text.isInfixOf`) . name) keys = Text.intercalate ", " $
        map fst $ group_tonic_mode $ map (flip (,) ()) keys
    | otherwise = Text.intercalate ", " $ map name keys
    where name (Pitch.Key k) = k

-- | Assuming keys are formatted @tonic-mode@, group keys by mode and replace
-- the tonics with a pattern.
group_tonic_mode :: [(Pitch.Key, a)] -> [(Text, a)]
group_tonic_mode = map extract . Seq.keyed_group_on key . map (first split)
    where
    extract (mode, group) = (fmt mode (map (fst . fst) group), snd (head group))
    key ((_, mode), _) = mode
    split (Pitch.Key t) = (pre, Text.drop 1 post)
        where (pre, post) = Text.break (=='-') t
    fmt mode keys = "(" <> Text.intercalate "|" keys <> ")-" <> mode

-- * implementation

show_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either Scale.ScaleError Pitch.Note
show_pitch smap key pitch
    | 1 <= nn && nn <= 127 =
        Right $ TheoryFormat.show_pitch (smap_fmt smap) key pitch
    | otherwise = Left Scale.InvalidTransposition
    where
    nn = Theory.semis_to_nn $ Theory.pitch_to_semis (smap_layout smap) pitch

read_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Pitch.Pitch
read_pitch = TheoryFormat.read_pitch . smap_fmt

read_env_key :: ScaleMap -> TrackLang.Environ
    -> Either Scale.ScaleError Theory.Key
read_env_key smap = Scales.read_environ
    (\k -> Map.lookup (Pitch.Key k) (smap_keys smap))
    (smap_default_key smap) Environ.key

read_key :: ScaleMap -> Maybe Pitch.Key -> Either Scale.ScaleError Theory.Key
read_key smap = lookup_key (smap_default_key smap) (smap_keys smap)

lookup_key :: key -> Map.Map Pitch.Key key -> Maybe Pitch.Key
    -> Either Scale.ScaleError key
lookup_key deflt _ Nothing = Right deflt
lookup_key _ keys (Just key) = maybe (Left err) Right $ Map.lookup key keys
    where err = Scale.UnparseableEnviron Environ.key (Pretty.prettytxt key)

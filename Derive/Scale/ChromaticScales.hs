-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for equal-tempered chromatic scales with keys and modes.
module Derive.Scale.ChromaticScales where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Util.Seq as Seq
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch
import Global


-- | This contains all that is needed to define a European-like key system.
-- It fills a similar role to 'Scales.ScaleMap' for non-keyed scales.
data ScaleMap = ScaleMap {
    smap_fmt :: !TheoryFormat.Format
    , smap_keys :: !Keys
    , smap_default_key :: !Theory.Key
    , smap_layout :: !Theory.Layout
    -- | Configure how the scale converts 'Pitch.Semi's to frequency.
    , smap_semis_to_nn :: SemisToNoteNumber
    -- | Inclusive (bottom, top) of scale, for documentation.
    , smap_range :: !(Pitch.Semi, Pitch.Semi)
    }

type SemisToNoteNumber = PSignal.PitchConfig -> Pitch.FSemi
    -> Either Scale.ScaleError Pitch.NoteNumber

twelve_doc :: Text
twelve_doc = "Scales in the \"twelve\" family use European style note naming.\
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
    , smap_semis_to_nn = \_config -> return . Theory.fsemis_to_nn
    , smap_range = range
    }
    where range = (Theory.nn_to_semis 1, Theory.nn_to_semis 127)

type Keys = Map.Map Pitch.Key Theory.Key

make_keys :: TheoryFormat.Format -> [Theory.Key] -> Keys
make_keys fmt keys =
    Map.fromList $ zip (map (TheoryFormat.show_key fmt) keys) keys

make_scale :: Pitch.ScaleId -> ScaleMap -> Text -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { Scale.scale_id = scale_id
    , Scale.scale_pattern = TheoryFormat.fmt_pattern (smap_fmt smap)
    , Scale.scale_symbols = []
    , Scale.scale_transposers = Scales.standard_transposers
    , Scale.scale_read = read_pitch smap . Scales.environ_key
    , Scale.scale_show = show_pitch smap . Scales.environ_key
    , Scale.scale_layout = Theory.layout_intervals (smap_layout smap)
    , Scale.scale_transpose = transpose smap
    , Scale.scale_enharmonics = enharmonics smap
    , Scale.scale_note_to_call = note_to_call scale smap
    , Scale.scale_input_to_note = input_to_note smap
    , Scale.scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note smap) (note_to_call scale smap)
    , Scale.scale_call_doc = call_doc Scales.standard_transposers smap doc
    }
    where scale = PSignal.Scale scale_id Scales.standard_transposers

-- * functions

transpose :: ScaleMap -> Derive.Transpose
transpose smap transposition env steps pitch = do
    key <- read_environ_key smap env
    return $ trans key steps pitch
    where
    trans = case transposition of
        Scale.Chromatic -> Theory.transpose_chromatic
        Scale.Diatonic -> Theory.transpose_diatonic

enharmonics :: ScaleMap -> Derive.Enharmonics
enharmonics smap env note = do
    pitch <- read_pitch smap (Scales.environ_key env) note
    key <- read_environ_key smap env
    return $ Either.rights $ map (show_pitch smap (Scales.environ_key env)) $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: PSignal.Scale -> ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call scale smap note =
    case TheoryFormat.read_unadjusted_pitch (smap_fmt smap) note of
        Left _ -> Nothing
        Right pitch -> Just $ ScaleDegree.scale_degree scale
            (pitch_nn smap pitch) (pitch_note smap pitch)

-- | Create a PitchNote for 'ScaleDegree.scale_degree'.
pitch_note :: ScaleMap -> Pitch.Pitch -> Scale.PitchNote
pitch_note smap pitch (PSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error diatonic chromatic $ do
        let maybe_key = Scales.environ_key env
        let d = round diatonic
            c = round chromatic
        show_pitch smap maybe_key =<< if d == 0 && c == 0
            then return pitch
            else do
                key <- read_key smap maybe_key
                return $ Theory.transpose_chromatic key c $
                    Theory.transpose_diatonic key d pitch
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

-- | Create a PitchNn for 'ScaleDegree.scale_degree'.
pitch_nn :: ScaleMap -> Pitch.Pitch -> Scale.PitchNn
pitch_nn smap pitch config@(PSignal.PitchConfig env controls) =
    Scales.scale_to_pitch_error diatonic chromatic $ do
        let maybe_key = Scales.environ_key env
        pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap) maybe_key pitch
        dsteps <- if diatonic == 0 then Right 0 else do
            key <- read_key smap maybe_key
            return $ Theory.diatonic_to_chromatic key
                (Pitch.pitch_degree pitch) diatonic
        let semis = Theory.pitch_to_semis (smap_layout smap) pitch
            degree = fromIntegral semis + chromatic + dsteps
        nn <- smap_semis_to_nn smap config degree
        if 1 <= nn && nn <= 127 then Right nn
            else Left Scale.InvalidTransposition
    where
    chromatic = Map.findWithDefault 0 Controls.chromatic controls
    diatonic = Map.findWithDefault 0 Controls.diatonic controls

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap env (Pitch.Input kbd_type pitch frac) = do
    pitch <- Scales.kbd_to_scale kbd_type pc_per_octave (key_tonic key) pitch
    unless (Theory.layout_contains_degree key (Pitch.pitch_degree pitch)) $
        Left Scale.InvalidInput
    -- Relative scales don't need to figure out enharmonic spelling, and
    -- besides it would be wrong since it assumes Pitch 0 0 is C.
    let pick_enharmonic = if TheoryFormat.fmt_relative (smap_fmt smap) then id
            else Theory.pick_enharmonic key
    -- Don't pass the key, because I want the Input to also be relative, i.e.
    -- Pitch 0 0 should be scale degree 0 no matter the key.
    note <- invalid_input $ show_pitch smap Nothing $ pick_enharmonic pitch
    return $ ScaleDegree.pitch_expr frac note
    where
    invalid_input (Left Scale.InvalidTransposition) = Left Scale.InvalidInput
    invalid_input x = x
    pc_per_octave = Theory.layout_pc_per_octave (smap_layout smap)
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (smap_default_key smap) $
        flip Map.lookup (smap_keys smap) =<< Scales.environ_key env

call_doc :: Set.Set Score.Control -> ScaleMap -> Text -> Derive.DocumentedCall
call_doc transposers smap doc =
    Scales.annotate_call_doc transposers extra_doc fields $
        Derive.extract_val_doc call
    where
    call = ScaleDegree.scale_degree PSignal.no_scale err err
        where err _ = Left $ PSignal.PitchError "it was just an example!"
    extra_doc = doc <> "\n" <> twelve_doc
    -- Not efficient, but shouldn't matter for docs.
    default_key = fst <$> List.find ((== smap_default_key smap) . snd)
        (Map.toList (smap_keys smap))
    (bottom, top) = smap_range smap
    show_p = either pretty pretty . show_pitch smap Nothing
        . Theory.semis_to_pitch_sharps (smap_layout smap)
    fields = concat
        [ [("range", show_p bottom <> " to " <> show_p top)]
        , maybe [] (\n -> [("default key", pretty n)]) default_key
        , [ ("keys", format_keys $ Map.keys (smap_keys smap)) ]
        ]

format_keys :: [Pitch.Key] -> Text
format_keys keys
    | all (("-" `Text.isInfixOf`) . name) keys = Text.intercalate ", " $
        map fst $ group_tonic_mode $ map (flip (,) ()) keys
    | otherwise = Text.intercalate ", " $ map name keys
    where name (Pitch.Key k) = k

-- | Assuming keys are formatted @tonic-mode@, group keys by mode and replace
-- the tonics with a pattern.
group_tonic_mode :: [(Pitch.Key, a)] -> [(Text, a)]
group_tonic_mode = map extract . Seq.keyed_group_sort key . map (first split)
    where
    extract (mode, group) = (fmt mode (map (fst . fst) group), snd (head group))
    key ((_, mode), _) = mode
    split (Pitch.Key t) = (pre, Text.drop 1 post)
        where (pre, post) = Text.break (=='-') t
    fmt mode keys = "(" <> Text.intercalate "|" keys <> ")-" <> mode

-- * format

relative_fmt :: Theory.Key -> Keys -> TheoryFormat.RelativeFormat Theory.Key
relative_fmt default_key all_keys  = TheoryFormat.RelativeFormat
    { TheoryFormat.rel_acc_fmt = TheoryFormat.ascii_accidentals
    , TheoryFormat.rel_parse_key = Scales.get_key default_key all_keys
    , TheoryFormat.rel_default_key = default_key
    , TheoryFormat.rel_show_degree = TheoryFormat.show_degree_chromatic
    , TheoryFormat.rel_to_absolute = TheoryFormat.chromatic_to_absolute
    , TheoryFormat.rel_key_tonic = key_tonic
    }

-- * implementation

key_tonic :: Theory.Key -> Pitch.PitchClass
key_tonic = Pitch.degree_pc . Theory.key_tonic

show_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either Scale.ScaleError Pitch.Note
show_pitch smap key pitch
    | bottom <= semis && semis <= top = Right $
        TheoryFormat.show_pitch (smap_fmt smap) key pitch
    | otherwise = Left Scale.InvalidTransposition
    where
    (bottom, top) = smap_range smap
    semis = Theory.pitch_to_semis (smap_layout smap) pitch

read_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Pitch.Pitch
read_pitch smap = TheoryFormat.read_pitch (smap_fmt smap)

read_environ_key :: ScaleMap -> TrackLang.Environ
    -> Either Scale.ScaleError Theory.Key
read_environ_key smap = Scales.get_key (smap_default_key smap) (smap_keys smap)
    . Scales.environ_key

read_key :: ScaleMap -> Maybe Pitch.Key -> Either Scale.ScaleError Theory.Key
read_key smap = Scales.get_key (smap_default_key smap) (smap_keys smap)

-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

-- | Utilities for equal-tempered chromatic scales with keys and modes.
module Derive.Scale.ChromaticScales where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Util.Doc as Doc
import qualified Util.Seq as Seq
import qualified Derive.BaseTypes as BaseTypes
import qualified Derive.Call.ScaleDegree as ScaleDegree
import qualified Derive.Controls as Controls
import qualified Derive.Derive as Derive
import qualified Derive.Env as Env
import qualified Derive.PSignal as PSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Scales as Scales
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Score as Score
import qualified Derive.ShowVal as ShowVal

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
    -- | Inclusive (bottom, top) of scale.
    , smap_range :: !(Pitch.Semi, Pitch.Semi)
    }

type SemisToNoteNumber = PSignal.PitchConfig -> Pitch.FSemi
    -> Either BaseTypes.PitchError Pitch.NoteNumber

twelve_doc :: Doc.Doc
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
    where range = (Theory.nn_to_semis 0, Theory.nn_to_semis 127)

type Keys = Map Pitch.Key Theory.Key

make_keys :: TheoryFormat.Format -> [Theory.Key] -> Keys
make_keys fmt keys =
    Map.fromList $ zip (map (TheoryFormat.show_key fmt) keys) keys

make_scale :: Pitch.ScaleId -> ScaleMap -> Doc.Doc -> Scale.Scale
make_scale scale_id smap doc = Scale.Scale
    { scale_id = scale_id
    , scale_pattern = TheoryFormat.fmt_pattern (smap_fmt smap)
    , scale_symbols = []
    , scale_transposers = Scales.standard_transposers
    , scale_read = read_pitch smap . Scales.environ_key
    , scale_show = show_pitch smap . Scales.environ_key
    , scale_bottom = Theory.semis_to_pitch_sharps (smap_layout smap)
        (fst (smap_range smap))
    , scale_layout = Theory.layout_intervals (smap_layout smap)
    , scale_transpose = transpose smap
    , scale_enharmonics = enharmonics smap
    , scale_note_to_call = note_to_call scale smap
    , scale_input_to_note = input_to_note smap
    , scale_input_to_nn = Scales.computed_input_to_nn
        (input_to_note smap) (note_to_call scale smap)
    , scale_call_doc = call_doc Scales.standard_transposers smap doc
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
    case TheoryFormat.read_relative_pitch (smap_fmt smap) note of
        Left _ -> Nothing
        Right relative -> Just $ ScaleDegree.scale_degree scale
            (pitch_nn smap relative) (pitch_note smap relative)

-- | Create a PitchNote for 'ScaleDegree.scale_degree'.
pitch_note :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNote
pitch_note smap relative (PSignal.PitchConfig env controls) = do
    -- Adjustment to absolute is only necessary for 'pitch_nn', since
    -- NoteNumbers are absolute.
    -- TODO I should leave as relative to preserve naturals
    let pitch = TheoryFormat.relative_to_absolute relative
    let maybe_key = Scales.environ_key env
    let c = round chromatic
        o = round octave
        d = round diatonic
    show_pitch smap maybe_key =<< if o == 0 && d == 0 && c == 0
        then return pitch
        else do
            key <- read_key smap maybe_key
            return $ Pitch.add_octave o $ Theory.transpose_chromatic key c $
                Theory.transpose_diatonic key d pitch
    where
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls

-- | Create a PitchNn for 'ScaleDegree.scale_degree'.
pitch_nn :: ScaleMap -> TheoryFormat.RelativePitch -> Scale.PitchNn
pitch_nn smap relative config@(PSignal.PitchConfig env controls) = do
    let maybe_key = Scales.environ_key env
    pitch <- TheoryFormat.fmt_to_absolute (smap_fmt smap) maybe_key relative
    dsteps <- if diatonic == 0 then Right 0 else do
        key <- read_key smap maybe_key
        return $ Theory.diatonic_to_chromatic key
            (Pitch.pitch_degree pitch) diatonic
    let semis = Theory.pitch_to_semis (smap_layout smap) pitch
        degree = octave * fromIntegral per_octave
            + fromIntegral semis + chromatic + dsteps
    smap_semis_to_nn smap config degree
    where
    octave = get Controls.octave
    chromatic = get Controls.chromatic
    diatonic = get Controls.diatonic
    get m = Map.findWithDefault 0 m controls
    per_octave = Theory.layout_semis_per_octave (smap_layout smap)

input_to_note :: ScaleMap -> Scales.InputToNote
input_to_note smap env (Pitch.Input kbd_type pitch frac) = do
    pitch <- Scales.kbd_to_scale kbd_type pc_per_octave (key_tonic key) pitch
    let intervals = if is_relative
            then Theory.key_intervals key
            else Theory.layout_intervals (smap_layout smap)
    unless (Theory.contains_degree intervals (Pitch.pitch_degree pitch)
            && in_range smap pitch) $
        Left BaseTypes.InvalidInput
    -- Relative scales don't need to figure out enharmonic spelling, and
    -- besides it would be wrong since it assumes Pitch 0 0 is C.
    let pick_enharmonic = if is_relative then id else Theory.pick_enharmonic key
    -- Don't pass the key, because I want the Input to also be relative, i.e.
    -- Pitch 0 0 should be scale degree 0 no matter the key.
    note <- invalid_input $ show_pitch smap
        (if is_relative then Nothing else (Scales.environ_key env))
        (pick_enharmonic pitch)
    return $ ScaleDegree.pitch_expr frac note
    where
    is_relative = TheoryFormat.fmt_relative (smap_fmt smap)
    invalid_input (Left (BaseTypes.OutOfRange {})) = Left BaseTypes.InvalidInput
    invalid_input x = x
    pc_per_octave = Theory.layout_pc_per_octave (smap_layout smap)
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (smap_default_key smap) $
        flip Map.lookup (smap_keys smap) =<< Scales.environ_key env

in_range :: ScaleMap -> Pitch.Pitch -> Bool
in_range smap pitch = bottom <= semis && semis <= top
    where
    (bottom, top) = smap_range smap
    semis = Theory.pitch_to_semis (smap_layout smap) pitch

call_doc :: Set Score.Control -> ScaleMap -> Doc.Doc -> Derive.DocumentedCall
call_doc transposers smap doc =
    Scales.annotate_call_doc transposers doc fields $
        Derive.extract_val_doc call
    where
    call = ScaleDegree.scale_degree PSignal.no_scale err err
        where err _ = Left $ PSignal.PitchError "it was just an example!"
    -- Not efficient, but shouldn't matter for docs.
    default_key = fst <$> List.find ((== smap_default_key smap) . snd)
        (Map.toList (smap_keys smap))
    (bottom, top) = smap_range smap
    show_p = either Doc.pretty Doc.pretty . show_pitch smap Nothing
        . Theory.semis_to_pitch_sharps (smap_layout smap)
    fields = concat
        [ [("range", show_p bottom <> " to " <> show_p top)]
        , maybe [] (\n -> [("default key", ShowVal.doc n)]) default_key
        , [ ("keys", format_keys $ Map.keys (smap_keys smap)) ]
        ]

format_keys :: [Pitch.Key] -> Doc.Doc
format_keys keys
    | all (("-" `Text.isInfixOf`) . Pitch.key_text) keys = Doc.commas $
        map (Doc.literal . fst) $ group_tonic_mode $ map (, ()) keys
    | otherwise = Doc.commas (map Doc.pretty keys)

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
    { rel_config = TheoryFormat.default_config
    , rel_key_config = TheoryFormat.KeyConfig
        { key_parse = Scales.get_key default_key all_keys
        , key_default = default_key
        }
    , rel_show_degree = TheoryFormat.show_degree_chromatic
    , rel_to_absolute = TheoryFormat.chromatic_to_absolute
    }

-- * implementation

key_tonic :: Theory.Key -> Pitch.PitchClass
key_tonic = Pitch.degree_pc . Theory.key_tonic

show_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Pitch
    -> Either BaseTypes.PitchError Pitch.Note
show_pitch smap key = Right . TheoryFormat.show_pitch (smap_fmt smap) key
    -- Previously this would check for OutOfRange, but it meant I couldn't
    -- transpose a pitch to out of range even if I was going to later transpose
    -- it back in range (say via octave wrapping).  Since the range is
    -- ultimately which degrees can be mapped to frequencies, it seems it
    -- doesn't hurt anything for the symbolic pitch to be theoretically
    -- boundless.

read_pitch :: ScaleMap -> Maybe Pitch.Key -> Pitch.Note
    -> Either BaseTypes.PitchError Pitch.Pitch
read_pitch smap = TheoryFormat.read_pitch (smap_fmt smap)

read_environ_key :: ScaleMap -> Env.Environ
    -> Either BaseTypes.PitchError Theory.Key
read_environ_key smap = Scales.get_key (smap_default_key smap) (smap_keys smap)
    . Scales.environ_key

read_key :: ScaleMap -> Maybe Pitch.Key
    -> Either BaseTypes.PitchError Theory.Key
read_key smap = Scales.get_key (smap_default_key smap) (smap_keys smap)

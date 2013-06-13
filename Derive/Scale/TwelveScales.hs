-- Copyright 2013 Evan Laforge
-- This program is distributed under the terms of the GNU General Public
-- License 3.0, see COPYING or http://www.gnu.org/licenses/gpl-3.0.txt

module Derive.Scale.TwelveScales where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Pretty as Pretty
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.TheoryFormat as TheoryFormat
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- | This contains all that is needed to define a western-like key system.
-- It fills a similar role to 'Derive.Call.Util.ScaleMap' for non-keyed scales.
--
-- TODO this will need to be extended to handle non-equal-tempered scales.
data ScaleMap = ScaleMap {
    smap_fmt :: !TheoryFormat.Format
    , smap_pitch_to_degree :: Theory.Pitch -> Pitch.Degree
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
    , smap_pitch_to_degree =
        Pitch.Degree . Theory.semis_to_nn . Theory.pitch_to_semis layout
    , smap_keys = keys
    , smap_default_key = default_key
    , smap_layout = layout
    }

type Keys = Map.Map Pitch.Key Theory.Key


-- * functions

transpose :: ScaleMap -> Derive.Transpose
transpose smap maybe_key octaves steps note = do
    key <- read_key smap maybe_key
    pitch <- Theory.modify_octave (+octaves) <$> read_pitch smap note
    case steps of
        Pitch.Chromatic steps -> show_pitch smap maybe_key $
            Theory.transpose_chromatic key (floor steps) pitch
        Pitch.Diatonic steps -> show_pitch smap maybe_key $
            Theory.transpose_diatonic key (floor steps) pitch

enharmonics :: ScaleMap -> Derive.Enharmonics
enharmonics smap maybe_key note = do
    key <- read_key smap maybe_key
    pitch <- read_pitch smap note
    return $ Either.rights $ map (show_pitch smap maybe_key) $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call smap note = case TheoryFormat.read_pitch (smap_fmt smap) note of
    Left _ -> Nothing
    Right pitch ->
        Just $ Call.Pitch.scale_degree (pitch_nn pitch) (pitch_note pitch)
    where
    convert_error = Util.scale_to_pitch_error
    pitch_nn :: Theory.Pitch -> Scale.PitchNn
    pitch_nn pitch env controls = convert_error diatonic chromatic $ do
        pitch <- TheoryFormat.fmt_adjust (smap_fmt smap)
            (Util.lookup_key env) pitch
        dsteps <- if diatonic == 0 then Right 0 else do
            key <- read_env_key smap env
            return $ Theory.diatonic_to_chromatic key
                (Theory.pitch_note pitch) diatonic
        let degree = smap_pitch_to_degree smap pitch
        let nn = Pitch.NoteNumber $ fromIntegral degree + chromatic + dsteps
        if 1 <= nn && nn <= 127 then Right nn
            else Left Scale.InvalidTransposition
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls

    pitch_note :: Theory.Pitch -> Scale.PitchNote
    pitch_note pitch env controls = convert_error diatonic chromatic $ do
        let d = round diatonic
            c = round chromatic
        show_pitch smap (Util.lookup_key env) =<< if d == 0 && c == 0
            then return pitch
            else do
                key <- read_env_key smap env
                return $ Theory.transpose_chromatic key c $
                    Theory.transpose_diatonic key d pitch
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls

input_to_note :: ScaleMap
    -> Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note smap maybe_key (Pitch.InputKey key_nn) =
    case show_pitch smap maybe_key $
            Theory.semis_to_pitch key (Theory.nn_to_semis nn_semis) of
        Left _ -> Nothing
        Right note -> Just $ Pitch.Note $ Call.Pitch.pitch_expr note cents
    where
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (smap_default_key smap) $
        flip Map.lookup (smap_keys smap) =<< maybe_key
    (nn_semis, cents) = properFraction key_nn

call_doc :: Set.Set Score.Control -> ScaleMap -> Text -> Derive.DocumentedCall
call_doc transposers smap doc =
    Util.annotate_call_doc transposers extra_doc fields $
        Derive.extract_val_doc call
    where
    call = Call.Pitch.scale_degree err err
        where err _ _ = Left $ PitchSignal.PitchError "it was just an example!"
    extra_doc = doc <> "\n" <> twelve_doc
    fields =
        [ ("default key", Pretty.prettytxt $
            TheoryFormat.show_key (smap_fmt smap) (smap_default_key smap))
        , ("keys", Text.intercalate ", " $
            map Pretty.prettytxt (Map.keys (smap_keys smap)))
        ]

-- * implementation

read_note :: ScaleMap -> Maybe Pitch.Key -> Pitch.Note
    -> Either Scale.ScaleError Theory.Pitch
read_note smap key =
    TheoryFormat.fmt_adjust fmt key <=< TheoryFormat.read_pitch fmt
    where fmt = smap_fmt smap

show_pitch :: ScaleMap -> Maybe Pitch.Key -> Theory.Pitch
    -> Either Scale.ScaleError Pitch.Note
show_pitch smap key pitch
    | 1 <= nn && nn <= 127 =
        Right $ TheoryFormat.show_pitch (smap_fmt smap) key pitch
    | otherwise = Left Scale.InvalidTransposition
    where
    nn = Theory.semis_to_nn $ Theory.pitch_to_semis (smap_layout smap) pitch

read_pitch :: ScaleMap -> Pitch.Note -> Either Scale.ScaleError Theory.Pitch
read_pitch = TheoryFormat.read_pitch . smap_fmt

read_env_key :: ScaleMap -> TrackLang.Environ
    -> Either Scale.ScaleError Theory.Key
read_env_key smap = Util.read_environ
    (\k -> Map.lookup (Pitch.Key k) (smap_keys smap))
    (smap_default_key smap) Environ.key

read_key :: ScaleMap -> Maybe Pitch.Key -> Either Scale.ScaleError Theory.Key
read_key smap Nothing = Right (smap_default_key smap)
read_key smap (Just key) =
    maybe (Left err) Right $ Map.lookup key (smap_keys smap)
    where err = Scale.UnparseableEnviron Environ.key (txt (Pretty.pretty key))

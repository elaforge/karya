module Derive.Scale.TwelveScales where
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Util.Control
import qualified Util.Num as Num
import qualified Util.Pretty as Pretty
import qualified Util.Seq as Seq

import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Environ as Environ
import qualified Derive.PitchSignal as PitchSignal
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory
import qualified Derive.Scale.Util as Util
import qualified Derive.Score as Score
import qualified Derive.TrackLang as TrackLang

import qualified Perform.Pitch as Pitch


-- | This contains all that is needed to define a western-like key system.
-- It fills a similar role to 'Derive.Call.Util.ScaleMap' for non-keyed scales.
--
-- TODO this will need to be extended to handle non-equal-tempered scales.
data ScaleMap = ScaleMap {
    smap_note_to_degree :: NoteToDegree
    , smap_keys :: Keys
    , smap_default_key :: Theory.Key
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

scale_map :: Theory.Layout -> [Theory.Pitch] -> Keys -> Theory.Key -> ScaleMap
scale_map layout pitches keys default_key =
    ScaleMap (make_note_to_degree layout pitches) keys default_key

type NoteToDegree = Map.Map Pitch.Note (Theory.Pitch, Pitch.Degree)
type Keys = Map.Map Pitch.Key Theory.Key


-- * functions

transpose :: ScaleMap -> Derive.Transpose
transpose smap maybe_key octaves steps note = do
    key <- read_key smap maybe_key
    pitch <- Theory.modify_octave (+octaves) <$>
        read_pitch (Theory.key_layout key) note
    case steps of
        Pitch.Chromatic steps -> pitch_note smap $
            Theory.transpose_chromatic key (floor steps) pitch
        Pitch.Diatonic steps -> pitch_note smap $
            Theory.transpose_diatonic key (floor steps) pitch

enharmonics :: ScaleMap -> Derive.Enharmonics
enharmonics smap maybe_key note = do
    key <- read_key smap maybe_key
    pitch <- read_pitch (Theory.key_layout key) note
    return $ Either.rights $ map (pitch_note smap) $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: ScaleMap -> Pitch.Note -> Maybe Derive.ValCall
note_to_call smap note = case Map.lookup note (smap_note_to_degree smap) of
    Nothing -> Nothing
    Just (pitch, degree) ->
        Just $ Call.Pitch.scale_degree (pitch_nn pitch degree)
            (pitch_note pitch)
    where
    -- The Degree can be derived from the Pitch given the layout, so it's
    -- redundant to pass both, but convenient to precompute the Degrees.
    pitch_nn :: Theory.Pitch -> Pitch.Degree -> Scale.PitchNn
    pitch_nn pitch (Pitch.Degree degree) env controls =
        Util.scale_to_pitch_error diatonic chromatic $ do
            dsteps <- if diatonic == 0 then Right 0 else do
                key <- read_env_key smap env
                return $ Theory.diatonic_to_chromatic key
                    (Theory.pitch_note pitch) diatonic
            let nn = Pitch.NoteNumber $ fromIntegral degree + chromatic + dsteps
            if Num.in_range 1 127 nn then Right nn
                else Left Scale.InvalidTransposition
        where
        chromatic = Map.findWithDefault 0 Score.c_chromatic controls
        diatonic = Map.findWithDefault 0 Score.c_diatonic controls

    pitch_note :: Theory.Pitch -> Scale.PitchNote
    pitch_note pitch env controls =
        Util.scale_to_pitch_error diatonic chromatic $ do
            let d = round diatonic
                c = round chromatic
            show_pitch <$> if d == 0 && c == 0
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
    case pitch_note smap $
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
        [ ("note range", note_range)
        , ("default key", txt $ Pretty.pretty $
            Theory.show_key (smap_default_key smap))
        , ("keys", Text.intercalate ", " $
            map (txt . Pretty.pretty) (Map.keys (smap_keys smap)))
        ]
    note_range = case (Seq.minimum_on (snd . snd) notes,
            Seq.maximum_on (snd . snd) notes) of
        (Just (note1, _), Just (note2, _)) ->
            txt (Pretty.pretty note1) <> " to " <> txt (Pretty.pretty note2)
        _ -> ""
    notes = Map.toList (smap_note_to_degree smap)

-- * implementation

make_note_to_degree :: Theory.Layout -> [Theory.Pitch] -> NoteToDegree
make_note_to_degree layout all_pitches = Map.fromList $ filter in_range $
    concat [[note "#" "x" "b" "bb" p, note "`#`" "`##`" "`b`" "`bb`" p]
        | p <- all_pitches]
    where
    note s s2 f f2 p = (Pitch.Note $ Theory.show_pitch s s2 f f2 p,
        (p, Pitch.Degree $ Theory.semis_to_nn $ Theory.pitch_to_semis layout p))
    in_range = Num.in_range 1 128 . snd . snd

-- | Don't emit a 'Pitch.Note' that's out of range, because it won't be
-- recognized when it comes time to play it back.
pitch_note :: ScaleMap -> Theory.Pitch
    -> Either Scale.ScaleError Pitch.Note
pitch_note smap pitch
    | Map.member note (smap_note_to_degree smap) = Right note
    | otherwise = Left Scale.InvalidTransposition
    where note = show_pitch pitch

show_pitch :: Theory.Pitch -> Pitch.Note
show_pitch = Pitch.Note . Theory.show_pitch "#" "x" "b" "bb"

read_env_key :: ScaleMap -> TrackLang.Environ
    -> Either Scale.ScaleError Theory.Key
read_env_key smap = Util.read_environ
    (\k -> Map.lookup (Pitch.Key k) (smap_keys smap))
    (smap_default_key smap) Environ.key

read_key :: ScaleMap -> Maybe Pitch.Key -> Either Scale.ScaleError Theory.Key
read_key smap Nothing = Right (smap_default_key smap)
read_key smap (Just key) =
    maybe (Left err) Right $ Map.lookup key (smap_keys smap)
    where
    err = Scale.UnparseableEnviron Environ.key (txt (Pretty.pretty key))

read_pitch :: Theory.Layout -> Pitch.Note
    -> Either Scale.ScaleError Theory.Pitch
read_pitch layout note = maybe (Left Scale.UnparseableNote) Right $
    Theory.read_pitch layout (Pitch.note_text note)

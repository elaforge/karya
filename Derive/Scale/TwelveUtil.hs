module Derive.Scale.TwelveUtil where
import qualified Data.Either as Either
import qualified Data.Map as Map

import Util.Control
import qualified Util.Num as Num
import qualified Ui.Track as Track
import qualified Derive.Call.Pitch as Call.Pitch
import qualified Derive.Derive as Derive
import qualified Derive.Scale as Scale
import qualified Derive.Scale.Theory as Theory

import qualified Perform.Pitch as Pitch


-- | This contains all that is needed to define a western-like key system.
-- It fills a similar role to 'Derive.Call.Util.ScaleMap' for non-keyed scales.
--
-- TODO the name is crummy.
--
-- TODO this will need to be extended to handle non-equal-tempered scales.
data System = System {
    sys_note_to_degree :: NoteToDegree
    , sys_keys :: Keys
    , sys_default_key :: Theory.Key
    }

system :: Theory.Layout -> [Theory.Pitch] -> Keys -> Theory.Key -> System
system layout pitches keys default_key =
    System (make_note_to_degree layout pitches) keys default_key

type NoteToDegree = Map.Map Pitch.Note (Theory.Pitch, Pitch.Degree)
type Keys = Map.Map Pitch.Key Theory.Key


-- * functions

scale_map :: System -> Track.ScaleMap
scale_map sys =
    Track.make_scale_map [(Pitch.note_text n, fromIntegral d)
        | (n, (_, d)) <- Map.toList (sys_note_to_degree sys)]

transpose :: System -> Derive.Transpose
transpose sys maybe_key octaves steps note = do
    key <- read_key sys maybe_key
    pitch <- Theory.modify_octave (+octaves) <$> read_pitch key note
    case steps of
        Pitch.Chromatic steps -> pitch_note sys $
            Theory.transpose_chromatic key (floor steps) pitch
        Pitch.Diatonic steps -> pitch_note sys $
            Theory.transpose_diatonic key (floor steps) pitch

enharmonics :: System -> Derive.Enharmonics
enharmonics sys maybe_key note = do
    key <- read_key sys maybe_key
    pitch <- read_pitch key note
    return $ Either.rights $ map (pitch_note sys) $
        Theory.enharmonics_of (Theory.key_layout key) pitch

note_to_call :: System -> Pitch.Note -> Maybe Derive.ValCall
note_to_call sys note = case Map.lookup note (sys_note_to_degree sys) of
    Nothing -> Nothing
    Just (pitch, degree) ->
        Just $ Call.Pitch.note_call note (note_number pitch degree)
    where
    -- The Degree can be derived from the Pitch given the layout, so it's
    -- redundant to pass both, but convenient to precompute the Degrees.
    note_number :: Theory.Pitch -> Pitch.Degree -> Scale.GetNoteNumber
    note_number pitch (Pitch.Degree degree) chromatic diatonic mb_str_key = do
        dsteps <- if diatonic == 0 then Right 0 else do
            str_key <- maybe (Left Scale.KeyNeeded) Right mb_str_key
            key <- read_key sys (Just str_key)
            return $ Theory.diatonic_to_chromatic key
                (Theory.pitch_note pitch) diatonic
        let nn = Pitch.NoteNumber $ fromIntegral degree + chromatic + dsteps
        if Num.in_range 1 127 nn then Right nn
            else Left Scale.InvalidTransposition

input_to_note :: System
    -> Maybe Pitch.Key -> Pitch.InputKey -> Maybe Pitch.Note
input_to_note sys maybe_key (Pitch.InputKey key_nn) =
    case pitch_note sys $ Theory.semis_to_pitch key degree of
        Left _ -> Nothing
        Right note -> Just $ Pitch.Note $ Call.Pitch.note_expr note cents
    where
    -- Default to a key because otherwise you couldn't enter notes in an
    -- empty score!
    key = fromMaybe (sys_default_key sys) $
        flip Map.lookup (sys_keys sys) =<< maybe_key
    (degree, cents) = properFraction key_nn

input_to_nn :: Pitch.InputKey -> Maybe Pitch.NoteNumber
input_to_nn (Pitch.InputKey nn) = Just (Pitch.NoteNumber nn)


-- * implementation

make_note_to_degree :: Theory.Layout -> [Theory.Pitch] -> NoteToDegree
make_note_to_degree layout all_pitches = Map.fromList $ filter in_range $
    concat $ [[note "#" "x" "b" "bb" p, note "`#`" "`##`" "`b`" "`bb`" p]
        | p <- all_pitches]
    where
    note s s2 f f2 p = (Pitch.Note $ Theory.show_pitch s s2 f f2 p,
        (p, Pitch.Degree $ Theory.pitch_to_semis layout p))
    in_range = Num.in_range 1 128 . snd . snd

-- | Don't emit a 'Pitch.Note' that's out of range, because it won't be
-- recognized when it comes time to play it back.
pitch_note :: System -> Theory.Pitch
    -> Either Scale.ScaleError Pitch.Note
pitch_note sys pitch
    | Map.member note (sys_note_to_degree sys) = Right note
    | otherwise = Left Scale.InvalidTransposition
    where note = show_pitch pitch

show_pitch :: Theory.Pitch -> Pitch.Note
show_pitch = Pitch.Note . Theory.show_pitch "#" "x" "b" "bb"

read_key :: System -> Maybe Pitch.Key -> Either Scale.ScaleError Theory.Key
read_key sys Nothing = Right (sys_default_key sys)
read_key sys (Just key) = maybe (Left Scale.UnparseableKey) Right $
    Map.lookup key (sys_keys sys)

read_pitch :: Theory.Key -> Pitch.Note -> Either Scale.ScaleError Theory.Pitch
read_pitch key note = maybe (Left Scale.UnparseableNote) Right $
    Theory.read_pitch (Theory.key_layout key) (Pitch.note_text note)
